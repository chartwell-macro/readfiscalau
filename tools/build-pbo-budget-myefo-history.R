#!/usr/bin/env Rscript

# Maintainer-only deterministic builder for chart-ready PBO historical Budget,
# MYEFO and PEFO forecast vintages. The caller supplies the exact reviewed PBO
# workbook; the script performs no network access.

fail <- function(...) stop(paste0(...), call. = FALSE)

parse_args <- function(args) {
  out <- list(workbook = NULL, output = NULL)
  for (arg in args) {
    if (startsWith(arg, "--workbook=")) {
      out$workbook <- sub("^--workbook=", "", arg)
    } else if (startsWith(arg, "--output=")) {
      out$output <- sub("^--output=", "", arg)
    } else {
      fail("Unknown argument: ", arg)
    }
  }
  if (is.null(out$workbook) || is.null(out$output)) {
    fail("Required: --workbook=PATH --output=RDS")
  }
  out
}

source_contract <- data.frame(
  snapshot_id = "pbo-budget-myefo-forecast-history-2026-27-budget",
  source_url = paste0(
    "https://www.pbo.gov.au/sites/default/files/2026-05/",
    "2026-27%20Budget%20-%20Historical%20budget%20forecasts%20",
    "and%20measures.xlsx"
  ),
  source_sha256 = paste0(
    "0fec89ace2845bc7c0c5c6712c16a3f75681ca082886068ccd81cbc64",
    "513aba0"
  ),
  workbook_edition = "2026-27 Budget",
  source_date = as.Date("2026-05-13"),
  stringsAsFactors = FALSE
)

slug <- function(value) {
  value <- tolower(trimws(as.character(value)))
  value <- gsub("[^a-z0-9]+", "-", value)
  gsub("^-|-$", "", value)
}

unit_contract <- function(unit, aggregate) {
  if (identical(unit, "$m")) {
    return(list(unit = "AUD million", multiplier = 1e6))
  }
  if (identical(unit, "% of GDP")) {
    return(list(unit = "% of GDP", multiplier = 0.01))
  }
  if (startsWith(unit, "%")) {
    semantic_unit <- if (grepl(
      "unemployment rate|bond yield",
      aggregate,
      ignore.case = TRUE
    )) {
      "Per cent"
    } else {
      "Per cent change"
    }
    return(list(unit = semantic_unit, multiplier = 0.01))
  }
  fail("Unsupported PBO unit: ", unit)
}

publication_type <- function(update_label) {
  if (grepl("MYEFO", update_label, fixed = TRUE)) return("myefo")
  if (grepl("PEFO|Post-Election", update_label, ignore.case = TRUE)) {
    return("pefo")
  }
  if (grepl("Budget", update_label, fixed = TRUE)) return("budget")
  "other"
}

budget_vintage <- function(update_label) {
  matched <- regmatches(
    update_label,
    regexpr("(?:19|20)[0-9]{2}-[0-9]{2}", update_label, perl = TRUE)
  )
  if (length(matched) && nzchar(matched)) return(matched)
  matched <- regmatches(
    update_label,
    regexpr("(?:19|20)[0-9]{2}", update_label, perl = TRUE)
  )
  if (!length(matched) || !nzchar(matched)) return(NA_character_)
  matched
}

read_pbo_sheet <- function(path, sheet) {
  out <- suppressMessages(readxl::read_excel(path, sheet = sheet, skip = 2))
  names(out) <- trimws(names(out))
  out
}

parse_sheet <- function(path, sheet, table_id, table_label, row_filter) {
  input <- read_pbo_sheet(path, sheet)
  required <- c("Aggregate", "Unit", "Budget update", "Date of publication")
  if (!all(required %in% names(input))) {
    fail(sheet, " does not have the expected PBO column contract.")
  }
  input$.source_row <- seq_len(nrow(input)) + 3L
  input <- input[row_filter(input), , drop = FALSE]
  fiscal_columns <- grep(
    "^(?:19|20)[0-9]{2}-[0-9]{2}$",
    names(input),
    value = TRUE
  )
  if (!length(fiscal_columns)) fail(sheet, " has no fiscal-year columns.")

  rows <- vector("list", nrow(input))
  cursor <- 0L
  for (index in seq_len(nrow(input))) {
    aggregate <- trimws(as.character(input$Aggregate[[index]]))
    published_unit <- trimws(as.character(input$Unit[[index]]))
    update_label <- trimws(as.character(input$`Budget update`[[index]]))
    release_date <- as.Date(input$`Date of publication`[[index]])
    if (anyNA(c(aggregate, published_unit, update_label, release_date)) ||
        !nzchar(aggregate) || !nzchar(published_unit) ||
        !nzchar(update_label) || identical(update_label, "Historical actuals")) {
      next
    }
    publication <- publication_type(update_label)
    vintage <- budget_vintage(update_label)
    if (is.na(vintage)) next
    unit <- unit_contract(published_unit, aggregate)
    values <- suppressWarnings(as.numeric(input[index, fiscal_columns]))
    keep <- is.finite(values)
    if (!any(keep)) next
    years <- fiscal_columns[keep]
    periods <- as.Date(paste0(
      as.integer(substr(years, 1L, 4L)) + 1L,
      "-06-30"
    ))
    actual_forecast <- ifelse(
      periods < release_date,
      "actual",
      "forecast"
    )
    accounting_basis <- if ("Accounting" %in% names(input)) {
      tolower(trimws(as.character(input$Accounting[[index]])))
    } else if (identical(table_id, "pbo-economic-forecasts")) {
      "not_applicable"
    } else {
      "cash"
    }
    if (is.na(accounting_basis) || !nzchar(accounting_basis)) {
      accounting_basis <- "not_applicable"
    }
    display_series <- if (identical(
      aggregate,
      "Nominal GDP"
    ) && identical(unit$unit, "AUD million")) {
      "Nominal GDP level"
    } else {
      sub(" growth$", "", aggregate, ignore.case = TRUE)
    }
    cursor <- cursor + 1L
    rows[[cursor]] <- data.frame(
      snapshot_id = source_contract$snapshot_id,
      release_id = paste0(
        "pbo-forecast-vintage-", slug(update_label), "-",
        format(release_date, "%Y%m%d")
      ),
      publication = publication,
      budget_vintage = vintage,
      update_label = update_label,
      release_date = release_date,
      table_id = table_id,
      table_label = table_label,
      source_table = sheet,
      source_row = as.integer(input$.source_row[[index]]),
      note = if ("Note" %in% names(input)) {
        as.character(input$Note[[index]])
      } else {
        NA_character_
      },
      series_id = paste0("pbo-forecast--", slug(aggregate)),
      series = display_series,
      aggregate = aggregate,
      fiscal_year = years,
      period_end = periods,
      value = values[keep],
      unit = unit$unit,
      unit_multiplier = unit$multiplier,
      accounting_basis = accounting_basis,
      consolidation = if (identical(table_id, "pbo-economic-forecasts")) {
        "australian_economy"
      } else {
        "general_government_sector"
      },
      actual_forecast = actual_forecast,
      profile_type = ifelse(
        actual_forecast == "actual",
        "actual",
        publication
      ),
      source_url = source_contract$source_url,
      source_sha256 = source_contract$source_sha256,
      stringsAsFactors = FALSE
    )
  }
  if (!cursor) return(NULL)
  do.call(rbind, rows[seq_len(cursor)])
}

profile_id <- function(row) {
  multiplier <- writeBin(
    as.double(row$unit_multiplier[[1L]]),
    raw(),
    size = 8L,
    endian = "big"
  )
  fields <- c(
    "pbo_budget_myefo_profile_v1",
    row$release_id[[1L]],
    row$series_id[[1L]],
    row$unit[[1L]],
    paste(sprintf("%02x", as.integer(multiplier)), collapse = ""),
    row$accounting_basis[[1L]],
    row$consolidation[[1L]],
    row$profile_type[[1L]]
  )
  paste0(
    "pbo-budget-myefo-profile-v1-",
    digest::digest(
      paste(fields, collapse = "\r"),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

options <- parse_args(commandArgs(trailingOnly = TRUE))
if (!file.exists(options$workbook)) fail("PBO workbook does not exist.")
observed_hash <- digest::digest(
  file = options$workbook,
  algo = "sha256",
  serialize = FALSE
)
if (!identical(observed_hash, source_contract$source_sha256[[1L]])) {
  fail("PBO workbook SHA-256 does not match the reviewed source contract.")
}

observations <- do.call(rbind, Filter(Negate(is.null), list(
  parse_sheet(
    options$workbook,
    "Table 1",
    "pbo-medium-term-projections",
    "Medium-term projections",
    function(data) data$Aggregate %in% c(
      "Gross debt", "Net debt", "Net financial worth"
    )
  ),
  parse_sheet(
    options$workbook,
    "Table 2",
    "pbo-budget-estimates",
    "Budget estimates",
    function(data) rep(TRUE, nrow(data))
  ),
  parse_sheet(
    options$workbook,
    "Table 5",
    "pbo-economic-forecasts",
    "Economic forecasts",
    function(data) rep(TRUE, nrow(data))
  )
)))

observations <- observations[order(
  observations$release_date,
  observations$release_id,
  observations$series_id,
  observations$unit,
  observations$accounting_basis,
  observations$period_end,
  observations$source_table,
  observations$source_row
), , drop = FALSE]
observation_key <- paste(
  observations$release_id,
  observations$series_id,
  observations$unit,
  sprintf("%.17g", observations$unit_multiplier),
  observations$accounting_basis,
  observations$consolidation,
  observations$actual_forecast,
  observations$profile_type,
  observations$period_end,
  sep = "\r"
)
duplicate_groups <- split(seq_len(nrow(observations)), observation_key)
conflicts <- vapply(duplicate_groups, function(index) {
  length(index) > 1L && length(unique(observations$value[index])) > 1L
}, logical(1))
if (any(conflicts)) {
  fail(
    "PBO workbook has conflicting chart-ready observations for ",
    sum(conflicts), " logical date(s)."
  )
}
observations$is_preferred <- !duplicated(observation_key)

forecast <- observations[
  observations$actual_forecast == "forecast" & observations$is_preferred,
  ,
  drop = FALSE
]
profile_key <- paste(
  forecast$release_id,
  forecast$series_id,
  forecast$unit,
  sprintf("%.17g", forecast$unit_multiplier),
  forecast$accounting_basis,
  forecast$consolidation,
  forecast$profile_type,
  sep = "\r"
)
profile_groups <- split(seq_len(nrow(forecast)), profile_key)
profiles <- do.call(rbind, lapply(profile_groups, function(index) {
  rows <- forecast[index, , drop = FALSE]
  rows <- rows[order(rows$period_end), , drop = FALSE]
  first <- rows[1L, , drop = FALSE]
  first$profile_id <- profile_id(first)
  data.frame(
    profile_id = first$profile_id,
    snapshot_schema = "pbo_budget_myefo_history_v1",
    snapshot_id = first$snapshot_id,
    release_id = first$release_id,
    publication = first$publication,
    budget_vintage = first$budget_vintage,
    update_label = first$update_label,
    release_date = as.Date(first$release_date),
    series_id = first$series_id,
    series = rows$series[[nrow(rows)]],
    series_labels = paste(unique(rows$series), collapse = " | "),
    table_ids = paste(unique(rows$table_id), collapse = " | "),
    table_labels = paste(unique(rows$table_label), collapse = " | "),
    source_tables = paste(unique(rows$source_table), collapse = " | "),
    profile_type = first$profile_type,
    actual_forecast = "forecast",
    unit = first$unit,
    unit_multiplier = first$unit_multiplier,
    accounting_basis = first$accounting_basis,
    consolidation = first$consolidation,
    first_period = min(rows$period_end),
    last_period = max(rows$period_end),
    observation_count = as.integer(nrow(rows)),
    source_url = first$source_url,
    source_sha256 = first$source_sha256,
    stringsAsFactors = FALSE
  )
}))
profiles <- profiles[order(
  profiles$release_date,
  profiles$release_id,
  profiles$series_id,
  profiles$unit,
  profiles$accounting_basis
), , drop = FALSE]
row.names(profiles) <- NULL
row.names(observations) <- NULL

profile_dates <- paste(
  forecast$release_id,
  forecast$series_id,
  forecast$unit,
  sprintf("%.17g", forecast$unit_multiplier),
  forecast$accounting_basis,
  forecast$consolidation,
  forecast$profile_type,
  forecast$period_end,
  sep = "\r"
)
if (!nrow(observations) || !nrow(profiles) || anyDuplicated(profiles$profile_id) ||
    anyDuplicated(profile_dates) || anyNA(observations$value) ||
    anyNA(observations$period_end) || anyNA(profiles$first_period) ||
    any(profiles$first_period > profiles$last_period) ||
    sum(profiles$observation_count) != nrow(forecast) ||
    any(!profiles$publication %in% c("budget", "myefo", "pefo", "other")) ||
    any(!grepl(
      "^pbo-budget-myefo-profile-v1-[0-9a-f]{64}$",
      profiles$profile_id
    ))) {
  fail("Generated PBO Budget/MYEFO snapshot failed its contract.")
}

store <- list(
  schema_version = "pbo_budget_myefo_history_v1",
  built_from = list(
    builder = "tools/build-pbo-budget-myefo-history.R",
    source_sha256 = observed_hash
  ),
  source = source_contract,
  profiles = profiles,
  observations = observations,
  validation = list(
    observation_count = nrow(observations),
    preferred_forecast_observation_count = nrow(forecast),
    profile_count = nrow(profiles),
    release_count = length(unique(profiles$release_id)),
    publication_counts = table(profiles$publication),
    table_counts = table(observations$table_id),
    superseded_duplicate_count = sum(!observations$is_preferred)
  )
)
parent <- dirname(options$output)
if (!dir.exists(parent) &&
    !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
  fail("Could not create output directory: ", parent)
}
saveRDS(store, options$output, version = 3, compress = "xz")
message(
  "Wrote ", nrow(profiles), " PBO profiles and ",
  nrow(observations), " workbook observations: ", options$output
)
