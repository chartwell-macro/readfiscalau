`%||%` <- function(x, y) if (is.null(x)) y else x

fail <- function(...) {
  message(...)
  quit(status = 1L)
}

usage <- function(status = 0L) {
  message(
    "Usage: Rscript tools/build-myefo-pbo-observations.R ",
    "--pbo-workbook=PATH [--publication=myefo|budget] ",
    "[--input=PATH] [--output=PATH]"
  )
  quit(status = status)
}

parse_args <- function(args) {
  out <- list(
    pbo_workbook = NULL,
    publication = "myefo",
    input = NULL,
    output = NULL
  )
  for (arg in args) {
    if (identical(arg, "--help")) usage()
    if (startsWith(arg, "--pbo-workbook=")) {
      out$pbo_workbook <- sub("^--pbo-workbook=", "", arg)
    } else if (startsWith(arg, "--publication=")) {
      out$publication <- sub("^--publication=", "", arg)
    } else if (startsWith(arg, "--input=")) {
      out$input <- sub("^--input=", "", arg)
    } else if (startsWith(arg, "--output=")) {
      out$output <- sub("^--output=", "", arg)
    } else {
      fail("Unknown argument: ", arg)
    }
  }
  if (is.null(out$pbo_workbook) || !nzchar(out$pbo_workbook)) {
    fail("--pbo-workbook=PATH is required")
  }
  if (!out$publication %in% c("myefo", "budget")) {
    fail("--publication must be `myefo` or `budget`")
  }
  default_panel <- file.path(
    "inst",
    "extdata",
    paste0(out$publication, "-vintage-observations.csv")
  )
  out$input <- out$input %||% default_panel
  out$output <- out$output %||% default_panel
  out
}

slug <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-|-$", "", x)
}

treasury_release_dates <- function(publication) {
  if (identical(publication, "myefo")) {
    vintages <- c(
      "2000-01", "2001-02", "2002-03", "2003-04", "2004-05",
      "2005-06", "2006-07", "2007-08", "2008-09", "2009-10",
      "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
      "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
      "2020-21", "2021-22", "2023-24", "2024-25", "2025-26"
    )
    dates <- c(
      "2000-11-15", "2001-10-16", "2002-11-27", "2003-12-08",
      "2004-12-17", "2005-12-14", "2006-12-20", "2007-10-15",
      "2008-11-05", "2009-11-02",
      "2010-11-09", "2011-11-09", "2012-10-22", "2013-12-17",
      "2014-12-15", "2015-12-15", "2016-12-19", "2017-12-18",
      "2018-12-17", "2019-12-16", "2020-12-17", "2021-12-16",
      "2023-12-13", "2024-12-18", "2025-12-17"
    )
    return(data.frame(
      release_id = paste0("treasury-myefo-", vintages, "-final"),
      budget_vintage = vintages,
      update_label = paste(vintages, "MYEFO"),
      release_date = as.Date(dates),
      stringsAsFactors = FALSE
    ))
  }
  vintages <- c(
    "2000-01", "2001-02", "2002-03", "2003-04", "2004-05",
    "2005-06", "2006-07", "2007-08", "2008-09", "2009-10",
    "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
    "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
    "2020-21", "2021-22", "2022-23", "2022-23-october",
    "2023-24", "2024-25", "2025-26", "2026-27"
  )
  release_ids <- paste0("treasury-budget-", vintages, "-final")
  release_ids[[23L]] <- "treasury-budget-2022-23-march-final"
  update_labels <- paste(vintages, "Budget")
  update_labels[[23L]] <- "2022-23 Budget"
  update_labels[[24L]] <- "2022-23 October Budget"
  data.frame(
    release_id = release_ids,
    budget_vintage = vintages,
    update_label = update_labels,
    release_date = as.Date(c(
      "2000-05-09", "2001-05-22", "2002-05-14", "2003-05-13",
      "2004-05-11", "2005-05-10", "2006-05-09", "2007-05-08",
      "2008-05-13", "2009-05-12",
      "2010-05-11", "2011-05-10", "2012-05-08", "2013-05-14",
      "2014-05-13", "2015-05-12", "2016-05-03", "2017-05-09",
      "2018-05-08", "2019-04-02", "2020-10-06", "2021-05-11",
      "2022-03-29", "2022-10-25", "2023-05-09", "2024-05-14",
      "2025-03-25", "2026-05-12"
    )),
    stringsAsFactors = FALSE
  )
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

long_rows <- function(
    selected,
    release,
    table_family,
    table_label,
    source_table,
    pbo_url,
    pbo_sha,
    profile_type,
    force_forecast = FALSE,
    series_prefix = table_family) {
  fiscal_cols <- grep(
    "^(?:19|20)[0-9]{2}-[0-9]{2}$",
    names(selected),
    value = TRUE
  )
  if (!length(fiscal_cols)) {
    fail("No fiscal-year columns in ", source_table)
  }
  rows <- list()
  cursor <- 0L
  for (i in seq_len(nrow(selected))) {
    aggregate <- trimws(selected$Aggregate[[i]])
    unit <- trimws(selected$Unit[[i]])
    if (is.na(aggregate) || !nzchar(aggregate) ||
        is.na(unit) || !nzchar(unit)) {
      next
    }
    contract <- unit_contract(unit, aggregate)
    unit_key <- if (identical(contract$unit, "% of GDP")) {
      "percent-of-gdp"
    } else if (identical(contract$unit, "AUD million")) {
      "aud-million"
    } else {
      slug(contract$unit)
    }
    series <- if (identical(
      table_family,
      "major_economic_parameters"
    )) {
      if (identical(aggregate, "Nominal GDP") &&
          identical(contract$unit, "AUD million")) {
        "Nominal GDP level"
      } else {
        sub(" growth$", "", aggregate, ignore.case = TRUE)
      }
    } else {
      paste0(aggregate, " — ", contract$unit)
    }
    values <- suppressWarnings(as.numeric(selected[i, fiscal_cols]))
    keep <- !is.na(values)
    if (!any(keep)) next
    years <- fiscal_cols[keep]
    period_end <- as.Date(paste0(
      as.integer(substr(years, 1L, 4L)) + 1L,
      "-06-30"
    ))
    actual_forecast <- if (isTRUE(force_forecast)) {
      rep("forecast", length(years))
    } else {
      ifelse(period_end < release$release_date, "actual", "forecast")
    }
    cursor <- cursor + 1L
    rows[[cursor]] <- data.frame(
      release_id = release$release_id,
      source_url = release$source_url,
      source_sha256 = release$source_sha256,
      observation_source_url = pbo_url,
      observation_source_sha256 = pbo_sha,
      table_id = table_family,
      table_family = table_family,
      table_label = table_label,
      source_table = source_table,
      series_id = paste(
        series_prefix,
        slug(aggregate),
        unit_key,
        sep = "--"
      ),
      series = series,
      fiscal_year = years,
      period_end = period_end,
      value = values[keep],
      unit = contract$unit,
      unit_multiplier = contract$multiplier,
      accounting_basis = if ("Accounting" %in% names(selected)) {
        tolower(trimws(selected$Accounting[[i]]))
      } else {
        "not_applicable"
      },
      consolidation = if (identical(
        table_family,
        "major_economic_parameters"
      )) {
        "australian_economy"
      } else {
        "general_government_sector"
      },
      actual_forecast = actual_forecast,
      profile_type = ifelse(
        actual_forecast == "actual",
        "actual",
        profile_type
      ),
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

read_pbo_sheet <- function(path, sheet) {
  out <- suppressMessages(readxl::read_excel(path, sheet = sheet, skip = 2))
  names(out) <- trimws(names(out))
  out
}

options <- parse_args(commandArgs(trailingOnly = TRUE))
if (!file.exists(options$input)) fail("Input panel does not exist.")
if (!file.exists(options$pbo_workbook)) fail("PBO workbook does not exist.")
release_dates <- treasury_release_dates(options$publication)
publication_label <- if (identical(options$publication, "myefo")) {
  "MYEFO"
} else {
  "Budget"
}

base_all <- utils::read.csv(
  options$input,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
seed_rows <- base_all$table_id == "Table 1"
base <- base_all[seed_rows, , drop = FALSE]
preserved <- base_all[!seed_rows, , drop = FALSE]
manifest_path <- file.path("inst", "extdata", "releases.csv")
if (!file.exists(manifest_path)) {
  fail("Release manifest does not exist: ", manifest_path)
}
manifest <- utils::read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
manifest_rows <- match(release_dates$release_id, manifest$release_id)
if (anyNA(manifest_rows)) {
  fail("The release manifest does not include every configured release.")
}
release_key <- release_dates
release_key$source_url <- manifest$resource_url[manifest_rows]
release_key$source_sha256 <- manifest$expected_sha256[manifest_rows]
if (anyNA(release_key$source_url) ||
    any(!grepl("^[a-f0-9]{64}$", release_key$source_sha256))) {
  fail("The release manifest does not supply every pinned Treasury source.")
}

pbo_url <- if (identical(options$publication, "myefo")) {
  paste0(
    "https://www.pbo.gov.au/sites/default/files/2026-03/",
    "PBO%20Historical%20budget%20forecasts%20and%20measures%20-%20",
    "2025-26%20MYEFO.xlsx"
  )
} else {
  paste0(
    "https://www.pbo.gov.au/sites/default/files/2026-05/",
    "2026-27%20Budget%20-%20Historical%20budget%20forecasts%20",
    "and%20measures.xlsx"
  )
}
pbo_sha <- digest::digest(
  file = options$pbo_workbook,
  algo = "sha256",
  serialize = FALSE
)

if (nrow(base)) {
  base$observation_source_url <- base$source_url
  base$observation_source_sha256 <- base$source_sha256
  base$table_id <- "cash_receipts"
  base$table_family <- "cash_receipts"
  base$table_label <- "Cash receipts"
  base$source_table <-
    "Australian Government general government (cash) receipts"
}

table_1 <- read_pbo_sheet(options$pbo_workbook, "Table 1")
table_2 <- read_pbo_sheet(options$pbo_workbook, "Table 2")
table_5 <- read_pbo_sheet(options$pbo_workbook, "Table 5")

structured <- list()
cursor <- 0L
for (i in seq_len(nrow(release_key))) {
  release <- release_key[i, , drop = FALSE]
  update_label <- release$update_label
  budget_label <- paste(release$budget_vintage, "Budget")

  economic <- table_5[table_5$`Budget update` == update_label, ]
  if (nrow(economic)) {
    cursor <- cursor + 1L
    structured[[cursor]] <- long_rows(
      economic,
      release,
      "major_economic_parameters",
      "Major economic parameters",
      "PBO Table 5: Economic forecasts",
      pbo_url,
      pbo_sha,
      options$publication
    )
  }

  aggregates <- table_2[table_2$`Budget update` == update_label, ]
  if (nrow(aggregates)) {
    cursor <- cursor + 1L
    structured[[cursor]] <- long_rows(
      aggregates,
      release,
      "budget_aggregates",
      "Budget aggregates",
      "PBO Table 2: Budget estimates",
      pbo_url,
      pbo_sha,
      options$publication
    )
  }

  medium_term <- table_1[table_1$`Budget update` == update_label, ]
  medium_term <- medium_term[
    medium_term$Aggregate %in% c(
      "Gross debt", "Net debt", "Net financial worth"
    ),
  ]
  if (nrow(medium_term)) {
    cursor <- cursor + 1L
    structured[[cursor]] <- long_rows(
      medium_term,
      release,
      "budget_aggregates",
      "Budget aggregates",
      "PBO Table 1: Medium term projections",
      pbo_url,
      pbo_sha,
      options$publication
    )
  }

  if (identical(options$publication, "myefo")) {
    comparator <- table_2[table_2$`Budget update` == budget_label, ]
    if (nrow(comparator)) {
      cursor <- cursor + 1L
      structured[[cursor]] <- long_rows(
        comparator,
        release,
        "budget_comparator_aggregates",
        "Budget aggregates — Budget comparator",
        "PBO Table 2: Budget estimates",
        pbo_url,
        pbo_sha,
        "budget",
        force_forecast = TRUE
      )
    }

    comparator_debt <- table_1[table_1$`Budget update` == budget_label, ]
    comparator_debt <- comparator_debt[
      comparator_debt$Aggregate %in% c(
        "Gross debt", "Net debt", "Net financial worth"
      ),
    ]
    if (nrow(comparator_debt)) {
      cursor <- cursor + 1L
      structured[[cursor]] <- long_rows(
        comparator_debt,
        release,
        "budget_comparator_aggregates",
        "Budget aggregates — Budget comparator",
        "PBO Table 1: Medium term projections",
        pbo_url,
        pbo_sha,
        "budget",
        force_forecast = TRUE
      )
    }
  }
}

structured <- do.call(rbind, Filter(Negate(is.null), structured))
structured_key <- unique(paste(
  structured$release_id,
  structured$table_family,
  sep = "\r"
))
preserved_key <- paste(
  preserved$release_id,
  preserved$table_family,
  sep = "\r"
)
preserved <- preserved[!preserved_key %in% structured_key, , drop = FALSE]
base_fields <- unique(c(names(base), names(preserved)))
fill_base_fields <- function(x) {
  for (field in setdiff(base_fields, names(x))) x[[field]] <- NA
  x[base_fields]
}
base <- rbind(fill_base_fields(base), fill_base_fields(preserved))
base$period_end <- as.character(as.Date(paste0(
  as.integer(substr(base$fiscal_year, 1L, 4L)) + 1L,
  "-06-30"
)))
structured$period_end <- as.character(as.Date(paste0(
  as.integer(substr(structured$fiscal_year, 1L, 4L)) + 1L,
  "-06-30"
)))
all_fields <- unique(c(names(base), names(structured)))
add_missing <- function(x) {
  for (field in setdiff(all_fields, names(x))) x[[field]] <- NA
  x[all_fields]
}
observations <- rbind(add_missing(base), add_missing(structured))
observations <- observations[order(
  observations$release_id,
  observations$table_family,
  observations$series_id,
  observations$period_end,
  observations$profile_type
), ]

key <- paste(
  observations$release_id,
  observations$table_id,
  observations$series_id,
  observations$period_end,
  observations$actual_forecast,
  observations$profile_type,
  sep = "\r"
)
if (anyDuplicated(key) ||
    anyNA(observations$value) ||
    anyNA(observations$period_end) ||
    any(!observations$actual_forecast %in% c("actual", "forecast")) ||
    any(!unique(observations$release_id) %in% release_key$release_id)) {
  fail(
    "Generated ", publication_label,
    " structured panel failed its observation contract."
  )
}

parent <- dirname(options$output)
if (!dir.exists(parent) &&
    !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
  fail("Could not create output directory: ", parent)
}
utils::write.csv(observations, options$output, row.names = FALSE, na = "")
message(
  "Wrote ", nrow(observations), " observations across ",
  length(unique(observations$release_id)), " ", publication_label,
  " releases and ",
  length(unique(observations$table_family)), " table families: ",
  options$output
)
