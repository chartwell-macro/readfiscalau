#!/usr/bin/env Rscript

# Maintainer-only deterministic reparser for the raw table blocks already
# retained in a reviewed Budget/MYEFO snapshot. This is useful when parser
# semantics change but the official source inventory and extracted table
# blocks do not. Optional pinned Treasury chart-data CSV/ZIP inputs are parsed
# from a caller-supplied directory. The script never downloads data.

fail <- function(...) stop(paste0(...), call. = FALSE)

parse_args <- function(args) {
  out <- list(
    input = NULL,
    output = NULL,
    chart_inventory = NULL,
    chart_data_dir = NULL,
    families = NULL
  )
  for (arg in args) {
    if (startsWith(arg, "--input=")) {
      out$input <- sub("^--input=", "", arg)
    } else if (startsWith(arg, "--families=")) {
      out$families <- strsplit(sub("^--families=", "", arg), ",", fixed = TRUE)[[1L]]
    } else if (startsWith(arg, "--output=")) {
      out$output <- sub("^--output=", "", arg)
    } else if (startsWith(arg, "--chart-inventory=")) {
      out$chart_inventory <- sub("^--chart-inventory=", "", arg)
    } else if (startsWith(arg, "--chart-data-dir=")) {
      out$chart_data_dir <- sub("^--chart-data-dir=", "", arg)
    } else {
      fail("Unknown argument: ", arg)
    }
  }
  if (is.null(out$input) || is.null(out$output)) {
    fail(paste(
      "Required: --input=RDS --output=RDS",
      "[--chart-inventory=CSV --chart-data-dir=PATH]"
    ))
  }
  if (xor(is.null(out$chart_inventory), is.null(out$chart_data_dir))) {
    fail("Chart inventory and chart-data directory must be supplied together.")
  }
  out
}

options <- parse_args(commandArgs(trailingOnly = TRUE))
package_dir <- normalizePath(".", mustWork = TRUE)
input <- normalizePath(options$input, mustWork = TRUE)
output <- normalizePath(dirname(options$output), mustWork = TRUE)
output <- file.path(output, basename(options$output))

builder_path <- file.path(package_dir, "tools", "build-budget-myefo-history.R")
expressions <- parse(builder_path, keep.source = FALSE)
main_at <- which(vapply(expressions, function(expression) {
  is.call(expression) &&
    identical(expression[[1L]], as.name("<-")) &&
    identical(expression[[2L]], as.name("options"))
}, logical(1)))[[1L]]
builder <- new.env(parent = globalenv())
for (index in seq_len(main_at - 1L)) {
  eval(expressions[[index]], envir = builder)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

read_file_raw <- function(path) {
  size <- file.info(path)$size
  if (is.na(size) || size < 1) fail("Source file is empty: ", path)
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = size)
}

read_chart_source <- function(source, data_dir) {
  path <- file.path(data_dir, source$local_filename[[1L]])
  if (!file.exists(path)) fail("Missing chart-data source: ", path)
  observed_container_hash <- sha256_file(path)
  if (!identical(observed_container_hash, source$container_sha256[[1L]])) {
    fail("Chart-data container hash mismatch for ", source$release_id[[1L]], ".")
  }

  member <- source$archive_member[[1L]]
  if (!nzchar(member)) {
    raw <- read_file_raw(path)
  } else {
    listing <- tryCatch(
      utils::unzip(path, list = TRUE),
      error = function(error) fail(
        "Unreadable chart-data ZIP for ", source$release_id[[1L]], "."
      )
    )
    unsafe <- startsWith(listing$Name, "/") |
      grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", listing$Name)
    if (any(unsafe) || sum(listing$Name == member) != 1L) {
      fail("Chart-data ZIP member contract failed for ", source$release_id[[1L]], ".")
    }
    extract_dir <- tempfile("budget-myefo-chart-")
    dir.create(extract_dir)
    on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
    utils::unzip(path, files = member, exdir = extract_dir, junkpaths = FALSE)
    member_path <- file.path(extract_dir, member)
    raw <- read_file_raw(member_path)
  }
  observed_member_hash <- digest::digest(
    raw, algo = "sha256", serialize = FALSE
  )
  if (!identical(observed_member_hash, source$member_sha256[[1L]])) {
    fail("Chart-data member hash mismatch for ", source$release_id[[1L]], ".")
  }
  raw
}

chart_header <- function(value) {
  value <- trimws(value)
  match <- regexec(
    paste0(
      "^((?:19|20)[0-9]{2}-[0-9]{2})",
      "(?: \\((est|estimate|projected)\\))? \\(\\$",
      "(m|b|million|billion)\\)$"
    ),
    value,
    ignore.case = TRUE
  )
  parts <- regmatches(value, match)[[1L]]
  if (!length(parts)) fail("Unrecognised chart-data column: ", value)
  fiscal_year <- parts[[2L]]
  unit_key <- tolower(parts[[4L]])
  list(
    fiscal_year = fiscal_year,
    period_end = as.Date(paste0(
      as.integer(substr(fiscal_year, 1L, 4L)) + 1L,
      "-06-30"
    )),
    actual_forecast = if (nzchar(parts[[3L]])) "forecast" else "actual",
    unit = if (unit_key %in% c("b", "billion")) {
      "AUD billion"
    } else {
      "AUD million"
    },
    unit_multiplier = if (unit_key %in% c("b", "billion")) 1e9 else 1e6
  )
}

chart_number <- function(value) {
  value <- trimws(as.character(value))
  if (!nzchar(value) || value %in% c("-", "..", "na", "n/a")) {
    return(NA_real_)
  }
  negative <- startsWith(value, "(") && endsWith(value, ")")
  number <- suppressWarnings(as.numeric(gsub("[,$()]", "", value)))
  if (is.na(number)) fail("Unrecognised chart-data value: ", value)
  if (negative) -number else number
}

parse_chart_source <- function(source, release, raw) {
  text <- rawToChar(raw)
  table <- tryCatch(
    utils::read.csv(
      text = text,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      colClasses = "character",
      na.strings = character()
    ),
    error = function(error) fail(
      "Could not parse chart-data CSV for ", source$release_id[[1L]], ": ",
      conditionMessage(error)
    )
  )
  if (nrow(table) < 1L || ncol(table) < 2L) {
    fail("Chart-data CSV has no usable table for ", source$release_id[[1L]], ".")
  }
  labels <- trimws(as.character(table[[1L]]))
  if (anyNA(labels) || any(!nzchar(labels))) {
    fail("Chart-data series labels are incomplete for ", source$release_id[[1L]], ".")
  }
  headers <- lapply(names(table)[-1L], chart_header)
  fiscal_years <- vapply(headers, `[[`, character(1), "fiscal_year")
  if (anyDuplicated(fiscal_years)) {
    fail("Chart-data periods repeat for ", source$release_id[[1L]], ".")
  }
  period_ends <- as.Date(vapply(
    headers, function(header) as.character(header$period_end), character(1)
  ))
  if (is.unsorted(period_ends, strictly = TRUE)) {
    fail("Chart-data periods are not strictly increasing for ", source$release_id[[1L]], ".")
  }
  units <- vapply(headers, `[[`, character(1), "unit")
  multipliers <- vapply(headers, `[[`, numeric(1), "unit_multiplier")
  statuses <- vapply(headers, `[[`, character(1), "actual_forecast")
  if (length(unique(units)) != 1L ||
      any(statuses == "actual" & period_ends > as.Date(release$release_date))) {
    fail("Chart-data unit/status contract failed for ", source$release_id[[1L]], ".")
  }

  family_at <- match(source$family_id[[1L]], builder$family_spec$family_id)
  if (is.na(family_at)) fail("Unknown chart-data family: ", source$family_id[[1L]])
  instance <- paste(
    source$release_id[[1L]], source$family_id[[1L]],
    "treasury-chart-data-table-1", sep = "--"
  )
  values <- vector("list", nrow(table) * (ncol(table) - 1L))
  cursor <- 0L
  for (row in seq_len(nrow(table))) {
    series <- labels[[row]]
    if (identical(series,"Other taxes")) series <- "Other taxes — aggregate"
    for (column in seq_along(headers)) {
      value_text <- trimws(as.character(table[[column + 1L]][[row]]))
      value <- chart_number(value_text)
      if (is.na(value)) next
      cursor <- cursor + 1L
      values[[cursor]] <- data.frame(
        release_id = release$release_id,
        publication = release$publication,
        budget_vintage = release$budget_vintage,
        release_date = as.Date(release$release_date),
        family_id = source$family_id[[1L]],
        family_label = builder$family_spec$family_label[[family_at]],
        query_mode = builder$family_spec$query_mode[[family_at]],
        table_instance_id = instance,
        source_table = source$source_table[[1L]],
        source_page = NA_integer_,
        table_part = 1L,
        row_index = as.integer(row),
        row_label = series,
        row_path = series,
        row_depth = 0L,
        column_index = as.integer(column),
        column_label = fiscal_years[[column]],
        value_text = value_text,
        value = value,
        unit = units[[column]],
        unit_multiplier = multipliers[[column]],
        accounting_basis = "cash",
        consolidation = "general_government_sector",
        series_id = builder$canonical_series_id(series),
        series = series,
        series_eligible = TRUE,
        fiscal_year = fiscal_years[[column]],
        calendar_year = NA_integer_,
        period_end = period_ends[[column]],
        period_type = "fiscal_year",
        actual_forecast = statuses[[column]],
        profile_type = if (statuses[[column]] == "actual") {
          "actual"
        } else {
          "budget"
        },
        source_url = source$source_url[[1L]],
        source_sha256 = source$member_sha256[[1L]],
        raw_line = paste(c(series, unlist(table[row, -1L])), collapse = ","),
        source_kind = "treasury_chart_data_cell",
        source_container_sha256 = source$container_sha256[[1L]],
        source_archive_members = source$archive_member[[1L]],
        source_was_assembled = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  observations <- do.call(rbind, values[seq_len(cursor)])
  table_row <- data.frame(
    release_id = release$release_id,
    publication = release$publication,
    budget_vintage = release$budget_vintage,
    release_date = as.Date(release$release_date),
    family_id = source$family_id[[1L]],
    family_label = builder$family_spec$family_label[[family_at]],
    query_mode = builder$family_spec$query_mode[[family_at]],
    table_instance_id = instance,
    source_table = source$source_table[[1L]],
    source_page = NA_integer_,
    table_part = 1L,
    source_url = source$source_url[[1L]],
    source_sha256 = source$member_sha256[[1L]],
    raw_text = text,
    source_kind = "treasury_chart_data_cell",
    source_container_sha256 = source$container_sha256[[1L]],
    source_archive_members = source$archive_member[[1L]],
    source_was_assembled = FALSE,
    stringsAsFactors = FALSE
  )
  artifact <- data.frame(
    release_id = release$release_id,
    source_kind = "treasury_chart_data_cell",
    source_url = source$source_url[[1L]],
    source_sha256 = source$member_sha256[[1L]],
    source_container_sha256 = source$container_sha256[[1L]],
    source_archive_members = source$archive_member[[1L]],
    source_was_assembled = FALSE,
    stringsAsFactors = FALSE
  )
  list(table = table_row, observations = observations, artifact = artifact)
}

pdf_artifacts <- function(store) {
  data.frame(
    release_id = store$sources$release_id,
    source_kind = "pdf_table_cell",
    source_url = store$sources$source_url,
    source_sha256 = store$sources$pdf_sha256,
    source_container_sha256 = store$sources$container_sha256,
    source_archive_members = store$sources$archive_member,
    source_was_assembled = store$sources$assembled_from_members,
    stringsAsFactors = FALSE
  )
}

store <- readRDS(input)
if (!is.list(store) ||
    !all(c("tables", "sources", "observations", "validation") %in%
         names(store)) ||
    !all(c("raw_text", "release_id") %in% names(store$tables))) {
  fail("The input is not a supported Budget/MYEFO history snapshot.")
}

pdf_tables <- store$tables
if ("source_kind" %in% names(pdf_tables)) {
  pdf_tables <- pdf_tables[pdf_tables$source_kind == "pdf_table_cell", , drop = FALSE]
}
if (!is.null(options$families)) {
  if (any(!options$families %in% builder$family_spec$family_id)) fail("Unknown scoped table family.")
  pdf_tables <- pdf_tables[pdf_tables$family_id %in% options$families, , drop = FALSE]
}
parts <- lapply(seq_len(nrow(pdf_tables)), function(index) {
  table_row <- pdf_tables[index, , drop = FALSE]
  release <- store$sources[
    store$sources$release_id == table_row$release_id[[1L]],
    ,
    drop = FALSE
  ]
  if (nrow(release) != 1L) {
    fail("No unique source release for ", table_row$release_id[[1L]], ".")
  }
  block <- strsplit(table_row$raw_text[[1L]], "\n", fixed = TRUE)[[1L]]
  builder$parse_table_part(block, table_row, release)
})
observations <- do.call(rbind, Filter(Negate(is.null), parts))
if (!nrow(observations)) fail("Reparsing produced no observations.")

series_parent <- ifelse(
  grepl(" > ", observations$row_path, fixed = TRUE),
  sub(" > [^>]+$", "", observations$row_path),
  ""
)
observations$series_id <- mapply(
  builder$canonical_series_id,
  observations$series,
  series_parent,
  USE.NAMES = FALSE
)
observations$source_kind <- "pdf_table_cell"
source_index <- match(observations$release_id, store$sources$release_id)
if (anyNA(source_index)) fail("PDF observations have unknown releases.")
observations$source_container_sha256 <-
  store$sources$container_sha256[source_index]
observations$source_archive_members <- store$sources$archive_member[source_index]
observations$source_was_assembled <-
  store$sources$assembled_from_members[source_index]

pdf_table_source_index <- match(pdf_tables$release_id, store$sources$release_id)
if (anyNA(pdf_table_source_index)) fail("PDF tables have unknown releases.")
pdf_tables$source_kind <- "pdf_table_cell"
pdf_tables$source_container_sha256 <-
  store$sources$container_sha256[pdf_table_source_index]
pdf_tables$source_archive_members <-
  store$sources$archive_member[pdf_table_source_index]
pdf_tables$source_was_assembled <-
  store$sources$assembled_from_members[pdf_table_source_index]

artifacts <- pdf_artifacts(store)
if (!is.null(options$chart_inventory)) {
  chart_inventory_path <- normalizePath(
    options$chart_inventory, mustWork = TRUE
  )
  chart_data_dir <- normalizePath(options$chart_data_dir, mustWork = TRUE)
  chart_inventory <- utils::read.csv(
    chart_inventory_path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = character()
  )
  chart_required <- c(
    "release_id", "source_url", "container_sha256", "archive_member",
    "member_sha256", "local_filename", "family_id", "source_table"
  )
  if (!all(chart_required %in% names(chart_inventory)) ||
      !nrow(chart_inventory) ||
      anyDuplicated(paste(
        chart_inventory$release_id,
        chart_inventory$source_url,
        chart_inventory$archive_member,
        sep = "\r"
      )) ||
      any(!grepl("^https://", chart_inventory$source_url)) ||
      any(!grepl("^[0-9a-f]{64}$", chart_inventory$container_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", chart_inventory$member_sha256))) {
    fail("Chart-data source inventory failed its contract.")
  }
  chart_sources <- lapply(seq_len(nrow(chart_inventory)), function(index) {
    source <- chart_inventory[index, , drop = FALSE]
    release <- store$sources[
      store$sources$release_id == source$release_id[[1L]],
      ,
      drop = FALSE
    ]
    if (nrow(release) != 1L || release$publication[[1L]] != "budget") {
      fail("Chart-data source has no unique Budget release: ", source$release_id[[1L]])
    }
    parse_chart_source(
      source,
      release,
      read_chart_source(source, chart_data_dir)
    )
  })
  chart_tables <- do.call(rbind, lapply(chart_sources, `[[`, "table"))
  chart_observations <- do.call(
    rbind, lapply(chart_sources, `[[`, "observations")
  )
  observations <- rbind(observations, chart_observations)
  pdf_tables <- rbind(pdf_tables, chart_tables)
  artifacts <- rbind(
    artifacts,
    do.call(rbind, lapply(chart_sources, `[[`, "artifact"))
  )
} else {
  # Retained chart-data cells already carry verified member/container hashes.
  # A PDF parser repair must not silently drop the independent CSV inputs.
  chart_rows <- store$observations$source_kind == "treasury_chart_data_cell"
  chart_observations <- store$observations[chart_rows, , drop = FALSE]
  chart_observations$is_preferred <- NULL
  observations <- rbind(observations, chart_observations)
  pdf_tables <- rbind(pdf_tables, store$tables[
    store$tables$source_kind == "treasury_chart_data_cell", , drop = FALSE
  ])
  artifacts <- rbind(artifacts, store$source_artifacts[
    store$source_artifacts$source_kind == "treasury_chart_data_cell", , drop = FALSE
  ])
}

if (!is.null(options$families)) {
  retained <- store$observations[!store$observations$family_id %in% options$families & store$observations$source_kind != "treasury_chart_data_cell", , drop = FALSE]
  retained$is_preferred <- NULL
  observations <- rbind(retained, observations)
  pdf_tables <- rbind(store$tables[!store$tables$family_id %in% options$families & store$tables$source_kind != "treasury_chart_data_cell", , drop = FALSE], pdf_tables)
}
chart_other <- observations$source_kind=="treasury_chart_data_cell" & observations$row_label=="Other taxes"
observations$series[chart_other] <- "Other taxes — aggregate"
observations$series_id[chart_other] <- "other-taxes-aggregate"
observations$is_preferred <- FALSE
eligible <- which(
  observations$series_eligible & !is.na(observations$period_end)
)
family_priority <- setNames(
  seq_along(builder$family_spec$family_id),
  builder$family_spec$family_id
)
family_priority[c(
  "ggs_budget_aggregates",
  "underlying_to_headline_cash_bridge",
  "formal_financial_statements"
)] <- c(1L, 2L, 3L)
source_priority <- c(
  treasury_chart_data_cell = 1L,
  pdf_table_cell = 2L
)
key <- paste(
  observations$release_id[eligible],
  observations$series_id[eligible],
  observations$period_end[eligible],
  observations$unit[eligible],
  observations$accounting_basis[eligible],
  observations$consolidation[eligible],
  observations$profile_type[eligible],
  sep = "\r"
)
for (indices in split(eligible, key)) {
  order_index <- order(
    unname(source_priority[observations$source_kind[indices]]),
    unname(family_priority[observations$family_id[indices]]),
    observations$table_instance_id[indices]
  )
  observations$is_preferred[indices[order_index[[1L]]]] <- TRUE
}

if (any(observations$series_eligible & is.na(observations$period_end)) ||
    any(observations$series_eligible & observations$column_label == "Total") ||
    anyNA(source_priority[observations$source_kind]) ||
    any(!grepl("^[0-9a-f]{64}$", observations$source_sha256)) ||
    any(!grepl(
      "^[0-9a-f]{64}$", observations$source_container_sha256
    ))) {
  fail("Reparsed series eligibility failed the dated-column contract.")
}
preferred_key <- paste(
  observations$release_id[eligible],
  observations$series_id[eligible],
  observations$period_end[eligible],
  observations$unit[eligible],
  observations$accounting_basis[eligible],
  observations$consolidation[eligible],
  observations$profile_type[eligible],
  sep = "\r"
)
if (any(table(preferred_key[observations$is_preferred[eligible]]) != 1L)) {
  fail("Reparsed preferred observations are not unique.")
}

coverage_fields <- c(
  "release_id", "publication", "budget_vintage", "release_date",
  "family_id", "family_label", "query_mode", "table_instance_id",
  "source_table", "source_page", "table_part", "source_url",
  "source_sha256", "source_kind", "source_container_sha256",
  "source_archive_members", "source_was_assembled"
)
store$coverage <- unique(pdf_tables[coverage_fields])
store$coverage <- store$coverage[order(
  store$coverage$family_id,
  store$coverage$publication,
  store$coverage$release_date,
  store$coverage$table_instance_id
), , drop = FALSE]

store$schema_version <- "budget_myefo_history_v3"
store$built_from$extraction <- paste(
  "Maintainer-time pdftotext -layout retained raw table blocks;",
  "Unicode fiscal-year headings normalized before period parsing;",
  "pinned official Treasury chart-data CSV/ZIP members parsed at build time;",
  "no runtime source parsing"
)
store$built_from$source <- paste(
  "Official Australian Treasury Budget Paper No. 1 and MYEFO PDFs plus",
  "pinned Treasury chart-data CSV/ZIP members"
)
store$built_from$chart_data_source_count <-
  sum(artifacts$source_kind == "treasury_chart_data_cell")
store$tables <- pdf_tables
store$observations <- observations
store$series <- builder$build_catalogue(observations)
store$source_artifacts <- artifacts
store$validation$reparsed_from_retained_raw_tables <- TRUE
store$validation$unicode_fiscal_year_normalization <- TRUE
store$validation$treasury_chart_data_source_count <-
  sum(artifacts$source_kind == "treasury_chart_data_cell")
store$validation$treasury_chart_data_observation_count <-
  sum(observations$source_kind == "treasury_chart_data_cell")
store$validation$treasury_chart_data_release_count <- length(unique(
  observations$release_id[
    observations$source_kind == "treasury_chart_data_cell"
  ]
))
saveRDS(store, output, version = 3L, compress = "xz")
message(
  "Wrote ", nrow(pdf_tables), " table parts, ",
  nrow(observations), " observation cells and ",
  nrow(store$series), " series catalogue rows to ", output
)
