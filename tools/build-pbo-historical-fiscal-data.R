#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: build-pbo-historical-fiscal-data.R INPUT.xlsx OUTPUT.csv",
    call. = FALSE
  )
}

input <- normalizePath(args[[1L]], mustWork = TRUE)
output <- args[[2L]]
source_url <- paste0(
  "https://www.pbo.gov.au/sites/default/files/2026-07/",
  "PBO%20-%20Historical%20Fiscal%20Data%20-%202026-27%20Budget.xlsx"
)
source_sha256 <- digest::digest(
  file = input,
  algo = "sha256",
  serialize = FALSE
)
expected_sha256 <-
  "a6391394b9464c5e48650806ab8c3260d81cbed579e368423469e7af27888f01"
if (!identical(source_sha256, expected_sha256)) {
  stop("The PBO workbook does not match the pinned SHA-256.", call. = FALSE)
}

table_labels <- c(
  "Cash flow aggregates",
  "Debt, net interest payments and net worth",
  "Fiscal (accrual) aggregates",
  "Institutional sectors — cash flow and operating aggregates",
  "Revenue by head of revenue (accrual accounting)",
  "Receipts by head of revenue (cash accounting)",
  "Expenses by function and sub-function",
  "Historical cash flow data",
  "Historical debt data",
  "Medium-term projections data",
  "General government financial statements"
)
accounting_basis <- c(
  "cash", "mixed", "accrual", "mixed", "accrual", "cash",
  "accrual", "cash", "mixed", "mixed", "accrual"
)
consolidation <- c(
  rep("general_government_sector", 3L),
  "mixed",
  rep("general_government_sector", 4L),
  "commonwealth_government",
  "general_government_sector",
  "general_government_sector"
)

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("[[:space:]]+", " ", x))
}

slug <- function(x) {
  x <- tolower(clean_text(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

parse_value <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x <- clean_text(x)
  x[x %in% c("", "-", "–", "—", "n.a.", "N/A")] <- NA_character_
  negative <- grepl("^\\(.*\\)$", x)
  x <- gsub("[,$%*†‡]", "", x)
  x <- gsub("^\\((.*)\\)$", "\\1", x)
  value <- suppressWarnings(as.numeric(x))
  value[negative & !is.na(value)] <- -value[negative & !is.na(value)]
  value
}

normalize_unit <- function(x) {
  x <- clean_text(x)
  switch(
    x,
    "$m" = "AUD million",
    "%" = "Per cent",
    "% of GDP" = "% of GDP",
    "% of total" = "Per cent of total",
    "% growth" = "Per cent change",
    "% real growth" = "Per cent change",
    "ASL" = "Average staffing level",
    "ratio" = "Ratio",
    "index" = "Index",
    x
  )
}

unit_multiplier <- function(unit) {
  ifelse(
    unit == "AUD million",
    1e6,
    ifelse(
      unit %in% c("% of GDP", "Per cent", "Per cent of total"),
      0.01,
      1
    )
  )
}

find_year_header <- function(data) {
  counts <- vapply(seq_len(nrow(data)), function(row) {
    sum(grepl(
      "^(18|19|20)[0-9]{2}-[0-9]{2}$",
      clean_text(unlist(data[row, ], use.names = FALSE))
    ))
  }, integer(1))
  which.max(counts)
}

find_unit_column <- function(data, header_row, first_year_column) {
  for (column in seq_len(first_year_column - 1L)) {
    values <- clean_text(unlist(
      data[seq_len(header_row), column, drop = FALSE],
      use.names = FALSE
    ))
    if ("Units" %in% values) {
      return(column)
    }
  }
  candidates <- vapply(seq_len(first_year_column - 1L), function(column) {
    values <- clean_text(data[[column]])
    sum(grepl(
      "^(\\$m|%|% of GDP|% of total|% growth|% real growth|ASL)$",
      values
    ))
  }, integer(1))
  if (max(candidates) > 0L) {
    return(which.max(candidates))
  }
  NA_integer_
}

rows <- list()
row_index <- 0L
for (table_number in seq_along(table_labels)) {
  sheet <- paste("Table", table_number)
  data <- as.data.frame(
    readxl::read_excel(
      input,
      sheet = sheet,
      col_names = FALSE,
      .name_repair = "minimal"
    ),
    stringsAsFactors = FALSE
  )
  header_row <- find_year_header(data)
  header <- clean_text(unlist(data[header_row, ], use.names = FALSE))
  year_columns <- which(grepl(
    "^(18|19|20)[0-9]{2}-[0-9]{2}$",
    header
  ))
  fiscal_years <- header[year_columns]
  actual_columns <- year_columns[
    as.integer(substr(fiscal_years, 1L, 4L)) <= 2024L
  ]
  actual_years <- header[actual_columns]
  unit_column <- find_unit_column(
    data,
    header_row,
    min(year_columns)
  )
  last_label <- ""
  last_unit <- ""
  last_explicit_series <- ""
  section <- ""

  for (source_row in seq.int(header_row + 1L, nrow(data))) {
    label <- clean_text(data[[1L]][[source_row]])
    raw_unit <- if (!is.na(unit_column)) {
      clean_text(data[[unit_column]][[source_row]])
    } else {
      ""
    }
    all_values <- parse_value(unlist(
      data[source_row, year_columns, drop = FALSE],
      use.names = FALSE
    ))
    actual_values <- parse_value(unlist(
      data[source_row, actual_columns, drop = FALSE],
      use.names = FALSE
    ))
    has_any_value <- any(!is.na(all_values))
    has_actual_value <- any(!is.na(actual_values))

    if (!has_any_value) {
      if (nzchar(label) &&
          !grepl("^(source|note|memorandum|of which):?$",
                 tolower(label))) {
        section <- label
      }
      next
    }
    if (!has_actual_value) {
      next
    }

    explicit_label <- nzchar(label)
    explicit_unit <- nzchar(raw_unit)
    if (!explicit_label) {
      label <- last_label
    }
    if (!nzchar(label)) {
      next
    }
    if (!explicit_unit) {
      raw_unit <- last_unit
    }
    unit <- normalize_unit(raw_unit)
    if (!nzchar(unit)) {
      unit <- "Published value"
    }

    series <- label
    if (explicit_label && !explicit_unit && nzchar(last_explicit_series) &&
        tolower(label) %in% c(
          "current series", "difference", "total", "commonwealth",
          "public non-financial corporations", "public financial corporations"
        )) {
      series <- paste(last_explicit_series, label, sep = " — ")
    }
    if (explicit_label && explicit_unit) {
      last_explicit_series <- label
    }
    if (explicit_label) {
      last_label <- label
    }
    if (explicit_unit) {
      last_unit <- raw_unit
    }

    keep <- !is.na(actual_values)
    if (!any(keep)) {
      next
    }
    row_index <- row_index + 1L
    row_series_id <- paste0(
      "pbo-hfd--table-",
      sprintf("%02d", table_number),
      "--row-",
      sprintf("%03d", source_row),
      "--",
      slug(series),
      "--",
      slug(unit)
    )
    selected_years <- actual_years[keep]
    rows[[row_index]] <- data.frame(
      release_id = "pbo-historical-fiscal-data-2026-27-budget",
      table_id = paste0(
        "pbo_historical_table_",
        sprintf("%02d", table_number)
      ),
      table_number = as.integer(table_number),
      table_label = table_labels[[table_number]],
      source_table = sheet,
      source_row = as.integer(source_row),
      section = section,
      series_id = row_series_id,
      series = series,
      fiscal_year = selected_years,
      period_end = paste0(
        as.integer(substr(selected_years, 1L, 4L)) + 1L,
        "-06-30"
      ),
      value = actual_values[keep],
      unit = unit,
      unit_multiplier = unit_multiplier(unit),
      accounting_basis = accounting_basis[[table_number]],
      consolidation = consolidation[[table_number]],
      actual_forecast = "actual",
      profile_type = "actual",
      frequency = "annual",
      source_url = source_url,
      source_sha256 = source_sha256,
      stringsAsFactors = FALSE
    )
  }
}

panel <- do.call(rbind, rows)
key <- paste(panel$series_id, panel$fiscal_year, sep = "\r")
if (!nrow(panel) ||
    anyDuplicated(key) ||
    anyNA(panel$value) ||
    anyNA(as.Date(panel$period_end)) ||
    any(!grepl("^[a-f0-9]{64}$", panel$source_sha256)) ||
    any(panel$actual_forecast != "actual") ||
    any(as.integer(substr(panel$fiscal_year, 1L, 4L)) > 2024L) ||
    !setequal(unique(panel$table_number), seq_len(11L))) {
  stop("The generated PBO historical panel failed its contract.", call. = FALSE)
}

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
connection <- if (grepl("\\.gz$", output, ignore.case = TRUE)) {
  gzfile(output, open = "wt", compression = 9L)
} else {
  file(output, open = "wt")
}
utils::write.csv(panel, connection, row.names = FALSE, na = "")
close(connection)
cat(
  sprintf(
    "Wrote %s observations, %s series, and %s tables to %s\n",
    format(nrow(panel), big.mark = ","),
    format(length(unique(panel$series_id)), big.mark = ","),
    length(unique(panel$table_id)),
    output
  )
)
