.rf_header_value <- function(x, pattern, field) {
  hit <- regmatches(x, regexpr(pattern, x, ignore.case = TRUE, perl = TRUE))
  if (!length(hit) || !nzchar(hit)) {
    .rf_abort(sprintf("Finance header lacks %s.", field), "schema",
              field = field, header = x)
  }
  hit
}

.rf_parse_finance_header <- function(x) {
  x <- paste(x, collapse = "\n")
  month <- .rf_header_value(
    x, "\\b(January|February|March|April|May|July|August|September|October|November|December)\\b",
    "a supported month"
  )
  if (identical(tolower(month), "june")) {
    .rf_abort("June must come from FBO or CFS, never a monthly workbook.",
              "june_is_annual")
  }
  fy <- .rf_header_value(x, "20[0-9]{2}-20[0-9]{2}", "fiscal year")
  unit_hit <- .rf_header_value(x, "\\$(m|b)\\b", "unit")
  profile <- if (grepl("revised budget", x, ignore.case = TRUE)) {
    "revised_budget_profile"
  } else if (grepl("myefo", x, ignore.case = TRUE)) {
    "myefo_profile"
  } else if (grepl("budget", x, ignore.case = TRUE)) {
    "budget_profile"
  } else if (grepl("actual", x, ignore.case = TRUE)) {
    "actual"
  } else {
    .rf_abort("Finance header lacks an actual/profile status.", "schema")
  }
  list(
    month = month,
    fiscal_year = .rf_normalize_fiscal_year(fy),
    unit = if (tolower(unit_hit) == "$b") "AUD billion" else "AUD million",
    unit_multiplier = if (tolower(unit_hit) == "$b") 1e9 else 1e6,
    profile_type = profile,
    actual_forecast = if (profile == "actual") "actual" else "forecast",
    cumulative = grepl("\\bYTD\\b", x, ignore.case = TRUE)
  )
}

.rf_period_end_for_month <- function(month, fiscal_year) {
  month_no <- match(
    tolower(month), tolower(month.name)
  )
  start <- as.integer(substr(fiscal_year, 1L, 4L))
  year <- if (month_no <= 6L) start + 1L else start
  first <- as.Date(sprintf("%04d-%02d-01", year, month_no))
  next_first <- if (month_no == 12L) {
    as.Date(sprintf("%04d-01-01", year + 1L))
  } else {
    as.Date(sprintf("%04d-%02d-01", year, month_no + 1L))
  }
  next_first - 1
}

.rf_parse_finance_xlsx <- function(
    path,
    release,
    sheet = release$fiscal_year,
    include_non_ytd = FALSE) {
  supported <- c("finance_mfs_pre_2008_v1", "finance_mfs_post_2008_v2")
  if (!release$schema_family %in% supported) {
    .rf_abort(
      sprintf("Unsupported Finance workbook schema `%s`.",
              release$schema_family),
      "schema"
    )
  }
  start_year <- as.integer(substr(release$fiscal_year, 1L, 4L))
  expected <- if (start_year < 2008L) {
    "finance_mfs_pre_2008_v1"
  } else {
    "finance_mfs_post_2008_v2"
  }
  if (!identical(release$schema_family, expected)) {
    .rf_abort(
      "Finance schema family conflicts with the 2008-09 accounting boundary.",
      "schema"
    )
  }
  sheets <- readxl::excel_sheets(path)
  if (!sheet %in% sheets) {
    .rf_abort(sprintf("Workbook does not contain fiscal-year sheet `%s`.", sheet),
              "schema")
  }
  grid <- suppressMessages(readxl::read_excel(
    path, sheet = sheet, col_names = FALSE, .name_repair = "minimal"
  ))
  grid <- as.data.frame(grid, stringsAsFactors = FALSE)
  if (nrow(grid) < 4L || ncol(grid) < 2L) {
    .rf_abort("Finance workbook table is too small.", "schema")
  }
  title <- as.character(grid[1, 1])
  if (!grepl("Australian Government general government sector", title,
             fixed = TRUE)) {
    .rf_abort("Finance workbook title does not identify the GGS.", "schema")
  }
  table_id <- as.character(grid[2, 1])
  if (is.na(table_id) || !nzchar(trimws(table_id))) {
    .rf_abort("Finance workbook lacks a table identifier.", "schema")
  }
  labels <- trimws(as.character(grid[-c(1, 2), 1]))
  data_rows <- !is.na(labels) & nzchar(labels) &
    !grepl("^\\*|^\\([a-z]\\)", labels, ignore.case = TRUE)
  if (!any(data_rows)) {
    .rf_abort("Finance workbook contains no labelled series.", "schema")
  }
  labels <- labels[data_rows]
  series_id <- .rf_slug(labels)
  if (anyDuplicated(series_id)) {
    .rf_abort("Finance series labels collide after normalisation.",
              "duplicate_observation")
  }
  selected_month <- .rf_period_month(release$period_end)
  parsed <- list()
  expected_unit <- if (start_year >= 2020L) {
    "AUD billion"
  } else {
    "AUD million"
  }
  headers <- lapply(2:ncol(grid), function(j) {
    header <- as.character(grid[2, j])
    if (is.na(header) || !nzchar(trimws(header))) return(NULL)
    .rf_parse_finance_header(header)
  })
  selected <- which(vapply(headers, function(meta) {
    !is.null(meta) &&
      identical(meta$fiscal_year, release$fiscal_year) &&
      identical(tolower(meta$month), tolower(selected_month))
  }, logical(1)))
  if (!length(selected)) {
    .rf_abort(sprintf("Workbook has no column for release month `%s`.",
                      selected_month), "schema")
  }
  if (length(selected) > 1L) {
    .rf_abort("Workbook has duplicate columns for the release month.", "schema")
  }
  selected_meta <- headers[[selected]]
  if (!isTRUE(selected_meta$cumulative)) {
    .rf_abort(
      "Finance monthly aggregates must identify cumulative YTD scope.",
      "schema"
    )
  }
  for (j in 2:ncol(grid)) {
    meta <- headers[[j - 1L]]
    if (is.null(meta)) next
    if (!identical(meta$fiscal_year, release$fiscal_year)) {
      .rf_abort("Workbook header fiscal year conflicts with release metadata.",
                "schema")
    }
    period_end <- .rf_period_end_for_month(meta$month, meta$fiscal_year)
    if (period_end > release$period_end) next
    if (!identical(meta$profile_type, selected_meta$profile_type) ||
        !identical(meta$actual_forecast, selected_meta$actual_forecast)) next
    if (!identical(meta$unit, expected_unit)) {
      .rf_abort(
        "Finance workbook unit conflicts with its schema-family contract.",
        "schema",
        expected_unit = expected_unit,
        actual_unit = meta$unit
      )
    }
    # Some early workbooks contain non-YTD monthly July/August columns beside
    # cumulative columns. They are a different measure and must not be spliced
    # into the cumulative series.
    if (!isTRUE(meta$cumulative) && !isTRUE(include_non_ytd)) next
    value_text <- as.character(grid[-c(1, 2), j])[data_rows]
    value <- .rf_parse_numeric(value_text, "Finance workbook")
    parsed[[length(parsed) + 1L]] <- tibble::tibble(
      table_id = .rf_slug(table_id),
      series_id = series_id,
      series = labels,
      fiscal_year = meta$fiscal_year,
      period_end = period_end,
      value = value,
      unit = meta$unit,
      unit_multiplier = meta$unit_multiplier,
      accounting_basis = release$accounting_basis,
      consolidation = release$consolidation,
      actual_forecast = meta$actual_forecast,
      profile_type = meta$profile_type,
      cumulative = meta$cumulative,
      source_value_type = if (isTRUE(meta$cumulative)) "ytd" else "monthly"
    )
  }
  if (!length(parsed)) {
    .rf_abort("Workbook contains no cumulative observations through the release month.",
              "schema")
  }
  out <- .rf_assert_unique_observations(.rf_bind_rows(parsed))
  if (!any(out$period_end == release$period_end)) {
    .rf_abort("Workbook does not contain the declared release period.",
              "schema")
  }
  if (!isTRUE(include_non_ytd)) {
    out$source_value_type <- NULL
  }
  out
}
