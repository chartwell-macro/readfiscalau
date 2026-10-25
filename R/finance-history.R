.rf_finance_series_contract <- data.frame(
  source_key = c(
    "receipts", "underlying cash receipts",
    "payments", "underlying cash payments",
    "underlying cash balance", "headline cash balance",
    "revenue", "income", "expenses",
    "net operating balance", "operating result",
    "net capital investment", "fiscal balance",
    "net future fund earnings",
    "total assets", "assets", "total liabilities", "liabilities",
    "net worth", "net assets", "net debt"
  ),
  series_id = c(
    "receipts", "receipts",
    "payments", "payments",
    "underlying_cash_balance", "headline_cash_balance",
    "revenue", "income", "expenses",
    "net_operating_balance", "operating_result",
    "net_capital_investment", "fiscal_balance",
    "net_future_fund_earnings",
    "total_assets", "total_assets", "total_liabilities",
    "total_liabilities", "net_worth", "net_worth", "net_debt"
  ),
  series = c(
    "Receipts", "Receipts",
    "Payments", "Payments",
    "Underlying cash balance", "Headline cash balance",
    "Revenue", "Income", "Expenses",
    "Net operating balance", "Operating result",
    "Net capital investment", "Fiscal balance",
    "Net Future Fund earnings",
    "Total assets", "Total assets", "Total liabilities",
    "Total liabilities", "Net worth", "Net worth", "Net debt"
  ),
  measure_type = c(
    rep("flow", 14L),
    rep("stock", 7L)
  ),
  accounting_basis = c(
    "cash", "cash", "cash", "cash", "cash", "cash",
    "accrual", "accrual", "accrual", "accrual", "accrual",
    "accrual", "accrual", "cash",
    "accrual", "accrual", "accrual", "accrual",
    "accrual", "accrual", "accrual"
  ),
  stringsAsFactors = FALSE
)

.rf_finance_source_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^less\\s+", "", x)
  x <- gsub("\\s*\\([a-z]\\)", "", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x))
}

.rf_finance_apply_legacy_framework <- function(data) {
  data$accounting_framework <- "GFS"
  start_year <- as.integer(substr(data$fiscal_year, 1L, 4L))
  legacy <- start_year < 2007L
  replacements <- c(
    revenue = "revenue_aas",
    income = "income_aas",
    expenses = "expenses_aas",
    operating_result = "operating_result_aas",
    total_assets = "total_assets_aas",
    total_liabilities = "total_liabilities_aas",
    net_worth = "net_assets_aas"
  )
  labels <- c(
    revenue_aas = "Revenue (AAS)",
    income_aas = "Income (AAS)",
    expenses_aas = "Expenses (AAS)",
    operating_result_aas = "Operating result (AAS)",
    total_assets_aas = "Total assets (AAS)",
    total_liabilities_aas = "Total liabilities (AAS)",
    net_assets_aas = "Net assets (AAS)"
  )
  replace <- legacy & data$series_id %in% names(replacements)
  data$series_id[replace] <- unname(replacements[data$series_id[replace]])
  data$series[replace] <- unname(labels[data$series_id[replace]])
  data$accounting_framework[replace] <- "AAS"
  data
}

.rf_finance_history_release <- function(
    release, manifest, expected_type, argument) {
  if (is.character(release) && length(release) == 1L) {
    row <- manifest[manifest$release_id == release, , drop = FALSE]
  } else {
    row <- .rf_validate_manifest(release)
  }
  if (nrow(row) != 1L) {
    .rf_abort(
      sprintf("`%s` must select exactly one release.", argument),
      "invalid_argument",
      argument = argument
    )
  }
  if (!identical(row$release_type, expected_type)) {
    .rf_abort(
      sprintf("`%s` must select a %s release.", argument, expected_type),
      "release_type",
      argument = argument
    )
  }
  row
}

.rf_finance_history_year <- function(x, argument) {
  .rf_assert_scalar_string(x, argument)
  .rf_normalize_fiscal_year(x)
}

.rf_finance_history_sheet_release <- function(release, fiscal_year) {
  start_year <- as.integer(substr(fiscal_year, 1L, 4L))
  release$fiscal_year <- fiscal_year
  release$period_end <- as.Date(paste0(start_year + 1L, "-05-31"))
  release$schema_family <- if (start_year < 2008L) {
    "finance_mfs_pre_2008_v1"
  } else {
    "finance_mfs_post_2008_v2"
  }
  release
}

.rf_finance_history_source <- function(path, release, fiscal_years) {
  parsed <- lapply(fiscal_years, function(fiscal_year) {
    sheet_release <- .rf_finance_history_sheet_release(
      release, fiscal_year
    )
    data <- .rf_parse_finance_xlsx(
      path,
      sheet_release,
      sheet = fiscal_year,
      include_non_ytd = TRUE
    )
    data$schema_family <- sheet_release$schema_family
    data
  })
  out <- .rf_bind_rows(parsed)
  key <- .rf_finance_source_key(out$series)
  match_index <- match(key, .rf_finance_series_contract$source_key)
  if (anyNA(match_index)) {
    .rf_abort(
      "Finance history contains an unmapped aggregate series.",
      "history_contract",
      series = unique(out$series[is.na(match_index)])
    )
  }
  contract <- .rf_finance_series_contract[match_index, , drop = FALSE]
  out$source_series_id <- out$series_id
  out$source_series <- out$series
  out$series_id <- contract$series_id
  out$series <- contract$series
  out$measure_type <- contract$measure_type
  out$accounting_basis <- contract$accounting_basis
  out <- .rf_finance_apply_legacy_framework(out)
  out$source_value <- out$value
  out$source_unit <- out$unit
  out$value_million <- out$value * out$unit_multiplier / 1e6
  out
}

.rf_finance_expected_periods <- function(fiscal_year) {
  do.call(c, lapply(
    month.name[c(7:12, 1:5)],
    .rf_period_end_for_month,
    fiscal_year = fiscal_year
  ))
}

.rf_finance_derive_group <- function(group) {
  group <- group[order(group$period_end), , drop = FALSE]
  expected <- .rf_finance_expected_periods(group$fiscal_year[[1L]])
  if (!identical(group$period_end, expected)) {
    .rf_abort(
      "Finance history must contain exactly July through May for each series.",
      "history_contract",
      fiscal_year = group$fiscal_year[[1L]],
      series_id = group$series_id[[1L]]
    )
  }
  if (identical(group$measure_type[[1L]], "stock")) {
    group$monthly_value <- group$value_million
    group$ytd_value <- group$value_million
    group$monthly_derivation <- "reported_month_end_level"
    group$ytd_derivation <- "reported_month_end_level"
    missing <- is.na(group$value_million)
    group$monthly_derivation[missing] <- "source_level_missing"
    group$ytd_derivation[missing] <- "source_level_missing"
    return(group)
  }
  monthly <- numeric(nrow(group))
  ytd <- numeric(nrow(group))
  previous_ytd <- 0
  for (index in seq_len(nrow(group))) {
    if (identical(group$source_value_type[[index]], "monthly")) {
      monthly[[index]] <- group$value_million[[index]]
      ytd[[index]] <- previous_ytd + monthly[[index]]
    } else {
      ytd[[index]] <- group$value_million[[index]]
      monthly[[index]] <- ytd[[index]] - previous_ytd
    }
    previous_ytd <- ytd[[index]]
  }
  group$monthly_value <- monthly
  group$ytd_value <- ytd
  group$monthly_derivation <- ifelse(
    group$source_value_type == "monthly",
    "reported_monthly",
    ifelse(
      seq_len(nrow(group)) == 1L,
      "july_ytd_equals_monthly",
      "difference_of_ytd"
    )
  )
  group$ytd_derivation <- ifelse(
    group$source_value_type == "monthly",
    "reconstructed_ytd_from_monthly",
    "reported_ytd"
  )
  missing_monthly <- is.na(group$monthly_value)
  group$monthly_derivation[missing_monthly] <- ifelse(
    is.na(group$value_million[missing_monthly]),
    "source_value_missing",
    "adjacent_ytd_missing"
  )
  missing_ytd <- is.na(group$ytd_value)
  group$ytd_derivation[missing_ytd] <- ifelse(
    is.na(group$value_million[missing_ytd]),
    "source_value_missing",
    "prior_ytd_missing"
  )
  group
}

.rf_finance_derive_months <- function(data) {
  key <- paste(data$fiscal_year, data$series_id, sep = "\r")
  groups <- split(seq_len(nrow(data)), key)
  .rf_bind_rows(lapply(groups, function(rows) {
    .rf_finance_derive_group(data[rows, , drop = FALSE])
  }))
}

.rf_finance_append_june <- function(data, annual) {
  data$is_june <- FALSE
  data$annual_table_id <- NA_character_
  data$annual_series <- NA_character_
  data$annual_value <- NA_real_
  data$annual_release_id <- NA_character_
  data$annual_vintage_date <- as.Date(NA)
  data$annual_resource_url <- NA_character_
  data$annual_raw_content_sha256 <- NA_character_
  data$revision_residual_possible <- FALSE
  if (is.null(annual) || !nrow(annual)) return(data)

  annual_key <- paste(annual$fiscal_year, annual$series_id, sep = "\r")
  if (anyDuplicated(annual_key)) {
    .rf_abort(
      "FBO history repeats a canonical annual outcome.",
      "duplicate_observation"
    )
  }
  annual_index <- stats::setNames(seq_len(nrow(annual)), annual_key)
  grouped <- split(
    seq_len(nrow(data)),
    paste(data$fiscal_year, data$series_id, sep = "\r")
  )
  june <- list()
  for (rows in grouped) {
    group <- data[rows, , drop = FALSE]
    key <- paste(group$fiscal_year[[1L]], group$series_id[[1L]], sep = "\r")
    annual_row <- unname(annual_index[key])
    if (!length(annual_row) || is.na(annual_row)) next
    annual_observation <- annual[annual_row, , drop = FALSE]
    last <- group[which.max(group$period_end), , drop = FALSE]
    last$period_end <- annual_observation$period_end
    last$source_value <- annual_observation$value
    last$source_unit <- annual_observation$unit
    last$source_value_type <- "annual"
    last$value_million <- annual_observation$value
    last$ytd_value <- annual_observation$value
    if (identical(last$measure_type, "flow")) {
      last$monthly_value <- annual_observation$value -
        group$ytd_value[which.max(group$period_end)]
      last$monthly_derivation <- "annual_outcome_less_may_ytd"
      last$ytd_derivation <- "reported_annual_outcome"
      last$revision_residual_possible <- TRUE
    } else {
      last$monthly_value <- annual_observation$value
      last$monthly_derivation <- "reported_annual_end_of_year_level"
      last$ytd_derivation <- "reported_annual_end_of_year_level"
    }
    last$is_june <- TRUE
    last$annual_table_id <- annual_observation$table_id
    last$annual_series <- annual_observation$series
    last$annual_value <- annual_observation$value
    last$annual_release_id <- annual_observation$release_id
    last$annual_vintage_date <- annual_observation$vintage_date
    last$annual_resource_url <- annual_observation$resource_url
    last$annual_raw_content_sha256 <-
      annual_observation$raw_content_sha256
    june[[length(june) + 1L]] <- last
  }
  .rf_bind_rows(c(list(data), june))
}

.rf_finance_history_coverage <- function(data) {
  key <- paste(data$fiscal_year, data$series_id, sep = "\r")
  groups <- split(seq_len(nrow(data)), key)
  .rf_bind_rows(lapply(groups, function(rows) {
    group <- data[rows, , drop = FALSE]
    data.frame(
      fiscal_year = group$fiscal_year[[1L]],
      series_id = group$series_id[[1L]],
      measure_type = group$measure_type[[1L]],
      months = nrow(group),
      june_available = any(group$is_june),
      monthly_values_complete = nrow(group) == 12L &&
        any(group$is_june) && !anyNA(group$monthly_value),
      ytd_values_complete = nrow(group) == 12L &&
        any(group$is_june) && !anyNA(group$ytd_value),
      stringsAsFactors = FALSE
    )
  }))
}

#' Read complete monthly Australian Government fiscal history
#'
#' Reads every fiscal-year sheet in the selected Department of Finance
#' general-government-sector aggregate workbook and normalises the result to
#' AUD million. The monthly source supplies July through May. For flow series,
#' July is the July year-to-date (YTD) value, later YTD columns are differenced
#' to monthly flows, and an explicitly monthly source column is retained and
#' used to reconstruct the next YTD level. Published blanks remain `NA`.
#'
#' When `annual_release` is supplied, compatible Final Budget Outcome (FBO)
#' historical rows complete June. A June flow is the annual FBO actual less May
#' YTD and is flagged because the residual can include revisions to earlier
#' months. Stocks (`total_assets`, `total_liabilities`, `net_worth`, and
#' `net_debt`) are never differenced: they remain month-end levels, and June is
#' added only when the FBO contains a directly comparable annual level. With
#' `annual_release = NULL`, the result intentionally ends in May and no fiscal
#' year is marked complete. The pre-2007 AAS revenue, income, expenses,
#' operating-result, and balance-sheet concepts retain distinct canonical IDs;
#' the function never recasts them into later GFS concepts.
#'
#' `output = "monthly"` places derived monthly flows or month-end stock levels
#' in `value`; `output = "ytd"` places cumulative flow values in `value` while
#' leaving stock levels unchanged. In both modes `value_type`, `derivation`,
#' `is_june`, `complete_fiscal_year`, accounting fields, and source release
#' provenance make the transformation explicit. Rows are ordered by ascending
#' `period_end` and then `series_id`.
#'
#' If `monthly_raw` or `annual_raw` is supplied, the exact source bytes are
#' parsed without a network request. Otherwise missing sources are fetched over
#' HTTPS with the common bounded transport and written atomically to
#' `cache_dir`; an injected `fetcher` provides deterministic offline transport.
#' The function writes only to `cache_dir` and temporary paths, never to the
#' working directory or a global workspace. It emits no warnings during normal
#' operation. Invalid fiscal-year ranges, empty series selections, release-type
#' or accounting-contract mismatches, absent series, malformed workbooks,
#' duplicate annual rows, and transport/hash failures are reported as structured
#' `readfiscalau_*` errors.
#'
#' @param monthly_release A monthly Finance release ID or a complete one-row
#'   manifest. The default is
#'   `"finance-mfs-2026-05-actual-aggregates"`; it must be an actual Department
#'   of Finance general-government-sector aggregate release. The workbook must
#'   contain sheets named in `YYYY-YY` form.
#' @param annual_release An FBO history release ID or a complete one-row
#'   manifest. The default is `"treasury-fbo-2024-25-final-history"`.
#'   Supply `NULL` to return July-to-May observations without annual June
#'   completion. A supplied row must be an actual Treasury
#'   `treasury_fbo_history_docx_v1` release.
#' @param start_fiscal_year Earliest fiscal year to read, in `YYYY-YY` form;
#'   the default is `"2000-01"`.
#' @param end_fiscal_year Optional latest fiscal year in `YYYY-YY` form. The
#'   default `NULL` retains every sheet at or after `start_fiscal_year`.
#' @param series Optional non-empty character vector of canonical series labels
#'   or `series_id`s. The default `NULL` retains all recognised series.
#' @param output Character scalar selecting `"monthly"` (the default) for
#'   derived monthly flows and month-end stock levels, or `"ytd"` for
#'   cumulative flow values and month-end stock levels.
#' @param manifest Optional complete replacement release manifest. `NULL` (the
#'   default) reads the installed manifest.
#' @param cache_dir Bounded hash-addressed cache directory. The default is
#'   [readfiscalau_default_cache()]. It is created as needed for raw-byte or
#'   network reads.
#' @param fetcher Optional offline fetcher passed to the common reader when a
#'   corresponding raw vector is `NULL`. It must provide `raw`, `status`,
#'   `content_type`, `final_url`, and `retrieved_at`.
#' @param monthly_raw Optional exact raw bytes for `monthly_release`; when
#'   supplied, no monthly network request is made.
#' @param annual_raw Optional exact raw bytes for `annual_release`; ignored
#'   when `annual_release = NULL` and otherwise bypasses the FBO network request.
#' @returns A tibble with class
#'   `c("readfiscalau_monthly_history", "tbl_df", "tbl", "data.frame")`,
#'   ordered by `period_end` then `series_id`. It has these 35 columns, in
#'   order: `series_id`, `series`, `source_series_id`, `source_series`,
#'   `measure_type`, `value_type`, `fiscal_year`, `period_end`, `value`,
#'   `unit`, `unit_multiplier`, `derivation`, `source_value`, `source_unit`,
#'   `source_value_type`, `monthly_value`, `ytd_value`, `is_june`,
#'   `complete_fiscal_year`, `revision_residual_possible`, `accounting_basis`,
#'   `accounting_framework`, `consolidation`, `schema_family`,
#'   `monthly_release_id`, `monthly_vintage_date`, `monthly_resource_url`,
#'   `monthly_raw_content_sha256`, `annual_release_id`, `annual_vintage_date`,
#'   `annual_resource_url`, `annual_raw_content_sha256`, `annual_table_id`,
#'   `annual_series`, and `annual_value`. `period_end`,
#'   `monthly_vintage_date`, and `annual_vintage_date` are `Date`; numeric
#'   values and multipliers are numeric; `is_june`, `complete_fiscal_year`, and
#'   `revision_residual_possible` are logical; all other fields are character
#'   except `annual_value`, which is numeric. `value` is always in AUD million
#'   and `unit_multiplier` is always `1e6`. `value_type` is
#'   `"monthly_flow"`, `"cumulative_ytd"`, or `"month_end_level"`.
#'   The `readfiscalau_monthly_history_provenance` attribute is a list with
#'   `output`, monthly and annual source provenances, earliest/latest fiscal
#'   year, the annual June rule, stock rule, accounting-boundary note, and a
#'   coverage tibble containing completeness by fiscal year and series.
#' @seealso [read_au_monthly_financials()], [read_au_fbo()],
#'   [list_au_fiscal_releases()], [readfiscalau_default_cache()]
#' @family historical-readers
#' @examples
#' example_raw <- function(name) {
#'   path <- system.file("extdata", "examples", name,
#'                       package = "readfiscalau")
#'   stopifnot(nzchar(path), file.exists(path))
#'   con <- file(path, open = "rb")
#'   on.exit(close(con), add = TRUE)
#'   readBin(con, what = "raw", n = file.info(path)$size)
#' }
#' manifest <- list_au_fiscal_releases()
#' monthly <- manifest[
#'   manifest$release_id == "finance-mfs-2026-05-actual-aggregates", ,
#'   drop = FALSE
#' ]
#' annual <- manifest[
#'   manifest$release_id == "treasury-fbo-2024-25-final-history", ,
#'   drop = FALSE
#' ]
#' stopifnot(nrow(monthly) == 1L, nrow(annual) == 1L)
#' monthly$release_id <- "example-monthly-history"
#' monthly$resource_id <- "example-monthly-history-xlsx"
#' monthly$fiscal_year <- "2024-25"
#' monthly$period_end <- as.Date("2025-05-31")
#' monthly$expected_sha256 <- NA_character_
#' monthly$revision_semantics <- "Documentation replay fixture, not official data."
#' annual$release_id <- "example-fbo-history"
#' annual$resource_id <- "example-fbo-history-docx"
#' annual$expected_sha256 <- NA_character_
#' annual$revision_semantics <- "Documentation replay fixture, not official data."
#' history <- read_au_monthly_financial_history(
#'   monthly_release = monthly$release_id,
#'   annual_release = annual$release_id,
#'   start_fiscal_year = "2024-25",
#'   end_fiscal_year = "2024-25",
#'   series = "receipts",
#'   manifest = rbind(monthly, annual),
#'   monthly_raw = example_raw("finance-history.xlsx"),
#'   annual_raw = example_raw("fbo-history.docx"),
#'   cache_dir = tempfile("readfiscalau-monthly-history-")
#' )
#' print(history[c(
#'   "period_end", "value", "unit", "value_type", "derivation",
#'   "is_june", "complete_fiscal_year"
#' )])
#' attr(history, "readfiscalau_monthly_history_provenance")$coverage
#' @export
read_au_monthly_financial_history <- function(
    monthly_release = "finance-mfs-2026-05-actual-aggregates",
    annual_release = "treasury-fbo-2024-25-final-history",
    start_fiscal_year = "2000-01",
    end_fiscal_year = NULL,
    series = NULL,
    output = c("monthly", "ytd"),
    manifest = NULL,
    cache_dir = readfiscalau_default_cache(),
    fetcher = NULL,
    monthly_raw = NULL,
    annual_raw = NULL) {
  output <- match.arg(output)
  start_fiscal_year <- .rf_finance_history_year(
    start_fiscal_year, "start_fiscal_year"
  )
  if (!is.null(end_fiscal_year)) {
    end_fiscal_year <- .rf_finance_history_year(
      end_fiscal_year, "end_fiscal_year"
    )
    if (end_fiscal_year < start_fiscal_year) {
      .rf_abort(
        "`end_fiscal_year` must not precede `start_fiscal_year`.",
        "invalid_argument"
      )
    }
  }
  if (!is.null(series) &&
      (!is.character(series) || !length(series) ||
       anyNA(series) || any(!nzchar(series)))) {
    .rf_abort("`series` must be a non-empty character vector.",
              "invalid_argument")
  }

  releases <- .rf_resolve_manifest(manifest)
  monthly_row <- .rf_finance_history_release(
    monthly_release, releases, "monthly_financial_statement",
    "monthly_release"
  )
  if (monthly_row$source_id != "department_finance" ||
      monthly_row$consolidation != "general_government_sector" ||
      monthly_row$actual_forecast != "actual") {
    .rf_abort(
      "Monthly history requires actual Department of Finance GGS aggregates.",
      "history_contract",
      release_id = monthly_row$release_id
    )
  }
  monthly_seed <- .rf_read_release(
    monthly_row,
    expected_type = "monthly_financial_statement",
    cache_dir = cache_dir,
    fetcher = fetcher,
    raw = monthly_raw
  )
  monthly_provenance <- attr(monthly_seed, "readfiscalau_provenance")
  workbook_path <- monthly_provenance$raw_snapshot_path
  sheets <- readxl::excel_sheets(workbook_path)
  fiscal_years <- sheets[grepl("^20[0-9]{2}-[0-9]{2}$", sheets)]
  keep_year <- fiscal_years >= start_fiscal_year
  if (!is.null(end_fiscal_year)) {
    keep_year <- keep_year & fiscal_years <= end_fiscal_year
  }
  fiscal_years <- fiscal_years[keep_year]
  fiscal_years <- sort(unique(fiscal_years))
  if (!length(fiscal_years)) {
    .rf_abort(
      "The Finance workbook has no fiscal-year sheets in the requested range.",
      "history_series"
    )
  }
  source <- .rf_finance_history_source(
    workbook_path, monthly_row, fiscal_years
  )
  derived <- .rf_finance_derive_months(source)

  annual <- NULL
  annual_provenance <- NULL
  if (!is.null(annual_release)) {
    annual_row <- .rf_finance_history_release(
      annual_release, releases, "fbo", "annual_release"
    )
    if (annual_row$source_id != "australian_treasury" ||
        annual_row$consolidation != "general_government_sector" ||
        annual_row$actual_forecast != "actual" ||
        annual_row$schema_family != "treasury_fbo_history_docx_v1") {
      .rf_abort(
        "Annual completion requires actual Treasury FBO historical tables.",
        "history_contract",
        release_id = annual_row$release_id
      )
    }
    annual <- .rf_read_release(
      annual_row,
      expected_type = "fbo",
      cache_dir = cache_dir,
      fetcher = fetcher,
      raw = annual_raw
    )
    annual_provenance <- attr(annual, "readfiscalau_provenance")
    annual <- annual[
      annual$fiscal_year %in% fiscal_years,
      ,
      drop = FALSE
    ]
  }
  out <- .rf_finance_append_june(derived, annual)

  if (!is.null(series)) {
    wanted <- .rf_slug(series)
    out <- out[
      out$series_id %in% wanted | out$series %in% series,
      ,
      drop = FALSE
    ]
    if (!nrow(out)) {
      .rf_abort(
        "None of the requested canonical Finance series are present.",
        "history_series",
        series = series
      )
    }
  }
  out$value <- if (output == "monthly") {
    out$monthly_value
  } else {
    out$ytd_value
  }
  out$value_type <- ifelse(
    out$measure_type == "stock",
    "month_end_level",
    if (output == "monthly") "monthly_flow" else "cumulative_ytd"
  )
  out$derivation <- if (output == "monthly") {
    out$monthly_derivation
  } else {
    out$ytd_derivation
  }
  out$unit <- "AUD million"
  out$unit_multiplier <- 1e6
  out$monthly_release_id <- monthly_row$release_id
  out$monthly_vintage_date <- monthly_row$release_date
  out$monthly_resource_url <- monthly_provenance$source_url
  out$monthly_raw_content_sha256 <-
    monthly_provenance$raw_content_sha256
  coverage <- .rf_finance_history_coverage(out)
  coverage$complete_fiscal_year <- if (output == "monthly") {
    coverage$monthly_values_complete
  } else {
    coverage$ytd_values_complete
  }
  complete_key <- paste(
    coverage$fiscal_year[coverage$complete_fiscal_year],
    coverage$series_id[coverage$complete_fiscal_year],
    sep = "\r"
  )
  out$complete_fiscal_year <- paste(
    out$fiscal_year, out$series_id, sep = "\r"
  ) %in% complete_key
  out <- out[order(out$period_end, out$series_id), , drop = FALSE]

  keep <- c(
    "series_id", "series", "source_series_id", "source_series",
    "measure_type", "value_type", "fiscal_year", "period_end", "value",
    "unit", "unit_multiplier", "derivation", "source_value",
    "source_unit", "source_value_type", "monthly_value", "ytd_value",
    "is_june", "complete_fiscal_year", "revision_residual_possible",
    "accounting_basis", "accounting_framework", "consolidation",
    "schema_family",
    "monthly_release_id", "monthly_vintage_date", "monthly_resource_url",
    "monthly_raw_content_sha256", "annual_release_id",
    "annual_vintage_date", "annual_resource_url",
    "annual_raw_content_sha256", "annual_table_id", "annual_series",
    "annual_value"
  )
  out <- tibble::as_tibble(out[keep])
  attr(out, "readfiscalau_monthly_history_provenance") <- list(
    output = output,
    monthly_release = monthly_provenance,
    annual_release = annual_provenance,
    earliest_fiscal_year = min(fiscal_years),
    latest_fiscal_year = max(fiscal_years),
    annual_completion_rule = paste(
      "For flows, June equals the FBO annual outcome less Finance May YTD;",
      "the residual can include revisions to earlier months."
    ),
    stock_rule = paste(
      "Stocks are month-end levels and are never differenced;",
      "June is added only for a directly crosswalked annual FBO level."
    ),
    accounting_boundary = paste(
      "Finance identifies a material format/accounting change at 2008-09;",
      "schema families remain explicit and historical values are not recast."
    ),
    coverage = coverage
  )
  class(out) <- c("readfiscalau_monthly_history", class(out))
  out
}

#' Print a monthly fiscal-history result
#'
#' Prints a compact header containing the output mode, fiscal-year span, and
#' latest complete fiscal year before delegating to the standard data-frame
#' printer. Printing does not fetch data, write cache files, or alter the
#' derivation flags.
#'
#' @param x A `readfiscalau_monthly_history` object returned by
#'   [read_au_monthly_financial_history()].
#' @param ... Additional arguments passed to the next print method.
#' @returns The input `x`, invisibly.
#' @seealso [read_au_monthly_financial_history()]
#' @family historical-readers
#' @rdname read_au_monthly_financial_history
#' @method print readfiscalau_monthly_history
#' @export
print.readfiscalau_monthly_history <- function(x, ...) {
  provenance <- attr(x, "readfiscalau_monthly_history_provenance")
  complete <- provenance$coverage
  complete <- complete[complete$complete_fiscal_year, , drop = FALSE]
  latest_complete <- if (nrow(complete)) {
    max(complete$fiscal_year)
  } else {
    "none"
  }
  cat(sprintf(
    "<readfiscalau_monthly_history> %s | %s to %s | latest complete %s\n",
    provenance$output,
    provenance$earliest_fiscal_year,
    provenance$latest_fiscal_year,
    latest_complete
  ))
  NextMethod()
}
