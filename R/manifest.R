.rf_release_fields <- c(
  "release_id", "source_id", "release_type", "release_status", "release_date",
  "period_end", "fiscal_year", "frequency", "accounting_basis",
  "consolidation", "actual_forecast", "revision_semantics", "landing_url",
  "resource_id", "resource_role", "resource_url", "resource_updated_at",
  "expected_sha256", "media_type", "schema_family", "parser_version",
  "terms_url", "license", "attribution"
)

.rf_manifest_is_safe_https_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url) ||
      !startsWith(url, "https://")) {
    return(FALSE)
  }
  # curl can interpret `https:///path` as a URL whose host is `path`.  Check
  # the authority in the original spelling before relying on parsed fields.
  authority <- sub("[/?#].*$", "", substring(url, 9L))
  if (!nzchar(authority) || grepl("@", authority, fixed = TRUE)) {
    return(FALSE)
  }
  isTRUE(.rf_is_safe_https_url(url))
}

.rf_validate_manifest <- function(x) {
  x <- tibble::as_tibble(x)
  missing <- setdiff(.rf_release_fields, names(x))
  if (length(missing)) {
    .rf_abort(
      sprintf("Release manifest is missing: %s.", paste(missing, collapse = ", ")),
      "schema", missing_fields = missing
    )
  }
  x <- x[.rf_release_fields]
  character_fields <- setdiff(
    .rf_release_fields,
    c("release_date", "period_end", "resource_updated_at", "expected_sha256")
  )
  incomplete <- vapply(
    x[character_fields],
    function(value) anyNA(value) || any(!nzchar(trimws(as.character(value)))),
    logical(1)
  )
  if (any(incomplete)) {
    .rf_abort(
      sprintf(
        "Release manifest has empty required field(s): %s.",
        paste(names(incomplete)[incomplete], collapse = ", ")
      ),
      "schema", fields = names(incomplete)[incomplete]
    )
  }
  if (anyNA(x$release_id) || any(!grepl("^[a-z0-9][a-z0-9-]*$", x$release_id)) ||
      anyDuplicated(x$release_id)) {
    .rf_abort(
      "Release IDs must be unique lowercase letters, numbers, and hyphens.",
      "schema"
    )
  }
  if (any(!grepl("^[a-z0-9][a-z0-9_]*$", x$resource_role))) {
    .rf_abort("Resource roles must be lowercase snake-case identifiers.",
              "schema")
  }
  vintage_key <- paste(
    x$source_id, x$release_type, x$release_date, x$period_end,
    x$release_status, x$resource_role, sep = "\r"
  )
  if (anyDuplicated(vintage_key)) {
    .rf_abort(
      "Release manifest contains a publication-vintage collision.",
      "vintage_collision",
      release_ids = x$release_id[duplicated(vintage_key) |
        duplicated(vintage_key, fromLast = TRUE)]
    )
  }
  valid_types <- c(
    "budget", "myefo", "fbo", "monthly_financial_statement",
    "cfs", "historical_fiscal_data"
  )
  bad_type <- setdiff(unique(x$release_type), valid_types)
  if (length(bad_type)) {
    .rf_abort(sprintf("Unsupported release type(s): %s.",
                      paste(bad_type, collapse = ", ")), "schema")
  }
  x$release_date <- as.Date(x$release_date)
  x$period_end <- as.Date(x$period_end)
  if (anyNA(x$release_date) || anyNA(x$period_end)) {
    .rf_abort("Release and period dates must be complete ISO dates.", "schema")
  }
  resource_updated_at <- suppressWarnings(as.POSIXct(
    x$resource_updated_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  ))
  if (anyNA(resource_updated_at)) {
    .rf_abort(
      "Resource update timestamps must use YYYY-MM-DDTHH:MM:SSZ.",
      "schema"
    )
  }
  x$resource_updated_at <- format(
    resource_updated_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  )
  supplied_hash <- !is.na(x$expected_sha256) & nzchar(x$expected_sha256)
  if (any(supplied_hash &
          !grepl("^[a-f0-9]{64}$", x$expected_sha256))) {
    .rf_abort("Expected SHA-256 values must be 64 lowercase hex characters.",
              "schema")
  }
  x$fiscal_year <- .rf_normalize_fiscal_year(x$fiscal_year)
  start_year <- as.integer(substr(x$fiscal_year, 1L, 4L))
  period_year <- as.integer(format(x$period_end, "%Y"))
  period_month <- as.integer(format(x$period_end, "%m"))
  expected_start <- ifelse(period_month <= 6L, period_year - 1L, period_year)
  if (any(start_year != expected_start)) {
    .rf_abort("Fiscal year conflicts with the selected reporting period.",
              "schema")
  }
  next_month <- as.Date(format(x$period_end + 4L, "%Y-%m-01"))
  if (any(x$period_end != next_month - 1L)) {
    .rf_abort("Reporting periods must end on the final day of a month.",
              "schema")
  }
  monthly <- x$release_type == "monthly_financial_statement"
  june <- monthly & format(x$period_end, "%m") == "06"
  if (any(june)) {
    .rf_abort(
      "June cannot be classified as a monthly financial statement; use FBO or CFS.",
      "june_is_annual", release_ids = x$release_id[june]
    )
  }
  annual <- x$release_type %in% c(
    "budget", "myefo", "fbo", "cfs", "historical_fiscal_data"
  )
  if (any(annual & format(x$period_end, "%m") != "06")) {
    .rf_abort("Annual fiscal releases must use a 30 June period end.",
              "schema")
  }
  valid_sources <- c(
    "australian_treasury",
    "department_finance",
    "parliamentary_budget_office"
  )
  if (any(!x$source_id %in% valid_sources)) {
    .rf_abort("Release manifest contains an unsupported source.", "schema")
  }
  if (any(x$release_type %in% c("budget", "myefo", "fbo") &
          x$source_id != "australian_treasury") ||
      any(x$release_type %in% c("monthly_financial_statement", "cfs") &
          x$source_id != "department_finance") ||
      any(x$release_type == "historical_fiscal_data" &
          x$source_id != "parliamentary_budget_office")) {
    .rf_abort("Release type conflicts with its responsible source.", "schema")
  }
  valid_status <- c("final", "revised", "snapshot", "superseded")
  if (any(!x$release_status %in% valid_status)) {
    .rf_abort("Release manifest contains an unsupported release status.",
              "schema")
  }
  expected_frequency <- ifelse(
    monthly,
    "monthly",
    ifelse(
      x$release_type %in%
        c("fbo", "cfs", "historical_fiscal_data"),
      "annual",
      "release"
    )
  )
  if (any(x$frequency != expected_frequency)) {
    .rf_abort("Release frequency conflicts with release type.", "schema")
  }
  if (any(!x$accounting_basis %in% c("cash", "accrual", "mixed")) ||
      any(!x$consolidation %in%
          c("general_government_sector", "whole_of_government", "mixed")) ||
      any(!x$actual_forecast %in% c("actual", "forecast", "mixed"))) {
    .rf_abort("Release accounting or observation semantics are unsupported.",
              "schema")
  }
  urls <- c(x$landing_url, x$resource_url, x$terms_url)
  if (any(!grepl("^https://", urls))) {
    .rf_abort("Release URLs must use HTTPS.", "schema")
  }
  if (any(!vapply(urls, .rf_manifest_is_safe_https_url, logical(1)))) {
    .rf_abort(
      "Release URLs must be valid, credential-free HTTPS URLs.",
      "schema"
    )
  }
  csv_schema <- x$schema_family %in%
    c("treasury_chart_csv_v1", "treasury_chart_csv_v2")
  zip_schema <- x$schema_family == "treasury_chart_zip_csv_v2"
  budget_pdf_schema <-
    x$schema_family == "treasury_budget_pdf_snapshot_v1"
  myefo_pdf_schema <-
    x$schema_family == "treasury_myefo_pdf_snapshot_v1"
  fbo_history_schema <- x$schema_family == "treasury_fbo_history_docx_v1"
  pbo_history_schema <-
    x$schema_family == "pbo_historical_fiscal_data_xlsx_v1"
  finance_schema <- x$schema_family %in%
    c("finance_mfs_pre_2008_v1", "finance_mfs_post_2008_v2")
  if (any(!(csv_schema | zip_schema | budget_pdf_schema |
            myefo_pdf_schema | fbo_history_schema |
            pbo_history_schema | finance_schema)) ||
      any(csv_schema & !grepl("csv", x$media_type, ignore.case = TRUE)) ||
      any(zip_schema & !grepl("zip", x$media_type, ignore.case = TRUE)) ||
      any(budget_pdf_schema &
          !grepl("pdf|zip", x$media_type, ignore.case = TRUE)) ||
      any(budget_pdf_schema & x$release_type != "budget") ||
      any(myefo_pdf_schema &
          !grepl("pdf", x$media_type, ignore.case = TRUE)) ||
      any(myefo_pdf_schema & x$release_type != "myefo") ||
      any(fbo_history_schema &
          !grepl("wordprocessingml|document", x$media_type,
                 ignore.case = TRUE)) ||
      any(fbo_history_schema & x$release_type != "fbo") ||
      any(pbo_history_schema &
          !grepl("spreadsheet|excel", x$media_type,
                 ignore.case = TRUE)) ||
      any(pbo_history_schema &
          x$release_type != "historical_fiscal_data") ||
      any(finance_schema &
          !grepl("spreadsheet|excel", x$media_type, ignore.case = TRUE)) ||
      any(finance_schema & !monthly)) {
    .rf_abort("Media type, schema family, and release type are incompatible.",
              "schema")
  }
  expected_parser <- vapply(
    x$schema_family,
    function(schema_family) {
      .rf_schema_contract(schema_family)$parser_version[[1L]]
    },
    character(1)
  )
  if (any(x$parser_version != expected_parser)) {
    .rf_abort(
      "Release parser version conflicts with the supported schema contract.",
      "schema",
      release_ids = x$release_id[x$parser_version != expected_parser]
    )
  }
  for (index in seq_len(nrow(x))) {
    terms <- .rf_terms_contract(x$source_id[[index]], x$resource_url[[index]])
    actual <- unname(unlist(
      x[index, c("terms_url", "license", "attribution"), drop = FALSE],
      use.names = FALSE
    ))
    expected <- unname(unlist(terms, use.names = FALSE))
    if (!identical(actual, expected)) {
      .rf_abort(
        "Release terms, licence, or attribution conflicts with its source contract.",
        "schema",
        release_id = x$release_id[[index]]
      )
    }
  }
  x
}

.rf_builtin_manifest <- function() {
  path <- system.file("extdata", "releases.csv", package = "readfiscalau")
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "releases.csv")
  }
  cached <- .rf_internal_file_cache_get("release_manifest", path)
  if (!is.null(cached)) {
    return(cached)
  }
  manifest <- .rf_validate_manifest(utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE, na.strings = ""
  ))
  .rf_internal_file_cache_set("release_manifest", path, manifest)
  manifest
}

.rf_resolve_manifest <- function(manifest = NULL) {
  if (is.null(manifest)) {
    return(.rf_builtin_manifest())
  }
  .rf_validate_manifest(manifest)
}

#' List the curated Australian fiscal release manifest
#'
#' Returns the installed, versioned manifest without contacting a publisher.
#' Each row identifies a publication vintage or an explicitly dated upstream
#' snapshot, not just a reporting period. Release rows retain their own
#' `release_id`, `resource_updated_at`, expected SHA-256, schema family, parser
#' version, revision description, and source terms. The built-in manifest is
#' read from `inst/extdata/releases.csv` and memoised in the current R process;
#' passing `manifest` validates the supplied replacement instead.
#'
#' The source filter maps `"treasury"` to Australian Treasury, `"finance"` to
#' the Department of Finance, and `"pbo"` to the Parliamentary Budget Office.
#' A release-type filter may be a character vector, including
#' `"budget"`, `"myefo"`, `"fbo"`, `"cfs"`,
#' `"monthly_financial_statement"`, or `"historical_fiscal_data"`.
#'
#' @param source Character scalar selecting `"all"`, `"treasury"`,
#'   `"finance"`, or `"pbo"`; the default is `"all"`.
#' @param release_type Optional character vector of release types to retain;
#'   `NULL` (the default) retains every validated type.
#' @param manifest Optional data frame containing the complete 24-column
#'   manifest contract. `NULL` (the default) reads the installed manifest.
#' @returns A tibble with exactly these columns, in this order:
#'   `release_id`, `source_id`, `release_type`, `release_status`,
#'   `release_date`, `period_end`, `fiscal_year`, `frequency`,
#'   `accounting_basis`, `consolidation`, `actual_forecast`,
#'   `revision_semantics`, `landing_url`, `resource_id`, `resource_role`,
#'   `resource_url`, `resource_updated_at`, `expected_sha256`, `media_type`,
#'   `schema_family`, `parser_version`, `terms_url`, `license`, and
#'   `attribution`. `release_date` and `period_end` are `Date` vectors;
#'   `expected_sha256` may contain `NA` for an unpinned custom row; all other
#'   fields are non-empty character vectors. Rows are ordered by decreasing
#'   `release_date` and then decreasing `release_id`.
#' @details The manifest validator rejects missing fields, invalid IDs or
#'   timestamps, unsupported source/release/schema combinations, duplicate
#'   vintage keys, non-HTTPS URLs, parser-version mismatches, and terms or
#'   licence strings that disagree with the package's source contract. This
#'   function never downloads a release and does not write the source cache.
#' @seealso [list_au_budget_series()], [list_au_myefo_series()],
#'   [read_au_fiscal_release()]
#' @family release-catalogue
#' @examples
#' pbo <- list_au_fiscal_releases(source = "pbo")
#' print(pbo[, c("release_id", "release_date", "resource_role")],
#'       row.names = FALSE)
#' stopifnot(nrow(pbo) >= 1L, inherits(pbo$release_date, "Date"))
#' @export
list_au_fiscal_releases <- function(
    source = c("all", "treasury", "finance", "pbo"),
    release_type = NULL,
    manifest = NULL) {
  source <- match.arg(source)
  x <- .rf_resolve_manifest(manifest)
  if (source != "all") {
    id <- switch(
      source,
      treasury = "australian_treasury",
      finance = "department_finance",
      pbo = "parliamentary_budget_office"
    )
    x <- x[x$source_id == id, , drop = FALSE]
  }
  if (!is.null(release_type)) {
    x <- x[x$release_type %in% release_type, , drop = FALSE]
  }
  x[order(x$release_date, x$release_id, decreasing = TRUE), , drop = FALSE]
}

.rf_vintage_series_coverage <- function(panel) {
  key <- paste(
    panel$release_id,
    panel$table_family,
    panel$series_id,
    panel$actual_forecast,
    sep = "\r"
  )
  ordered_rows <- order(key, panel$period_end)
  ordered <- panel[ordered_rows, , drop = FALSE]
  ordered_key <- key[ordered_rows]
  first_rows <- which(!duplicated(ordered_key))
  last_rows <- c(first_rows[-1L] - 1L, nrow(ordered))

  coverage <- data.frame(
    release_id = ordered$release_id[first_rows],
    table_id = ordered$table_id[first_rows],
    table_family = ordered$table_family[first_rows],
    table_label = ordered$table_label[first_rows],
    source_table = ordered$source_table[first_rows],
    series_id = ordered$series_id[first_rows],
    series = ordered$series[first_rows],
    actual_forecast = ordered$actual_forecast[first_rows],
    profile_type = ordered$profile_type[first_rows],
    start_date = as.Date(ordered$period_end[first_rows]),
    end_date = as.Date(ordered$period_end[last_rows]),
    observation_count = last_rows - first_rows + 1L,
    unit = ordered$unit[first_rows],
    accounting_basis = ordered$accounting_basis[first_rows],
    consolidation = ordered$consolidation[first_rows],
    observation_source_url =
      ordered$observation_source_url[first_rows],
    observation_source_sha256 =
      ordered$observation_source_sha256[first_rows],
    stringsAsFactors = FALSE
  )
  row.names(coverage) <- NULL
  coverage
}

#' List exact coverage for Budget-vintage observations
#'
#' Reads the bundled curated Budget observation panel and returns one row for
#' each publication, table family, series, and available actual/forecast status.
#' The five supported families are major economic parameters, Budget
#' aggregates, detailed domestic-economy forecasts, cash receipts, and cash
#' interest. This is a local metadata operation: it does not download or
#' retain the source PDF, workbook, or chart document. The panel and the
#' supplied release manifest are cross-checked by resource URL and SHA-256;
#' those checks are what make the reported coverage reproducible.
#'
#' @param manifest Optional complete release manifest with the same contract
#'   accepted by [list_au_fiscal_releases()]. `NULL` (the default) uses the
#'   installed manifest.
#' @returns A tibble with one row per unique
#'   `release_id`/`table_family`/`series_id`/`actual_forecast` combination and
#'   exactly these columns, in this order: `release_id`, `source_id`,
#'   `release_date`, `release_status`, `table_id`, `table_family`,
#'   `table_label`, `source_table`, `series_id`, `series`, `actual_forecast`,
#'   `profile_type`, `start_date`, `end_date`, `observation_count`, `unit`,
#'   `accounting_basis`, `consolidation`, `observation_source_url`,
#'   `observation_source_sha256`, `resource_url`, and `source_sha256`.
#'   `release_date`, `start_date`, and `end_date` are `Date` vectors;
#'   `observation_count` is a positive integer-valued vector; all other
#'   columns are character vectors; hashes are lowercase 64-character SHA-256
#'   strings; and `actual_forecast` is either `"actual"` or `"forecast"`.
#'   For Budget coverage, `profile_type` is `"actual"` for actual rows and
#'   `"budget"` for forecast rows (including Budget-comparator rows). Rows are
#'   ordered by decreasing `release_date`, then `release_id`, `table_family`,
#'   `series_id`, and `actual_forecast`.
#' @details A `source_hash` error means the bundled panel no longer agrees with
#'   the manifest; a `schema` error means its coverage contract is incomplete
#'   or inconsistent. The function may reuse an in-memory file-fingerprint
#'   cache, but has no network or filesystem side effect beyond reading the
#'   installed panel.
#' @seealso [list_au_fiscal_releases()], [list_au_myefo_series()],
#'   [read_au_budget()], [read_au_budget_history()]
#' @family release-coverage
#' @examples
#' budget <- list_au_budget_series()
#' print(utils::head(budget[, c(
#'   "release_id", "table_family", "series", "actual_forecast",
#'   "start_date", "end_date"
#' )], 4L), row.names = FALSE)
#' stopifnot(
#'   nrow(budget) > 0L,
#'   all(budget$start_date <= budget$end_date),
#'   all(budget$observation_count >= 1L)
#' )
#' @export
list_au_budget_series <- function(manifest = NULL) {
  releases <- .rf_resolve_manifest(manifest)
  panel <- .rf_budget_vintage_observations()
  panel_path <- attr(panel, "readfiscalau_panel_path")
  cache_kind <- paste0(
    "budget_series_coverage_",
    digest::digest(releases, algo = "sha256")
  )
  cached <- .rf_internal_file_cache_get(cache_kind, panel_path)
  if (!is.null(cached)) {
    return(cached)
  }
  panel_release_rows <- match(panel$release_id, releases$release_id)
  if (anyNA(panel_release_rows) ||
      any(panel$source_url != releases$resource_url[panel_release_rows]) ||
      any(panel$source_sha256 !=
            releases$expected_sha256[panel_release_rows])) {
    .rf_abort(
      "Curated Budget-series coverage conflicts with the release manifest.",
      "source_hash"
    )
  }

  coverage <- .rf_vintage_series_coverage(panel)
  release_rows <- match(coverage$release_id, releases$release_id)
  if (anyNA(release_rows)) {
    .rf_abort(
      "Budget-series coverage has no complete release-manifest binding.",
      "schema"
    )
  }
  coverage$source_id <- releases$source_id[release_rows]
  coverage$release_date <- releases$release_date[release_rows]
  coverage$release_status <- releases$release_status[release_rows]
  coverage$resource_url <- releases$resource_url[release_rows]
  coverage$source_sha256 <- releases$expected_sha256[release_rows]
  coverage <- coverage[c(
    "release_id", "source_id", "release_date", "release_status",
    "table_id", "table_family", "table_label", "source_table",
    "series_id", "series", "actual_forecast", "profile_type",
    "start_date", "end_date",
    "observation_count", "unit", "accounting_basis", "consolidation",
    "observation_source_url", "observation_source_sha256",
    "resource_url", "source_sha256"
  )]
  coverage$key <- paste(
    coverage$release_id,
    coverage$table_family,
    coverage$series_id,
    coverage$actual_forecast,
    sep = "\r"
  )
  forecast_key <- paste(
    coverage$release_id[coverage$actual_forecast == "forecast"],
    coverage$table_family[coverage$actual_forecast == "forecast"],
    coverage$series_id[coverage$actual_forecast == "forecast"],
    sep = "\r"
  )
  required_forecast_key <- unique(paste(
    panel$release_id,
    panel$table_family,
    panel$series_id,
    sep = "\r"
  ))
  if (anyDuplicated(coverage$key) ||
      anyNA(coverage$start_date) ||
      anyNA(coverage$end_date) ||
      any(coverage$start_date > coverage$end_date) ||
      any(coverage$observation_count < 1L) ||
      any(!coverage$actual_forecast %in% c("actual", "forecast")) ||
      any(!grepl("^[a-f0-9]{64}$", coverage$source_sha256)) ||
      any(!grepl(
        "^[a-f0-9]{64}$",
        coverage$observation_source_sha256
      )) ||
      any(!nzchar(coverage$table_family)) ||
      any(!nzchar(coverage$table_label)) ||
      !setequal(
        unique(coverage$table_family),
        c(
          "major_economic_parameters",
          "budget_aggregates",
          "domestic_economy_forecasts",
          "cash_receipts",
          "cash_interest"
        )
      ) ||
      !setequal(forecast_key, required_forecast_key)) {
    .rf_abort(
      "Budget-series observation coverage failed its contract.",
      "schema"
    )
  }
  coverage$key <- NULL
  coverage <- coverage[order(
    -as.numeric(coverage$release_date),
    coverage$release_id,
    coverage$table_family,
    coverage$series_id,
    coverage$actual_forecast
  ), , drop = FALSE]
  row.names(coverage) <- NULL
  coverage <- tibble::as_tibble(coverage)
  .rf_internal_file_cache_set(cache_kind, panel_path, coverage)
  coverage
}

#' List exact coverage for MYEFO-vintage observations
#'
#' Reads the bundled curated Mid-Year Economic and Fiscal Outlook (MYEFO)
#' observation panel and returns one row for each publication, table family,
#' series, and available actual/forecast status. The six supported families are
#' major economic parameters, fiscal aggregates, Budget-comparator aggregates,
#' detailed domestic-economy forecasts, cash receipts, and cash interest.
#' This operation is local and does not fetch or retain a MYEFO PDF. The
#' bundled panel is checked against the release manifest's resource URLs and
#' SHA-256 values before coverage is returned.
#'
#' @param manifest Optional complete release manifest with the contract
#'   accepted by [list_au_fiscal_releases()]. `NULL` (the default) uses the
#'   installed manifest.
#' @returns A tibble with one row per unique
#'   `release_id`/`table_family`/`series_id`/`actual_forecast` combination and
#'   exactly these columns, in this order: `release_id`, `source_id`,
#'   `release_date`, `release_status`, `table_id`, `table_family`,
#'   `table_label`, `source_table`, `series_id`, `series`, `actual_forecast`,
#'   `profile_type`, `start_date`, `end_date`, `observation_count`, `unit`,
#'   `accounting_basis`, `consolidation`, `observation_source_url`,
#'   `observation_source_sha256`, `resource_url`, and `source_sha256`.
#'   `release_date`, `start_date`, and `end_date` are `Date` vectors;
#'   `observation_count` is a positive integer-valued vector; all other
#'   columns are character vectors; hashes are lowercase 64-character SHA-256
#'   strings; and `actual_forecast` is either `"actual"` or `"forecast"`.
#'   For MYEFO coverage, `profile_type` is `"actual"` for actual rows,
#'   `"myefo"` for ordinary forecast rows, and `"budget"` for
#'   Budget-comparator forecasts. Rows are ordered by decreasing
#'   `release_date`, then `release_id`, `table_family`, `series_id`, and
#'   `actual_forecast`.
#' @details A `source_hash` error indicates disagreement between the curated
#'   panel and manifest. A `schema` error indicates missing, duplicate, or
#'   semantically invalid coverage. The function may reuse an in-memory
#'   file-fingerprint cache and performs no network request or cache-file write.
#'   The observations retain the source URL and hash required by the publisher's
#'   terms; consult `inst/TERMS.md` for the source-specific licence caveats,
#'   including the PBO terms that apply to PBO-sourced inputs elsewhere in the
#'   package.
#' @seealso [list_au_fiscal_releases()], [list_au_budget_series()],
#'   [read_au_myefo()], [read_au_budget_history()]
#' @family release-coverage
#' @examples
#' myefo <- list_au_myefo_series()
#' print(utils::head(myefo[, c(
#'   "release_id", "table_family", "series", "actual_forecast",
#'   "start_date", "end_date"
#' )], 4L), row.names = FALSE)
#' stopifnot(
#'   nrow(myefo) > 0L,
#'   all(myefo$start_date <= myefo$end_date),
#'   all(myefo$observation_count >= 1L)
#' )
#' @export
list_au_myefo_series <- function(manifest = NULL) {
  releases <- .rf_resolve_manifest(manifest)
  panel <- .rf_myefo_vintage_observations()
  panel_path <- attr(panel, "readfiscalau_panel_path")
  cache_kind <- paste0(
    "myefo_series_coverage_",
    digest::digest(releases, algo = "sha256")
  )
  cached <- .rf_internal_file_cache_get(cache_kind, panel_path)
  if (!is.null(cached)) {
    return(cached)
  }
  panel_release_rows <- match(panel$release_id, releases$release_id)
  if (anyNA(panel_release_rows) ||
      any(panel$source_url != releases$resource_url[panel_release_rows]) ||
      any(panel$source_sha256 !=
            releases$expected_sha256[panel_release_rows])) {
    .rf_abort(
      "Curated MYEFO-series coverage conflicts with the release manifest.",
      "source_hash"
    )
  }

  coverage <- .rf_vintage_series_coverage(panel)
  release_rows <- match(coverage$release_id, releases$release_id)
  coverage$source_id <- releases$source_id[release_rows]
  coverage$release_date <- releases$release_date[release_rows]
  coverage$release_status <- releases$release_status[release_rows]
  coverage$resource_url <- releases$resource_url[release_rows]
  coverage$source_sha256 <- releases$expected_sha256[release_rows]
  coverage <- coverage[c(
    "release_id", "source_id", "release_date", "release_status",
    "table_id", "table_family", "table_label", "source_table",
    "series_id", "series", "actual_forecast", "profile_type",
    "start_date", "end_date",
    "observation_count", "unit", "accounting_basis", "consolidation",
    "observation_source_url", "observation_source_sha256",
    "resource_url", "source_sha256"
  )]
  coverage$key <- paste(
    coverage$release_id,
    coverage$table_family,
    coverage$series_id,
    coverage$actual_forecast,
    sep = "\r"
  )
  forecast_key <- paste(
    coverage$release_id[coverage$actual_forecast == "forecast"],
    coverage$table_family[coverage$actual_forecast == "forecast"],
    coverage$series_id[coverage$actual_forecast == "forecast"],
    sep = "\r"
  )
  required_forecast_key <- unique(paste(
    panel$release_id,
    panel$table_family,
    panel$series_id,
    sep = "\r"
  ))
  if (anyDuplicated(coverage$key) ||
      anyNA(coverage$start_date) ||
      anyNA(coverage$end_date) ||
      any(coverage$start_date > coverage$end_date) ||
      any(coverage$observation_count < 1L) ||
      any(!coverage$actual_forecast %in% c("actual", "forecast")) ||
      any(!grepl("^[a-f0-9]{64}$", coverage$source_sha256)) ||
      any(!grepl(
        "^[a-f0-9]{64}$",
        coverage$observation_source_sha256
      )) ||
      any(!nzchar(coverage$table_family)) ||
      any(!nzchar(coverage$table_label)) ||
      !setequal(forecast_key, required_forecast_key)) {
    .rf_abort(
      "MYEFO-series observation coverage failed its contract.",
      "schema"
    )
  }
  coverage$key <- NULL
  coverage <- coverage[order(
    -as.numeric(coverage$release_date),
    coverage$release_id,
    coverage$table_family,
    coverage$series_id,
    coverage$actual_forecast
  ), , drop = FALSE]
  row.names(coverage) <- NULL
  coverage <- tibble::as_tibble(coverage)
  .rf_internal_file_cache_set(cache_kind, panel_path, coverage)
  coverage
}

`%||%` <- function(x, y) if (is.null(x)) y else x
