.rf_attach_provenance <- function(data, release, fetched) {
  repeated <- c(
    "release_id", "source_id", "release_type", "release_status", "release_date",
    "revision_semantics", "landing_url", "resource_url", "schema_family",
    "resource_id", "resource_role", "resource_updated_at", "parser_version",
    "frequency", "media_type", "terms_url", "license", "attribution"
  )
  release_columns <- as.data.frame(
    lapply(repeated, function(field) rep(release[[field]], nrow(data))),
    stringsAsFactors = FALSE
  )
  names(release_columns) <- repeated
  release_columns$vintage_date <- rep(release$release_date, nrow(data))
  release_columns$retrieved_at <- rep(fetched$retrieved_at, nrow(data))
  release_columns$raw_content_sha256 <- rep(fetched$sha256, nrow(data))
  release_columns$observation_status <- rep(
    release$release_status, nrow(data)
  )
  data <- tibble::as_tibble(cbind(release_columns, data))
  provenance <- list(
    release = as.list(release),
    source_url = fetched$source_url,
    final_url = fetched$final_url,
    retrieved_at = fetched$retrieved_at,
    raw_content_sha256 = fetched$sha256,
    raw_snapshot_path = fetched$path,
    raw_bytes = fetched$raw,
    response_status = fetched$status,
    response_bytes = fetched$response_bytes,
    announced_bytes = fetched$announced_bytes,
    cache_hit = isTRUE(fetched$cache_hit),
    parser_version = release$parser_version,
    parser_revision = .rf_parser_revision,
    contract_revision = .rf_contract_revision,
    schema_version = release$schema_family,
    cache_namespace_sha256 = fetched$cache_namespace_sha256,
    terms_url = release$terms_url,
    license = release$license,
    attribution = release$attribution,
    dependency_versions = .rf_runtime_dependency_versions()
  )
  attr(data, "readfiscalau_provenance") <- provenance
  class(data) <- c("readfiscalau_data", class(data))
  data
}

.rf_read_release <- function(
    release,
    expected_type = NULL,
    cache_dir = readfiscalau_default_cache(),
    fetcher = NULL,
    raw = NULL,
    table_id = NULL,
    sheet = NULL) {
  release <- .rf_release_row(release, expected_type)
  if (release$release_type == "monthly_financial_statement" &&
      format(release$period_end, "%m") == "06") {
    .rf_abort("June must be read from FBO or CFS, never monthly statements.",
              "june_is_annual")
  }
  expected_sha256 <- release$expected_sha256
  if (is.na(expected_sha256) || !nzchar(expected_sha256)) {
    expected_sha256 <- NULL
  }
  cache_namespace <- .rf_release_cache_namespace(release)
  media <- tolower(release$media_type)
  materialize_raw <- grepl("csv", media) ||
    grepl("\\.csv([?#]|$)", release$resource_url)
  if (is.null(raw)) {
    fetched <- .rf_fetch(
      release$resource_url, cache_dir = cache_dir, fetcher = fetcher,
      expected_media_type = release$media_type,
      expected_sha256 = expected_sha256,
      cache_namespace = cache_namespace,
      materialize_raw = materialize_raw
    )
  } else {
    if (!is.raw(raw)) .rf_abort("`raw` must contain raw source bytes.",
                                "invalid_argument")
    .rf_validate_payload(raw, release$media_type)
    raw_hash <- .rf_sha256(raw)
    if (!is.null(expected_sha256) &&
        !identical(raw_hash, expected_sha256)) {
      .rf_abort(
        "Source bytes do not match the pinned release SHA-256.",
        "source_hash", expected_sha256 = expected_sha256,
        actual_sha256 = raw_hash
      )
    }
    fetched <- .rf_write_atomic(
      raw,
      cache_dir,
      .rf_extension(release$resource_url, release$media_type),
      cache_namespace = cache_namespace
    )
    fetched <- c(fetched, list(
      raw = raw, source_url = release$resource_url,
      final_url = release$resource_url,
      retrieved_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      content_type = release$media_type,
      status = NA_integer_,
      response_bytes = length(raw),
      announced_bytes = length(raw),
      cache_hit = FALSE
    ))
    readfiscalau_cache_prune(
      cache_dir, max_bytes = 500 * 1024^2, max_files = 128L,
      max_age_seconds = 90 * 24 * 60 * 60,
      preserve = fetched$path
    )
  }
  use_budget_panel <- identical(release$release_type, "budget") &&
    release$release_id %in%
      unique(.rf_budget_vintage_observations()$release_id)
  if (use_budget_panel) {
    data <- .rf_parse_budget_pdf_snapshot(release, table_id)
  } else if (grepl("csv", media) ||
             grepl("\\.csv([?#]|$)", release$resource_url)) {
    data <- .rf_parse_chart_csv(fetched$raw, release, table_id)
  } else if (grepl("zip", media) ||
             grepl("\\.zip([?#]|$)", release$resource_url)) {
    data <- .rf_parse_chart_zip(fetched$path, release, table_id)
  } else if ((grepl("spreadsheet|excel", media) ||
              grepl("\\.xlsx([?#]|$)", release$resource_url)) &&
             release$release_type == "monthly_financial_statement") {
    data <- .rf_parse_finance_xlsx(
      fetched$path, release, sheet %||% release$fiscal_year
    )
  } else if (
    identical(release$schema_family, "treasury_fbo_history_docx_v1") &&
      release$release_type == "fbo"
  ) {
    data <- .rf_parse_fbo_history_docx(fetched$path, release)
  } else if (
    identical(
      release$schema_family,
      "pbo_historical_fiscal_data_xlsx_v1"
    ) &&
      release$release_type == "historical_fiscal_data"
  ) {
    data <- .rf_parse_pbo_historical_snapshot(release, table_id)
  } else if (
    identical(
      release$schema_family,
      "treasury_myefo_pdf_snapshot_v1"
    ) &&
      release$release_type == "myefo"
  ) {
    data <- .rf_parse_myefo_pdf_snapshot(release, table_id)
  } else {
    .rf_abort(sprintf("No parser for media type `%s`.", release$media_type),
              "unsupported_media")
  }
  if (!isTRUE(materialize_raw)) {
    fetched$raw <- raw()
  }
  if (!identical(release$actual_forecast, "mixed") &&
      any(data$actual_forecast != release$actual_forecast)) {
    .rf_abort(
      "Parsed actual/forecast status conflicts with release metadata.",
      "schema"
    )
  }
  member <- attr(data, "readfiscalau_member")
  out <- .rf_attach_provenance(data, release, fetched)
  if (!is.null(member)) {
    provenance <- attr(out, "readfiscalau_provenance")
    provenance$archive_member <- member
    attr(out, "readfiscalau_provenance") <- provenance
  }
  out
}

#' Read one versioned Australian fiscal release
#'
#' `read_au_fiscal_release()` is the general entry point for one row of the
#' validated release manifest.  The type-specific aliases below select the
#' same parser while enforcing their documented release type.  A release is a
#' publication vintage, not merely a reporting year: the result retains its
#' release identity, publication status, source terms and exact source hash.
#'
#' Supported source schemas are Treasury chart CSV/ZIP resources, the curated
#' Budget and MYEFO vintage snapshots, Department of Finance monthly XLSX
#' workbooks, the admitted FBO historical DOCX, and the PBO historical XLSX.
#' Multi-table ZIP and PBO resources require `table_id`; Finance workbooks use
#' `sheet` (defaulting to the release fiscal year).  The parser fails closed on
#' unsupported media, malformed schema, duplicate natural keys, unit or
#' status drift, unsafe archive members, partial responses and pinned hash
#' mismatches.
#'
#' If `raw` is supplied it must be the exact source bytes and no network call
#' is made.  Otherwise the resource is fetched over HTTPS with bounded
#' timeout, retries and size, then atomically written to the hash-addressed
#' cache.  CSV reads retain source bytes in
#' `readfiscalau_provenance$raw_bytes`; path-based workbook, DOCX and ZIP reads
#' retain their verified `readfiscalau_provenance$raw_snapshot_path` and set
#' `readfiscalau_provenance$raw_bytes` to an empty raw vector.  The reader
#' writes only to `cache_dir` and temporary directories; it never writes to
#' the working directory or downloads credentials.  Finance June releases
#' must be read as FBO or CFS, never as a monthly statement.
#'
#' @param release A release ID present in `list_au_fiscal_releases()` or a
#'   complete one-row release manifest with all 24 manifest fields.
#' @param cache_dir Bounded hash-addressed cache directory.  The default is
#'   `readfiscalau_default_cache()`; it is created as needed by a network or
#'   raw-byte read.
#' @param fetcher Optional offline fetcher used instead of HTTP when `raw` is
#'   `NULL`.  It must return a list containing `raw`, `status`, `content_type`,
#'   `final_url` and `retrieved_at`.
#' @param raw Optional exact source bytes as a raw vector.  When supplied,
#'   transport is bypassed but media-type and expected SHA-256 validation still
#'   apply.
#' @param table_id Optional chart table identifier.  It is required for a
#'   multi-table chart ZIP or PBO workbook; it may be a canonical table name,
#'   number or ID accepted by the selected reader.
#' @param sheet Optional Finance workbook sheet name.  `NULL` uses the release
#'   fiscal year.
#' @param ... Additional arguments passed by the type-specific aliases to the
#'   common reader; use the named parameters above for stable code.
#' @returns A tibble with class `c("readfiscalau_data", "tbl_df", "tbl",
#'   "data.frame")`.  All readers prepend release/provenance columns
#'   `release_id`, `source_id`, `release_type`, `release_status`,
#'   `release_date`, `revision_semantics`, `landing_url`, `resource_url`,
#'   `schema_family`, `resource_id`, `resource_role`, `resource_updated_at`,
#'   `parser_version`, `frequency`, `media_type`, `terms_url`, `license`,
#'   `attribution`, `vintage_date`, `retrieved_at`, `raw_content_sha256` and
#'   `observation_status`.  The parser-specific columns include
#'   `table_id`, `series_id`, `series`, `fiscal_year`, `period_end`, `value`,
#'   `unit`, `unit_multiplier`, `accounting_basis`, `consolidation`,
#'   `actual_forecast` and `profile_type`; FBO rows additionally include
#'   `cumulative`, and PBO rows include table/section/source-row fields.
#'   `period_end` and `vintage_date` are `Date`; natural observations are
#'   unique on `table_id`, `series_id`, `period_end`, `actual_forecast` and
#'   `profile_type`.  The `readfiscalau_provenance` attribute is a list with
#'   release, source/final URL, UTC retrieval, raw SHA-256/snapshot path,
#'   response metadata, cache-hit flag, parser/contract revisions, schema,
#'   cache namespace, terms and dependency versions; ZIP/DOCX/PBO reads may
#'   also include `archive_member`.
#' @seealso [list_au_fiscal_releases()], [readfiscalau_default_cache()],
#'   [readfiscalau_cache_prune()]
#' @family fiscal readers
#' @examples
#' fixture_raw <- function(name) {
#'   path <- system.file("extdata", "examples", name,
#'                       package = "readfiscalau")
#'   stopifnot(nzchar(path), file.exists(path))
#'   con <- file(path, open = "rb")
#'   on.exit(close(con), add = TRUE)
#'   readBin(con, what = "raw", n = file.info(path)$size)
#' }
#' fixture_release <- function(
#'     release_type = "budget",
#'     release_id = paste0("example-", release_type),
#'     source_id = if (release_type %in% c("cfs", "monthly_financial_statement"))
#'       "department_finance" else "australian_treasury",
#'     release_date = "2026-05-12",
#'     period_end = "2027-06-30",
#'     fiscal_year = "2026-27",
#'     media_type = "text/csv",
#'     resource_url = paste0("https://example.gov.au/", release_type, ".csv"),
#'     schema_family = "treasury_chart_csv_v1",
#'     accounting_basis = "mixed",
#'     consolidation = if (source_id == "department_finance")
#'       "general_government_sector" else "general_government_sector",
#'     actual_forecast = "mixed",
#'     resource_role = paste0("example_", gsub("-", "_", release_type))) {
#'   monthly <- identical(release_type, "monthly_financial_statement")
#'   annual <- release_type %in% c("fbo", "cfs", "historical_fiscal_data")
#'   data.frame(
#'     release_id = release_id, source_id = source_id,
#'     release_type = release_type, release_status = "final",
#'     release_date = release_date, period_end = period_end,
#'     fiscal_year = fiscal_year,
#'     frequency = if (monthly) "monthly" else if (annual) "annual" else "release",
#'     accounting_basis = accounting_basis, consolidation = consolidation,
#'     actual_forecast = actual_forecast,
#'     revision_semantics = paste("Documentation fixture", release_id),
#'     landing_url = "https://example.gov.au/release",
#'     resource_id = paste0("resource-", release_id),
#'     resource_role = resource_role, resource_url = resource_url,
#'     resource_updated_at = paste0(release_date, "T00:00:00Z"),
#'     expected_sha256 = NA_character_, media_type = media_type,
#'     schema_family = schema_family, parser_version = "0.1.0",
#'     terms_url = if (source_id == "australian_treasury")
#'       "https://budget.gov.au/legal-notices.htm#h1" else
#'       "https://www.finance.gov.au/about-us/copyright-notice",
#'     license = if (source_id == "australian_treasury")
#'       "Creative Commons Attribution 4.0 International" else
#'       "Creative Commons Attribution 4.0 International",
#'     attribution = if (source_id == "australian_treasury")
#'       "\u00a9 Commonwealth of Australia" else
#'       "\u00a9 Commonwealth of Australia, Department of Finance",
#'     stringsAsFactors = FALSE
#'   )
#' }
#' # The helper supplies non-empty `terms_url`, `license` and `attribution`
#' # values because those fields are required by the release-manifest contract.
#' # They are illustrative metadata for these synthetic documentation fixtures;
#' # they do not claim that the generated fixture bytes are official publisher
#' # material or carry the publisher's terms.
#' cache <- tempfile("readfiscalau-example-cache-")
#' dir.create(cache)
#' csv <- fixture_raw("budget-current.csv")
#' docx <- fixture_raw("fbo-history.docx")
#' xlsx <- fixture_raw("finance-current.xlsx")
#' budget_release <- fixture_release()
#' budget <- read_au_fiscal_release(
#'   budget_release, raw = csv, cache_dir = cache
#' )
#' budget_alias <- read_au_budget(
#'   budget_release, raw = csv, cache_dir = cache
#' )
#' myefo_release <- fixture_release(
#'   release_type = "myefo", release_id = "example-myefo"
#' )
#' myefo <- read_au_myefo(myefo_release, raw = csv, cache_dir = cache)
#' fbo_release <- fixture_release(
#'   release_type = "fbo", release_id = "example-fbo",
#'   release_date = "2025-09-29", period_end = "2025-06-30",
#'   fiscal_year = "2024-25",
#'   media_type = paste0("application/vnd.openxmlformats-officedocument.",
#'                        "wordprocessingml.document"),
#'   resource_url = "https://example.gov.au/fbo-history.docx",
#'   schema_family = "treasury_fbo_history_docx_v1",
#'   actual_forecast = "actual", resource_role = "fbo_historical_aggregates_docx"
#' )
#' fbo <- read_au_fbo(fbo_release, raw = docx, cache_dir = cache)
#' cfs_release <- fixture_release(
#'   release_type = "cfs", release_id = "example-cfs",
#'   source_id = "department_finance", resource_role = "example_cfs_csv"
#' )
#' cfs <- read_au_cfs(cfs_release, raw = csv, cache_dir = cache)
#' monthly_release <- fixture_release(
#'   release_type = "monthly_financial_statement", release_id = "example-monthly",
#'   release_date = "2026-06-26", period_end = "2026-05-31",
#'   fiscal_year = "2025-26",
#'   media_type = paste0("application/vnd.openxmlformats-officedocument.",
#'                        "spreadsheetml.sheet"),
#'   resource_url = "https://example.gov.au/finance-current.xlsx",
#'   schema_family = "finance_mfs_post_2008_v2",
#'   actual_forecast = "actual", resource_role = "example_monthly_xlsx"
#' )
#' monthly <- read_au_monthly_financials(
#'   monthly_release, raw = xlsx, cache_dir = cache, sheet = "2025-26"
#' )
#' stopifnot(
#'   inherits(budget, "readfiscalau_data"),
#'   inherits(budget_alias, "readfiscalau_data"),
#'   inherits(myefo, "readfiscalau_data"),
#'   inherits(fbo, "readfiscalau_data"),
#'   inherits(cfs, "readfiscalau_data"),
#'   inherits(monthly, "readfiscalau_data"),
#'   all(budget$actual_forecast %in% c("actual", "forecast"))
#' )
#' @export
read_au_fiscal_release <- function(
    release,
    cache_dir = readfiscalau_default_cache(),
    fetcher = NULL,
    raw = NULL,
    table_id = NULL,
    sheet = NULL) {
  .rf_read_release(
    release, cache_dir = cache_dir, fetcher = fetcher, raw = raw,
    table_id = table_id, sheet = sheet
  )
}

#' @rdname read_au_fiscal_release
#' @export
read_au_budget <- function(release, ...) {
  .rf_read_release(release, expected_type = "budget", ...)
}

#' @rdname read_au_fiscal_release
#' @export
read_au_myefo <- function(release, ...) {
  .rf_read_release(release, expected_type = "myefo", ...)
}

#' @rdname read_au_fiscal_release
#' @export
read_au_fbo <- function(release, ...) {
  .rf_read_release(release, expected_type = "fbo", ...)
}

#' @rdname read_au_fiscal_release
#' @export
read_au_cfs <- function(release, ...) {
  .rf_read_release(release, expected_type = "cfs", ...)
}

#' @rdname read_au_fiscal_release
#' @export
read_au_monthly_financials <- function(release, ...) {
  .rf_read_release(
    release, expected_type = "monthly_financial_statement", ...
  )
}

#' Print a `readfiscalau_data` result
#'
#' The registered S3 method prints a compact release ID, vintage date and
#' prefix of the raw SHA-256, followed by the ordinary tibble print method.
#' It does not fetch, mutate or reparse the object.
#'
#' @param x A `readfiscalau_data` object returned by one of the fiscal release
#'   readers, with its `readfiscalau_provenance` attribute intact.
#' @param ... Additional arguments passed to the underlying tibble/data-frame
#'   print method.
#' @returns `x`, invisibly, after writing the human-readable summary to the
#'   console.
#' @seealso [read_au_fiscal_release()]
#' @family fiscal readers
#' @examples
#' fixture <- system.file("extdata", "examples", "budget-current.csv",
#'                        package = "readfiscalau")
#' raw <- readBin(fixture, what = "raw", n = file.info(fixture)$size)
#' release <- data.frame(
#'   release_id = "example-print", source_id = "australian_treasury",
#'   release_type = "budget", release_status = "final",
#'   release_date = "2026-05-12", period_end = "2027-06-30",
#'   fiscal_year = "2026-27", frequency = "release",
#'   accounting_basis = "mixed", consolidation = "general_government_sector",
#'   actual_forecast = "mixed", revision_semantics = "Documentation fixture",
#'   landing_url = "https://example.gov.au/release",
#'   resource_id = "example-print-resource", resource_role = "example_print_csv",
#'   resource_url = "https://example.gov.au/print.csv",
#'   resource_updated_at = "2026-05-12T00:00:00Z", expected_sha256 = NA_character_,
#'   media_type = "text/csv", schema_family = "treasury_chart_csv_v1",
#'   parser_version = "0.1.0",
#'   terms_url = "https://budget.gov.au/legal-notices.htm#h1",
#'   license = "Creative Commons Attribution 4.0 International",
#'   attribution = "\u00a9 Commonwealth of Australia", stringsAsFactors = FALSE
#' )
#' print(read_au_budget(release, raw = raw, cache_dir = tempdir()))
#' @export
print.readfiscalau_data <- function(x, ...) {
  provenance <- attr(x, "readfiscalau_provenance")
  cat(sprintf(
    "<readfiscalau_data> %s | vintage %s | sha256 %s\n",
    provenance$release$release_id,
    provenance$release$release_date,
    substr(provenance$raw_content_sha256, 1L, 12L)
  ))
  NextMethod()
}
