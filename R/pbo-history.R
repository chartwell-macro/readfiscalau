.rf_pbo_historical_release_id <-
  "pbo-historical-fiscal-data-2026-27-budget"

.rf_pbo_historical_observations <- function() {
  path <- system.file(
    "extdata",
    "pbo-historical-observations.csv.gz",
    package = "readfiscalau"
  )
  if (!nzchar(path)) {
    path <- file.path(
      "inst",
      "extdata",
      "pbo-historical-observations.csv.gz"
    )
  }
  cached <- .rf_internal_file_cache_get("pbo_historical_panel", path)
  if (!is.null(cached)) {
    return(cached)
  }
  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  panel <- utils::read.csv(
    connection,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  close(connection)
  on.exit(NULL, add = FALSE)
  required <- c(
    "release_id", "table_id", "table_number", "table_label",
    "source_table", "source_row", "section", "series_id", "series",
    "fiscal_year", "period_end", "value", "unit", "unit_multiplier",
    "accounting_basis", "consolidation", "actual_forecast",
    "profile_type", "frequency", "source_url", "source_sha256"
  )
  missing <- setdiff(required, names(panel))
  if (length(missing)) {
    .rf_abort(
      sprintf(
        "The bundled PBO historical panel is missing: %s.",
        paste(missing, collapse = ", ")
      ),
      "schema"
    )
  }
  panel <- panel[required]
  panel$section[is.na(panel$section)] <- ""
  panel$period_end <- as.Date(panel$period_end)
  panel$table_number <- suppressWarnings(as.integer(panel$table_number))
  panel$source_row <- suppressWarnings(as.integer(panel$source_row))
  panel$value <- suppressWarnings(as.numeric(panel$value))
  panel$unit_multiplier <- suppressWarnings(
    as.numeric(panel$unit_multiplier)
  )
  key <- paste(panel$series_id, panel$fiscal_year, sep = "\r")
  if (!nrow(panel) ||
      anyDuplicated(key) ||
      anyNA(panel$period_end) ||
      anyNA(panel$value) ||
      anyNA(panel$unit_multiplier) ||
      any(panel$release_id != .rf_pbo_historical_release_id) ||
      !setequal(unique(panel$table_number), seq_len(11L)) ||
      any(!nzchar(panel$table_id)) ||
      any(!nzchar(panel$table_label)) ||
      any(!nzchar(panel$series_id)) ||
      any(!nzchar(panel$series)) ||
      any(!nzchar(panel$unit)) ||
      any(panel$actual_forecast != "actual") ||
      any(panel$profile_type != "actual") ||
      any(panel$frequency != "annual") ||
      any(as.integer(substr(panel$fiscal_year, 1L, 4L)) > 2024L) ||
      any(!grepl("^[a-f0-9]{64}$", panel$source_sha256))) {
    .rf_abort(
      "The bundled PBO historical panel failed its contract.",
      "schema"
    )
  }
  attr(panel, "readfiscalau_panel_path") <- path
  .rf_internal_file_cache_set("pbo_historical_panel", path, panel)
  panel
}

#' List PBO historical fiscal-data tables
#'
#' Returns the 11 table families in the package's pinned Parliamentary Budget
#' Office (PBO) Historical Fiscal Data release. The bundled panel contains
#' completed actual observations through 2024-25; estimates and projections
#' remain available through the Budget and MYEFO vintage APIs. This function is
#' a local catalogue operation and is useful for discovering the `table_id`
#' values accepted by [read_au_pbo_historical()].
#'
#' The table catalogue is derived from the package's selected, normalized
#' panel, while the release URL and SHA-256 identify the authoritative PBO
#' workbook. The panel is not a replacement for the publisher's workbook.
#' Consult `inst/TERMS.md` and the PBO copyright notice before reusing or
#' redistributing source-derived observations: the recorded PBO terms are
#' Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International
#' unless the publisher states otherwise, and the package does not claim wider
#' rights for its normalized selection.
#'
#' @returns A tibble with exactly 11 rows and these columns, in this order:
#'   `release_id`, `table_id`, `table_number`, `table_label`, `source_table`,
#'   `start_date`, `end_date`, `series_count`, `observation_count`,
#'   `source_url`, and `source_sha256`. `release_id`, `table_id`,
#'   `table_label`, `source_table`, `source_url`, and `source_sha256` are
#'   character vectors (the first five are non-empty, and `source_sha256` is a
#'   lowercase 64-character SHA-256 string); `table_number`, `series_count`,
#'   and `observation_count` are positive integer-valued vectors; and
#'   `start_date` and `end_date` are `Date` vectors with `start_date <=
#'   end_date`. Rows are ordered by ascending `table_number`.
#' @details The panel is read from the installed
#'   `pbo-historical-observations.csv.gz` resource and validated before the
#'   summary is returned. The function never downloads the workbook, writes a
#'   source-cache file, or modifies the working directory; it may reuse an
#'   in-memory panel/summary cache for the current R process. A `schema` error
#'   indicates a missing, malformed, duplicated, or semantically inconsistent
#'   bundled panel.
#' @seealso [list_au_pbo_historical_series()],
#'   [read_au_pbo_historical()], [list_au_fiscal_releases()]
#' @family pbo-data
#' @examples
#' tables <- list_au_pbo_historical_tables()
#' print(tables[, c("table_id", "table_label", "start_date", "end_date")][1:3, ],
#'       row.names = FALSE)
#' stopifnot(
#'   nrow(tables) == 11L,
#'   identical(tables$table_number, sort(tables$table_number)),
#'   all(tables$start_date <= tables$end_date)
#' )
#' @export
list_au_pbo_historical_tables <- function() {
  panel <- .rf_pbo_historical_observations()
  path <- attr(panel, "readfiscalau_panel_path")
  cached <- .rf_internal_file_cache_get(
    "pbo_historical_table_coverage",
    path
  )
  if (!is.null(cached)) {
    return(cached)
  }
  groups <- split(seq_len(nrow(panel)), panel$table_id)
  rows <- lapply(groups, function(index) {
    selected <- panel[index, , drop = FALSE]
    data.frame(
      release_id = selected$release_id[[1L]],
      table_id = selected$table_id[[1L]],
      table_number = selected$table_number[[1L]],
      table_label = selected$table_label[[1L]],
      source_table = selected$source_table[[1L]],
      start_date = min(selected$period_end),
      end_date = max(selected$period_end),
      series_count = length(unique(selected$series_id)),
      observation_count = nrow(selected),
      source_url = selected$source_url[[1L]],
      source_sha256 = selected$source_sha256[[1L]],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$table_number), , drop = FALSE]
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)
  .rf_internal_file_cache_set(
    "pbo_historical_table_coverage",
    path,
    out
  )
  out
}

#' List PBO historical fiscal series and local coverage
#'
#' Returns one row per unique series in the pinned PBO historical observation
#' panel. Use `table_id` to restrict the catalogue to one canonical table. A
#' table may be named by its canonical ID (for example,
#' `"pbo_historical_table_01"`), by the published form `"Table 1"`, or by an
#' integer-like table number. The result describes the normalized local panel;
#' it does not fetch or parse the live PBO workbook.
#'
#' @param table_id Optional scalar table selector. `NULL` (the default) lists
#'   all tables; otherwise supply a canonical table ID, `"Table N"`, or a
#'   numeric/integer-like table number from
#'   [list_au_pbo_historical_tables()]. Values are matched case-insensitively
#'   after trimming whitespace.
#' @returns A tibble with one row per unique `series_id` and exactly these 19
#'   columns, in this order: `release_id`, `table_id`, `table_number`,
#'   `table_label`, `source_table`, `source_row`, `section`, `series_id`,
#'   `series`, `actual_forecast`, `start_date`, `end_date`,
#'   `observation_count`, `unit`, `accounting_basis`, `consolidation`,
#'   `frequency`, `source_url`, and `source_sha256`. `release_id`, `table_id`,
#'   `table_label`, `source_table`, `series_id`, `series`, `unit`,
#'   `accounting_basis`, `consolidation`, `frequency`, `source_url`, and
#'   `source_sha256` are non-empty character vectors; `section` is character
#'   and may be `""`; `actual_forecast` is character and is always
#'   `"actual"`; `table_number`, `source_row`, and `observation_count` are
#'   positive integer-valued vectors; and `start_date` and `end_date` are
#'   `Date` vectors with `start_date <= end_date`. `frequency` is always
#'   `"annual"`, and `source_sha256` is a lowercase 64-character SHA-256
#'   string. Rows are ordered by `table_number`, `source_row`, `unit`, and
#'   `series_id` and are filtered after that stable ordering when `table_id` is
#'   supplied.
#' @details The catalogue is derived from the bundled
#'   `pbo-historical-observations.csv.gz` panel and may reuse an in-memory
#'   cache. It makes no network request, writes no source-cache file, and has
#'   no persistent side effects. An invalid or ambiguous `table_id` raises a
#'   `table_required` error and includes the available canonical table IDs;
#'   malformed bundled data raises a `schema` error. Source URLs and hashes
#'   identify the authoritative workbook, but the normalized selection remains
#'   subject to the PBO terms documented in `inst/TERMS.md` and
#'   `inst/COPYRIGHTS`.
#' @seealso [list_au_pbo_historical_tables()],
#'   [read_au_pbo_historical()], [list_au_fiscal_releases()]
#' @family pbo-data
#' @examples
#' table_id <- list_au_pbo_historical_tables()$table_id[[1L]]
#' series <- list_au_pbo_historical_series(table_id)
#' print(utils::head(series[, c(
#'   "table_id", "series_id", "series", "start_date", "end_date", "unit"
#' )], 4L), row.names = FALSE)
#' stopifnot(
#'   nrow(series) > 0L,
#'   identical(unique(series$table_id), table_id),
#'   all(series$actual_forecast == "actual")
#' )
#' @export
list_au_pbo_historical_series <- function(table_id = NULL) {
  panel <- .rf_pbo_historical_observations()
  path <- attr(panel, "readfiscalau_panel_path")
  if (!is.null(table_id)) {
    table_id <- .rf_pbo_historical_table_id(table_id, panel)
  }
  out <- .rf_internal_file_cache_get(
    "pbo_historical_series_coverage",
    path
  )
  if (is.null(out)) {
    groups <- split(seq_len(nrow(panel)), panel$series_id)
    rows <- lapply(groups, function(index) {
      selected <- panel[index, , drop = FALSE]
      data.frame(
        release_id = selected$release_id[[1L]],
        table_id = selected$table_id[[1L]],
        table_number = selected$table_number[[1L]],
        table_label = selected$table_label[[1L]],
        source_table = selected$source_table[[1L]],
        source_row = selected$source_row[[1L]],
        section = selected$section[[1L]],
        series_id = selected$series_id[[1L]],
        series = selected$series[[1L]],
        actual_forecast = "actual",
        start_date = min(selected$period_end),
        end_date = max(selected$period_end),
        observation_count = nrow(selected),
        unit = selected$unit[[1L]],
        accounting_basis = selected$accounting_basis[[1L]],
        consolidation = selected$consolidation[[1L]],
        frequency = selected$frequency[[1L]],
        source_url = selected$source_url[[1L]],
        source_sha256 = selected$source_sha256[[1L]],
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    out <- out[order(
      out$table_number,
      out$source_row,
      out$unit,
      out$series_id
    ), , drop = FALSE]
    row.names(out) <- NULL
    out <- tibble::as_tibble(out)
    .rf_internal_file_cache_set(
      "pbo_historical_series_coverage",
      path,
      out
    )
  }
  if (!is.null(table_id)) {
    out <- out[out$table_id == table_id, , drop = FALSE]
  }
  out
}

.rf_pbo_historical_table_id <- function(table_id, panel = NULL) {
  if (is.numeric(table_id) && length(table_id) == 1L &&
      !is.na(table_id)) {
    table_id <- as.character(as.integer(table_id))
  }
  .rf_assert_scalar_string(table_id, "table_id")
  panel <- panel %||% .rf_pbo_historical_observations()
  value <- tolower(trimws(table_id))
  if (grepl("^table [0-9]+$", value)) {
    value <- sub("^table ", "", value)
  }
  if (grepl("^[0-9]+$", value)) {
    value <- paste0(
      "pbo_historical_table_",
      sprintf("%02d", as.integer(value))
    )
  }
  tables <- unique(panel$table_id)
  matched <- tables[tolower(tables) == value]
  if (length(matched) != 1L) {
    .rf_abort(
      sprintf(
        "PBO historical observations do not include table `%s`.",
        table_id
      ),
      "table_required",
      tables = tables
    )
  }
  matched[[1L]]
}

.rf_parse_pbo_historical_snapshot <- function(release, table_id = NULL) {
  panel <- .rf_pbo_historical_observations()
  if (is.null(table_id)) {
    .rf_abort(
      "The PBO historical workbook has multiple tables; select `table_id`.",
      "table_required",
      tables = unique(panel$table_id)
    )
  }
  table_id <- .rf_pbo_historical_table_id(table_id, panel)
  selected <- panel[
    panel$release_id == release$release_id &
      panel$table_id == table_id,
    ,
    drop = FALSE
  ]
  if (!nrow(selected) ||
      any(selected$source_url != release$resource_url) ||
      any(selected$source_sha256 != release$expected_sha256)) {
    .rf_abort(
      "Bundled PBO historical observations conflict with the pinned source.",
      "source_hash",
      release_id = release$release_id
    )
  }
  data <- tibble::as_tibble(selected[c(
    "table_id", "table_number", "table_label", "source_table",
    "source_row", "section", "series_id", "series", "fiscal_year",
    "period_end", "value", "unit", "unit_multiplier",
    "accounting_basis", "consolidation", "actual_forecast",
    "profile_type"
  )])
  data <- .rf_assert_unique_observations(data)
  path <- attr(panel, "readfiscalau_panel_path")
  attr(data, "readfiscalau_member") <- list(
    name = basename(path),
    sha256 = digest::digest(
      file = path,
      algo = "sha256",
      serialize = FALSE
    ),
    bytes = as.numeric(file.info(path)$size),
    derivation = paste(
      "Maintainer-normalised historical observations from every published",
      "series in the exact PBO Historical Fiscal Data workbook; unfinished",
      "2025-26 and later periods are excluded so forecast vintages remain",
      "governed by the Budget and MYEFO release pipeline."
    )
  )
  data
}

#' Read one PBO historical fiscal-data table
#'
#' Reads one selected table from the pinned PBO Historical Fiscal Data release
#' and returns normalized annual actual observations with release provenance.
#' `table_id` is required because the source workbook contains multiple
#' tables. The default examples use the bundled metadata only; a live read is
#' deliberately opt-in so documentation builds, tests, and offline users do
#' not depend on PBO availability.
#'
#' When a live read is requested, the general release reader resolves the
#' release manifest row, fetches the HTTPS workbook if it is not already in the
#' bounded hash-addressed cache, verifies its media type and pinned SHA-256,
#' and attaches source terms and retrieval metadata. Supplying `raw` through
#' `...` bypasses transport but still verifies the exact bytes. The normalized
#' observations are derived from the bundled panel and retain the authoritative
#' source URL/hash; they do not grant reuse rights beyond the PBO terms in
#' `inst/TERMS.md` and `inst/COPYRIGHTS`.
#'
#' @param table_id Required scalar table selector: a canonical table ID,
#'   `"Table N"`, or an integer-like table number accepted by
#'   [list_au_pbo_historical_tables()].
#' @param release Character scalar release ID, or a complete one-row release
#'   manifest accepted by [read_au_fiscal_release()]. The default is the
#'   pinned `"pbo-historical-fiscal-data-2026-27-budget"` release.
#' @param ... Additional arguments passed to [read_au_fiscal_release()], such
#'   as `cache_dir`, an offline `fetcher`, or exact `raw` source bytes. These
#'   arguments do not change the selected table contract.
#' @returns A tibble with class
#'   `c("readfiscalau_data", "tbl_df", "tbl", "data.frame")`. The 22
#'   release/provenance columns are prepended in this order:
#'   `release_id`, `source_id`, `release_type`, `release_status`,
#'   `release_date`, `revision_semantics`, `landing_url`, `resource_url`,
#'   `schema_family`, `resource_id`, `resource_role`, `resource_updated_at`,
#'   `parser_version`, `frequency`, `media_type`, `terms_url`, `license`,
#'   `attribution`, `vintage_date`, `retrieved_at`, `raw_content_sha256`, and
#'   `observation_status`. All provenance fields are character except
#'   `release_date` and `vintage_date`, which are `Date`; source, hash, and
#'   terms fields preserve the manifest's non-empty strings and exact hashes.
#'   The table-specific columns follow in this order: `table_id`,
#'   `table_number`, `table_label`, `source_table`, `source_row`, `section`,
#'   `series_id`, `series`, `fiscal_year`, `period_end`, `value`, `unit`,
#'   `unit_multiplier`, `accounting_basis`, `consolidation`,
#'   `actual_forecast`, and `profile_type`. The ID, label, section, series,
#'   fiscal-year, unit, accounting, consolidation, status, and profile fields
#'   are character; `table_number` and `source_row` are integer-valued;
#'   `period_end` is `Date`; and `value` and `unit_multiplier` are numeric.
#'   `actual_forecast` and `profile_type` are `"actual"`; `frequency` is
#'   `"annual"`; and each natural observation is unique on `table_id`,
#'   `series_id`, `period_end`, `actual_forecast`, and `profile_type`. The
#'   `readfiscalau_provenance` attribute is a list with the
#'   release, source/final URL, UTC retrieval time, raw SHA-256/snapshot path,
#'   response and cache metadata, parser/contract revisions, schema, terms,
#'   licence, attribution, dependency versions, and an `archive_member`
#'   derivation record.
#' @details `table_required` is raised when the table selector is missing or
#'   invalid; `source_hash`, `schema`, `unsupported_media`, and transport/cache
#'   errors fail closed when the supplied or fetched resource does not satisfy
#'   the pinned contract. The function writes only to the selected
#'   `cache_dir` and temporary directories during a live/raw read. It never
#'   writes to the working directory or performs a default network request in
#'   the examples. PBO historical data is source-derived material; consult the
#'   publisher's current copyright notice before commercial reuse or
#'   redistribution.
#' @seealso [list_au_pbo_historical_tables()],
#'   [list_au_pbo_historical_series()], [read_au_fiscal_release()],
#'   [readfiscalau_default_cache()]
#' @family pbo-data
#' @examples
#' tables <- list_au_pbo_historical_tables()
#' table_id <- tables$table_id[[1L]]
#' series <- list_au_pbo_historical_series(table_id)
#' print(utils::head(series[, c(
#'   "series", "start_date", "end_date", "unit"
#' )], 4L), row.names = FALSE)
#' stopifnot(nrow(series) > 0L, identical(unique(series$table_id), table_id))
#' if (interactive() && identical(Sys.getenv("READFISCALAU_RUN_LIVE"), "true")) {
#'   x <- read_au_pbo_historical(table_id)
#'   print(utils::head(x[, c("series", "fiscal_year", "value", "unit")], 4L),
#'         row.names = FALSE)
#' }
#' @export
read_au_pbo_historical <- function(
    table_id,
    release = .rf_pbo_historical_release_id,
    ...) {
  read_au_fiscal_release(
    release = release,
    table_id = table_id,
    ...
  )
}
