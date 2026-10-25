.rf_budget_history_tables <- data.frame(
  table_key = c(
    "cash_receipts", "cash_receipts_gdp",
    "accrual_revenue", "accrual_revenue_gdp"
  ),
  table_number = 1:4,
  table_title = c(
    "Australian Government (cash) receipts",
    "Major categories of (cash) receipts as a proportion of gross domestic product",
    "Australian Government (accrual) revenue",
    "Major categories of (accrual) revenue as a proportion of gross domestic product"
  ),
  resource_role = c(
    "budget_table_1_cash_receipts_csv",
    "budget_table_2_cash_receipts_gdp_csv",
    "budget_table_3_accrual_revenue_csv",
    "budget_table_4_accrual_revenue_gdp_csv"
  ),
  accounting_basis = c("cash", "cash", "accrual", "accrual"),
  unit = c("AUD million", "% of GDP", "AUD million", "% of GDP"),
  stringsAsFactors = FALSE
)

.rf_budget_history_table <- function(table_id) {
  .rf_assert_scalar_string(table_id, "table_id")
  key <- .rf_slug(table_id)
  aliases <- list(
    cash_receipts = c(
      "1", "table-1", "cash-receipts",
      "australian-government-cash-receipts"
    ),
    cash_receipts_gdp = c(
      "2", "table-2", "cash-receipts-gdp",
      "major-categories-of-cash-receipts-as-a-proportion-of-gross-domestic-product"
    ),
    accrual_revenue = c(
      "3", "table-3", "accrual-revenue",
      "australian-government-accrual-revenue"
    ),
    accrual_revenue_gdp = c(
      "4", "table-4", "accrual-revenue-gdp",
      "major-categories-of-accrual-revenue-as-a-proportion-of-gross-domestic-product"
    )
  )
  hit <- vapply(
    aliases,
    function(values) key %in% .rf_slug(values),
    logical(1)
  )
  if (sum(hit) != 1L) {
    .rf_abort(
      paste(
        "`table_id` must identify canonical Budget Table 1, 2, 3, or 4;",
        "arbitrary chart tables require an explicit audited crosswalk."
      ),
      "table_required",
      tables = .rf_budget_history_tables$table_title
    )
  }
  .rf_budget_history_tables[
    .rf_budget_history_tables$table_key == names(hit)[hit],
    ,
    drop = FALSE
  ]
}

.rf_history_release_row <- function(release, manifest) {
  if (is.character(release) && length(release) == 1L) {
    row <- manifest[manifest$release_id == release, , drop = FALSE]
  } else {
    row <- .rf_validate_manifest(release)
  }
  if (nrow(row) != 1L) {
    .rf_abort("Exactly one terminal release must be selected.",
              "invalid_argument")
  }
  if (row$source_id != "australian_treasury" ||
      row$release_type != "budget") {
    .rf_abort(
      "Chained Budget history requires an Australian Treasury Budget release.",
      "release_type"
    )
  }
  row
}

.rf_history_raw <- function(raw, release_id) {
  if (is.null(raw)) return(NULL)
  if (!is.list(raw) || is.null(names(raw)) || any(!nzchar(names(raw)))) {
    .rf_abort(
      "`raw` must be a named list keyed by release ID.",
      "invalid_argument"
    )
  }
  value <- raw[[release_id]]
  if (is.null(value)) return(NULL)
  if (!is.raw(value)) {
    .rf_abort(
      sprintf("Raw source `%s` is not a raw vector.", release_id),
      "invalid_argument"
    )
  }
  value
}

.rf_history_resource_roles <- function(table_contract) {
  roles <- c(
    "budget_chart_csv_archive",
    table_contract$resource_role
  )
  if (identical(table_contract$table_key, "cash_receipts")) {
    roles <- c(
      roles,
      "budget_release_table_1_cash_receipts_csv",
      "budget_table_1_cash_receipts_pdf_snapshot"
    )
  }
  unique(roles)
}

.rf_history_prefer_coherent_releases <- function(
    candidates,
    terminal,
    table_contract) {
  if (!identical(table_contract$table_key, "cash_receipts")) {
    return(candidates)
  }
  coherent_role <- "budget_release_table_1_cash_receipts_csv"
  legacy_role <- "budget_table_1_cash_receipts_csv"
  coherent_dates <- candidates$release_date[
    candidates$resource_role == coherent_role
  ]
  drop <- candidates$resource_role == legacy_role &
    candidates$release_date %in% coherent_dates
  if (identical(terminal$resource_role, legacy_role)) {
    terminal_date <- terminal$release_date[[1L]]
    drop <- drop |
      (candidates$resource_role == coherent_role &
         candidates$release_date == terminal_date)
    drop[candidates$release_id == terminal$release_id] <- FALSE
  }
  candidates[!drop, , drop = FALSE]
}

.rf_history_read_one <- function(
    release, table_contract, cache_dir, fetcher, raw) {
  supported_roles <- .rf_history_resource_roles(table_contract)
  generic <- release$resource_role == "budget_chart_csv_archive"
  if (!release$resource_role %in% supported_roles) return(NULL)
  table_name <- paste0("Table ", table_contract$table_number)
  bundled_table <- generic ||
    release$schema_family == "treasury_budget_pdf_snapshot_v1"
  read_au_budget(
    release,
    cache_dir = cache_dir,
    fetcher = fetcher,
    raw = .rf_history_raw(raw, release$release_id),
    table_id = if (bundled_table) table_name else table_contract$table_title
  )
}

.rf_history_validate_observations <- function(data, table_contract) {
  if (any(data$accounting_basis != table_contract$accounting_basis) ||
      any(data$consolidation != "general_government_sector") ||
      any(data$unit != table_contract$unit)) {
    .rf_abort(
      "A release conflicts with the canonical table basis, scope, or unit.",
      "history_contract",
      release_ids = unique(data$release_id)
    )
  }
  data
}

.rf_history_resolve <- function(data) {
  keys <- paste(
    data$period_end, data$actual_forecast, data$accounting_basis,
    data$consolidation, data$unit, sep = "\r"
  )
  groups <- split(seq_len(nrow(data)), keys)
  selected <- integer(length(groups))
  revisions <- integer(length(groups))
  index <- 0L
  for (rows in groups) {
    index <- index + 1L
    latest <- max(data$vintage_date[rows])
    candidates <- rows[data$vintage_date[rows] == latest]
    values <- unique(data$value[candidates])
    values <- values[!is.na(values)]
    if (length(values) > 1L) {
      .rf_abort(
        "Same-vintage releases disagree for one canonical observation.",
        "vintage_collision",
        release_ids = data$release_id[candidates]
      )
    }
    candidates <- candidates[order(data$release_id[candidates])]
    selected[[index]] <- candidates[[length(candidates)]]
    revisions[[index]] <- length(unique(data$value[rows]))
  }
  out <- data[selected, , drop = FALSE]
  out$revision_count <- revisions
  out$selected_vintage <- TRUE
  out[order(out$period_end), , drop = FALSE]
}

#' Read a provenance-preserving chain of Budget history
#'
#' Reads one canonical Budget receipts or revenue series from the selected
#' terminal Budget vintage and every earlier compatible pinned vintage. The
#' function is deliberately narrower than [read_au_budget()]: it accepts only
#' the four audited Treasury table families (`"cash_receipts"`,
#' `"cash_receipts_gdp"`, `"accrual_revenue"`, and `"accrual_revenue_gdp"`)
#' and rejects arbitrary chart tables. That restriction prevents a series from
#' being spliced across accounting bases, units, consolidations, or scopes.
#'
#' `observation` is matched before any parsing, so actuals and forecasts are
#' never combined. With the default `vintages = "resolved"`, overlapping
#' observations are resolved within one status, accounting basis,
#' consolidation, and unit by selecting the latest official publication
#' vintage. `vintages = "all"` retains every admitted vintage and marks the
#' latest row for each period with `selected_vintage`; `revision_count` counts
#' the distinct values retained for that period/status contract.
#'
#' The result is ordered by ascending `period_end` for `"resolved"`, and by
#' `period_end`, `vintage_date`, then `release_id` for `"all"`. Dates are
#' fiscal-year ends (30 June), and `value` is accompanied by its published
#' `unit`, `accounting_basis`, `consolidation`, `actual_forecast`, release
#' identity, source URL, and raw-source SHA-256. The
#' `readfiscalau_history_provenance` attribute records the terminal release,
#' selected contract, coverage limits, overlap rule, and per-release coverage;
#' it does not claim that the admitted vintages are a complete history of every
#' publication ever issued.
#'
#' If `raw` is supplied, it must be a named list of exact source-byte vectors
#' keyed by release ID and transport is bypassed for those releases. Otherwise
#' the reader may fetch missing releases over HTTPS. Parsed bytes are written
#' atomically to the bounded, hash-addressed `cache_dir` and normal cache
#' pruning applies. The function writes only to that cache and temporary
#' directories; it does not write to the working directory or global state.
#' An injected `fetcher` is the offline alternative when a raw vector is not
#' convenient. No warnings are emitted for ordinary operation; malformed
#' manifests, unsupported tables, absent series/statuses, source-contract
#' conflicts, same-vintage disagreements, and transport/hash failures are
#' reported as structured `readfiscalau_*` errors.
#'
#' @param release Terminal Budget release ID or a complete one-row release
#'   manifest. The selected row must be an Australian Treasury `"budget"`
#'   release and must contain all 24 manifest fields.
#' @param table_id Canonical Budget table number (`1`, `2`, `3`, or `4`), key,
#'   or published title. Arbitrary chart-table names are rejected.
#' @param series Exact published series label or its normalised `series_id`.
#' @param observation Character scalar selecting `"actual"` (the default) or
#'   `"forecast"`; values from the other status are not returned.
#' @param vintages Character scalar selecting `"resolved"` (the default) or
#'   `"all"`. `"resolved"` keeps the latest compatible vintage per period;
#'   `"all"` keeps every compatible vintage.
#' @param manifest Optional complete replacement release manifest. `NULL` (the
#'   default) reads the installed manifest.
#' @param cache_dir Bounded hash-addressed cache directory. The default is
#'   [readfiscalau_default_cache()]. It is created as needed when a raw source
#'   is read or fetched.
#' @param fetcher Optional offline fetcher used when a release has no supplied
#'   `raw` bytes. It must return the transport metadata required by the common
#'   reader (`raw`, `status`, `content_type`, `final_url`, and
#'   `retrieved_at`).
#' @param raw Optional named list of exact raw source bytes keyed by release ID.
#'   Entries may be omitted when a verified cache hit or `fetcher` is intended;
#'   omitted entries can otherwise trigger network transport.
#' @returns A tibble with class
#'   `c("readfiscalau_history", "tbl_df", "tbl", "data.frame")`, ordered as
#'   described above. It has these columns, in order: `release_id`, `source_id`,
#'   `release_type`, `release_status`, `release_date`, `revision_semantics`,
#'   `landing_url`, `resource_url`, `schema_family`, `resource_id`,
#'   `resource_role`, `resource_updated_at`, `parser_version`, `frequency`,
#'   `media_type`, `terms_url`, `license`, `attribution`, `vintage_date`,
#'   `retrieved_at`, `raw_content_sha256`, `observation_status`, `table_id`,
#'   `series_id`, `series`, `fiscal_year`, `period_end`, `value`, `unit`,
#'   `unit_multiplier`, `accounting_basis`, `consolidation`,
#'   `actual_forecast`, `profile_type`, `revision_count`, and
#'   `selected_vintage`. Character metadata and labels are character vectors;
#'   `release_date`, `vintage_date`, and `period_end` are `Date`; `value` and
#'   `unit_multiplier` are numeric; and `selected_vintage` is logical. Each
#'   natural observation is unique within the returned vintage/status contract.
#'   The `readfiscalau_history_provenance` attribute is a list containing
#'   `terminal_release_id`, `terminal_release_date`, `table_key`, `table_title`,
#'   `series_id`, `observation`, `vintages`, `overlap_rule`,
#'   `observation_period_coverage`, `release_vintage_coverage`,
#'   `release_vintage_coverage_complete`, `earliest_period`, `latest_period`,
#'   and a `release_coverage` data frame. For `"resolved"`, every returned row
#'   has `selected_vintage = TRUE`; for `"all"`, at most the latest admitted
#'   row per period is selected.
#' @seealso [read_au_budget()], [list_au_budget_series()],
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
#' template <- list_au_fiscal_releases(
#'   source = "treasury", release_type = "budget"
#' )
#' fixture <- template[
#'   template$release_id == "treasury-budget-2026-27-final", , drop = FALSE
#' ]
#' fixture$release_id <- "example-budget-history"
#' fixture$release_date <- as.Date("2027-07-01")
#' fixture$period_end <- as.Date("2027-06-30")
#' fixture$fiscal_year <- "2026-27"
#' fixture$resource_id <- "example-budget-history-csv"
#' fixture$resource_role <- "budget_chart_csv_archive"
#' fixture$resource_updated_at <- "2027-07-01T00:00:00Z"
#' fixture$resource_url <- "https://budget.gov.au/content/download/example.csv"
#' fixture$media_type <- "text/csv"
#' fixture$schema_family <- "treasury_chart_csv_v2"
#' fixture$expected_sha256 <- NA_character_
#' fixture$accounting_basis <- "cash"
#' fixture$revision_semantics <- "Documentation replay fixture, not official data."
#' history <- read_au_budget_history(
#'   release = fixture$release_id,
#'   table_id = "Table 1",
#'   series = "Total receipts",
#'   manifest = fixture,
#'   raw = setNames(list(example_raw("budget-current.csv")), fixture$release_id),
#'   cache_dir = tempfile("readfiscalau-history-")
#' )
#' print(history[c(
#'   "period_end", "value", "unit", "actual_forecast",
#'   "release_id", "revision_count", "selected_vintage"
#' )])
#' attr(history, "readfiscalau_history_provenance")$release_coverage
#' @export
read_au_budget_history <- function(
    release,
    table_id,
    series,
    observation = c("actual", "forecast"),
    vintages = c("resolved", "all"),
    manifest = NULL,
    cache_dir = readfiscalau_default_cache(),
    fetcher = NULL,
    raw = NULL) {
  observation <- match.arg(observation)
  vintages <- match.arg(vintages)
  .rf_assert_scalar_string(series, "series")
  releases <- .rf_resolve_manifest(manifest)
  terminal <- .rf_history_release_row(release, releases)
  table_contract <- .rf_budget_history_table(table_id)
  resource_roles <- .rf_history_resource_roles(table_contract)

  candidates <- releases[
    releases$source_id == "australian_treasury" &
      releases$release_type == "budget" &
      releases$release_date <= terminal$release_date &
      releases$resource_role %in% resource_roles,
    ,
    drop = FALSE
  ]
  candidates <- .rf_history_prefer_coherent_releases(
    candidates,
    terminal,
    table_contract
  )
  candidates <- candidates[order(
    candidates$release_date, candidates$release_id
  ), , drop = FALSE]
  if (!nrow(candidates) ||
      !terminal$release_id %in% candidates$release_id) {
    .rf_abort(
      "The terminal release does not contain the selected canonical table.",
      "table_required"
    )
  }

  observations <- list()
  coverage <- vector("list", nrow(candidates))
  wanted <- .rf_slug(series)
  for (index in seq_len(nrow(candidates))) {
    parsed <- .rf_history_read_one(
      candidates[index, , drop = FALSE], table_contract,
      cache_dir, fetcher, raw
    )
    parsed <- .rf_history_validate_observations(parsed, table_contract)
    rows <- parsed$series_id == wanted | parsed$series == series
    selected <- parsed[rows & parsed$actual_forecast == observation, ,
                       drop = FALSE]
    coverage[[index]] <- data.frame(
      release_id = candidates$release_id[[index]],
      release_date = candidates$release_date[[index]],
      series_present = any(rows),
      observation_present = nrow(selected) > 0L,
      earliest_period = if (nrow(selected)) min(selected$period_end) else
        as.Date(NA),
      latest_period = if (nrow(selected)) max(selected$period_end) else
        as.Date(NA),
      stringsAsFactors = FALSE
    )
    if (nrow(selected)) observations[[length(observations) + 1L]] <- selected
  }
  coverage <- .rf_bind_rows(coverage)
  terminal_coverage <- coverage$release_id == terminal$release_id
  if (!any(terminal_coverage) ||
      !coverage$observation_present[terminal_coverage]) {
    .rf_abort(
      "The requested series/status is absent from the terminal release.",
      "history_series",
      release_id = terminal$release_id,
      series = series,
      observation = observation
    )
  }
  panel <- .rf_bind_rows(observations)
  panel <- panel[order(panel$period_end, panel$vintage_date,
                       panel$release_id), , drop = FALSE]
  if (vintages == "resolved") {
    out <- .rf_history_resolve(panel)
  } else {
    out <- panel
    out$revision_count <- stats::ave(
      out$value,
      paste(out$period_end, out$actual_forecast,
            out$accounting_basis, out$consolidation, out$unit),
      FUN = function(value) length(unique(value))
    )
    latest <- stats::ave(
      as.numeric(out$vintage_date), out$period_end, FUN = max
    )
    out$selected_vintage <- as.numeric(out$vintage_date) == latest
  }
  attr(out, "readfiscalau_history_provenance") <- list(
    terminal_release_id = terminal$release_id,
    terminal_release_date = terminal$release_date,
    table_key = table_contract$table_key,
    table_title = table_contract$table_title,
    series_id = wanted,
    observation = observation,
    vintages = vintages,
    overlap_rule = paste(
      "Within one accounting basis, consolidation, unit, and status,",
      "select the latest official publication vintage for each period."
    ),
    observation_period_coverage = "complete_as_published_by_terminal_release",
    release_vintage_coverage = "admitted_pinned_vintages_only",
    release_vintage_coverage_complete = FALSE,
    earliest_period = min(out$period_end),
    latest_period = max(out$period_end),
    release_coverage = coverage
  )
  class(out) <- c("readfiscalau_history", class(out))
  out
}

#' Print a Budget-history result
#'
#' Prints a compact header containing the selected series, period span, and
#' terminal release before delegating to the standard data-frame printer.
#' Printing does not fetch data, alter the cache, or change the object.
#'
#' @param x A `readfiscalau_history` object returned by
#'   [read_au_budget_history()].
#' @param ... Additional arguments passed to the next print method.
#' @returns The input `x`, invisibly.
#' @seealso [read_au_budget_history()]
#' @family historical-readers
#' @rdname read_au_budget_history
#' @method print readfiscalau_history
#' @export
print.readfiscalau_history <- function(x, ...) {
  provenance <- attr(x, "readfiscalau_history_provenance")
  cat(sprintf(
    "<readfiscalau_history> %s | %s to %s | terminal %s\n",
    provenance$series_id,
    provenance$earliest_period,
    provenance$latest_period,
    provenance$terminal_release_id
  ))
  NextMethod()
}
