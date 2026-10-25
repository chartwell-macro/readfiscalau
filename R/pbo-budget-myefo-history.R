.rf_pbo_budget_myefo_store <- function() {
  path <- system.file(
    "extdata", "pbo-budget-myefo-history.rds", package = "readfiscalau"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "pbo-budget-myefo-history.rds")
  }
  if (!file.exists(path)) {
    .rf_abort(
      "The bundled PBO Budget/MYEFO forecast snapshot is missing.",
      "schema"
    )
  }
  cached <- .rf_internal_file_cache_get("pbo_budget_myefo_history", path)
  if (!is.null(cached)) return(cached)

  store <- readRDS(path)
  required <- c(
    "schema_version", "built_from", "source", "profiles", "observations",
    "validation"
  )
  profile_columns <- c(
    "profile_id", "snapshot_schema", "snapshot_id", "release_id",
    "publication", "budget_vintage", "update_label", "release_date",
    "series_id", "series", "series_labels", "table_ids", "table_labels",
    "source_tables", "profile_type", "actual_forecast", "unit",
    "unit_multiplier", "accounting_basis", "consolidation", "first_period",
    "last_period", "observation_count", "source_url", "source_sha256"
  )
  observation_columns <- c(
    "snapshot_id", "release_id", "publication", "budget_vintage",
    "update_label", "release_date", "table_id", "table_label",
    "source_table", "source_row", "note", "series_id", "series",
    "aggregate", "fiscal_year", "period_end", "value", "unit",
    "unit_multiplier", "accounting_basis", "consolidation",
    "actual_forecast", "profile_type", "source_url", "source_sha256",
    "is_preferred"
  )
  if (!is.list(store) || !identical(
    store$schema_version,
    "pbo_budget_myefo_history_v1"
  ) || !all(required %in% names(store)) ||
      !identical(names(store$profiles), profile_columns) ||
      !identical(names(store$observations), observation_columns) ||
      nrow(store$profiles) != 1212L ||
      nrow(store$observations) != 6396L ||
      sum(store$profiles$observation_count) != 6057L ||
      length(unique(store$profiles$release_id)) != 53L ||
      anyDuplicated(store$profiles$profile_id) ||
      any(!store$profiles$publication %in% c(
        "budget", "myefo", "pefo", "other"
      )) ||
      any(store$profiles$actual_forecast != "forecast") ||
      any(!grepl(
        "^pbo-budget-myefo-profile-v1-[0-9a-f]{64}$",
        store$profiles$profile_id
      )) ||
      any(!grepl("^[0-9a-f]{64}$", store$profiles$source_sha256)) ||
      any(store$profiles$source_sha256 != store$source$source_sha256) ||
      any(store$observations$source_sha256 != store$source$source_sha256) ||
      any(store$profiles$first_period > store$profiles$last_period) ||
      any(store$profiles$observation_count < 1L) ||
      anyNA(store$observations$period_end) ||
      anyNA(store$observations$value)) {
    .rf_abort(
      "The bundled PBO Budget/MYEFO forecast snapshot failed its contract.",
      "schema"
    )
  }
  attr(store, "readfiscalau_store_path") <- path
  .rf_internal_file_cache_set("pbo_budget_myefo_history", path, store)
  store
}

.rf_pbo_budget_myefo_filter <- function(value, name, choices) {
  if (is.null(value)) return(NULL)
  .rf_assert_scalar_string(value, name)
  if (!value %in% choices) {
    .rf_abort(
      sprintf("Unknown PBO forecast %s `%s`.", name, value),
      "invalid_argument",
      argument = name,
      choices = sort(unique(choices))
    )
  }
  value
}

#' List PBO historical Budget, MYEFO and other forecast vintages
#'
#' Lists chart-ready forecast paths parsed from the Parliamentary Budget
#' Office's latest Historical Australian Government Budget Forecasts and
#' Measures workbook. This is a separate PBO source contract; it is not mixed
#' with the Treasury Budget/MYEFO table snapshot.
#'
#' @param publication Optional exact update class: `"budget"`, `"myefo"`,
#'   `"pefo"`, or `"other"`.
#' @param release Optional exact PBO forecast-vintage `release_id`.
#' @param table Optional exact PBO table ID.
#' @param series Optional exact canonical PBO `series_id`.
#' @param profile_type Optional exact update profile type.
#' @returns A tibble with one row per unique forecast path. Each profile fixes
#'   the PBO update, series, published unit and multiplier, accounting basis,
#'   consolidation and update type. Coverage columns give its first and last
#'   forecast period, observation count, contributing PBO tables, workbook URL,
#'   and exact workbook SHA-256.
#' @details The bundled snapshot is built from the reviewed 2026-27 Budget
#'   edition of the PBO workbook and contains chart-ready rows from its
#'   medium-term projections, Budget estimates and economic forecasts tables.
#'   Only cells published as forecasts for each update are exposed as profiles.
#'   The function is local, memoized in process, and performs no network access
#'   or persistent write. PBO reuse terms are recorded in `inst/TERMS.md`.
#' @seealso [read_au_pbo_budget_myefo_profile()],
#'   [list_au_pbo_historical_series()]
#' @family pbo-data
#' @examples
#' profiles <- list_au_pbo_budget_myefo_profiles(
#'   publication = "myefo",
#'   series = "pbo-forecast--underlying-cash-balance"
#' )
#' stopifnot(nrow(profiles) >= 1L)
#' @export
list_au_pbo_budget_myefo_profiles <- function(
    publication = NULL,
    release = NULL,
    table = NULL,
    series = NULL,
    profile_type = NULL) {
  store <- .rf_pbo_budget_myefo_store()
  profiles <- store$profiles
  publication <- .rf_pbo_budget_myefo_filter(
    publication, "publication", unique(profiles$publication)
  )
  release <- .rf_pbo_budget_myefo_filter(
    release, "release", unique(profiles$release_id)
  )
  table <- .rf_pbo_budget_myefo_filter(
    table,
    "table",
    unique(unlist(strsplit(profiles$table_ids, " | ", fixed = TRUE)))
  )
  series <- .rf_pbo_budget_myefo_filter(
    series, "series", unique(profiles$series_id)
  )
  profile_type <- .rf_pbo_budget_myefo_filter(
    profile_type, "profile_type", unique(profiles$profile_type)
  )
  out <- profiles
  if (!is.null(publication)) out <- out[out$publication == publication, ]
  if (!is.null(release)) out <- out[out$release_id == release, ]
  if (!is.null(table)) {
    table_values <- strsplit(out$table_ids, " | ", fixed = TRUE)
    out <- out[vapply(
      table_values,
      function(values) table %in% values,
      logical(1)
    ), ]
  }
  if (!is.null(series)) out <- out[out$series_id == series, ]
  if (!is.null(profile_type)) {
    out <- out[out$profile_type == profile_type, ]
  }
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

#' Read one exact PBO historical forecast profile
#'
#' Reads the package-owned forecast path identified by
#' [list_au_pbo_budget_myefo_profiles()]. Applications can replay this stable
#' profile directly; they do not need to reconstruct or join workbook rows.
#'
#' @param profile Exact PBO `profile_id`.
#' @returns A tibble ordered by `period_end`, retaining the exact PBO update,
#'   workbook table and row, series, value, unit, accounting basis, source URL,
#'   workbook SHA-256 and preferred-row flag. The
#'   `readfiscalau_pbo_budget_myefo_provenance` attribute pins the profile and
#'   installed snapshot.
#' @details Only forecast observations are returned. Completed-year actual
#'   histories remain available separately through [read_au_pbo_historical()].
#'   The function is local and has no persistent filesystem side effect.
#' @seealso [list_au_pbo_budget_myefo_profiles()],
#'   [read_au_pbo_historical()]
#' @family pbo-data
#' @examples
#' profiles <- list_au_pbo_budget_myefo_profiles(
#'   publication = "budget",
#'   series = "pbo-forecast--underlying-cash-balance"
#' )
#' forecast <- read_au_pbo_budget_myefo_profile(
#'   profiles$profile_id[[nrow(profiles)]]
#' )
#' stopifnot(!anyDuplicated(forecast$period_end))
#' @export
read_au_pbo_budget_myefo_profile <- function(profile) {
  store <- .rf_pbo_budget_myefo_store()
  profile <- .rf_pbo_budget_myefo_filter(
    profile, "profile", store$profiles$profile_id
  )
  coverage <- store$profiles[
    store$profiles$profile_id == profile,
    ,
    drop = FALSE
  ]
  out <- store$observations[
    store$observations$release_id == coverage$release_id[[1L]] &
      store$observations$series_id == coverage$series_id[[1L]] &
      store$observations$unit == coverage$unit[[1L]] &
      store$observations$unit_multiplier ==
        coverage$unit_multiplier[[1L]] &
      store$observations$accounting_basis ==
        coverage$accounting_basis[[1L]] &
      store$observations$consolidation == coverage$consolidation[[1L]] &
      store$observations$profile_type == coverage$profile_type[[1L]] &
      store$observations$actual_forecast == "forecast" &
      store$observations$is_preferred,
    ,
    drop = FALSE
  ]
  out <- out[order(out$period_end), , drop = FALSE]
  if (nrow(coverage) != 1L ||
      nrow(out) != coverage$observation_count[[1L]] ||
      anyDuplicated(out$period_end) ||
      !identical(min(out$period_end), coverage$first_period[[1L]]) ||
      !identical(max(out$period_end), coverage$last_period[[1L]]) ||
      any(out$actual_forecast != "forecast")) {
    .rf_abort("The selected PBO forecast profile failed its contract.", "schema")
  }
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)
  attr(out, "readfiscalau_pbo_budget_myefo_provenance") <- list(
    profile_id = profile,
    snapshot_schema = coverage$snapshot_schema[[1L]],
    snapshot_id = coverage$snapshot_id[[1L]],
    release_id = coverage$release_id[[1L]],
    release_date = coverage$release_date[[1L]],
    update_label = coverage$update_label[[1L]],
    source_url = coverage$source_url[[1L]],
    source_sha256 = coverage$source_sha256[[1L]]
  )
  out
}
