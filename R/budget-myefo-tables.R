.rf_budget_myefo_history_store <- function() {
  path <- system.file(
    "extdata", "budget-myefo-history.rds", package = "readfiscalau"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "budget-myefo-history.rds")
  }
  if (!file.exists(path)) {
    .rf_abort(
      "The bundled Budget/MYEFO table-history snapshot is missing.",
      "schema"
    )
  }
  cached <- .rf_internal_file_cache_get("budget_myefo_history", path)
  if (!is.null(cached)) return(cached)

  store <- readRDS(path)
  required <- c(
    "schema_version", "built_from", "families", "sources", "coverage",
    "tables", "observations", "series", "validation", "source_artifacts"
  )
  if (!is.list(store) || !identical(store$schema_version,
                                    "budget_myefo_history_v3") ||
      !all(required %in% names(store)) ||
      nrow(store$sources) != 53L ||
      nrow(store$source_artifacts) != 56L ||
      length(unique(store$families$family_id)) != 22L ||
      any(!nzchar(store$families$family_id)) ||
      any(!nzchar(store$observations$series_id[
        store$observations$series_eligible
      ])) ||
      any(store$observations$series_eligible &
          is.na(store$observations$period_end)) ||
      any(!grepl("^[0-9a-f]{64}$", store$sources$pdf_sha256)) ||
      any(!grepl("^[0-9a-f]{64}$", store$sources$container_sha256)) ||
      any(store$sources$assembled_from_members &
          !nzchar(store$sources$archive_member)) ||
      any(!store$source_artifacts$source_kind %in% c(
        "pdf_table_cell", "treasury_chart_data_cell"
      )) ||
      sum(store$source_artifacts$source_kind ==
            "treasury_chart_data_cell") != 3L ||
      any(!grepl(
        "^[0-9a-f]{64}$", store$source_artifacts$source_sha256
      )) ||
      any(!grepl(
        "^[0-9a-f]{64}$", store$source_artifacts$source_container_sha256
      )) ||
      anyDuplicated(store$source_artifacts[c("release_id", "source_kind")]) ||
      !all(c(
        "source_kind", "source_container_sha256", "source_archive_members",
        "source_was_assembled"
      ) %in% names(store$coverage)) ||
      !all(c(
        "source_kind", "source_container_sha256", "source_archive_members",
        "source_was_assembled"
      ) %in% names(store$tables)) ||
      !all(c(
        "source_kind", "source_container_sha256", "source_archive_members",
        "source_was_assembled"
      ) %in% names(store$observations))) {
    .rf_abort(
      "The bundled Budget/MYEFO table-history snapshot failed its contract.",
      "schema"
    )
  }
  attr(store, "readfiscalau_store_path") <- path
  .rf_internal_file_cache_set("budget_myefo_history", path, store)
  store
}

.rf_history_add_source_container <- function(x, store) {
  provenance_columns <- c(
    "source_container_sha256", "source_archive_members",
    "source_was_assembled"
  )
  if (all(provenance_columns %in% names(x))) return(x)

  source_index <- match(x$release_id, store$sources$release_id)
  x$source_container_sha256 <- store$sources$container_sha256[source_index]
  x$source_archive_members <- store$sources$archive_member[source_index]
  x$source_was_assembled <-
    store$sources$assembled_from_members[source_index]
  x
}

.rf_history_publication <- function(publication) {
  if (is.null(publication)) return(NULL)
  .rf_assert_scalar_string(publication, "publication")
  publication <- tolower(publication)
  if (!publication %in% c("budget", "myefo")) {
    .rf_abort(
      "`publication` must be either `budget` or `myefo`.",
      "invalid_argument",
      argument = "publication"
    )
  }
  publication
}

.rf_history_known_value <- function(value, name, choices, subclass) {
  if (is.null(value)) return(NULL)
  .rf_assert_scalar_string(value, name)
  if (!value %in% choices) {
    .rf_abort(
      sprintf("Unknown %s `%s`.", name, value),
      subclass,
      argument = name,
      choices = sort(unique(choices))
    )
  }
  value
}

.rf_history_profile_value_key <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- "<NA>"
  value
}

.rf_history_profile_group_key <- function(observations) {
  multiplier <- sprintf("%.17g", as.numeric(observations$unit_multiplier))
  multiplier[is.na(observations$unit_multiplier)] <- "<NA>"
  fields <- list(
    observations$release_id,
    observations$series_id,
    observations$unit,
    multiplier,
    observations$accounting_basis,
    observations$consolidation,
    observations$profile_type
  )
  fields <- lapply(fields, .rf_history_profile_value_key)
  do.call(paste, c(fields, sep = "\r"))
}

.rf_history_profile_id <- function(row) {
  multiplier_raw <- writeBin(
    as.double(row$unit_multiplier[[1L]]),
    raw(),
    size = 8L,
    endian = "big"
  )
  multiplier_key <- paste(sprintf("%02x", as.integer(multiplier_raw)),
                          collapse = "")
  components <- c(
    "budget_myefo_series_profile_v2",
    as.character(row$release_id[[1L]]),
    as.character(row$series_id[[1L]]),
    as.character(row$unit[[1L]]),
    multiplier_key,
    as.character(row$accounting_basis[[1L]]),
    as.character(row$consolidation[[1L]]),
    as.character(row$profile_type[[1L]])
  )
  paste0(
    "budget-myefo-series-profile-v2-",
    digest::digest(
      paste(components, collapse = "\r"),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

.rf_budget_myefo_series_profiles <- function() {
  store <- .rf_budget_myefo_history_store()
  path <- attr(store, "readfiscalau_store_path")
  cached <- .rf_internal_file_cache_get(
    "budget_myefo_series_profiles",
    path
  )
  if (!is.null(cached)) return(cached)

  observations <- store$observations[
    store$observations$series_eligible &
      store$observations$is_preferred &
      store$observations$actual_forecast == "forecast",
    ,
    drop = FALSE
  ]
  group_key <- .rf_history_profile_group_key(observations)
  order_index <- order(
    group_key,
    observations$period_end,
    observations$family_id,
    observations$table_instance_id
  )
  observations <- observations[order_index, , drop = FALSE]
  group_key <- group_key[order_index]
  first <- !duplicated(group_key)
  last <- !duplicated(group_key, fromLast = TRUE)
  group_id <- cumsum(first)

  joined_values <- function(value, keep_empty = FALSE) {
    groups <- split(value, group_id)
    unname(vapply(groups, function(group) {
      group <- unique(as.character(group[!is.na(group)]))
      if (!isTRUE(keep_empty)) group <- group[nzchar(group)]
      paste(group, collapse = " | ")
    }, character(1)))
  }

  out <- observations[first, c(
    "release_id", "publication", "budget_vintage", "release_date",
    "series_id", "series", "unit", "unit_multiplier",
    "accounting_basis", "consolidation", "profile_type",
    "actual_forecast", "source_url", "source_sha256", "source_kind",
    "source_container_sha256", "source_archive_members",
    "source_was_assembled"
  ), drop = FALSE]
  # Use the last dated canonical label for display while retaining every label
  # below for discovery. Stable series identity never depends on label text.
  out$series <- observations$series[last]
  out$profile_id <- vapply(
    seq_len(nrow(out)),
    function(index) .rf_history_profile_id(out[index, , drop = FALSE]),
    character(1)
  )
  out$snapshot_schema <- rep(store$schema_version, nrow(out))
  out$first_period <- as.Date(observations$period_end[first])
  out$last_period <- as.Date(observations$period_end[last])
  out$observation_count <- as.integer(rle(group_key)$lengths)
  out$series_labels <- joined_values(observations$series)
  out$family_ids <- joined_values(observations$family_id)
  out$family_labels <- joined_values(observations$family_label)
  out$table_instance_ids <- joined_values(observations$table_instance_id)
  out$source_tables <- joined_values(observations$source_table)
  out$period_types <- joined_values(observations$period_type)
  out$source_kinds <- joined_values(observations$source_kind)
  out <- .rf_history_add_source_container(out, store)
  out <- out[c(
    "profile_id", "snapshot_schema", "release_id", "publication",
    "budget_vintage", "release_date", "series_id", "series",
    "series_labels", "family_ids", "family_labels", "table_instance_ids",
    "source_tables", "profile_type", "actual_forecast", "unit",
    "unit_multiplier", "accounting_basis", "consolidation", "period_types",
    "first_period", "last_period", "observation_count", "source_url",
    "source_sha256", "source_kind", "source_kinds",
    "source_container_sha256", "source_archive_members",
    "source_was_assembled"
  )]
  out <- out[order(
    out$release_date,
    out$series_id,
    out$profile_type,
    out$unit,
    out$accounting_basis,
    out$consolidation
  ), , drop = FALSE]
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)

  if (!nrow(out) || anyDuplicated(out$profile_id) ||
      anyNA(out$first_period) || anyNA(out$last_period) ||
      any(out$first_period > out$last_period) ||
      any(out$observation_count < 1L) ||
      sum(out$observation_count) != nrow(observations) ||
      any(!grepl(
        "^budget-myefo-series-profile-v2-[0-9a-f]{64}$",
        out$profile_id
      ))) {
    .rf_abort(
      "The bundled Budget/MYEFO series-profile catalogue failed its contract.",
      "schema"
    )
  }
  .rf_internal_file_cache_set(
    "budget_myefo_series_profiles",
    path,
    out
  )
  out
}

.rf_budget_myefo_actual_chain_policy <- "latest_budget_myefo_actual_then_final_outcome_v2"

.rf_history_actual_group_key <- function(observations) {
  multiplier <- sprintf("%.17g", as.numeric(observations$unit_multiplier))
  multiplier[is.na(observations$unit_multiplier)] <- "<NA>"
  fields <- list(
    observations$series_id,
    observations$unit,
    multiplier,
    observations$accounting_basis,
    observations$consolidation
  )
  fields <- lapply(fields, .rf_history_profile_value_key)
  do.call(paste, c(fields, sep = "\r"))
}

.rf_history_actual_id <- function(row) {
  multiplier_raw <- writeBin(
    as.double(row$unit_multiplier[[1L]]),
    raw(),
    size = 8L,
    endian = "big"
  )
  multiplier_key <- paste(sprintf("%02x", as.integer(multiplier_raw)),
                          collapse = "")
  components <- c(
    "budget_myefo_actual_history_v1",
    as.character(row$series_id[[1L]]),
    as.character(row$unit[[1L]]),
    multiplier_key,
    as.character(row$accounting_basis[[1L]]),
    as.character(row$consolidation[[1L]]),
    .rf_budget_myefo_actual_chain_policy
  )
  paste0(
    "budget-myefo-actual-history-v1-",
    digest::digest(
      paste(components, collapse = "\r"),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

.rf_treasury_actual_supplement <- function() {
  path <- system.file("extdata", "treasury-actual-supplement.rds", package = "readfiscalau")
  if (!nzchar(path)) path <- file.path("inst", "extdata", "treasury-actual-supplement.rds")
  if (!file.exists(path)) .rf_abort("The Treasury Final Budget Outcome actual-history supplement is missing.", "schema")
  cached <- .rf_internal_file_cache_get("treasury_actual_supplement", path)
  if (!is.null(cached)) return(cached)
  out <- readRDS(path)
  if (!is.list(out) || !identical(out$schema, "treasury_actual_supplement_v1") ||
      nrow(out$sources) != 25L || anyDuplicated(out$sources$release_id) ||
      any(!grepl("^[0-9a-f]{64}$", out$sources$pdf_sha256)) ||
      !nrow(out$observations) || any(out$observations$publication != "fbo") ||
      any(out$observations$actual_forecast != "actual") ||
      any(out$observations$profile_type != "actual") ||
      anyNA(out$observations$period_end) ||
      any(!out$observations$series_eligible) ||
      any(!out$observations$release_id %in% out$sources$release_id) ||
      any(!grepl("^[0-9a-f]{64}$", out$observations$source_sha256))) {
    .rf_abort("The Treasury Final Budget Outcome actual-history supplement failed its contract.", "schema")
  }
  .rf_internal_file_cache_set("treasury_actual_supplement", path, out)
  out
}

.rf_budget_myefo_chained_actual_data <- function() {
  store <- .rf_budget_myefo_history_store()
  path <- attr(store, "readfiscalau_store_path")
  cached <- .rf_internal_file_cache_get(
    "budget_myefo_chained_actual_data",
    path
  )
  if (!is.null(cached)) return(cached)

  candidates <- store$observations[
    store$observations$series_eligible &
      store$observations$is_preferred &
      store$observations$actual_forecast == "actual" &
      store$observations$profile_type == "actual",
    ,
    drop = FALSE
  ]
  candidates <- .rf_history_add_source_container(candidates, store)
  supplement <- .rf_treasury_actual_supplement()$observations
  # Fill only published logical measures already supported by the package.
  # Units, multiplier, basis and sector must match exactly; never convert units.
  supported <- candidates
  supplement <- supplement[.rf_history_actual_group_key(supplement) %in%
    unique(.rf_history_actual_group_key(supported)), names(candidates), drop = FALSE]
  candidates <- rbind(candidates, supplement)
  candidates$.history_group <- .rf_history_actual_group_key(candidates)
  candidates <- candidates[order(
    candidates$.history_group,
    candidates$period_end,
    # Final Outcome is a gap-filling source. Within its publications, the
    # reporting vintage orders restatements without inventing a release day.
    candidates$publication != "fbo",
    ifelse(candidates$publication == "fbo", candidates$budget_vintage, ""),
    candidates$release_date,
    candidates$publication,
    candidates$release_id,
    candidates$family_id,
    candidates$table_instance_id
  ), , drop = FALSE]

  period_key <- paste(
    candidates$.history_group,
    as.character(candidates$period_end),
    sep = "\r"
  )
  # A historical date is represented once. When later publications restate
  # that date, the latest Budget/MYEFO actual wins. Final Outcome fills gaps;
  # the selected row retains its
  # complete source identity and the coverage contract reports revisions.
  selected <- candidates[!duplicated(period_key, fromLast = TRUE),
                         , drop = FALSE]
  representatives <- selected[
    !duplicated(selected$.history_group),
    ,
    drop = FALSE
  ]
  history_ids <- vapply(
    seq_len(nrow(representatives)),
    function(index) .rf_history_actual_id(
      representatives[index, , drop = FALSE]
    ),
    character(1)
  )
  selected$history_id <- history_ids[match(
    selected$.history_group,
    representatives$.history_group
  )]
  selected$chain_policy <- .rf_budget_myefo_actual_chain_policy

  selected_key <- paste(selected$history_id, selected$period_end, sep = "\r")
  if (!nrow(selected) || anyDuplicated(selected_key) ||
      anyNA(selected$period_end) || any(!is.na(selected$value) & !is.finite(selected$value)) ||
      any(selected$actual_forecast != "actual") ||
      any(selected$profile_type != "actual") ||
      any(!grepl(
        "^budget-myefo-actual-history-v1-[0-9a-f]{64}$",
        selected$history_id
      ))) {
    .rf_abort(
      "The bundled Budget/MYEFO chained-actual data failed its contract.",
      "schema"
    )
  }

  out <- list(candidates = candidates, selected = selected)
  .rf_internal_file_cache_set(
    "budget_myefo_chained_actual_data",
    path,
    out
  )
  out
}

.rf_budget_myefo_actual_histories <- function() {
  store <- .rf_budget_myefo_history_store()
  path <- attr(store, "readfiscalau_store_path")
  cached <- .rf_internal_file_cache_get(
    "budget_myefo_actual_histories",
    path
  )
  if (!is.null(cached)) return(cached)

  chained <- .rf_budget_myefo_chained_actual_data()
  candidates <- chained$candidates
  selected <- chained$selected
  group_key <- selected$.history_group
  first <- !duplicated(group_key)
  last <- !duplicated(group_key, fromLast = TRUE)
  group_id <- cumsum(first)
  groups <- split(seq_len(nrow(selected)), group_id)

  joined_values <- function(value) {
    unname(vapply(groups, function(index) {
      group <- unique(as.character(value[index]))
      group <- group[!is.na(group) & nzchar(group)]
      paste(group, collapse = " | ")
    }, character(1)))
  }
  provenance_id <- unname(vapply(groups, function(index) {
    rows <- selected[index, , drop = FALSE]
    row_key <- paste(
      rows$release_id,
      as.character(rows$release_date),
      as.character(rows$period_end),
      sprintf("%.17g", as.numeric(rows$value)),
      rows$source_sha256,
      rows$source_container_sha256,
      sep = "\r"
    )
    paste0(
      "budget-myefo-actual-provenance-v1-",
      digest::digest(
        paste(row_key, collapse = "\n"),
        algo = "sha256",
        serialize = FALSE
      )
    )
  }, character(1)))

  candidate_groups <- split(
    seq_len(nrow(candidates)),
    candidates$.history_group
  )
  candidate_count <- vapply(
    selected$.history_group[first],
    function(key) length(candidate_groups[[key]]),
    integer(1)
  )
  revised_period_count <- vapply(
    selected$.history_group[first],
    function(key) {
      rows <- candidates[candidate_groups[[key]], , drop = FALSE]
      values <- split(rows$value, as.character(rows$period_end))
      sum(vapply(values, function(value) {
        length(unique(as.numeric(value))) > 1L
      }, logical(1)))
    },
    integer(1)
  )

  out <- selected[first, c(
    "history_id", "series_id", "series", "unit", "unit_multiplier",
    "accounting_basis", "consolidation", "profile_type",
    "actual_forecast", "chain_policy"
  ), drop = FALSE]
  out$snapshot_schema <- rep(store$schema_version, nrow(out))
  out$series <- selected$series[last]
  out$series_labels <- joined_values(selected$series)
  out$family_ids <- joined_values(selected$family_id)
  out$family_labels <- joined_values(selected$family_label)
  out$table_instance_ids <- joined_values(selected$table_instance_id)
  out$source_tables <- joined_values(selected$source_table)
  out$period_types <- joined_values(selected$period_type)
  out$first_period <- as.Date(selected$period_end[first])
  out$last_period <- as.Date(selected$period_end[last])
  out$observation_count <- as.integer(vapply(groups, length, integer(1)))
  out$missing_year_count <- as.integer(vapply(groups, function(index) {
    years <- unique(as.integer(format(selected$period_end[index], "%Y")))
    max(years) - min(years) + 1L - length(years)
  }, integer(1)))
  out$coverage_type <- ifelse(
    out$observation_count == 1L, "single_observation",
    ifelse(out$missing_year_count > 0L, "sparse", "continuous")
  )
  out$coverage_label <- ifelse(
    out$coverage_type == "single_observation",
    paste("Single observation:", format(out$first_period, "%Y")),
    sprintf("%s: %d observations, %s–%s%s",
      ifelse(out$coverage_type == "sparse", "Sparse history", "Continuous history"),
      out$observation_count, format(out$first_period, "%Y"),
      format(out$last_period, "%Y"),
      ifelse(out$missing_year_count > 0L,
        sprintf(" (%d missing %s)", out$missing_year_count,
          ifelse(out$missing_year_count == 1L, "year", "years")), ""))
  )
  missing_values <- vapply(groups,function(index)sum(is.na(selected$value[index])),integer(1))
  has_missing <- missing_values>0L
  out$coverage_label[has_missing] <- paste0(out$coverage_label[has_missing],
    sprintf(" (%d values not published)",missing_values[has_missing]))
  out$candidate_observation_count <- as.integer(candidate_count)
  out$superseded_observation_count <- as.integer(
    out$candidate_observation_count - out$observation_count
  )
  out$source_release_count <- as.integer(vapply(groups, function(index) {
    length(unique(selected$release_id[index]))
  }, integer(1)))
  out$revised_period_count <- as.integer(revised_period_count)
  out$provenance_id <- provenance_id
  out <- out[c(
    "history_id", "snapshot_schema", "chain_policy", "series_id",
    "series", "series_labels", "family_ids", "family_labels",
    "table_instance_ids", "source_tables", "profile_type",
    "actual_forecast", "unit", "unit_multiplier", "accounting_basis",
    "consolidation", "period_types", "first_period", "last_period",
    "observation_count", "candidate_observation_count",
    "superseded_observation_count", "source_release_count",
    "revised_period_count", "provenance_id", "coverage_type",
    "missing_year_count", "coverage_label"
  )]
  out <- out[order(
    out$series_id,
    out$unit,
    out$accounting_basis,
    out$consolidation
  ), , drop = FALSE]
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)

  if (!nrow(out) || anyDuplicated(out$history_id) ||
      anyNA(out$first_period) || anyNA(out$last_period) ||
      any(out$first_period > out$last_period) ||
      any(out$observation_count < 1L) ||
      any(out$candidate_observation_count < out$observation_count) ||
      any(out$superseded_observation_count < 0L) ||
      any(out$source_release_count < 1L) ||
      any(out$profile_type != "actual") ||
      any(out$actual_forecast != "actual") ||
      any(out$chain_policy != .rf_budget_myefo_actual_chain_policy) ||
      any(!grepl(
        "^budget-myefo-actual-provenance-v1-[0-9a-f]{64}$",
        out$provenance_id
      ))) {
    .rf_abort(
      "The bundled Budget/MYEFO actual-history catalogue failed its contract.",
      "schema"
    )
  }
  .rf_internal_file_cache_set(
    "budget_myefo_actual_histories",
    path,
    out
  )
  out
}

#' List historical Budget and MYEFO table families
#'
#' Summarises the 22 selected table families extracted from official Australian
#' Treasury Budget Paper No. 1 and MYEFO publications. Coverage starts with the
#' 2000-01 publications, but individual families begin later or contain gaps.
#' Use this catalogue before selecting a family for
#' [list_au_budget_myefo_tables()] or [read_au_budget_myefo_table()].
#'
#' `query_mode` is `"series_and_table"` when dated rows are available both as
#' canonical series and complete tables, `"hybrid"` when durable headline rows
#' are series-queryable while variable reconciliation detail remains available
#' through the table reader, and `"table_only"` when the complete published
#' table is the public contract. No chart is included. The snapshot is static,
#' local, and makes no network request or filesystem write.
#'
#' @returns A tibble with one row per family and these columns:
#'   `family_id`, `family_label`, `query_mode`, `budget_first_vintage`,
#'   `budget_last_vintage`, `budget_release_count`, `myefo_first_vintage`,
#'   `myefo_last_vintage`, `myefo_release_count`, `table_instance_count`,
#'   `table_part_count`, and `series_count`. Vintage columns are character;
#'   count columns are integer. An unavailable publication/family combination
#'   has `NA` first/last vintages and a zero release count.
#' @details Coverage is calculated from the installed snapshot. A release
#'   count is the number of distinct publications in which the family was
#'   found, not a claim that table numbering, row labels, accounting concepts,
#'   or forecast horizons are unchanged. The source URLs and hashes are
#'   available from [list_au_budget_myefo_tables()].
#' @seealso [list_au_budget_myefo_tables()],
#'   [list_au_budget_myefo_series()], [read_au_budget_myefo_series()]
#' @family budget-myefo-table-history
#' @examples
#' families <- list_au_budget_myefo_families()
#' families[families$family_id %in% c(
#'   "domestic_economy_detailed_forecasts", "detailed_cash_receipts"
#' ), c("family_label", "query_mode", "budget_first_vintage",
#'      "myefo_first_vintage")]
#' stopifnot(nrow(families) == 22L)
#' @export
list_au_budget_myefo_families <- function() {
  store <- .rf_budget_myefo_history_store()
  coverage <- store$coverage
  families <- store$families[c("family_id", "family_label", "query_mode")]
  rows <- lapply(seq_len(nrow(families)), function(i) {
    family <- families$family_id[[i]]
    selected <- coverage[coverage$family_id == family, , drop = FALSE]
    by_publication <- function(publication) {
      x <- selected[selected$publication == publication, , drop = FALSE]
      releases <- x[!duplicated(x$release_id), , drop = FALSE]
      if (!nrow(releases)) {
        return(list(first = NA_character_, last = NA_character_, count = 0L))
      }
      releases <- releases[order(releases$release_date), , drop = FALSE]
      list(
        first = releases$budget_vintage[[1L]],
        last = releases$budget_vintage[[nrow(releases)]],
        count = nrow(releases)
      )
    }
    budget <- by_publication("budget")
    myefo <- by_publication("myefo")
    eligible <- store$observations$family_id == family &
      store$observations$series_eligible
    data.frame(
      family_id = family,
      family_label = families$family_label[[i]],
      query_mode = families$query_mode[[i]],
      budget_first_vintage = budget$first,
      budget_last_vintage = budget$last,
      budget_release_count = as.integer(budget$count),
      myefo_first_vintage = myefo$first,
      myefo_last_vintage = myefo$last,
      myefo_release_count = as.integer(myefo$count),
      table_instance_count = as.integer(length(unique(
        selected$table_instance_id
      ))),
      table_part_count = as.integer(nrow(selected)),
      series_count = as.integer(length(unique(
        store$observations$series_id[eligible]
      ))),
      stringsAsFactors = FALSE
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

#' List historical Budget and MYEFO tables
#'
#' Lists the installed official-publication table snapshots, with one row per
#' physical table part. Filters are optional and exact. Use `table_instance_id`
#' to join continued parts or select one table in
#' [read_au_budget_myefo_table()].
#'
#' @param publication Optional `"budget"` or `"myefo"` filter.
#' @param release Optional exact release ID, such as
#'   `"treasury-budget-2026-27-final"`.
#' @param family Optional canonical family ID returned by
#'   [list_au_budget_myefo_families()].
#' @returns A tibble with these columns: `release_id`, `publication`,
#'   `budget_vintage`, `release_date`, `family_id`, `family_label`,
#'   `query_mode`, `table_instance_id`, `source_table`, `source_page`,
#'   `table_part`, `source_kind`, `source_url`, `source_sha256`,
#'   `source_container_sha256`, `source_archive_members`, and
#'   `source_was_assembled`. `source_sha256` hashes the parsed PDF or chart-data
#'   CSV member; `source_kind` distinguishes those inputs. For ZIP sources, the
#'   container hash and ordered member list identify the downloadable archive.
#'   Dates use
#'   class `Date`; pages and parts are integer-valued;
#'   `source_was_assembled` is logical; all other columns are character.
#' @details The function reads only the bundled RDS snapshot, may reuse an
#'   in-process cache, makes no network request, and has no persistent side
#'   effect. Unknown filter identifiers raise typed errors; a known filter
#'   combination with no matching table returns a zero-row tibble.
#' @seealso [list_au_budget_myefo_families()],
#'   [read_au_budget_myefo_table()]
#' @family budget-myefo-table-history
#' @examples
#' tables <- list_au_budget_myefo_tables(
#'   publication = "budget",
#'   release = "treasury-budget-2026-27-final",
#'   family = "domestic_economy_detailed_forecasts"
#' )
#' tables[, c("budget_vintage", "source_table", "source_page")]
#' stopifnot(nrow(tables) >= 1L)
#' @export
list_au_budget_myefo_tables <- function(
    publication = NULL,
    release = NULL,
    family = NULL) {
  store <- .rf_budget_myefo_history_store()
  publication <- .rf_history_publication(publication)
  release <- .rf_history_known_value(
    release, "release", store$sources$release_id, "release_not_found"
  )
  family <- .rf_history_known_value(
    family, "family", store$families$family_id, "table_family"
  )
  out <- store$coverage
  if (!is.null(publication)) out <- out[out$publication == publication, ]
  if (!is.null(release)) out <- out[out$release_id == release, ]
  if (!is.null(family)) out <- out[out$family_id == family, ]
  out <- .rf_history_add_source_container(out, store)
  out <- out[order(
    out$release_date, out$family_id, out$table_instance_id, out$table_part
  ), , drop = FALSE]
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

#' List canonical series in historical Budget and MYEFO tables
#'
#' Returns coverage for the durable rows exposed by the historical table
#' snapshot. The same concept uses one canonical `series_id` across summary,
#' detailed, and reconciliation tables. Publication profiles such as a prior
#' Budget estimate and the updated MYEFO estimate remain distinct observations
#' under that shared ID.
#'
#' @param publication Optional `"budget"` or `"myefo"` filter.
#' @param family Optional canonical family ID.
#' @param series Optional exact canonical `series_id`.
#' @returns A tibble with `publication`, `series_id`, `family_id`, `family_label`,
#'   `series`, `unit`, `unit_multiplier`, `accounting_basis`, `consolidation`,
#'   `first_period`, `last_period`, `first_release`, `last_release`,
#'   `profile_types`, `release_count`, and `observation_count`. Period and
#'   release bounds are `Date`; counts are integer-valued.
#' @details Complete-table-only and non-time cells are omitted. Coverage is
#'   descriptive: series can have gaps and definitions can change between
#'   publications. Inspect the returned source table/page fields from
#'   [read_au_budget_myefo_series()] and the complete table when comparing
#'   accounting or presentation changes.
#' @seealso [read_au_budget_myefo_series()],
#'   [list_au_budget_myefo_families()]
#' @family budget-myefo-table-history
#' @examples
#' series <- list_au_budget_myefo_series(
#'   publication = "budget",
#'   series = "underlying-cash-balance"
#' )
#' series[, c("series_id", "unit", "first_period", "last_period")]
#' stopifnot(length(unique(series$series_id)) == 1L)
#' @export
list_au_budget_myefo_series <- function(
    publication = NULL,
    family = NULL,
    series = NULL) {
  store <- .rf_budget_myefo_history_store()
  publication <- .rf_history_publication(publication)
  family <- .rf_history_known_value(
    family, "family", store$families$family_id, "table_family"
  )
  series <- .rf_history_known_value(
    series, "series", store$series$series_id, "series_not_found"
  )
  out <- store$series
  if (!is.null(publication)) out <- out[out$publication == publication, ]
  if (!is.null(family)) out <- out[out$family_id == family, ]
  if (!is.null(series)) out <- out[out$series_id == series, ]
  out$family_label <- store$families$family_label[
    match(out$family_id, store$families$family_id)
  ]
  out <- out[c(
    "publication", "series_id", "family_id", "family_label", "series",
    "unit", "unit_multiplier", "accounting_basis", "consolidation",
    "first_period", "last_period", "first_release", "last_release",
    "profile_types", "release_count", "observation_count"
  )]
  out <- out[order(out$publication, out$series_id, out$family_id, out$unit), ]
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

#' List chart-ready historical Budget and MYEFO forecast profiles
#'
#' Lists the exact non-duplicating forecast paths available from the historical
#' table snapshot. One profile fixes publication release, canonical series,
#' published unit and multiplier, accounting basis, consolidation, and
#' publication profile. Those fields are required together because a release
#' can print multiple Budget, MYEFO, or PEFO forecast profiles for the same
#' canonical concept, sometimes under more than one accounting presentation.
#' Actuals are exposed separately as package-constructed histories by
#' [list_au_budget_myefo_actual_histories()].
#'
#' @param publication Optional `"budget"` or `"myefo"` filter.
#' @param release Optional exact release ID.
#' @param family Optional canonical family ID. A profile is retained when any
#'   of its preferred observations comes from that family.
#' @param series Optional exact canonical `series_id`.
#' @param profile_type Optional exact published forecast profile: `"budget"`,
#'   `"myefo"`, or `"pefo"`.
#' @param actual_forecast Optional `"forecast"` filter, retained for a uniform
#'   package search contract.
#' @returns A tibble with one row per exact dated path. Scalar columns are
#'   `profile_id`, `snapshot_schema`, `release_id`, `publication`,
#'   `budget_vintage`, `release_date`, `series_id`, `series`, `profile_type`,
#'   `actual_forecast`, `unit`, `unit_multiplier`, `accounting_basis`,
#'   `consolidation`, `first_period`, `last_period`, `observation_count`,
#'   `source_url`, `source_sha256`, `source_kind`, `source_kinds`,
#'   `source_container_sha256`,
#'   `source_archive_members`, and `source_was_assembled`. The character
#'   columns `series_labels`, `family_ids`, `family_labels`,
#'   `table_instance_ids`, `source_tables`, `source_kinds`, and `period_types`
#'   retain every
#'   distinct value used by the preferred observations in that path, joined in
#'   first-seen order with `" | "`. Dates use class `Date` and
#'   `observation_count` is integer-valued.
#' @details `profile_id` is a deterministic SHA-256 identity over the fixed
#'   semantic fields under the `budget_myefo_series_profile_v2` contract. It is
#'   suitable for saved application specifications; label text and source-table
#'   placement are deliberately not identity. Reading the matching canonical
#'   series with [read_au_budget_myefo_series()] and filtering the returned rows
#'   by the fixed fields yields exactly `observation_count` unique dates.
#'   Complete-table-only and non-time cells are excluded. The function is
#'   local, memoized in process, and has no network or persistent filesystem
#'   side effect.
#' @seealso [read_au_budget_myefo_series()],
#'   [list_au_budget_myefo_actual_histories()],
#'   [list_au_budget_myefo_series()], [list_au_budget_myefo_tables()]
#' @family budget-myefo-table-history
#' @examples
#' profiles <- list_au_budget_myefo_series_profiles(
#'   release = "treasury-budget-2026-27-final",
#'   series = "underlying-cash-balance",
#'   profile_type = "budget"
#' )
#' profiles[, c(
#'   "profile_id", "unit", "accounting_basis",
#'   "first_period", "last_period", "observation_count"
#' )]
#' stopifnot(!anyDuplicated(profiles$profile_id))
#' @export
list_au_budget_myefo_series_profiles <- function(
    publication = NULL,
    release = NULL,
    family = NULL,
    series = NULL,
    profile_type = NULL,
    actual_forecast = NULL) {
  store <- .rf_budget_myefo_history_store()
  publication <- .rf_history_publication(publication)
  release <- .rf_history_known_value(
    release, "release", store$sources$release_id, "release_not_found"
  )
  family <- .rf_history_known_value(
    family, "family", store$families$family_id, "table_family"
  )
  series <- .rf_history_known_value(
    series, "series", store$series$series_id, "series_not_found"
  )
  profile_type <- .rf_history_known_value(
    profile_type,
    "profile_type",
    unique(store$observations$profile_type[
      store$observations$series_eligible &
        store$observations$is_preferred &
        store$observations$actual_forecast == "forecast"
    ]),
    "invalid_argument"
  )
  actual_forecast <- .rf_history_known_value(
    actual_forecast,
    "actual_forecast",
    "forecast",
    "invalid_argument"
  )

  out <- .rf_budget_myefo_series_profiles()
  if (!is.null(publication)) out <- out[out$publication == publication, ]
  if (!is.null(release)) out <- out[out$release_id == release, ]
  if (!is.null(family)) {
    family_values <- strsplit(out$family_ids, " | ", fixed = TRUE)
    keep <- vapply(
      family_values,
      function(values) family %in% values,
      logical(1)
    )
    out <- out[keep, ]
  }
  if (!is.null(series)) out <- out[out$series_id == series, ]
  if (!is.null(profile_type)) {
    out <- out[out$profile_type == profile_type, ]
  }
  if (!is.null(actual_forecast)) {
    out <- out[out$actual_forecast == actual_forecast, ]
  }
  row.names(out) <- NULL
  out
}

#' Read one exact Budget or MYEFO forecast profile
#'
#' Reads the package-owned forecast path identified by
#' [list_au_budget_myefo_series_profiles()]. Applications can replay this
#' stable identity directly without filtering publication cells themselves.
#'
#' @param profile Exact `profile_id` from
#'   [list_au_budget_myefo_series_profiles()].
#' @returns A tibble ordered by date with the same observation and source
#'   columns as [read_au_budget_myefo_series()]. The
#'   `readfiscalau_series_profile_provenance` attribute records the profile,
#'   snapshot, release, source hashes, and source container.
#' @details Only release-specific forecast profiles are accepted. Chained
#'   actuals use [read_au_budget_myefo_actual_history()]. The function is local,
#'   deterministic, and has no persistent filesystem side effect.
#' @seealso [list_au_budget_myefo_series_profiles()],
#'   [read_au_budget_myefo_actual_history()]
#' @family budget-myefo-table-history
#' @examples
#' profiles <- list_au_budget_myefo_series_profiles(
#'   release = "treasury-budget-2026-27-final",
#'   series = "underlying-cash-balance",
#'   profile_type = "budget"
#' )
#' profile <- profiles[
#'   profiles$unit == "AUD billion" &
#'     profiles$accounting_basis == "cash",
#' ]
#' forecast <- read_au_budget_myefo_series_profile(profile$profile_id[[1L]])
#' stopifnot(!anyDuplicated(forecast$period_end))
#' @export
read_au_budget_myefo_series_profile <- function(profile) {
  coverage <- .rf_budget_myefo_series_profiles()
  profile <- .rf_history_known_value(
    profile,
    "profile",
    coverage$profile_id,
    "series_not_found"
  )
  coverage_row <- coverage[coverage$profile_id == profile, , drop = FALSE]
  out <- read_au_budget_myefo_series(
    coverage_row$series_id[[1L]],
    publication = coverage_row$publication[[1L]],
    release = coverage_row$release_id[[1L]],
    unit = coverage_row$unit[[1L]],
    all_sources = FALSE
  )
  out <- out[
    out$profile_type == coverage_row$profile_type[[1L]] &
      out$actual_forecast == "forecast" &
      as.numeric(out$unit_multiplier) ==
        as.numeric(coverage_row$unit_multiplier[[1L]]) &
      out$accounting_basis == coverage_row$accounting_basis[[1L]] &
      out$consolidation == coverage_row$consolidation[[1L]] &
      out$is_preferred,
    ,
    drop = FALSE
  ]
  out <- out[order(out$period_end), , drop = FALSE]
  if (nrow(coverage_row) != 1L ||
      nrow(out) != coverage_row$observation_count[[1L]] ||
      anyDuplicated(out$period_end) ||
      !identical(min(out$period_end), coverage_row$first_period[[1L]]) ||
      !identical(max(out$period_end), coverage_row$last_period[[1L]]) ||
      any(out$actual_forecast != "forecast")) {
    .rf_abort(
      "The selected Budget/MYEFO forecast profile failed its contract.",
      "schema"
    )
  }
  row.names(out) <- NULL
  out <- tibble::as_tibble(out)
  attr(out, "readfiscalau_series_profile_provenance") <- list(
    profile_id = profile,
    snapshot_schema = coverage_row$snapshot_schema[[1L]],
    release_id = coverage_row$release_id[[1L]],
    release_date = coverage_row$release_date[[1L]],
    source_url = coverage_row$source_url[[1L]],
    source_sha256 = coverage_row$source_sha256[[1L]],
    source_kind = coverage_row$source_kind[[1L]],
    source_kinds = coverage_row$source_kinds[[1L]],
    source_container_sha256 =
      coverage_row$source_container_sha256[[1L]],
    source_archive_members =
      coverage_row$source_archive_members[[1L]],
    source_was_assembled = coverage_row$source_was_assembled[[1L]]
  )
  out
}

#' List chained actual histories from Budget and MYEFO tables
#'
#' Returns one chart-ready actual history for each stable combination of
#' canonical series, published unit and multiplier, accounting basis, and
#' consolidation. The package constructs each history across all installed
#' Budget and MYEFO publications, with missing actual years filled from the
#' bundled Final Budget Outcome publications; applications do not stitch
#' release fragments themselves. Native units and accounting scope must match.
#'
#' @param family Optional canonical family ID. A history is retained when any
#'   selected observation comes from that family.
#' @param series Optional exact canonical `series_id`.
#' @param unit Optional exact published unit.
#' @param accounting_basis Optional exact accounting basis.
#' @param consolidation Optional exact consolidation.
#' @returns A tibble with one row per chained actual history. `history_id` is
#'   the stable logical identity and `provenance_id` pins the selected source
#'   rows in the installed snapshot. Coverage and audit columns report the
#'   date range, selected observation count, candidate and superseded cells,
#'   contributing releases, and dates whose published actual value changed.
#'   `coverage_type` distinguishes `single_observation`, `sparse`, and
#'   `continuous` annual coverage. `missing_year_count` counts absent years
#'   between the endpoints; `coverage_label` describes this for display.
#' @details The latest Budget/MYEFO actual for an observation date wins.
#'   Final Budget Outcome supplies missing dates for the same measure, unit,
#'   multiplier, basis and sector. No unit conversion or interpolation occurs.
#'   The selected row retains its
#'   release, table, source kind, URL, source/member hash, and container hash in
#'   [read_au_budget_myefo_actual_history()]. Forecast profiles are not part of
#'   this contract; they remain release-specific in
#'   [list_au_budget_myefo_series_profiles()]. The function is local,
#'   deterministic, memoized in process, and has no persistent side effect.
#' @seealso [read_au_budget_myefo_actual_history()],
#'   [list_au_budget_myefo_series_profiles()]
#' @family budget-myefo-table-history
#' @examples
#' histories <- list_au_budget_myefo_actual_histories(
#'   series = "agricultural-levies",
#'   unit = "AUD million",
#'   accounting_basis = "cash"
#' )
#' stopifnot(nrow(histories) == 1L)
#' @export
list_au_budget_myefo_actual_histories <- function(
    family = NULL,
    series = NULL,
    unit = NULL,
    accounting_basis = NULL,
    consolidation = NULL) {
  store <- .rf_budget_myefo_history_store()
  family <- .rf_history_known_value(
    family, "family", store$families$family_id, "table_family"
  )
  series <- .rf_history_known_value(
    series, "series", store$series$series_id, "series_not_found"
  )
  out <- .rf_budget_myefo_actual_histories()
  unit <- .rf_history_known_value(
    unit, "unit", unique(out$unit), "invalid_argument"
  )
  accounting_basis <- .rf_history_known_value(
    accounting_basis,
    "accounting_basis",
    unique(out$accounting_basis),
    "invalid_argument"
  )
  consolidation <- .rf_history_known_value(
    consolidation,
    "consolidation",
    unique(out$consolidation),
    "invalid_argument"
  )
  if (!is.null(family)) {
    family_values <- strsplit(out$family_ids, " | ", fixed = TRUE)
    keep <- vapply(
      family_values,
      function(values) family %in% values,
      logical(1)
    )
    out <- out[keep, ]
  }
  if (!is.null(series)) out <- out[out$series_id == series, ]
  if (!is.null(unit)) out <- out[out$unit == unit, ]
  if (!is.null(accounting_basis)) {
    out <- out[out$accounting_basis == accounting_basis, ]
  }
  if (!is.null(consolidation)) {
    out <- out[out$consolidation == consolidation, ]
  }
  row.names(out) <- NULL
  out
}

#' Read one package-constructed Budget/MYEFO actual history
#'
#' Reads the exact chained actual history identified by
#' [list_au_budget_myefo_actual_histories()]. Each returned observation retains
#' the official publication and table row selected by the package chain.
#'
#' @param history Exact `history_id` from
#'   [list_au_budget_myefo_actual_histories()].
#' @returns A tibble ordered by date with `history_id`, `chain_policy`, release,
#'   publication, table, series, period, value, unit, accounting, source kind,
#'   URL, source/member hash, container hash, archive-member, and
#'   preferred-source columns. The
#'   `readfiscalau_actual_history_provenance` attribute records the snapshot,
#'   chain policy, provenance digest, and unique contributing releases.
#' @details The versioned policy keeps the latest Budget/MYEFO actual and
#'   fills missing dates from Final Budget Outcome. No forecast or estimate
#'   is promoted to an actual:
#'   only cells classified as both `actual_forecast == "actual"` and
#'   `profile_type == "actual"` are eligible. The function is local and has no
#'   persistent filesystem side effect.
#' @seealso [list_au_budget_myefo_actual_histories()],
#'   [read_au_budget_myefo_series()]
#' @family budget-myefo-table-history
#' @examples
#' coverage <- list_au_budget_myefo_actual_histories(
#'   series = "agricultural-levies",
#'   unit = "AUD million",
#'   accounting_basis = "cash"
#' )
#' agricultural_levies <- read_au_budget_myefo_actual_history(
#'   coverage$history_id[[1L]]
#' )
#' stopifnot(!anyDuplicated(agricultural_levies$period_end))
#' @export
read_au_budget_myefo_actual_history <- function(history) {
  coverage <- .rf_budget_myefo_actual_histories()
  history <- .rf_history_known_value(
    history,
    "history",
    coverage$history_id,
    "series_not_found"
  )
  selected <- .rf_budget_myefo_chained_actual_data()$selected
  out <- selected[selected$history_id == history, , drop = FALSE]
  out <- out[order(out$period_end), , drop = FALSE]
  coverage_row <- coverage[coverage$history_id == history, , drop = FALSE]
  if (!nrow(out) || nrow(coverage_row) != 1L ||
      nrow(out) != coverage_row$observation_count[[1L]] ||
      anyDuplicated(out$period_end) ||
      any(out$actual_forecast != "actual") ||
      any(out$profile_type != "actual")) {
    .rf_abort(
      "The selected Budget/MYEFO actual history failed its contract.",
      "schema"
    )
  }
  out$.history_group <- NULL
  out <- out[c(
    "history_id", "chain_policy", "release_id", "publication",
    "budget_vintage", "release_date", "family_id", "table_instance_id",
    "source_table", "source_page", "series_id", "series", "row_label",
    "row_path", "fiscal_year", "calendar_year", "period_end",
    "actual_forecast", "profile_type", "value", "unit", "unit_multiplier",
    "accounting_basis", "consolidation", "source_kind", "source_url",
    "source_sha256", "source_container_sha256", "source_archive_members",
    "source_was_assembled", "is_preferred"
  )]
  release_columns <- c(
    "release_id", "publication", "budget_vintage", "release_date",
    "source_kind", "source_url", "source_sha256", "source_container_sha256",
    "source_archive_members", "source_was_assembled"
  )
  releases <- out[
    !duplicated(out[release_columns]), release_columns, drop = FALSE
  ]
  row.names(out) <- NULL
  row.names(releases) <- NULL
  out <- tibble::as_tibble(out)
  attr(out, "readfiscalau_actual_history_provenance") <- list(
    history_id = history,
    snapshot_schema = coverage_row$snapshot_schema[[1L]],
    chain_policy = coverage_row$chain_policy[[1L]],
    provenance_id = coverage_row$provenance_id[[1L]],
    source_releases = tibble::as_tibble(releases)
  )
  out
}

#' Read a complete historical Budget or MYEFO table
#'
#' Reads one table family from one publication release. `format = "cells"`
#' returns every parsed numeric cell, including non-time `Total` columns and
#' rows that are deliberately not series-queryable. `format = "text"` returns
#' the complete layout-preserving extracted text for each table part. This is
#' the appropriate interface for volatile reconciliations and the table-only
#' expense-estimate reconciliation family.
#'
#' @param release Exact release ID from [list_au_budget_myefo_tables()].
#' @param family Exact canonical family ID.
#' @param table_instance_id Optional exact instance ID. Omit it to return every
#'   matching table instance/continued part in the release and family.
#' @param format Either `"cells"` (the default) or `"text"`.
#' @returns For `format = "cells"`, a tibble with one row per extracted numeric
#'   cell and the stored release, family, table, row, column, value, unit,
#'   period, series-eligibility, profile, source URL/hash, and raw-line fields.
#'   For `format = "text"`, a tibble with one row per physical table part and
#'   the same table provenance plus `raw_text`. Row order follows source page,
#'   table part, row, and column. A table can contain non-time columns with
#'   missing `period_end`; those cells are never canonical series.
#' @details Data are read from the installed static RDS file. PDFs are parsed
#'   only by the maintainer build tool; this function performs no PDF parsing,
#'   network access, cache write, or working-directory write. `raw_text`
#'   preserves the extracted table layout, while the source URL, page and
#'   SHA-256 identify the official publication for verification. Returned rows
#'   also identify the downloadable container hash, any ordered ZIP members,
#'   and whether those members were assembled into the parsed PDF.
#' @seealso [read_au_budget_myefo_series()],
#'   [list_au_budget_myefo_tables()]
#' @family budget-myefo-table-history
#' @examples
#' table <- read_au_budget_myefo_table(
#'   "treasury-budget-2026-27-final",
#'   "domestic_economy_detailed_forecasts"
#' )
#' table[table$series_id == "mining-investment",
#'       c("fiscal_year", "value", "unit")]
#' stopifnot(any(table$series_id == "mining-investment"))
#' @export
read_au_budget_myefo_table <- function(
    release,
    family,
    table_instance_id = NULL,
    format = c("cells", "text")) {
  store <- .rf_budget_myefo_history_store()
  release <- .rf_history_known_value(
    release, "release", store$sources$release_id, "release_not_found"
  )
  family <- .rf_history_known_value(
    family, "family", store$families$family_id, "table_family"
  )
  table_instance_id <- .rf_history_known_value(
    table_instance_id, "table_instance_id", store$tables$table_instance_id,
    "table_required"
  )
  format <- match.arg(format)
  source <- if (identical(format, "cells")) store$observations else store$tables
  out <- source[
    source$release_id == release & source$family_id == family,
    ,
    drop = FALSE
  ]
  if (!is.null(table_instance_id)) {
    out <- out[out$table_instance_id == table_instance_id, , drop = FALSE]
  }
  if (!nrow(out)) {
    .rf_abort(
      "The selected release does not contain that table family or instance.",
      "table_required",
      release = release,
      family = family,
      table_instance_id = table_instance_id
    )
  }
  out <- .rf_history_add_source_container(out, store)
  if (identical(format, "cells")) {
    out <- out[order(
      out$source_page, out$table_part, out$row_index, out$column_index
    ), , drop = FALSE]
  } else {
    out <- out[order(out$source_page, out$table_part), , drop = FALSE]
  }
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

#' Read a canonical series across historical Budget and MYEFO tables
#'
#' Reads one canonical economic or fiscal concept across publication vintages.
#' The default keeps one preferred source occurrence per release, period, unit,
#' accounting basis, and profile, preventing duplicate concepts from summary
#' and detailed tables. Set `all_sources = TRUE` to inspect every source-table
#' occurrence without creating a second series identity.
#'
#' @param series Exact canonical `series_id` from
#'   [list_au_budget_myefo_series()].
#' @param publication Optional `"budget"` or `"myefo"` filter.
#' @param release Optional exact release ID.
#' @param unit Optional exact unit, for example `"AUD billion"` or
#'   `"% of GDP"`.
#' @param all_sources One non-missing logical value. `FALSE` returns preferred
#'   deduplicated occurrences; `TRUE` returns every matching source occurrence.
#' @returns A tibble ordered by publication, release date, period and profile,
#'   with columns `release_id`, `publication`, `budget_vintage`, `release_date`,
#'   `family_id`, `table_instance_id`, `source_table`, `source_page`,
#'   `series_id`, `series`, `row_label`, `row_path`, `fiscal_year`,
#'   `calendar_year`, `period_end`, `actual_forecast`, `profile_type`, `value`,
#'   `unit`, `unit_multiplier`, `accounting_basis`, `consolidation`,
#'   `source_kind`, `source_url`, `source_sha256`, `source_container_sha256`,
#'   `source_archive_members`, `source_was_assembled`, and `is_preferred`.
#'   Dates use class `Date`.
#'   Budget and MYEFO forecasts for the same period remain separate profiles.
#' @details Values reproduce the cells printed in each named publication
#'   vintage. This low-level reader does not chain them into an actual history;
#'   use [read_au_budget_myefo_actual_history()] for that package-owned path.
#'   Definitions,
#'   accounting bases, units, table structures and horizons can change, so use
#'   the provenance columns and complete-table reader for material comparisons.
#'   The function is local, may use an in-process cache, and has no network or
#'   persistent filesystem side effect.
#' @seealso [list_au_budget_myefo_series()],
#'   [read_au_budget_myefo_actual_history()],
#'   [read_au_budget_myefo_table()]
#' @family budget-myefo-table-history
#' @examples
#' ucb <- read_au_budget_myefo_series(
#'   "underlying-cash-balance",
#'   publication = "budget",
#'   unit = "AUD billion"
#' )
#' ucb[ucb$release_id == "treasury-budget-2026-27-final",
#'     c("fiscal_year", "value", "profile_type")]
#' stopifnot(length(unique(ucb$series_id)) == 1L)
#' @export
read_au_budget_myefo_series <- function(
    series,
    publication = NULL,
    release = NULL,
    unit = NULL,
    all_sources = FALSE) {
  store <- .rf_budget_myefo_history_store()
  series <- .rf_history_known_value(
    series, "series", store$series$series_id, "series_not_found"
  )
  publication <- .rf_history_publication(publication)
  release <- .rf_history_known_value(
    release, "release", store$sources$release_id, "release_not_found"
  )
  if (!is.null(unit)) {
    .rf_assert_scalar_string(unit, "unit")
    known_units <- unique(store$observations$unit[
      store$observations$series_id == series &
        store$observations$series_eligible
    ])
    unit <- .rf_history_known_value(unit, "unit", known_units,
                                    "invalid_argument")
  }
  if (!is.logical(all_sources) || length(all_sources) != 1L ||
      is.na(all_sources)) {
    .rf_abort(
      "`all_sources` must be one non-missing logical value.",
      "invalid_argument",
      argument = "all_sources"
    )
  }
  out <- store$observations[
    store$observations$series_eligible &
      store$observations$series_id == series,
    ,
    drop = FALSE
  ]
  if (!is.null(publication)) out <- out[out$publication == publication, ]
  if (!is.null(release)) out <- out[out$release_id == release, ]
  if (!is.null(unit)) out <- out[out$unit == unit, ]
  if (!all_sources) out <- out[out$is_preferred, ]
  if (!nrow(out)) {
    .rf_abort(
      "No observation matches the selected series filters.",
      "series_not_found",
      series = series,
      publication = publication,
      release = release,
      unit = unit
    )
  }
  out <- .rf_history_add_source_container(out, store)
  out <- out[c(
    "release_id", "publication", "budget_vintage", "release_date",
    "family_id", "table_instance_id", "source_table", "source_page",
    "series_id", "series", "row_label", "row_path", "fiscal_year",
    "calendar_year", "period_end", "actual_forecast", "profile_type",
    "value", "unit", "unit_multiplier", "accounting_basis",
    "consolidation", "source_kind", "source_url", "source_sha256",
    "source_container_sha256", "source_archive_members",
    "source_was_assembled", "is_preferred"
  )]
  out <- out[order(
    out$publication, out$release_date, out$period_end, out$profile_type,
    out$family_id, out$table_instance_id
  ), , drop = FALSE]
  row.names(out) <- NULL
  tibble::as_tibble(out)
}
