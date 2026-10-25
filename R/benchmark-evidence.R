.rf_validate_benchmark_evidence <- function(
    data,
    expected_facts,
    minimum_repetitions = 3L) {
  required <- c(
    "case", "mode", "repetition", "elapsed_seconds",
    "rss_before_mb", "rss_after_mb", "rss_delta_mb",
    "rows", "raw_sha256", "cache_state", "process_state"
  )
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      sprintf(
        "Benchmark evidence is missing column(s): %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  expected_columns <- c("case", "rows", "raw_sha256")
  if (!is.data.frame(expected_facts) ||
      !identical(names(expected_facts), expected_columns) ||
      !nrow(expected_facts) ||
      anyNA(expected_facts) ||
      any(!nzchar(expected_facts$case)) ||
      anyDuplicated(expected_facts$case) ||
      any(expected_facts$rows < 1L) ||
      any(expected_facts$rows != as.integer(expected_facts$rows)) ||
      any(!grepl("^[a-f0-9]{64}$", expected_facts$raw_sha256))) {
    stop(
      paste0(
        "`expected_facts` must contain unique case IDs, positive row ",
        "counts, and lowercase SHA-256 values."
      ),
      call. = FALSE
    )
  }
  expected_cases <- expected_facts$case
  minimum_repetitions <- suppressWarnings(as.integer(minimum_repetitions))
  if (length(minimum_repetitions) != 1L ||
      is.na(minimum_repetitions) || minimum_repetitions < 3L ||
      minimum_repetitions > 20L) {
    stop("`minimum_repetitions` must be from 3 to 20.", call. = FALSE)
  }
  if (!identical(sort(unique(data$case)), sort(expected_cases))) {
    stop("Benchmark evidence cases do not match the expected cases.",
         call. = FALSE)
  }
  if (!identical(sort(unique(data$mode)), c("cold", "warm"))) {
    stop("Benchmark evidence must contain exactly cold and warm modes.",
         call. = FALSE)
  }
  key <- paste(data$case, data$mode, data$repetition, sep = "\r")
  if (anyDuplicated(key)) {
    stop("Benchmark evidence contains duplicate case/mode/repetition rows.",
         call. = FALSE)
  }
  expected_groups <- as.vector(outer(
    expected_cases,
    c("cold", "warm"),
    paste,
    sep = "\r"
  ))
  group <- paste(data$case, data$mode, sep = "\r")
  counts <- table(factor(group, levels = expected_groups))
  if (any(counts < minimum_repetitions)) {
    stop(
      "Every benchmark case must have at least three repetitions per mode.",
      call. = FALSE
    )
  }
  if (any(counts > 20L)) {
    stop(
      "Every benchmark case must have at most 20 repetitions per mode.",
      call. = FALSE
    )
  }
  if (!is.numeric(data$repetition) || anyNA(data$repetition) ||
      any(!is.finite(data$repetition)) ||
      any(data$repetition != as.integer(data$repetition)) ||
      any(data$repetition < 1L) || any(data$repetition > 20L)) {
    stop(
      "Benchmark repetition IDs must be integers from 1 to 20.",
      call. = FALSE
    )
  }
  repetitions <- split(data$repetition, group)
  if (any(vapply(repetitions, function(value) {
    !identical(sort(unique(as.integer(value))), seq_len(length(value)))
  }, logical(1)))) {
    stop(
      "Benchmark repetitions must be consecutive within each case and mode.",
      call. = FALSE
    )
  }
  state_is_stable <- function(column) {
    states <- split(data[[column]], group)
    all(vapply(states, function(value) {
      length(unique(value)) == 1L && !is.na(value[[1L]]) &&
        nzchar(value[[1L]])
    }, logical(1)))
  }
  if (!state_is_stable("cache_state") ||
      !state_is_stable("process_state")) {
    stop(
      "Cache and process state must be explicit and stable within each case/mode.",
      call. = FALSE
    )
  }
  numeric_columns <- c(
    "elapsed_seconds", "rss_before_mb", "rss_after_mb", "rss_delta_mb",
    "rows"
  )
  if (any(vapply(data[numeric_columns], function(value) {
    !is.numeric(value) || anyNA(value) || any(!is.finite(value))
  }, logical(1)))) {
    stop("Benchmark measurements must be finite numeric values.",
         call. = FALSE)
  }
  matched <- match(data$case, expected_facts$case)
  if (!identical(
      as.integer(data$rows),
      as.integer(expected_facts$rows[matched]))) {
    stop(
      "Each benchmark case must retain its expected row count across both modes.",
      call. = FALSE
    )
  }
  if (!identical(
      as.character(data$raw_sha256),
      as.character(expected_facts$raw_sha256[matched]))) {
    stop(
      "Each benchmark case must retain its expected raw SHA-256 across both modes.",
      call. = FALSE
    )
  }
  invisible(data)
}

.rf_summarise_benchmark_evidence <- function(data) {
  groups <- split(
    data,
    interaction(data$case, data$mode, drop = TRUE, lex.order = TRUE)
  )
  summary <- lapply(groups, function(group) {
    data.frame(
      case = group$case[[1L]],
      mode = group$mode[[1L]],
      repetitions = nrow(group),
      cache_state = group$cache_state[[1L]],
      process_state = group$process_state[[1L]],
      latency_min_seconds = min(group$elapsed_seconds),
      latency_median_seconds = stats::median(group$elapsed_seconds),
      latency_max_seconds = max(group$elapsed_seconds),
      rss_delta_min_mb = min(group$rss_delta_mb),
      rss_delta_median_mb = stats::median(group$rss_delta_mb),
      rss_delta_max_mb = max(group$rss_delta_mb),
      observed_peak_rss_mb = max(group$rss_before_mb, group$rss_after_mb),
      rows = unique(group$rows)[[1L]],
      raw_sha256 = unique(group$raw_sha256)[[1L]],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, summary)
  rownames(out) <- NULL
  out[order(out$case, out$mode), , drop = FALSE]
}

.rf_validate_benchmark_repetitions <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (length(numeric_value) != 1L || is.na(numeric_value) ||
      !is.finite(numeric_value) || numeric_value != as.integer(numeric_value) ||
      numeric_value < 3L || numeric_value > 20L) {
    stop("Benchmark repetitions must be an integer from 3 to 20.",
         call. = FALSE)
  }
  as.integer(numeric_value)
}

.rf_canonical_output_target <- function(path, label) {
  if (!is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(trimws(path))) {
    stop(sprintf("%s must be one non-empty path.", label), call. = FALSE)
  }
  expanded <- path.expand(path)
  link <- Sys.readlink(expanded)
  if (length(link) == 1L && !is.na(link) && nzchar(link)) {
    stop(sprintf("%s must not be a symbolic link.", label), call. = FALSE)
  }
  if (dir.exists(expanded)) {
    stop(sprintf("%s must identify a file, not a directory.", label),
         call. = FALSE)
  }
  parent <- dirname(expanded)
  if (!dir.exists(parent)) {
    stop(sprintf("%s parent directory does not exist.", label),
         call. = FALSE)
  }
  canonical_parent <- normalizePath(
    parent,
    winslash = "/",
    mustWork = TRUE
  )
  candidate <- file.path(canonical_parent, basename(expanded))
  if (file.exists(expanded)) {
    normalizePath(expanded, winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(candidate, winslash = "/", mustWork = FALSE)
  }
}

.rf_validate_benchmark_output_targets <- function(
    raw_output,
    summary_output,
    input_paths) {
  raw_target <- .rf_canonical_output_target(raw_output, "Raw output")
  summary_target <- .rf_canonical_output_target(
    summary_output,
    "Summary output"
  )
  compare_path <- function(path) {
    if (Sys.info()[["sysname"]] %in% c("Darwin", "Windows")) {
      tolower(path)
    } else {
      path
    }
  }
  if (identical(compare_path(raw_target), compare_path(summary_target))) {
    stop("Raw and summary outputs must not alias the same file.",
         call. = FALSE)
  }
  if (!is.character(input_paths) || !length(input_paths) ||
      anyNA(input_paths) || any(!file.exists(input_paths))) {
    stop("Official benchmark inputs must identify existing files.",
         call. = FALSE)
  }
  input_targets <- normalizePath(
    input_paths,
    winslash = "/",
    mustWork = TRUE
  )
  if (any(compare_path(c(raw_target, summary_target)) %in%
          compare_path(input_targets))) {
    stop("Benchmark outputs must not overwrite an official input file.",
         call. = FALSE)
  }
  if (file.exists(raw_output) || file.exists(summary_output)) {
    stop(
      paste0(
        "Benchmark outputs must not already exist; use fresh private output ",
        "paths."
      ),
      call. = FALSE
    )
  }
  list(raw = raw_target, summary = summary_target)
}
