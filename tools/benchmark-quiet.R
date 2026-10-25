#!/usr/bin/env Rscript

if (!identical(Sys.getenv("READFISCALAU_RUN_BENCHMARKS"), "true") ||
    !identical(Sys.getenv("READFISCALAU_QUIET_SLOT_CONFIRMED"), "true")) {
  stop(
    paste(
      "Quiet-slot benchmarks are gated.",
      "Set READFISCALAU_RUN_BENCHMARKS=true and",
      "READFISCALAU_QUIET_SLOT_CONFIRMED=true only on an idle machine."
    ),
    call. = FALSE
  )
}
if (!requireNamespace("readfiscalau", quietly = TRUE) ||
    !requireNamespace("ps", quietly = TRUE)) {
  stop("Install readfiscalau and ps before running benchmarks.",
       call. = FALSE)
}

repetitions <- readfiscalau:::.rf_validate_benchmark_repetitions(
  Sys.getenv("READFISCALAU_BENCHMARK_REPS", unset = "3")
)
budget_path <- Sys.getenv("READFISCALAU_BUDGET_ZIP", unset = "")
finance_path <- Sys.getenv("READFISCALAU_FINANCE_XLSX", unset = "")
if (!file.exists(budget_path) || !file.exists(finance_path)) {
  stop(
    paste(
      "Set READFISCALAU_BUDGET_ZIP and READFISCALAU_FINANCE_XLSX",
      "to the pinned official source files."
    ),
    call. = FALSE
  )
}
output <- Sys.getenv(
  "READFISCALAU_BENCHMARK_OUTPUT",
  unset = "readfiscalau-benchmark.csv"
)
summary_output <- Sys.getenv(
  "READFISCALAU_BENCHMARK_SUMMARY_OUTPUT",
  unset = sub("\\.csv$", "-summary.csv", output, ignore.case = TRUE)
)
if (identical(summary_output, output)) {
  summary_output <- paste0(output, "-summary.csv")
}
output_targets <- readfiscalau:::.rf_validate_benchmark_output_targets(
  output,
  summary_output,
  c(budget_path, finance_path)
)
output <- output_targets$raw
summary_output <- output_targets$summary

read_raw <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection))
  readBin(connection, what = "raw", n = file.info(path)$size)
}
budget_raw <- read_raw(budget_path)
finance_raw <- read_raw(finance_path)
case_facts <- data.frame(
  case = c(
    "budget_2026_table_1",
    "finance_2026_may",
    "finance_2008_snapshot"
  ),
  rows = c(725L, 143L, 121L),
  raw_sha256 = c(
    "2069eb7986f890423323732f12be0ef172ece996bffbd1bbaf0e814d324a1681",
    "8b74f69be62ff34a04a18ac4455cc0c706034368d22dbcf3126200d3a0c6ca96",
    "8b74f69be62ff34a04a18ac4455cc0c706034368d22dbcf3126200d3a0c6ca96"
  ),
  stringsAsFactors = FALSE
)
actual_hashes <- c(
  budget_2026_table_1 = readfiscalau:::.rf_sha256(budget_raw),
  finance_2026_may = readfiscalau:::.rf_sha256(finance_raw),
  finance_2008_snapshot = readfiscalau:::.rf_sha256(finance_raw)
)
if (!identical(unname(actual_hashes), case_facts$raw_sha256)) {
  stop("Official benchmark inputs do not match the pinned SHA-256 values.",
       call. = FALSE)
}
benchmark_cache_dirs <- character()
on.exit(unlink(benchmark_cache_dirs, recursive = TRUE), add = TRUE)

rss_mb <- function() {
  as.numeric(ps::ps_memory_info(ps::ps_handle())[["rss"]]) / 1024^2
}
measure <- function(
    case,
    mode,
    repetition,
    cache_state,
    process_state,
    expression) {
  gc()
  rss_before <- rss_mb()
  value <- NULL
  elapsed <- system.time({
    value <- force(expression)
  })[["elapsed"]]
  rss_after <- rss_mb()
  provenance <- attr(value, "readfiscalau_provenance")
  measured_sha256 <- provenance$raw_content_sha256
  if (is.null(measured_sha256) || length(measured_sha256) != 1L) {
    measured_sha256 <- NA_character_
  }
  result <- data.frame(
    case = case,
    mode = mode,
    repetition = repetition,
    elapsed_seconds = unname(elapsed),
    rss_before_mb = rss_before,
    rss_after_mb = rss_after,
    rss_delta_mb = rss_after - rss_before,
    rows = nrow(value),
    raw_sha256 = measured_sha256,
    cache_state = cache_state,
    process_state = process_state,
    stringsAsFactors = FALSE
  )
  value <- NULL
  result
}

read_once <- function(case, cache_dir) {
  switch(
    case,
    budget_2026_table_1 = readfiscalau::read_au_budget(
      "treasury-budget-2026-27-final",
      raw = budget_raw,
      table_id = "Table 1",
      cache_dir = cache_dir
    ),
    finance_2026_may = readfiscalau::read_au_monthly_financials(
      "finance-mfs-2026-05-actual-aggregates",
      raw = finance_raw,
      cache_dir = cache_dir
    ),
    finance_2008_snapshot = readfiscalau::read_au_monthly_financials(
      "finance-mfs-history-snapshot-2026-06-may-2008",
      raw = finance_raw,
      cache_dir = cache_dir
    )
  )
}

cases <- case_facts$case
process_state <- paste(
  "single_r_process_package_loaded",
  "os_file_cache_uncontrolled",
  sep = ";"
)
rows <- list()
for (case in cases) {
  for (mode in c("cold", "warm")) {
    for (repetition in seq_len(repetitions)) {
      cache_dir <- tempfile("readfiscalau-benchmark-cache-")
      dir.create(cache_dir)
      benchmark_cache_dirs <- c(benchmark_cache_dirs, cache_dir)
      if (identical(mode, "warm")) {
        invisible(read_once(case, cache_dir))
      }
      cache_state <- if (identical(mode, "cold")) {
        "raw_input_in_memory;disk_snapshot_absent;no_parsed_cache"
      } else {
        "raw_input_in_memory;disk_snapshot_preseeded;no_parsed_cache"
      }
      rows[[length(rows) + 1L]] <- measure(
        case,
        mode,
        repetition,
        cache_state,
        process_state,
        read_once(case, cache_dir)
      )
    }
  }
}
results <- do.call(rbind, rows)
readfiscalau:::.rf_validate_benchmark_evidence(
  results,
  expected_facts = case_facts,
  minimum_repetitions = repetitions
)
summary <- readfiscalau:::.rf_summarise_benchmark_evidence(results)
utils::write.csv(results, output, row.names = FALSE)
utils::write.csv(summary, summary_output, row.names = FALSE)
print(summary, row.names = FALSE)
message("Raw benchmark measurements written to ", normalizePath(output))
message("Benchmark summary written to ", normalizePath(summary_output))
