test_that("committed chart evidence matches parser fixtures and release pins", {
  evidence <- utils::read.csv(
    system.file(
      "evidence",
      "value-crosscheck.csv",
      package = "readfiscalau"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_setequal(
    evidence$release_id,
    c(
      "treasury-budget-2026-27-final",
      "finance-mfs-2026-05-actual-aggregates"
    )
  )

  budget <- read_au_budget(
    fixture_release(),
    raw = fixture_raw("budget-current.csv"),
    cache_dir = local_tempdir()
  )
  budget_expected <- evidence[evidence$chart == "budget_receipts", ]
  budget_actual <- budget[
    budget$series == "Total receipts" &
      budget$fiscal_year %in% budget_expected$period,
  ]
  budget_actual <- budget_actual[
    match(budget_expected$period, budget_actual$fiscal_year),
  ]
  expect_equal(budget_actual$value, budget_expected$value)
  expect_equal(
    budget_actual$actual_forecast,
    budget_expected$actual_forecast
  )
  expect_equal(budget_actual$unit, budget_expected$unit)

  finance_release <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "finance-evidence",
    release_date = "2026-06-26",
    period_end = "2026-05-31",
    fiscal_year = "2025-26",
    media_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    resource_url = "https://example.gov.au/finance-current.xlsx",
    schema_family = "finance_mfs_post_2008_v2",
    source_id = "department_finance"
  )
  finance_release$actual_forecast <- "actual"
  finance <- read_au_monthly_financials(
    finance_release,
    raw = fixture_raw("finance-current.xlsx"),
    cache_dir = local_tempdir()
  )
  finance_expected <- evidence[evidence$chart == "finance_may", ]
  finance_actual <- finance[
    finance$period_end == as.Date("2026-05-31"),
  ]
  finance_actual <- finance_actual[
    match(finance_expected$series, finance_actual$series),
  ]
  expect_equal(finance_actual$value, finance_expected$value)
  expect_equal(
    finance_actual$actual_forecast,
    finance_expected$actual_forecast
  )
  expect_equal(finance_actual$unit, finance_expected$unit)

  manifest <- list_au_fiscal_releases()
  pinned <- manifest[
    match(unique(evidence$release_id), manifest$release_id),
  ]
  for (release_id in unique(evidence$release_id)) {
    expected <- evidence[evidence$release_id == release_id, ]
    release <- pinned[pinned$release_id == release_id, ]
    expect_true(all(expected$resource_url == release$resource_url))
    expect_true(all(expected$raw_sha256 == release$expected_sha256))
  }
})

test_that("benchmark evidence requires three cold and warm repetitions per case", {
  facts <- data.frame(
    case = c(
      "budget_2026_table_1",
      "finance_2026_may",
      "finance_2008_snapshot"
    ),
    rows = c(725L, 13L, 11L),
    raw_sha256 = c(strrep("a", 64L), strrep("b", 64L), strrep("b", 64L)),
    stringsAsFactors = FALSE
  )
  evidence <- expand.grid(
    case = facts$case,
    mode = c("cold", "warm"),
    repetition = seq_len(3L),
    stringsAsFactors = FALSE
  )
  evidence$elapsed_seconds <- seq_len(nrow(evidence)) / 100
  evidence$rss_before_mb <- 100
  evidence$rss_after_mb <- 101
  evidence$rss_delta_mb <- 1
  evidence$rows <- facts$rows[match(evidence$case, facts$case)]
  evidence$raw_sha256 <- facts$raw_sha256[
    match(evidence$case, facts$case)
  ]
  evidence$cache_state <- ifelse(
    evidence$mode == "cold",
    "raw_input_in_memory;disk_snapshot_absent;no_parsed_cache",
    "raw_input_in_memory;disk_snapshot_preseeded;no_parsed_cache"
  )
  evidence$process_state <-
    "single_r_process_package_loaded;os_file_cache_uncontrolled"

  expect_invisible(readfiscalau:::.rf_validate_benchmark_evidence(
    evidence,
    expected_facts = facts
  ))
  summary <- readfiscalau:::.rf_summarise_benchmark_evidence(evidence)
  expect_equal(nrow(summary), nrow(facts) * 2L)
  expect_true(all(summary$repetitions == 3L))
  expect_true(all(c(
    "latency_min_seconds",
    "latency_median_seconds",
    "latency_max_seconds",
    "rss_delta_min_mb",
    "rss_delta_median_mb",
    "rss_delta_max_mb",
    "observed_peak_rss_mb"
  ) %in% names(summary)))

  missing_warm <- evidence[
    !(evidence$case == facts$case[[1L]] &
        evidence$mode == "warm" &
        evidence$repetition == 3L),
  ]
  expect_error(
    readfiscalau:::.rf_validate_benchmark_evidence(
      missing_warm,
      expected_facts = facts
    ),
    "at least three repetitions per mode"
  )

  duplicate <- rbind(evidence, evidence[1L, , drop = FALSE])
  expect_error(
    readfiscalau:::.rf_validate_benchmark_evidence(
      duplicate,
      expected_facts = facts
    ),
    "duplicate case/mode/repetition"
  )

  excessive_id <- evidence
  excessive_id$repetition[[1L]] <- 21L
  expect_error(
    readfiscalau:::.rf_validate_benchmark_evidence(
      excessive_id,
      expected_facts = facts
    ),
    "integers from 1 to 20"
  )

  excessive_groups <- expand.grid(
    case = facts$case,
    mode = c("cold", "warm"),
    repetition = seq_len(21L),
    stringsAsFactors = FALSE
  )
  excessive_groups <- merge(
    excessive_groups,
    evidence[
      !duplicated(evidence[c("case", "mode")]),
      setdiff(names(evidence), "repetition"),
      drop = FALSE
    ],
    by = c("case", "mode"),
    sort = FALSE
  )
  expect_error(
    readfiscalau:::.rf_validate_benchmark_evidence(
      excessive_groups,
      expected_facts = facts
    ),
    "at most 20 repetitions per mode"
  )

  row_drift <- evidence
  row_drift$rows[[1L]] <- row_drift$rows[[1L]] + 1L
  expect_error(
    readfiscalau:::.rf_validate_benchmark_evidence(
      row_drift,
      expected_facts = facts
    ),
    "expected row count"
  )

  hash_drift <- evidence
  hash_drift$raw_sha256[[1L]] <- strrep("f", 64L)
  expect_error(
    readfiscalau:::.rf_validate_benchmark_evidence(
      hash_drift,
      expected_facts = facts
    ),
    "expected raw SHA-256"
  )
})

test_that("benchmark repetitions are capped and output targets are isolated", {
  expect_identical(
    readfiscalau:::.rf_validate_benchmark_repetitions("3"),
    3L
  )
  expect_error(
    readfiscalau:::.rf_validate_benchmark_repetitions("21"),
    "3 to 20"
  )

  root <- tempfile("readfiscalau-output-guard-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  budget <- file.path(root, "official.zip")
  finance <- file.path(root, "official.xlsx")
  writeBin(charToRaw("budget"), budget)
  writeBin(charToRaw("finance"), finance)
  raw <- file.path(root, "raw.csv")
  summary <- file.path(root, "summary.csv")

  targets <- readfiscalau:::.rf_validate_benchmark_output_targets(
    raw,
    summary,
    c(budget, finance)
  )
  expect_identical(
    targets$raw,
    file.path(normalizePath(root, winslash = "/"), "raw.csv")
  )
  expect_identical(
    targets$summary,
    file.path(normalizePath(root, winslash = "/"), "summary.csv")
  )
  expect_error(
    readfiscalau:::.rf_validate_benchmark_output_targets(
      raw,
      raw,
      c(budget, finance)
    ),
    "must not alias"
  )
  expect_error(
    readfiscalau:::.rf_validate_benchmark_output_targets(
      budget,
      summary,
      c(budget, finance)
    ),
    "must not overwrite"
  )
})

test_that("benchmark output rejects symbolic-link targets", {
  root <- tempfile("readfiscalau-output-symlink-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  budget <- file.path(root, "official.zip")
  finance <- file.path(root, "official.xlsx")
  writeBin(charToRaw("budget"), budget)
  writeBin(charToRaw("finance"), finance)
  summary <- file.path(root, "summary.csv")
  symlink <- file.path(root, "raw-link.csv")
  symlink_created <- suppressWarnings(file.symlink(summary, symlink))
  if (!isTRUE(symlink_created)) {
    skip("Symbolic-link creation is unavailable on this platform.")
  }
  expect_error(
    readfiscalau:::.rf_validate_benchmark_output_targets(
      symlink,
      summary,
      c(budget, finance)
    ),
    "must not be a symbolic link"
  )
})

test_that("benchmark output rejects hard links to source inputs", {
  root <- tempfile("readfiscalau-output-input-link-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  budget <- file.path(root, "official.zip")
  finance <- file.path(root, "official.xlsx")
  writeBin(charToRaw("budget"), budget)
  writeBin(charToRaw("finance"), finance)
  summary <- file.path(root, "summary.csv")
  hard_link_to_input <- file.path(root, "raw-input-hard-link.csv")
  hard_link_input_created <- suppressWarnings(
    file.link(budget, hard_link_to_input)
  )
  if (!isTRUE(hard_link_input_created)) {
    skip("Hard-link creation is unavailable on this platform.")
  }
  expect_error(
    readfiscalau:::.rf_validate_benchmark_output_targets(
      hard_link_to_input,
      summary,
      c(budget, finance)
    ),
    "must not already exist"
  )
})

test_that("benchmark output rejects hard links between output targets", {
  root <- tempfile("readfiscalau-output-output-link-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  budget <- file.path(root, "official.zip")
  finance <- file.path(root, "official.xlsx")
  writeBin(charToRaw("budget"), budget)
  writeBin(charToRaw("finance"), finance)
  summary <- file.path(root, "summary-output-hard-link.csv")
  existing_raw <- file.path(root, "existing-raw.csv")
  writeBin(charToRaw("evidence"), existing_raw)
  hard_link_output_created <- suppressWarnings(
    file.link(existing_raw, summary)
  )
  if (!isTRUE(hard_link_output_created)) {
    skip("Hard-link creation is unavailable on this platform.")
  }
  expect_error(
    readfiscalau:::.rf_validate_benchmark_output_targets(
      existing_raw,
      summary,
      c(budget, finance)
    ),
    "must not already exist"
  )
})

test_that("committed performance evidence is complete and reproducible", {
  evidence_dir <- system.file("evidence", package = "readfiscalau")
  raw <- utils::read.csv(
    file.path(evidence_dir, "quiet-slot-performance-raw.csv"),
    stringsAsFactors = FALSE
  )
  summary <- utils::read.csv(
    file.path(evidence_dir, "quiet-slot-performance-summary.csv"),
    stringsAsFactors = FALSE
  )
  readme <- readLines(file.path(evidence_dir, "README.md"), warn = FALSE)

  facts <- data.frame(
    case = c(
      "budget_2026_table_1",
      "finance_2026_may",
      "finance_2008_snapshot"
    ),
    rows = c(725L, 13L, 11L),
    raw_sha256 = c(
      "2069eb7986f890423323732f12be0ef172ece996bffbd1bbaf0e814d324a1681",
      "8b74f69be62ff34a04a18ac4455cc0c706034368d22dbcf3126200d3a0c6ca96",
      "8b74f69be62ff34a04a18ac4455cc0c706034368d22dbcf3126200d3a0c6ca96"
    ),
    stringsAsFactors = FALSE
  )
  expect_invisible(readfiscalau:::.rf_validate_benchmark_evidence(
    raw,
    expected_facts = facts,
    minimum_repetitions = 3L
  ))
  expect_equal(nrow(raw), 18L)
  expect_equal(
    summary,
    readfiscalau:::.rf_summarise_benchmark_evidence(raw),
    tolerance = 0
  )
  expect_lte(max(summary$latency_max_seconds), 1.5)
  expect_lt(max(summary$observed_peak_rss_mb), 1024)
  expect_true(any(grepl(
    "PASS_WITH_MEMORY_WARNING",
    readme,
    fixed = TRUE
  )))
  expect_false(any(grepl("PERFORMANCE_NOT_RERUN", readme, fixed = TRUE)))
})
