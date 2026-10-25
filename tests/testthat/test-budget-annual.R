test_that("Budget current and historical chart schemas are normalised", {
  current <- read_au_budget(
    fixture_release(),
    raw = fixture_raw("budget-current.csv"),
    cache_dir = local_tempdir(),
    table_id = "cash_receipts"
  )
  expect_s3_class(current, "readfiscalau_data")
  expect_equal(nrow(current), 12L)
  expect_setequal(current$actual_forecast, c("actual", "forecast"))
  expect_true(all(current$unit == "AUD million"))
  expect_true(all(c(
    "vintage_date", "retrieved_at", "raw_content_sha256",
    "observation_status", "frequency", "media_type"
  ) %in% names(current)))
  expect_equal(unique(current$vintage_date), as.Date("2026-05-12"))
  expect_true(all(grepl("^[a-f0-9]{64}$", current$raw_content_sha256)))
  expect_equal(
    current$value[
      current$series == "Total receipts" &
        current$fiscal_year == "2026-27"
    ],
    798098
  )
  expect_equal(
    attr(current, "readfiscalau_provenance")$raw_content_sha256,
    digest::digest(
      fixture_raw("budget-current.csv"),
      algo = "sha256", serialize = FALSE
    )
  )

  historical_release <- fixture_release(
    release_id = "fixture-budget-historical",
    release_date = "2008-05-13",
    period_end = "2009-06-30",
    fiscal_year = "2008-09",
    schema_family = "treasury_chart_csv_v1"
  )
  historical <- read_au_budget(
    historical_release,
    raw = fixture_raw("budget-historical.csv"),
    cache_dir = local_tempdir()
  )
  expect_equal(nrow(historical), 6L)
  expect_equal(sum(historical$actual_forecast == "forecast"), 4L)
})

test_that("Budget, MYEFO, FBO and CFS readers enforce release type", {
  raw <- fixture_raw("budget-historical.csv")
  readers <- list(
    budget = read_au_budget,
    myefo = read_au_myefo,
    fbo = read_au_fbo,
    cfs = read_au_cfs
  )
  for (type in names(readers)) {
    release <- fixture_release(
      release_type = type,
      release_id = paste0("fixture-", type, "-2008"),
      release_date = "2008-09-30",
      period_end = "2008-06-30",
      fiscal_year = "2007-08",
      source_id = if (type == "cfs") {
        "department_finance"
      } else {
        "australian_treasury"
      }
    )
    value <- readers[[type]](
      release, raw = raw, cache_dir = local_tempdir()
    )
    expect_true(all(value$release_type == type))
    expect_true(all(value$release_id == release$release_id))
  }

  expect_error(
    read_au_myefo(
      fixture_release("budget"),
      raw = raw,
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_release_type"
  )
})

test_that("chart schema drift and ambiguous ZIP tables fail closed", {
  expect_error(
    read_au_budget(
      fixture_release(),
      raw = fixture_raw("budget-invalid.csv"),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_schema"
  )

  zip_raw <- fixture_raw("budget-chart-archive.zip")
  release <- fixture_release(
    media_type = "application/zip",
    resource_url = "https://example.gov.au/charts.zip"
  )
  expect_error(
    read_au_budget(release, raw = zip_raw, cache_dir = local_tempdir()),
    class = "readfiscalau_table_required"
  )
  selected <- read_au_budget(
    release, raw = zip_raw, cache_dir = local_tempdir(),
    table_id = "Table 1"
  )
  expect_equal(nrow(selected), 12L)
  expect_equal(
    attr(selected, "readfiscalau_provenance")$archive_member$name,
    "Table 1 - Australian Government (cash) receipts.csv"
  )
})

test_that("duplicate facts and actual-forecast conflicts fail closed", {
  duplicate <- charToRaw(paste0(
    ",2024-25 ($m)\n",
    "Same label,1\n",
    "Same label,2\n"
  ))
  expect_error(
    read_au_budget(
      fixture_release(),
      raw = duplicate,
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_duplicate_observation"
  )

  actual_only <- fixture_release()
  actual_only$actual_forecast <- "actual"
  forecast <- charToRaw(",2025-26 (est) ($m)\nReceipts,1\n")
  expect_error(
    read_au_budget(
      actual_only,
      raw = forecast,
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_schema"
  )
})

test_that("published thousands separators and accounting negatives parse", {
  published <- charToRaw(paste0(
    ",2024-25 ($m)\n",
    "Receipts,\"693,756\"\n",
    "Balance,\"(10,947)\"\n",
    "Total receipts,\"735,353\"\n"
  ))
  out <- read_au_budget(
    fixture_release(),
    raw = published,
    cache_dir = local_tempdir()
  )
  expect_equal(out$value, c(693756, -10947, 735353))

  malformed <- charToRaw(
    ",2024-25 ($m)\nReceipts,\"12,34\"\n"
  )
  expect_error(
    read_au_budget(
      fixture_release(),
      raw = malformed,
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_schema"
  )
})

test_that("Budget archive table names preserve accounting basis", {
  expect_equal(
    readfiscalau:::.rf_chart_accounting_basis(
      "Table 1 - Australian Government (cash) receipts.csv", "mixed"
    ),
    "cash"
  )
  expect_equal(
    readfiscalau:::.rf_chart_accounting_basis(
      "Table 3 - Australian Government (accrual) revenue.csv", "mixed"
    ),
    "accrual"
  )
  expect_error(
    readfiscalau:::.rf_chart_accounting_basis(
      "cash and accrual.csv", "mixed"
    ),
    class = "readfiscalau_schema"
  )
  expect_true(readfiscalau:::.rf_chart_member_has_gov_scope(
    paste(
      "Table 2 - Major categories of (cash) receipts as a proportion of",
      "gross domestic product.csv"
    )
  ))
  expect_false(readfiscalau:::.rf_chart_member_has_gov_scope(
    "Major categories of cash receipts.csv"
  ))
})

test_that("Budget period, unit, and actual-forecast drift fail closed", {
  duplicated_period <- charToRaw(paste0(
    ",2025-26 ($m),2025-26 (est) ($m)\n",
    "Receipts,1,2\n"
  ))
  expect_error(
    read_au_budget(
      fixture_release(),
      raw = duplicated_period,
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_schema"
  )

  mixed_units <- charToRaw(paste0(
    ",2024-25 ($m),2025-26 (est) ($b)\n",
    "Receipts,1,2\n"
  ))
  expect_error(
    read_au_budget(
      fixture_release(),
      raw = mixed_units,
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_schema"
  )

  future_actual <- charToRaw(
    ",2026-27 ($m)\nReceipts,1\n"
  )
  expect_error(
    read_au_budget(
      fixture_release(),
      raw = future_actual,
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_schema"
  )
})

test_that("committed official-shaped ZIP fixture covers archive selection", {
  selected <- read_au_budget(
    fixture_release(
      media_type = "application/zip",
      resource_url = "https://example.gov.au/chart-data-final.zip"
    ),
    raw = fixture_raw("budget-chart-archive.zip"),
    cache_dir = local_tempdir(),
    table_id = "Table 1"
  )
  provenance <- attr(selected, "readfiscalau_provenance")

  expect_equal(nrow(selected), 12L)
  expect_equal(
    provenance$archive_member$name,
    "Table 1 - Australian Government (cash) receipts.csv"
  )
  expect_equal(unique(selected$accounting_basis), "cash")
  expect_equal(unique(selected$consolidation),
               "general_government_sector")
})
