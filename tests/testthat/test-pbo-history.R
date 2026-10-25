test_that("PBO historical workbook exposes all tables and series", {
  readfiscalau:::.rf_clear_internal_file_cache()
  on.exit(readfiscalau:::.rf_clear_internal_file_cache(), add = TRUE)
  tables <- list_au_pbo_historical_tables()
  series <- list_au_pbo_historical_series()
  cached_tables <- list_au_pbo_historical_tables()
  cached_series <- list_au_pbo_historical_series()

  expect_s3_class(tables, "tbl_df")
  expect_s3_class(series, "tbl_df")
  expect_equal(nrow(tables), 11L)
  expect_equal(nrow(series), 616L)
  expect_setequal(tables$table_number, seq_len(11L))
  expect_equal(sum(tables$series_count), nrow(series))
  expect_equal(sum(tables$observation_count), 20552L)
  expect_true(all(tables$end_date == as.Date("2025-06-30")))
  expect_true(all(series$actual_forecast == "actual"))
  expect_true(all(series$end_date <= as.Date("2025-06-30")))
  expect_identical(cached_tables, tables)
  expect_identical(cached_series, series)
  expect_identical(
    list_au_pbo_historical_series(6),
    series[series$table_id == "pbo_historical_table_06", , drop = FALSE]
  )
  cache_keys <- ls(
    envir = readfiscalau:::.rf_internal_file_cache,
    all.names = TRUE
  )
  expect_true(any(grepl("pbo_historical_table_coverage", cache_keys)))
  expect_true(any(grepl("pbo_historical_series_coverage", cache_keys)))
  expect_setequal(
    unique(series$unit),
    c(
      "AUD million", "% of GDP", "Per cent change",
      "Average staffing level"
    )
  )
})

test_that("PBO historical tables preserve representative published values", {
  release <- list_au_fiscal_releases(source = "pbo")
  table_one <- readfiscalau:::.rf_parse_pbo_historical_snapshot(
    release,
    "Table 1"
  )
  receipts <- table_one[
    table_one$series == "Receipts" &
      table_one$unit == "AUD million" &
      table_one$fiscal_year == "1970-71",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(receipts), 1L)
  expect_equal(receipts$value, 8290)
  expect_equal(unique(table_one$table_label), "Cash flow aggregates")
  expect_true(all(table_one$actual_forecast == "actual"))
  expect_true(all(table_one$period_end <= as.Date("2025-06-30")))

  table_six <- readfiscalau:::.rf_parse_pbo_historical_snapshot(
    release,
    6
  )
  expect_equal(length(unique(table_six$series_id)), 42L)
  expect_true(all(table_six$unit == "AUD million"))
})

test_that("PBO historical source identity and table selection fail closed", {
  release <- list_au_fiscal_releases(source = "pbo")
  tampered <- release
  tampered$expected_sha256 <- strrep("0", 64L)

  expect_equal(
    unique(readfiscalau:::.rf_parse_pbo_historical_snapshot(
      release,
      "pbo_historical_table_10"
    )$table_number),
    10L
  )
  expect_error(
    readfiscalau:::.rf_parse_pbo_historical_snapshot(
      tampered,
      "Table 1"
    ),
    class = "readfiscalau_source_hash"
  )
  expect_error(
    readfiscalau:::.rf_parse_pbo_historical_snapshot(
      release,
      "Table 12"
    ),
    class = "readfiscalau_table_required"
  )
  expect_error(
    readfiscalau:::.rf_parse_pbo_historical_snapshot(release),
    class = "readfiscalau_table_required"
  )
})
