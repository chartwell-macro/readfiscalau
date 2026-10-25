monthly_history_release <- function() {
  release <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "fixture-monthly-history",
    release_date = "2025-06-27",
    period_end = "2025-05-31",
    fiscal_year = "2024-25",
    media_type = paste0(
      "application/vnd.openxmlformats-officedocument.",
      "spreadsheetml.sheet"
    ),
    resource_url = "https://example.gov.au/finance-history.xlsx",
    schema_family = "finance_mfs_post_2008_v2",
    source_id = "department_finance"
  )
  release$actual_forecast <- "actual"
  release
}

monthly_fbo_history_release <- function() {
  release <- fixture_release(
    release_type = "fbo",
    release_id = "fixture-fbo-history",
    release_date = "2025-09-29",
    period_end = "2025-06-30",
    fiscal_year = "2024-25",
    media_type = paste0(
      "application/vnd.openxmlformats-officedocument.",
      "wordprocessingml.document"
    ),
    resource_url = "https://example.gov.au/fbo-history.docx",
    schema_family = "treasury_fbo_history_docx_v1"
  )
  release$actual_forecast <- "actual"
  release$resource_role <- "fbo_historical_aggregates_docx"
  release
}

monthly_history_fixture <- function(output = "monthly", annual = TRUE) {
  monthly <- monthly_history_release()
  fbo <- monthly_fbo_history_release()
  read_au_monthly_financial_history(
    monthly_release = monthly,
    annual_release = if (annual) fbo else NULL,
    start_fiscal_year = "2024-25",
    output = output,
    manifest = rbind(monthly, fbo),
    monthly_raw = fixture_raw("finance-history.xlsx"),
    annual_raw = if (annual) fixture_raw("fbo-history.docx") else NULL,
    cache_dir = local_tempdir()
  )
}

test_that("monthly history derives flows and completes June from FBO", {
  out <- monthly_history_fixture()
  receipts <- out[out$series_id == "receipts", , drop = FALSE]

  expect_s3_class(out, "readfiscalau_monthly_history")
  expect_equal(nrow(receipts), 12L)
  expect_equal(receipts$value[receipts$period_end < as.Date("2025-06-01")],
               rep(10000, 11L))
  expect_equal(
    receipts$value[receipts$period_end == as.Date("2025-06-30")],
    15000
  )
  expect_equal(
    receipts$derivation[receipts$period_end == as.Date("2024-08-31")],
    "reported_monthly"
  )
  expect_equal(
    receipts$derivation[receipts$period_end == as.Date("2024-09-30")],
    "difference_of_ytd"
  )
  expect_equal(
    receipts$derivation[receipts$period_end == as.Date("2025-06-30")],
    "annual_outcome_less_may_ytd"
  )
  expect_true(receipts$revision_residual_possible[receipts$is_june])
  expect_true(all(receipts$complete_fiscal_year))
  expect_equal(unique(receipts$unit), "AUD million")
})

test_that("YTD output reconstructs early monthly columns and uses annual June", {
  out <- monthly_history_fixture(output = "ytd")
  receipts <- out[out$series_id == "receipts", , drop = FALSE]

  expect_equal(
    receipts$value[receipts$period_end == as.Date("2024-08-31")],
    20000
  )
  expect_equal(
    receipts$derivation[receipts$period_end == as.Date("2024-08-31")],
    "reconstructed_ytd_from_monthly"
  )
  expect_equal(
    receipts$value[receipts$period_end == as.Date("2025-05-31")],
    110000
  )
  expect_equal(
    receipts$value[receipts$period_end == as.Date("2025-06-30")],
    125000
  )
  expect_true(all(receipts$value_type == "cumulative_ytd"))
})

test_that("stocks remain levels and unmatched annual series remain incomplete", {
  out <- monthly_history_fixture()
  assets <- out[out$series_id == "total_assets", , drop = FALSE]
  worth <- out[out$series_id == "net_worth", , drop = FALSE]

  expect_equal(nrow(assets), 11L)
  expect_false(any(assets$is_june))
  expect_equal(assets$value[1:2], c(1000000, 1001000))
  expect_true(all(assets$value_type == "month_end_level"))
  expect_false(any(assets$complete_fiscal_year))

  expect_equal(nrow(worth), 12L)
  expect_equal(worth$value[worth$is_june], -120000)
  expect_equal(
    worth$derivation[worth$is_june],
    "reported_annual_end_of_year_level"
  )
  expect_true(all(worth$complete_fiscal_year))
})

test_that("history without FBO returns only July through May", {
  out <- monthly_history_fixture(annual = FALSE)
  expect_false(any(out$is_june))
  expect_true(all(out$period_end <= as.Date("2025-05-31")))
  expect_false(any(out$complete_fiscal_year))
})

test_that("legacy labels are not conflated across accounting concepts", {
  labels <- c(
    "Revenue(a)", "Income (a)", "Net operating balance",
    "Operating Result (a)(c)", "Net Assets (a)"
  )
  key <- readfiscalau:::.rf_finance_source_key(labels)
  index <- match(
    key,
    readfiscalau:::.rf_finance_series_contract$source_key
  )
  ids <- readfiscalau:::.rf_finance_series_contract$series_id[index]
  legacy <- readfiscalau:::.rf_finance_apply_legacy_framework(data.frame(
    fiscal_year = rep("2006-07", length(ids)),
    series_id = ids,
    series = labels,
    stringsAsFactors = FALSE
  ))
  expect_equal(
    legacy$series_id,
    c(
      "revenue_aas", "income_aas", "net_operating_balance",
      "operating_result_aas", "net_assets_aas"
    )
  )
  expect_equal(
    legacy$accounting_framework,
    c("AAS", "AAS", "GFS", "AAS", "AAS")
  )
})

test_that("post-2008 historical million sheets retain their source unit", {
  release <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "finance-post-2008-million",
    release_date = "2009-06-26",
    period_end = "2009-05-31",
    fiscal_year = "2008-09",
    media_type = paste0(
      "application/vnd.openxmlformats-officedocument.",
      "spreadsheetml.sheet"
    ),
    resource_url = "https://example.gov.au/finance-post-2008-millions.xlsx",
    schema_family = "finance_mfs_post_2008_v2",
    source_id = "department_finance"
  )
  release$actual_forecast <- "actual"
  out <- read_au_monthly_financials(
    release,
    raw = fixture_raw("finance-post-2008-millions.xlsx"),
    cache_dir = local_tempdir()
  )
  expect_equal(out$value, 280000)
  expect_equal(out$unit, "AUD million")
})

test_that("monthly history rejects non-actual or non-audited joins", {
  monthly <- monthly_history_release()
  monthly$actual_forecast <- "mixed"
  expect_error(
    read_au_monthly_financial_history(
      monthly_release = monthly,
      annual_release = NULL,
      start_fiscal_year = "2024-25",
      manifest = monthly,
      monthly_raw = fixture_raw("finance-history.xlsx"),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_history_contract"
  )

  monthly <- monthly_history_release()
  annual <- monthly_fbo_history_release()
  annual$schema_family <- "treasury_chart_csv_v1"
  annual$media_type <- "text/csv"
  expect_error(
    read_au_monthly_financial_history(
      monthly_release = monthly,
      annual_release = annual,
      start_fiscal_year = "2024-25",
      manifest = rbind(monthly, annual),
      monthly_raw = fixture_raw("finance-history.xlsx"),
      annual_raw = fixture_raw("budget-current.csv"),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_history_contract"
  )
})
