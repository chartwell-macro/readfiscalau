test_that("current and pre-2008 Finance workbook families parse separately", {
  current_release <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "finance-current",
    release_date = "2026-06-26",
    period_end = "2026-05-31",
    fiscal_year = "2025-26",
    media_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    resource_url = "https://example.gov.au/finance-current.xlsx",
    schema_family = "finance_mfs_post_2008_v2",
    source_id = "department_finance"
  )
  current <- read_au_monthly_financials(
    current_release,
    raw = fixture_raw("finance-current.xlsx"),
    cache_dir = local_tempdir()
  )
  expect_equal(nrow(current), 6L)
  expect_true(all(current$profile_type == "actual"))
  expect_true(all(current$actual_forecast == "actual"))
  expect_equal(
    sort(unique(current$period_end)),
    as.Date(c("2026-04-30", "2026-05-31"))
  )
  expect_true(all(current$unit == "AUD billion"))
  expect_equal(
    current$value[
      current$series == "Receipts(b)" &
        current$period_end == as.Date("2026-05-31")
    ],
    693.756160,
    tolerance = 1e-9
  )
  expect_equal(
    current$value[
      current$series == "Underlying cash balance" &
        current$period_end == as.Date("2026-05-31")
    ],
    -10.947312,
    tolerance = 1e-9
  )

  historical_release <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "finance-historical",
    release_date = "2008-06-27",
    period_end = "2008-05-31",
    fiscal_year = "2007-08",
    media_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    resource_url = "https://example.gov.au/finance-historical.xlsx",
    schema_family = "finance_mfs_pre_2008_v1",
    source_id = "department_finance"
  )
  historical <- read_au_monthly_financials(
    historical_release,
    raw = fixture_raw("finance-historical.xlsx"),
    cache_dir = local_tempdir()
  )
  expect_equal(nrow(historical), 6L)
  expect_true(all(historical$profile_type == "actual"))
  expect_true(all(historical$unit == "AUD million"))
  expect_equal(
    historical$value[
      historical$series == "Revenue" &
        historical$period_end == as.Date("2008-05-31")
    ],
    283403
  )
  expect_false(identical(
    unique(current$schema_family),
    unique(historical$schema_family)
  ))
})

test_that("Finance workbook schema drift fails closed", {
  release <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "finance-mismatched-schema",
    release_date = "2026-06-26",
    period_end = "2026-05-31",
    fiscal_year = "2025-26",
    media_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    resource_url = "https://example.gov.au/finance.xlsx",
    schema_family = "finance_mfs_post_2008_v2",
    source_id = "department_finance"
  )
  expect_error(
    read_au_monthly_financials(
      release,
      raw = fixture_raw("finance-historical.xlsx"),
      cache_dir = local_tempdir(),
      sheet = "2007-08"
    ),
    class = "readfiscalau_schema"
  )
})

test_that("June is annual FBO or CFS and never manufactured as monthly", {
  june_monthly <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "invalid-june-monthly",
    release_date = "2026-08-01",
    period_end = "2026-06-30",
    fiscal_year = "2025-26",
    media_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    resource_url = "https://example.gov.au/finance-june.xlsx",
    source_id = "department_finance"
  )
  expect_error(
    read_au_monthly_financials(
      june_monthly,
      raw = fixture_raw("finance-june.xlsx"),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_june_is_annual"
  )

  for (annual_type in c("fbo", "cfs")) {
    annual <- fixture_release(
      release_type = annual_type,
      release_id = paste0("valid-june-", annual_type),
      release_date = "2026-09-30",
      period_end = "2026-06-30",
      fiscal_year = "2025-26",
      source_id = if (annual_type == "fbo") {
        "australian_treasury"
      } else {
        "department_finance"
      }
    )
    parsed <- if (annual_type == "fbo") {
      read_au_fbo(
        annual, raw = fixture_raw("budget-current.csv"),
        cache_dir = local_tempdir()
      )
    } else {
      read_au_cfs(
        annual, raw = fixture_raw("budget-current.csv"),
        cache_dir = local_tempdir()
      )
    }
    expect_true(all(parsed$release_type == annual_type))
  }
})

test_that("Finance unit and cumulative-scope drift fail closed", {
  release <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "finance-current-contract",
    release_date = "2026-06-26",
    period_end = "2026-05-31",
    fiscal_year = "2025-26",
    media_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    resource_url = "https://example.gov.au/finance-current.xlsx",
    schema_family = "finance_mfs_post_2008_v2",
    source_id = "department_finance"
  )
  release$actual_forecast <- "actual"

  expect_error(
    read_au_monthly_financials(
      release,
      raw = fixture_raw("finance-unit-drift.xlsx"),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_schema"
  )
  expect_error(
    read_au_monthly_financials(
      release,
      raw = fixture_raw("finance-non-ytd.xlsx"),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_schema"
  )
})
