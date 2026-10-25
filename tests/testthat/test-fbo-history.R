fbo_history_release <- function() {
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

test_that("FBO historical DOCX parses audited annual aggregate tables", {
  out <- read_au_fbo(
    fbo_history_release(),
    raw = fixture_raw("fbo-history.docx"),
    cache_dir = local_tempdir()
  )

  expect_s3_class(out, "readfiscalau_data")
  expect_setequal(
    out$series_id,
    c(
      "receipts", "payments", "net_future_fund_earnings",
      "underlying_cash_balance", "headline_cash_balance", "net_debt",
      "revenue", "expenses", "net_operating_balance",
      "net_capital_investment", "fiscal_balance", "net_worth"
    )
  )
  expect_true(all(out$fiscal_year == "2024-25"))
  expect_true(all(out$period_end == as.Date("2025-06-30")))
  expect_true(all(out$actual_forecast == "actual"))
  expect_true(all(out$unit == "AUD million"))
  expect_equal(out$value[out$series_id == "receipts"], 125000)
  expect_equal(out$value[out$series_id == "fiscal_balance"], 18000)
  expect_equal(out$value[out$series_id == "net_worth"], -120000)

  provenance <- attr(out, "readfiscalau_provenance")
  expect_equal(provenance$archive_member$name, "word/document.xml")
  expect_match(provenance$archive_member$sha256, "^[a-f0-9]{64}$")
})

test_that("FBO DOCX schema is pinned in the built-in manifest", {
  release <- list_au_fiscal_releases(
    release_type = "fbo"
  )
  expect_equal(nrow(release), 1L)
  expect_equal(
    release$release_id,
    "treasury-fbo-2024-25-final-history"
  )
  expect_equal(
    release$expected_sha256,
    "b3768de160883be460a43a69dec1ae810289e7eabe1d090bd79805d5a16dbc32"
  )
  expect_equal(release$schema_family, "treasury_fbo_history_docx_v1")
})
