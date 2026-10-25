test_that("built-in manifest preserves distinct vintages and June semantics", {
  releases <- list_au_fiscal_releases()

  expect_s3_class(releases, "tbl_df")
  expect_setequal(
    releases$release_type,
    c(
      "budget", "myefo", "fbo", "monthly_financial_statement",
      "historical_fiscal_data"
    )
  )
  expect_identical(anyDuplicated(releases$release_id), 0L)
  expected_status <- c(
    "treasury-budget-2026-27-final" = "final",
    "treasury-budget-2025-26-final" = "final",
    "finance-mfs-2026-05-actual-aggregates" = "final",
    "finance-mfs-history-snapshot-2026-06-may-2008" = "snapshot"
  )
  expect_equal(
    releases$release_status[
      match(names(expected_status), releases$release_id)
    ],
    unname(expected_status)
  )
  expect_true(all(grepl("^https://", releases$landing_url)))
  expect_true(all(grepl("^https://", releases$resource_url)))
  expect_true(all(grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
    releases$resource_updated_at
  )))
  monthly <- releases$release_type == "monthly_financial_statement"
  expect_false(any(format(releases$period_end[monthly], "%m") == "06"))
  expect_true(all(nzchar(releases$revision_semantics)))
  expect_true(all(nzchar(releases$attribution)))
  expect_true(all(grepl("^[a-f0-9]{64}$", releases$expected_sha256)))
  treasury <- releases$source_id == "australian_treasury"
  expect_setequal(
    releases$accounting_basis[treasury],
    c("mixed", "cash", "accrual")
  )
  expect_true(all(releases$terms_url[
    releases$source_id == "australian_treasury"
  ] == "https://budget.gov.au/legal-notices.htm#h1"))
  expect_true(any(releases$release_id ==
                    "treasury-myefo-2025-26-final"))
  expect_false(any(releases$release_id %in% c(
    "finance-fbo-2024-25-final",
    "finance-cfs-2024-25-final"
  )))
  fbo <- releases[
    releases$release_id == "treasury-fbo-2024-25-final-history", ,
    drop = FALSE
  ]
  expect_equal(fbo$period_end, as.Date("2025-06-30"))
  expect_equal(fbo$actual_forecast, "actual")
  expect_equal(fbo$schema_family, "treasury_fbo_history_docx_v1")

  current <- releases[
    releases$release_id == "finance-mfs-2026-05-actual-aggregates", ,
    drop = FALSE
  ]
  expect_equal(current$resource_id,
               "1e1e039e-c54d-4c59-973b-7b15cde2ed63")
  expect_equal(current$actual_forecast, "actual")
  expect_match(current$resource_url, "/6\\.-aggregates\\.xlsx$")

  history <- releases[
    releases$release_id ==
      "finance-mfs-history-snapshot-2026-06-may-2008", ,
    drop = FALSE
  ]
  expect_equal(history$release_status, "snapshot")
  expect_equal(history$release_date, as.Date("2026-06-26"))
  expect_equal(history$period_end, as.Date("2008-05-31"))

  pbo <- releases[
    releases$release_id ==
      "pbo-historical-fiscal-data-2026-27-budget", ,
    drop = FALSE
  ]
  expect_equal(pbo$source_id, "parliamentary_budget_office")
  expect_equal(pbo$release_date, as.Date("2026-05-13"))
  expect_equal(pbo$period_end, as.Date("2025-06-30"))
  expect_equal(pbo$actual_forecast, "actual")
  expect_equal(
    pbo$schema_family,
    "pbo_historical_fiscal_data_xlsx_v1"
  )
})

test_that("manifest fails closed on release ID and vintage collisions", {
  one <- fixture_release()
  duplicate_id <- rbind(one, one)
  expect_error(
    readfiscalau:::.rf_validate_manifest(duplicate_id),
    class = "readfiscalau_schema"
  )

  collision <- rbind(one, one)
  collision$release_id[2] <- "different-id-same-vintage"
  collision$resource_url[2] <- "https://example.gov.au/revised.csv"
  expect_error(
    readfiscalau:::.rf_validate_manifest(collision),
    class = "readfiscalau_vintage_collision"
  )
})

test_that("manifest rejects inconsistent period, source, and media semantics", {
  wrong_fy <- fixture_release()
  wrong_fy$fiscal_year <- "2025-26"
  expect_error(
    readfiscalau:::.rf_validate_manifest(wrong_fy),
    class = "readfiscalau_schema"
  )

  wrong_source <- fixture_release(release_type = "fbo")
  wrong_source$source_id <- "department_finance"
  expect_error(
    readfiscalau:::.rf_validate_manifest(wrong_source),
    class = "readfiscalau_schema"
  )

  wrong_media <- fixture_release()
  wrong_media$media_type <- "text/html"
  expect_error(
    readfiscalau:::.rf_validate_manifest(wrong_media),
    class = "readfiscalau_schema"
  )
})

test_that("manifest pins parser implementation and exact source terms", {
  unsupported_parser <- fixture_release()
  unsupported_parser$parser_version <- "9.9.9"
  expect_error(
    readfiscalau:::.rf_validate_manifest(unsupported_parser),
    class = "readfiscalau_schema"
  )

  wrong_license <- fixture_release()
  wrong_license$license <- "Public domain"
  expect_error(
    readfiscalau:::.rf_validate_manifest(wrong_license),
    class = "readfiscalau_schema"
  )

  wrong_terms <- fixture_release()
  wrong_terms$terms_url <- "https://example.gov.au/terms"
  expect_error(
    readfiscalau:::.rf_validate_manifest(wrong_terms),
    class = "readfiscalau_schema"
  )
})

test_that("provenance binds contract, parser revision, and source terms", {
  out <- read_au_budget(
    fixture_release(),
    raw = fixture_raw("budget-current.csv"),
    cache_dir = local_tempdir()
  )
  provenance <- attr(out, "readfiscalau_provenance")

  expect_equal(
    provenance$contract_revision,
    readfiscalau:::.rf_contract_revision
  )
  expect_equal(
    provenance$parser_revision,
    readfiscalau:::.rf_parser_revision
  )
  expect_equal(provenance$terms_url, unique(out$terms_url))
  expect_equal(provenance$license, unique(out$license))
  expect_equal(provenance$attribution, unique(out$attribution))
  expect_match(provenance$cache_namespace_sha256, "^[a-f0-9]{64}$")
})

test_that("Treasury and Finance HTML discovery are fixture replayable", {
  treasury <- discover_au_fiscal_releases(
    "treasury",
    raw = fixture_raw("treasury-downloads.html")
  )
  expect_true(all(c("budget", "myefo", "fbo") %in%
                  treasury$release_type))
  expect_true(all(grepl("^https://", treasury$resource_url)))
  expect_setequal(
    treasury$fiscal_year,
    c("2026-27", "2025-26", "2024-25")
  )

  finance <- discover_au_fiscal_releases(
    "finance",
    raw = fixture_raw("finance-releases.html")
  )
  expect_true(any(finance$release_type == "monthly_financial_statement"))
  expect_true(any(finance$release_type == "fbo"))
  expect_false(any(grepl("\\bJune\\b", finance$title)))
})

test_that("Finance discovery rejects fabricated June monthlies", {
  expect_error(
    discover_au_fiscal_releases(
      "finance",
      raw = fixture_raw("finance-june-invalid.html")
    ),
    class = "readfiscalau_june_is_annual"
  )
})

test_that("CKAN discovery retains every resource vintage", {
  resources <- discover_finance_ckan_resources(
    fixture_raw("finance-ckan.json")
  )
  expect_equal(nrow(resources), 2L)
  expect_setequal(
    resources$resource_id,
    c("current-aggregates", "historical-operating")
  )
  expect_true(all(resources$license ==
    "Creative Commons Attribution 3.0 Australia"))

  payload <- jsonlite::fromJSON(
    rawToChar(fixture_raw("finance-ckan.json")),
    simplifyVector = FALSE
  )
  payload$result$resources <- c(
    payload$result$resources,
    list(payload$result$resources[[1]])
  )
  duplicate <- charToRaw(jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null"
  ))
  expect_error(
    discover_finance_ckan_resources(duplicate),
    class = "readfiscalau_schema"
  )
})

test_that("HTML URL discovery resolves parent paths and rejects HTTP", {
  html <- charToRaw(paste0(
    "<html><body>",
    "<a href='../2025-26/index.htm'>Budget 2025-26</a>",
    "</body></html>"
  ))
  discovered <- discover_au_fiscal_releases(
    "treasury",
    url = "https://archive.budget.gov.au/2026-27/downloads/index.htm",
    raw = html
  )
  expect_equal(
    discovered$resource_url,
    "https://archive.budget.gov.au/2026-27/2025-26/index.htm"
  )

  insecure <- charToRaw(
    "<html><body><a href='http://example.gov.au/budget-2025-26'>Budget 2025-26</a></body></html>"
  )
  expect_error(
    discover_au_fiscal_releases("treasury", raw = insecure),
    class = "readfiscalau_schema"
  )
})
