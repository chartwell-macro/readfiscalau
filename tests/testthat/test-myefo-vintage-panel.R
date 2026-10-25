myefo_vintage_release_ids <- paste0(
  "treasury-myefo-",
  c(
    "2000-01", "2001-02", "2002-03", "2003-04", "2004-05",
    "2005-06", "2006-07", "2007-08", "2008-09", "2009-10",
    "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
    "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
    "2020-21", "2021-22", "2023-24", "2024-25", "2025-26"
  ),
  "-final"
)

myefo_table_families <- c(
  "major_economic_parameters",
  "budget_aggregates",
  "budget_comparator_aggregates",
  "domestic_economy_forecasts",
  "cash_receipts",
  "cash_interest"
)

test_that("curated MYEFO panel covers every official vintage from 2000", {
  panel <- readfiscalau:::.rf_myefo_vintage_observations()
  releases <- list_au_fiscal_releases()
  admitted <- releases[
    releases$release_id %in% myefo_vintage_release_ids,
    ,
    drop = FALSE
  ]

  expect_setequal(unique(panel$release_id), myefo_vintage_release_ids)
  expect_setequal(unique(panel$table_family), myefo_table_families)
  expect_setequal(admitted$release_id, myefo_vintage_release_ids)
  expect_equal(nrow(admitted), 25L)
  expect_true(all(
    admitted$schema_family == "treasury_myefo_pdf_snapshot_v1"
  ))
  expect_true(all(admitted$release_type == "myefo"))
  expect_true(all(admitted$media_type == "application/pdf"))
  expect_true(all(grepl(
    "^https://archive\\.budget\\.gov\\.au/",
    admitted$resource_url
  )))

  source_row <- match(panel$release_id, admitted$release_id)
  expect_equal(panel$source_url, admitted$resource_url[source_row])
  expect_equal(
    panel$source_sha256,
    admitted$expected_sha256[source_row]
  )
  expect_true(all(nzchar(panel$table_label)))
  expect_true(all(nzchar(panel$source_table)))
  expect_true(all(grepl(
    "^https://",
    panel$observation_source_url
  )))
  expect_true(all(grepl(
    "^[a-f0-9]{64}$",
    panel$observation_source_sha256
  )))
  expect_true(all(panel$profile_type == ifelse(
    panel$actual_forecast == "actual",
    "actual",
    ifelse(
      panel$table_family == "budget_comparator_aggregates",
      "budget",
      "myefo"
    )
  )))
  expect_equal(
    anyDuplicated(panel[c(
      "release_id", "table_family", "series_id",
      "fiscal_year", "actual_forecast"
    )]),
    0L
  )
})

test_that("MYEFO series metadata reports exact table-level coverage", {
  readfiscalau:::.rf_clear_internal_file_cache()
  on.exit(readfiscalau:::.rf_clear_internal_file_cache(), add = TRUE)
  coverage <- list_au_myefo_series()
  cached <- list_au_myefo_series()

  expect_s3_class(coverage, "tbl_df")
  expect_identical(cached, coverage)
  expect_true(any(grepl(
    "myefo_series_coverage_",
    ls(envir = readfiscalau:::.rf_internal_file_cache, all.names = TRUE),
    fixed = TRUE
  )))
  expect_equal(nrow(coverage), 3485L)
  expect_setequal(
    unique(coverage$release_id),
    myefo_vintage_release_ids
  )
  expect_setequal(
    unique(coverage$table_family),
    myefo_table_families
  )
  expect_true(all(c(
    "table_id", "table_label", "source_table", "profile_type",
    "observation_source_url", "observation_source_sha256"
  ) %in% names(coverage)))
  expect_equal(
    anyDuplicated(coverage[c(
      "release_id", "table_family", "series_id", "actual_forecast"
    )]),
    0L
  )

  latest_gdp <- coverage[
    coverage$release_id == "treasury-myefo-2025-26-final" &
      coverage$table_family == "major_economic_parameters" &
      coverage$series == "Real GDP" &
      coverage$actual_forecast == "forecast",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(latest_gdp), 1L)
  expect_equal(latest_gdp$start_date, as.Date("2026-06-30"))
  expect_equal(latest_gdp$end_date, as.Date("2029-06-30"))
  expect_equal(latest_gdp$observation_count, 4L)
})

test_that("MYEFO table families preserve representative published values", {
  release <- list_au_fiscal_releases()
  release <- release[
    release$release_id == "treasury-myefo-2025-26-final",
    ,
    drop = FALSE
  ]
  cases <- data.frame(
    table_family = c(
      "major_economic_parameters",
      "domestic_economy_forecasts",
      "cash_receipts",
      "cash_interest",
      "budget_aggregates",
      "budget_comparator_aggregates"
    ),
    series = c(
      "Real GDP",
      "Household consumption",
      "Goods and services tax",
      "Interest receipts — AUD million",
      "Underlying cash balance — AUD million",
      "Receipts — AUD million"
    ),
    fiscal_year = c(
      "2025-26", "2025-26", "2025-26",
      "2025-26", "2025-26", "2025-26"
    ),
    value = c(2.25, 2.5, 95654, 9103, -36764, 735353),
    stringsAsFactors = FALSE
  )

  for (index in seq_len(nrow(cases))) {
    out <- readfiscalau:::.rf_parse_myefo_pdf_snapshot(
      release,
      cases$table_family[[index]]
    )
    expect_s3_class(out, "tbl_df")
    selected <- out[
      out$series == cases$series[[index]] &
        out$fiscal_year == cases$fiscal_year[[index]],
      ,
      drop = FALSE
    ]
    expect_equal(nrow(selected), 1L)
    expect_equal(selected$value, cases$value[[index]])
    expect_equal(
      unique(selected$table_family),
      cases$table_family[[index]]
    )
    member <- attr(out, "readfiscalau_member")
    expect_equal(member$name, "myefo-vintage-observations.csv")
    expect_match(member$sha256, "^[a-f0-9]{64}$")
  }
})

test_that("latest MYEFO retains the complete requested cash tables", {
  panel <- readfiscalau:::.rf_myefo_vintage_observations()
  latest <- panel[
    panel$release_id == "treasury-myefo-2025-26-final",
    ,
    drop = FALSE
  ]
  receipts <- latest[latest$table_family == "cash_receipts", , drop = FALSE]
  interest <- latest[latest$table_family == "cash_interest", , drop = FALSE]

  expect_equal(length(unique(receipts$series_id)), 39L)
  expect_equal(length(unique(interest$series_id)), 8L)
  expect_identical(
    unique(receipts$source_table),
    paste(
      "Table 3.5: Australian Government general government",
      "(cash) receipts"
    )
  )
  expect_identical(
    unique(interest$source_table),
    paste(
      "Table 4.9: Interest payments, interest receipts and net",
      "interest payments(a)"
    )
  )
})

test_that("historical MYEFO cash receipts retain the full published horizon", {
  panel <- readfiscalau:::.rf_myefo_vintage_observations()
  expected <- data.frame(
    vintage = c(
      "2006-07", "2007-08", "2008-09", "2009-10", "2010-11",
      "2011-12", "2012-13", "2013-14", "2014-15"
    ),
    final_year = c(
      "2009-10", "2010-11", "2011-12", "2012-13", "2013-14",
      "2014-15", "2015-16", "2016-17", "2017-18"
    ),
    stringsAsFactors = FALSE
  )

  for (index in seq_len(nrow(expected))) {
    release_id <- paste0(
      "treasury-myefo-", expected$vintage[[index]], "-final"
    )
    beer <- panel[
      panel$release_id == release_id &
        panel$table_family == "cash_receipts" &
        panel$series_id == "beer" &
        panel$actual_forecast == "forecast",
      ,
      drop = FALSE
    ]

    expect_equal(
      nrow(beer),
      4L,
      info = paste("Incomplete cash-receipts horizon for", release_id)
    )
    expect_equal(
      max(beer$fiscal_year),
      expected$final_year[[index]],
      info = paste("Incorrect cash-receipts end year for", release_id)
    )
  }
})

test_that("MYEFO table aliases and hash binding fail closed", {
  release <- list_au_fiscal_releases()
  release <- release[
    release$release_id == "treasury-myefo-2010-11-final",
    ,
    drop = FALSE
  ]
  tampered <- release
  tampered$expected_sha256 <- strrep("0", 64L)

  expect_equal(
    unique(readfiscalau:::.rf_parse_myefo_pdf_snapshot(
      release,
      "Table 1"
    )$table_family),
    "cash_receipts"
  )
  expect_error(
    readfiscalau:::.rf_parse_myefo_pdf_snapshot(
      tampered,
      "cash_receipts"
    ),
    class = "readfiscalau_source_hash"
  )
  expect_error(
    readfiscalau:::.rf_parse_myefo_pdf_snapshot(
      release,
      "not a table"
    ),
    class = "readfiscalau_table_required"
  )
})
