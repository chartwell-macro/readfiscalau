budget_vintage_release_ids <- c(
  paste0(
    "treasury-budget-",
    c(
      "2000-01", "2001-02", "2002-03", "2003-04", "2004-05",
      "2005-06", "2006-07", "2007-08", "2008-09", "2009-10",
      "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
      "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
      "2020-21", "2021-22"
    ),
    "-final"
  ),
  "treasury-budget-2022-23-march-final",
  "treasury-budget-2022-23-october-final",
  paste0(
    "treasury-budget-",
    c("2023-24", "2024-25", "2025-26", "2026-27"),
    "-final"
  )
)

budget_table_families <- c(
  "major_economic_parameters",
  "budget_aggregates",
  "domestic_economy_forecasts",
  "cash_receipts",
  "cash_interest"
)

test_that("immutable package tables are memoized without leaking mutations", {
  readfiscalau:::.rf_clear_internal_file_cache()
  on.exit(readfiscalau:::.rf_clear_internal_file_cache(), add = TRUE)

  first_manifest <- readfiscalau:::.rf_builtin_manifest()
  first_panel <- readfiscalau:::.rf_budget_vintage_observations()
  cache_keys <- ls(
    envir = readfiscalau:::.rf_internal_file_cache,
    all.names = TRUE
  )
  expect_length(cache_keys, 2L)

  first_manifest$release_id[[1L]] <- "mutated-release"
  first_panel$series_id[[1L]] <- "mutated-series"
  second_manifest <- readfiscalau:::.rf_builtin_manifest()
  second_panel <- readfiscalau:::.rf_budget_vintage_observations()

  expect_false(second_manifest$release_id[[1L]] == "mutated-release")
  expect_false(second_panel$series_id[[1L]] == "mutated-series")
  expect_identical(
    ls(
      envir = readfiscalau:::.rf_internal_file_cache,
      all.names = TRUE
    ),
    cache_keys
  )
})

test_that("curated Budget panel covers all vintages and requested families", {
  panel <- readfiscalau:::.rf_budget_vintage_observations()
  releases <- list_au_fiscal_releases()
  admitted <- releases[
    releases$release_id %in% budget_vintage_release_ids,
    ,
    drop = FALSE
  ]

  expect_equal(nrow(panel), 14352L)
  expect_setequal(unique(panel$release_id), budget_vintage_release_ids)
  expect_setequal(unique(panel$table_family), budget_table_families)
  expect_setequal(admitted$release_id, budget_vintage_release_ids)
  expect_equal(nrow(admitted), 28L)
  expect_true(all(admitted$release_type == "budget"))
  expect_true(all(grepl("^[a-f0-9]{64}$", admitted$expected_sha256)))

  source_row <- match(panel$release_id, admitted$release_id)
  expect_equal(panel$source_url, admitted$resource_url[source_row])
  expect_equal(
    panel$source_sha256,
    admitted$expected_sha256[source_row]
  )
  expect_true(all(nzchar(panel$table_label)))
  expect_true(all(nzchar(panel$source_table)))
  expect_true(all(grepl("^https://", panel$observation_source_url)))
  expect_true(all(grepl(
    "^[a-f0-9]{64}$",
    panel$observation_source_sha256
  )))
  expect_true(all(panel$profile_type == ifelse(
    panel$actual_forecast == "actual",
    "actual",
    "budget"
  )))
  expect_equal(
    anyDuplicated(panel[c(
      "release_id", "table_family", "series_id",
      "fiscal_year", "actual_forecast"
    )]),
    0L
  )
})

test_that("Budget metadata reports exact table-level coverage", {
  readfiscalau:::.rf_clear_internal_file_cache()
  on.exit(readfiscalau:::.rf_clear_internal_file_cache(), add = TRUE)
  coverage <- list_au_budget_series()
  cached <- list_au_budget_series()

  expect_s3_class(coverage, "tbl_df")
  expect_identical(cached, coverage)
  expect_equal(nrow(coverage), 4158L)
  expect_setequal(unique(coverage$release_id), budget_vintage_release_ids)
  expect_setequal(unique(coverage$table_family), budget_table_families)
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
    coverage$release_id == "treasury-budget-2026-27-final" &
      coverage$table_family == "major_economic_parameters" &
      coverage$series == "Real GDP" &
      coverage$actual_forecast == "forecast",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(latest_gdp), 1L)
  expect_equal(latest_gdp$start_date, as.Date("2026-06-30"))
  expect_equal(latest_gdp$end_date, as.Date("2030-06-30"))
  expect_equal(latest_gdp$observation_count, 5L)

  current_receipts <- coverage[
    coverage$release_id == "treasury-budget-2026-27-final" &
      coverage$table_family == "cash_receipts" &
      coverage$series == "Total receipts" &
      coverage$actual_forecast == "actual",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(current_receipts), 1L)
  expect_equal(current_receipts$start_date, as.Date("2006-06-30"))
  expect_equal(current_receipts$end_date, as.Date("2025-06-30"))
  expect_equal(current_receipts$observation_count, 20L)

  expect_identical(
    min(coverage$release_id[coverage$table_family == "cash_interest"]),
    "treasury-budget-2016-17-final"
  )
})

test_that("vintage coverage aggregation is independent of panel row order", {
  panel <- readfiscalau:::.rf_budget_vintage_observations()
  expected <- readfiscalau:::.rf_vintage_series_coverage(panel)
  set.seed(20260729)
  shuffled <- readfiscalau:::.rf_vintage_series_coverage(
    panel[sample.int(nrow(panel)), , drop = FALSE]
  )
  sort_rows <- function(coverage) {
    coverage[order(
      coverage$release_id,
      coverage$table_family,
      coverage$series_id,
      coverage$actual_forecast
    ), , drop = FALSE]
  }

  expect_identical(sort_rows(shuffled), sort_rows(expected))
})

test_that("current Budget cash receipts retain the complete BP1 table", {
  panel <- readfiscalau:::.rf_budget_vintage_observations()
  latest_receipts <- panel[
    panel$release_id == "treasury-budget-2026-27-final" &
      panel$table_family == "cash_receipts" &
      endsWith(panel$observation_source_url, ".pdf"),
    ,
    drop = FALSE
  ]
  latest_interest <- panel[
    panel$release_id == "treasury-budget-2026-27-final" &
      panel$table_family == "cash_interest",
    ,
    drop = FALSE
  ]

  expect_equal(length(unique(latest_receipts$series_id)), 37L)
  expect_true(all(c(
    "Petrol", "Diesel", "Tobacco", "Beer", "Spirits",
    "Visa application charges", "Sales of goods and services",
    "Interest received", "Other non-taxation receipts",
    "Capital gains tax"
  ) %in% latest_receipts$series))
  expect_identical(
    unique(latest_receipts$source_table),
    paste(
      "Table 5.7: Australian Government general government",
      "(cash) receipts"
    )
  )
  expect_identical(
    unique(latest_interest$source_table),
    paste(
      "Table 7.9: Interest payments, interest receipts and net",
      "interest payments (a)"
    )
  )
})

test_that("Budget table families preserve representative published values", {
  release <- list_au_fiscal_releases()
  release <- release[
    release$release_id == "treasury-budget-2026-27-final",
    ,
    drop = FALSE
  ]
  cases <- data.frame(
    table_family = c(
      "major_economic_parameters",
      "domestic_economy_forecasts",
      "cash_receipts",
      "cash_interest",
      "budget_aggregates"
    ),
    series = c(
      "Real GDP",
      "Household consumption",
      "Goods and services tax",
      "Interest receipts — AUD million",
      "Underlying cash balance — AUD million"
    ),
    fiscal_year = rep("2026-27", 5L),
    value = c(1.75, 1.75, 103249, 9636, -31542),
    stringsAsFactors = FALSE
  )

  for (index in seq_len(nrow(cases))) {
    out <- readfiscalau:::.rf_parse_budget_pdf_snapshot(
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
    expect_equal(member$name, "budget-vintage-observations.csv")
    expect_match(member$sha256, "^[a-f0-9]{64}$")
    expect_s3_class(member$observation_sources, "data.frame")
  }
})

test_that("Budget table aliases and hash binding fail closed", {
  release <- list_au_fiscal_releases()
  release <- release[
    release$release_id == "treasury-budget-2010-11-final",
    ,
    drop = FALSE
  ]
  tampered <- release
  tampered$expected_sha256 <- strrep("0", 64L)

  expect_equal(
    unique(readfiscalau:::.rf_parse_budget_pdf_snapshot(
      release,
      "Table 1"
    )$table_family),
    "cash_receipts"
  )
  expect_error(
    readfiscalau:::.rf_parse_budget_pdf_snapshot(
      tampered,
      "cash_receipts"
    ),
    class = "readfiscalau_source_hash"
  )
  expect_error(
    readfiscalau:::.rf_parse_budget_pdf_snapshot(
      release,
      "not a table"
    ),
    class = "readfiscalau_table_required"
  )
})

test_that("Budget history keeps the coherent Table 1 release family", {
  releases <- list_au_fiscal_releases()
  panel <- readfiscalau:::.rf_budget_vintage_observations()
  cash_budget_ids <- unique(panel$release_id[
    panel$table_family == "cash_receipts"
  ])
  contract <- readfiscalau:::.rf_budget_history_table("Table 1")
  roles <- readfiscalau:::.rf_history_resource_roles(contract)
  coherent <- releases[
    releases$release_id %in% cash_budget_ids,
    ,
    drop = FALSE
  ]

  expect_true(all(coherent$resource_role %in% roles))
  candidates <- releases[
    releases$source_id == "australian_treasury" &
      releases$release_type == "budget" &
      releases$resource_role %in% roles,
    ,
    drop = FALSE
  ]
  terminal <- releases[
    releases$release_id == "treasury-budget-2026-27-final",
    ,
    drop = FALSE
  ]
  preferred <- readfiscalau:::.rf_history_prefer_coherent_releases(
    candidates,
    terminal,
    contract
  )
  expect_setequal(preferred$release_id, cash_budget_ids)
})
