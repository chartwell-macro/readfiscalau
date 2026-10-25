history_latest_budget <- "treasury-budget-2026-27-final"
history_latest_myefo <- "treasury-myefo-2025-26-final"

test_that("historical table store has the frozen release and family contract", {
  families <- list_au_budget_myefo_families()
  tables <- list_au_budget_myefo_tables()

  expect_s3_class(families, "tbl_df")
  expect_s3_class(tables, "tbl_df")
  expect_equal(nrow(families), 22L)
  mode_counts <- table(families$query_mode)
  expect_identical(
    as.integer(mode_counts[c("hybrid", "series_and_table", "table_only")]),
    c(7L, 14L, 1L)
  )
  expect_equal(length(unique(tables$release_id)), 53L)
  expect_equal(length(unique(tables$release_id[tables$publication == "budget"])), 28L)
  expect_equal(length(unique(tables$release_id[tables$publication == "myefo"])), 25L)
  expect_identical(min(tables$release_date), as.Date("2000-05-09"))
  expect_identical(max(tables$release_date), as.Date("2026-05-12"))
  expect_true(all(grepl("^https://", tables$source_url)))
  expect_true(all(grepl("^[0-9a-f]{64}$", tables$source_sha256)))
  expect_true(all(grepl(
    "^[0-9a-f]{64}$", tables$source_container_sha256
  )))
  expect_setequal(
    unique(tables$source_kind),
    c("pdf_table_cell", "treasury_chart_data_cell")
  )
  expect_equal(sum(tables$source_kind == "treasury_chart_data_cell"), 3L)
  expect_false(any(grepl("^Chart ", tables$source_table, ignore.case = TRUE)))

  assembled <- tables[
    tables$release_id == "treasury-budget-2002-03-final",
    ,
    drop = FALSE
  ]
  expect_true(all(assembled$source_was_assembled))
  expect_true(all(assembled$source_sha256 != assembled$source_container_sha256))
  expect_true(all(grepl("BP1_Prelims.pdf", assembled$source_archive_members,
                        fixed = TRUE)))
})

test_that("complete family catalogue reports honest per-publication coverage", {
  families <- list_au_budget_myefo_families()
  domestic <- families[
    families$family_id == "domestic_economy_detailed_forecasts",
    ,
    drop = FALSE
  ]
  receipts <- families[
    families$family_id == "detailed_cash_receipts",
    ,
    drop = FALSE
  ]
  statements <- families[
    families$family_id == "formal_financial_statements",
    ,
    drop = FALSE
  ]

  expect_identical(domestic$budget_first_vintage, "2000-01")
  expect_identical(domestic$myefo_first_vintage, "2000-01")
  expect_identical(domestic$budget_release_count, 28L)
  expect_identical(domestic$myefo_release_count, 25L)
  expect_identical(receipts$budget_first_vintage, "2005-06")
  expect_identical(receipts$myefo_first_vintage, "2006-07")
  expect_identical(statements$budget_first_vintage, "2004-05")
  expect_identical(statements$myefo_first_vintage, "2003-04")
})

test_that("earliest legacy-font publications expose their complete dated rows", {
  domestic <- read_au_budget_myefo_table(
    "treasury-budget-2000-01-final",
    "domestic_economy_detailed_forecasts"
  )
  household <- domestic[
    domestic$series_id == "household-consumption" &
      domestic$series_eligible,
    ,
    drop = FALSE
  ]
  international <- read_au_budget_myefo_table(
    "treasury-myefo-2000-01-final",
    "international_gdp_forecasts"
  )

  expect_identical(household$column_label, c(
    "1998-99", "1999-2000", "2000-01", "Four quarters to June 2001"
  ))
  expect_identical(household$period_type, c(
    "fiscal_year", "fiscal_year", "fiscal_year", "four_quarters_to_june"
  ))
  expect_equal(household$value, c(4.1, 4.5, 3.75, 3.75))
  expect_true(all(c(
    "world", "total-oecd", "united-states", "japan", "european-union",
    "non-japan-east-asia"
  ) %in% international$series_id[international$series_eligible]))
})

test_that("MYEFO comparator rows retain profile and alternating units", {
  aggregates <- read_au_budget_myefo_table(
    "treasury-myefo-2023-24-final",
    "ggs_accrual_aggregates"
  )
  revenue <- aggregates[
    aggregates$series_id == "revenue" &
      aggregates$profile_type == "myefo" &
      aggregates$series_eligible,
    ,
    drop = FALSE
  ]

  expect_equal(nrow(revenue), 8L)
  expect_identical(revenue$unit, rep(c("AUD billion", "% of GDP"), 4L))
  expect_identical(revenue$fiscal_year, rep(
    c("2023-24", "2024-25", "2025-26", "2026-27"), each = 2L
  ))
})

test_that("latest detailed domestic forecast exposes every durable row", {
  table <- read_au_budget_myefo_table(
    history_latest_budget,
    "domestic_economy_detailed_forecasts"
  )
  eligible <- unique(table[table$series_eligible, c("series_id", "series")])
  mining <- table[
    table$series_id == "mining-investment" & table$series_eligible,
    ,
    drop = FALSE
  ]

  expect_equal(nrow(eligible), 23L)
  expect_setequal(
    eligible$series_id,
    c(
      "real-gross-domestic-product", "household-consumption",
      "dwelling-investment", "total-business-investment",
      "mining-investment", "non-mining-investment",
      "private-final-demand", "public-final-demand",
      "change-in-inventories", "gross-national-expenditure",
      "exports-of-goods-and-services", "imports-of-goods-and-services",
      "net-exports", "nominal-gross-domestic-product",
      "consumer-price-index", "wage-price-index", "gdp-deflator",
      "participation-rate", "employment", "unemployment-rate",
      "terms-of-trade", "current-account-balance",
      "net-overseas-migration"
    )
  )
  expect_identical(mining$fiscal_year, c(
    "2024-25", "2025-26", "2026-27", "2027-28"
  ))
  expect_equal(mining$value, c(-1, 0.5, 1, 0))
  expect_true(all(mining$unit == "Per cent change"))
})

test_that("latest detailed receipts exposes PDF and chart-data categories", {
  table <- read_au_budget_myefo_table(
    history_latest_budget,
    "detailed_cash_receipts"
  )
  eligible <- unique(table[table$series_eligible, c(
    "series_id", "series", "row_path"
  )])

  pdf_eligible <- unique(table[
    table$series_eligible & table$source_kind == "pdf_table_cell",
    c("series_id", "series", "row_path")
  ])
  chart_eligible <- unique(table[
    table$series_eligible &
      table$source_kind == "treasury_chart_data_cell",
    c("series_id", "series", "row_path")
  ])

  expect_equal(nrow(eligible), 54L)
  expect_equal(nrow(pdf_eligible), 37L)
  expect_equal(nrow(chart_eligible), 29L)
  expect_true(all(c(
    "gross-income-tax-withholding", "company-tax",
    "goods-and-services-tax", "petrol", "diesel", "tobacco",
    "visa-application-charges", "total-receipts", "capital-gains-tax"
  ) %in% eligible$series_id))
  expect_match(
    eligible$row_path[eligible$series_id == "petrol"],
    "Excise and customs duty > Petrol",
    fixed = TRUE
  )
})

test_that("canonical balances deduplicate repeated source tables", {
  preferred <- read_au_budget_myefo_series(
    "underlying-cash-balance",
    release = history_latest_budget,
    unit = "AUD million"
  )
  all_sources <- read_au_budget_myefo_series(
    "underlying-cash-balance",
    release = history_latest_budget,
    unit = "AUD million",
    all_sources = TRUE
  )
  headline <- read_au_budget_myefo_series(
    "headline-cash-balance",
    release = history_latest_budget,
    unit = "AUD million"
  )

  expect_gt(nrow(all_sources), nrow(preferred))
  expect_true(all(c(
    "underlying_cash_balance_reconciliation",
    "underlying_to_headline_cash_bridge",
    "formal_financial_statements"
  ) %in% all_sources$family_id))
  expect_identical(unique(preferred$family_id),
                   "underlying_to_headline_cash_bridge")
  expect_identical(preferred$fiscal_year, c(
    "2025-26", "2026-27", "2027-28", "2028-29", "2029-30"
  ))
  expect_equal(preferred$value, c(-28284, -31542, -31000, -34445, -25259))
  expect_identical(unique(headline$family_id),
                   "underlying_to_headline_cash_bridge")
  expect_equal(headline$value, c(-47913, -64086, -54333, -57230, -40963))

  key <- preferred[c(
    "release_id", "series_id", "period_end", "unit",
    "accounting_basis", "profile_type"
  )]
  expect_equal(anyDuplicated(key), 0L)
  expect_true(all(preferred$is_preferred))
  expect_true(all(unique(all_sources$series_id) == "underlying-cash-balance"))
})

test_that("series-profile catalogue exposes forecast-vintage paths", {
  profiles <- list_au_budget_myefo_series_profiles()

  expect_s3_class(profiles, "tbl_df")
  expect_equal(nrow(profiles), 17796L)
  expect_equal(sum(profiles$observation_count), 63773L)
  expect_equal(anyDuplicated(profiles$profile_id), 0L)
  expect_true(all(grepl(
    "^budget-myefo-series-profile-v2-[0-9a-f]{64}$",
    profiles$profile_id
  )))
  expect_true(all(profiles$snapshot_schema == "budget_myefo_history_v3"))
  expect_true(all(profiles$first_period <= profiles$last_period))
  expect_true(all(profiles$observation_count >= 1L))
  expect_setequal(
    unique(profiles$profile_type),
    c("budget", "myefo", "pefo")
  )
  expect_identical(unique(profiles$actual_forecast), "forecast")
  expect_true(all(nzchar(profiles$series_labels)))
  expect_true(all(nzchar(profiles$family_ids)))
  expect_true(all(nzchar(profiles$family_labels)))
  expect_true(all(nzchar(profiles$table_instance_ids)))
  expect_true(all(nzchar(profiles$source_tables)))
  expect_true(all(nzchar(profiles$source_kind)))
  expect_true(all(nzchar(profiles$source_kinds)))
  expect_true(all(nzchar(profiles$period_types)))
})

test_that("pinned Treasury chart CSV and ZIP members extend receipts", {
  tables <- list_au_budget_myefo_tables(
    family = "detailed_cash_receipts"
  )
  chart_tables <- tables[
    tables$source_kind == "treasury_chart_data_cell",
    ,
    drop = FALSE
  ]
  expected_sources <- data.frame(
    release_id = c(
      "treasury-budget-2024-25-final",
      "treasury-budget-2025-26-final",
      "treasury-budget-2026-27-final"
    ),
    source_sha256 = c(
      "2db5571dea5dba82cd90679f3db3796bb4ca308a862d89c6e53cdf2f4fce9597",
      "f7c567e140b04341660dc372b3025af441510648268f4917e953e283e0ff3a5f",
      "b905c5a1c7c4b13338fd37e390fe0d0c1fef4bc85fd85293a655ea2b41e20cd4"
    ),
    source_container_sha256 = c(
      "2db5571dea5dba82cd90679f3db3796bb4ca308a862d89c6e53cdf2f4fce9597",
      "81269ea4961979e8e5d4209a134e12dcd50d02c0ccc066032d61ed69e1466db9",
      "2069eb7986f890423323732f12be0ef172ece996bffbd1bbaf0e814d324a1681"
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(nrow(chart_tables), 3L)
  chart_tables <- chart_tables[match(
    expected_sources$release_id,
    chart_tables$release_id
  ), ]
  expect_identical(chart_tables$source_sha256, expected_sources$source_sha256)
  expect_identical(
    chart_tables$source_container_sha256,
    expected_sources$source_container_sha256
  )
  expect_identical(
    chart_tables$source_archive_members,
    c("", rep("Table 1 - Australian Government (cash) receipts.csv", 2L))
  )

  expanded_series <- c(
    "carbon-pricing-mechanism", "dividends-and-other", "fuel-excise",
    "other-excise", "other-sales-taxes",
    "total-other-indirect-taxation-receipts", "total-sales-taxes"
  )
  expanded <- do.call(rbind, lapply(expected_sources$release_id, function(id) {
    list_au_budget_myefo_series_profiles(
      release = id,
      family = "detailed_cash_receipts"
    )
  }))
  expanded <- expanded[
    expanded$series_id %in% expanded_series &
      expanded$unit == "AUD million" &
      expanded$accounting_basis == "cash",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(expanded), 21L)
  expect_true(all(expanded$observation_count == 5L))
  expect_true(all(expanded$source_kind == "treasury_chart_data_cell"))
  expect_true(all(expanded$source_kinds == "treasury_chart_data_cell"))
  profile_sources <- expanded[
    !duplicated(expanded$release_id),
    c(
      "release_id", "source_sha256", "source_container_sha256",
      "source_archive_members"
    ),
    drop = FALSE
  ]
  profile_sources <- profile_sources[match(
    expected_sources$release_id,
    profile_sources$release_id
  ), , drop = FALSE]
  expect_identical(
    profile_sources$source_sha256,
    expected_sources$source_sha256
  )
  expect_identical(
    profile_sources$source_container_sha256,
    expected_sources$source_container_sha256
  )
  expect_identical(
    profile_sources$source_archive_members,
    c("", rep("Table 1 - Australian Government (cash) receipts.csv", 2L))
  )

  fuel <- expanded[
    expanded$release_id == "treasury-budget-2026-27-final" &
      expanded$series_id == "fuel-excise",
    ,
    drop = FALSE
  ]
  observations <- read_au_budget_myefo_series_profile(fuel$profile_id[[1L]])
  expect_equal(nrow(observations), 5L)
  expect_identical(
    observations$period_end,
    as.Date(sprintf("%d-06-30", 2026:2030))
  )
  expect_true(all(
    observations$source_kind == "treasury_chart_data_cell"
  ))
  provenance <- attr(
    observations,
    "readfiscalau_series_profile_provenance"
  )
  expect_identical(
    provenance$source_container_sha256,
    expected_sources$source_container_sha256[[3L]]
  )
  expect_identical(
    provenance$source_archive_members,
    "Table 1 - Australian Government (cash) receipts.csv"
  )
})

test_that("one profile round-trips to one exact unique-date series", {
  profiles <- list_au_budget_myefo_series_profiles(
    release = history_latest_budget,
    series = "underlying-cash-balance",
    profile_type = "budget"
  )
  profile <- profiles[
    profiles$unit == "AUD billion" &
      profiles$accounting_basis == "cash" &
      profiles$consolidation == "general_government_sector",
    ,
    drop = FALSE
  ]
  observations <- read_au_budget_myefo_series(
    "underlying-cash-balance",
    release = history_latest_budget,
    unit = "AUD billion"
  )
  observations <- observations[
    observations$profile_type == profile$profile_type[[1L]] &
      observations$unit_multiplier == profile$unit_multiplier[[1L]] &
      observations$accounting_basis == profile$accounting_basis[[1L]] &
      observations$consolidation == profile$consolidation[[1L]],
    ,
    drop = FALSE
  ]

  expect_equal(nrow(profile), 1L)
  expect_identical(
    profile$profile_id,
    paste0(
      "budget-myefo-series-profile-v2-",
      "171d282193147b06a3d20dffc77b8153742ecab42a0cd3aa6e10622f9ddee9a4"
    )
  )
  expect_equal(nrow(observations), profile$observation_count)
  expect_equal(anyDuplicated(observations$period_end), 0L)
  expect_identical(min(observations$period_end), profile$first_period)
  expect_identical(max(observations$period_end), profile$last_period)
  expect_true(all(observations$actual_forecast == profile$actual_forecast))
  expect_true(all(observations$source_sha256 == profile$source_sha256))

  direct <- read_au_budget_myefo_series_profile(profile$profile_id[[1L]])
  expect_equal(direct, observations, ignore_attr = TRUE)
  provenance <- attr(direct, "readfiscalau_series_profile_provenance")
  expect_identical(provenance$profile_id, profile$profile_id[[1L]])
  expect_identical(provenance$snapshot_schema, "budget_myefo_history_v3")
  expect_identical(provenance$source_sha256, profile$source_sha256[[1L]])
})

test_that("series-profile filters are exact and typed", {
  receipts <- list_au_budget_myefo_series_profiles(
    publication = "budget",
    family = "detailed_cash_receipts",
    actual_forecast = "forecast"
  )

  expect_gt(nrow(receipts), 0L)
  expect_true(all(receipts$publication == "budget"))
  expect_true(all(receipts$actual_forecast == "forecast"))
  expect_true(all(vapply(
    strsplit(receipts$family_ids, " | ", fixed = TRUE),
    function(values) "detailed_cash_receipts" %in% values,
    logical(1)
  )))
  expect_error(
    list_au_budget_myefo_series_profiles(profile_type = "final"),
    class = "readfiscalau_invalid_argument"
  )
  expect_error(
    list_au_budget_myefo_series_profiles(actual_forecast = "mixed"),
    class = "readfiscalau_invalid_argument"
  )
  expect_error(
    list_au_budget_myefo_series_profiles(profile_type = "actual"),
    class = "readfiscalau_invalid_argument"
  )
})

test_that("actual histories are package-constructed across releases", {
  histories <- list_au_budget_myefo_actual_histories()
  agricultural <- list_au_budget_myefo_actual_histories(
    family = "detailed_cash_receipts",
    series = "agricultural-levies",
    unit = "AUD million",
    accounting_basis = "cash",
    consolidation = "general_government_sector"
  )

  expect_s3_class(histories, "tbl_df")
  expect_equal(nrow(histories), 670L)
  expect_equal(sum(histories$observation_count), 10566L)
  expect_equal(sum(histories$candidate_observation_count), 19814L)
  expect_equal(sum(histories$superseded_observation_count), 9248L)
  expect_equal(sum(histories$revised_period_count), 730L)
  expect_equal(anyDuplicated(histories$history_id), 0L)
  expect_true(all(histories$actual_forecast == "actual"))
  expect_true(all(histories$profile_type == "actual"))
  expect_true(all(
    histories$chain_policy == "latest_budget_myefo_actual_then_final_outcome_v2"
  ))
  expect_true(all(grepl(
    "^budget-myefo-actual-history-v1-[0-9a-f]{64}$",
    histories$history_id
  )))
  expect_true(all(grepl(
    "^budget-myefo-actual-provenance-v1-[0-9a-f]{64}$",
    histories$provenance_id
  )))

  expect_equal(nrow(agricultural), 1L)
  expect_identical(
    agricultural$history_id,
    paste0(
      "budget-myefo-actual-history-v1-",
      "a23fe18c08c76c85013342e73f99094991251486da135898a1143529251aa122"
    )
  )
  expect_identical(agricultural$first_period, as.Date("2004-06-30"))
  expect_identical(agricultural$last_period, as.Date("2025-06-30"))
  expect_identical(agricultural$observation_count, 22L)
  expect_identical(agricultural$candidate_observation_count, 107L)
  expect_identical(agricultural$superseded_observation_count, 85L)
  expect_identical(agricultural$source_release_count, 3L)
  expect_identical(agricultural$revised_period_count, 0L)

  actual <- read_au_budget_myefo_actual_history(
    agricultural$history_id[[1L]]
  )
  expect_equal(nrow(actual), 22L)
  expect_equal(anyDuplicated(actual$period_end), 0L)
  expect_equal(
    actual$period_end,
    as.Date(sprintf("%d-06-30", 2004:2025))
  )
  expect_equal(actual$value, c(
    603, 584, 610, 608, 611, 620, 395, 445, 421, 463, 495,
    515, 517, 547, 555, 556, 469, 525, 626, 666, 637, 664
  ))
  expect_true(all(actual$actual_forecast == "actual"))
  expect_true(all(actual$profile_type == "actual"))
  expect_true(all(actual$accounting_basis == "cash"))
  expect_identical(
    actual$source_kind,
    c(rep("pdf_table_cell", 2L), rep("treasury_chart_data_cell", 20L))
  )
  provenance <- attr(actual, "readfiscalau_actual_history_provenance")
  expect_identical(provenance$history_id, agricultural$history_id[[1L]])
  expect_identical(provenance$snapshot_schema, "budget_myefo_history_v3")
  expect_identical(
    provenance$provenance_id,
    agricultural$provenance_id[[1L]]
  )
  expect_equal(nrow(provenance$source_releases), 3L)
  expect_setequal(
    provenance$source_releases$source_kind,
    c("pdf_table_cell", "treasury_chart_data_cell")
  )
})

test_that("historical Budget balance chart has one path per selected vintage", {
  selected <- c(
    "2004-05", "2008-09", "2012-13", "2016-17",
    "2020-21", "2024-25", "2026-27"
  )
  balance <- read_au_budget_myefo_series(
    "underlying-cash-balance",
    publication = "budget",
    unit = "% of GDP"
  )
  chart_data <- balance[
    balance$budget_vintage %in% selected &
      balance$family_id == "ggs_budget_aggregates" &
      balance$profile_type == "budget" &
      !is.na(balance$period_end),
    ,
    drop = FALSE
  ]

  expect_setequal(unique(chart_data$budget_vintage), selected)
  expect_equal(anyDuplicated(chart_data[c("release_id", "period_end")]), 0L)
  expect_true(all(chart_data$actual_forecast == "forecast"))
  expect_true(all(chart_data$unit == "% of GDP"))

  actual_data <- readfiscalau:::.rf_parse_pbo_historical_snapshot(
    list_au_fiscal_releases(source = "pbo"), "pbo_historical_table_01"
  )
  actual_data <- actual_data[
    actual_data$series_id ==
      "pbo-hfd--table-01--row-024--underlying-cash-balance--of-gdp" &
      actual_data$period_end >= min(chart_data$period_end),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(actual_data), 22L)
  expect_equal(anyDuplicated(actual_data$period_end), 0L)
  expect_true(all(actual_data$actual_forecast == "actual"))
  expect_true(all(actual_data$unit == "% of GDP"))
  expect_equal(range(actual_data$period_end), as.Date(c(
    "2004-06-30", "2025-06-30"
  )))
})

test_that("summary and detailed economic forecasts share canonical IDs", {
  preferred <- read_au_budget_myefo_series(
    "real-gross-domestic-product",
    release = history_latest_budget
  )
  all_sources <- read_au_budget_myefo_series(
    "real-gross-domestic-product",
    release = history_latest_budget,
    all_sources = TRUE
  )

  expect_true(all(c(
    "major_economic_parameters",
    "domestic_economy_detailed_forecasts"
  ) %in% unique(all_sources$family_id)))
  expect_gt(nrow(all_sources), nrow(preferred))
  expect_identical(unique(preferred$family_id),
                   "major_economic_parameters")
  expect_identical(unique(all_sources$series_id),
                   "real-gross-domestic-product")
})

test_that("MYEFO profiles and complete-table totals retain their meaning", {
  series <- read_au_budget_myefo_series(
    "real-gross-domestic-product",
    publication = "myefo",
    release = history_latest_myefo,
    all_sources = TRUE
  )
  bridge <- read_au_budget_myefo_table(
    history_latest_myefo,
    "underlying_to_headline_cash_bridge"
  )

  expect_true(all(c("actual", "budget", "myefo") %in%
                  unique(series$profile_type)))
  totals <- bridge[bridge$column_label == "Total", , drop = FALSE]
  expect_gt(nrow(totals), 0L)
  expect_false(any(totals$series_eligible))
  expect_true(any(
    totals$series_id == "headline-cash-balance" & totals$value == -237076
  ))
})

test_that("volatile reconciliation and Table 3.13 remain complete tables", {
  cells <- read_au_budget_myefo_table(
    history_latest_budget,
    "expense_estimates_reconciliation"
  )
  text <- read_au_budget_myefo_table(
    history_latest_budget,
    "expense_estimates_reconciliation",
    format = "text"
  )
  coverage <- list_au_budget_myefo_series(
    family = "expense_estimates_reconciliation"
  )

  expect_gt(nrow(cells), 0L)
  expect_false(any(cells$series_eligible))
  expect_equal(nrow(coverage), 0L)
  expect_gt(nrow(text), 0L)
  expect_true(all(nzchar(text$raw_text)))
  expect_true(all(grepl("expense estimates", text$source_table,
                        ignore.case = TRUE)))
})

test_that("publication vintages and prose are not mistaken for table values", {
  parameters <- read_au_budget_myefo_table(
    "treasury-myefo-2023-24-final",
    "tax_receipts_economic_parameters"
  )
  nominal <- parameters[
    parameters$series_id == "nominal-gross-domestic-product" &
      parameters$series_eligible,
    ,
    drop = FALSE
  ]
  debt <- read_au_budget_myefo_table(
    "treasury-budget-2021-22-final",
    "net_debt_reconciliation"
  )
  current_debt <- debt[
    debt$row_label == "Net debt as at 2021-22 Budget",
    ,
    drop = FALSE
  ]
  series <- list_au_budget_myefo_series()

  expect_identical(nominal$fiscal_year, c(
    "2022-23", "2023-24", "2024-25", "2025-26", "2026-27"
  ))
  expect_true(all(is.na(nominal$calendar_year)))
  expect_equal(nominal$value, c(9.8, 4.25, 2.25, 4.5, 5))
  expect_identical(current_debt$fiscal_year, c(
    "2020-21", "2021-22", "2022-23", "2023-24"
  ))
  expect_equal(current_debt$value, c(617.5, 729, 835, 920.4))
  expect_false(any(grepl(
    "climate spending is shown on an aggregated basis",
    series$series,
    ignore.case = TRUE
  )))
  expect_false(any(grepl(
    "^factors affecting the change in face value",
    series$series,
    ignore.case = TRUE
  )))
  expect_lte(max(nchar(series$series)), 100L)
  expect_false(any(tolower(series$series) %in% c(
    "part", "page", "statement", "table"
  )))
})

test_that("historical table readers are memoized, local, and side-effect free", {
  readfiscalau:::.rf_clear_internal_file_cache()
  on.exit(readfiscalau:::.rf_clear_internal_file_cache(), add = TRUE)
  before <- list.files(".", all.files = TRUE, no.. = TRUE)

  first <- list_au_budget_myefo_families()
  cache_keys <- ls(
    envir = readfiscalau:::.rf_internal_file_cache,
    all.names = TRUE
  )
  first$family_id[[1L]] <- "mutated"
  second <- list_au_budget_myefo_families()

  expect_length(cache_keys, 1L)
  expect_false(second$family_id[[1L]] == "mutated")
  expect_identical(
    ls(
      envir = readfiscalau:::.rf_internal_file_cache,
      all.names = TRUE
    ),
    cache_keys
  )
  expect_identical(list.files(".", all.files = TRUE, no.. = TRUE), before)
})

test_that("historical table readers use typed selection errors", {
  expect_error(
    list_au_budget_myefo_tables(family = "not-a-family"),
    class = "readfiscalau_table_family"
  )
  expect_error(
    list_au_budget_myefo_series(series = "not-a-series"),
    class = "readfiscalau_series_not_found"
  )
  expect_error(
    list_au_budget_myefo_tables(publication = "pefo"),
    class = "readfiscalau_invalid_argument"
  )
  expect_error(
    read_au_budget_myefo_table(
      "treasury-budget-2000-01-final",
      "forecast_ags_on_issue"
    ),
    class = "readfiscalau_table_required"
  )
  expect_error(
    read_au_budget_myefo_series(
      "underlying-cash-balance",
      all_sources = NA
    ),
    class = "readfiscalau_invalid_argument"
  )
})
