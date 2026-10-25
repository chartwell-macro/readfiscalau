test_that("confirmed extraction fragments share the canonical concept", {
  histories <- list_au_budget_myefo_actual_histories()
  expect_false(any(histories$series_id %in% c(
    "gross-income-tax-w-ithholding", "incom-e-taxation-receipts",
    "dw-elling-investment", "of-which-other-excisablebeverages", "billion",
    "net-lending-borrowing",
    "other-than-fuels-manufacturing-and-construction"
  )))
  withholding <- list_au_budget_myefo_actual_histories(
    series = "gross-income-tax-withholding", unit = "AUD million",
    accounting_basis = "cash"
  )
  expect_equal(nrow(withholding), 1L)
  values <- read_au_budget_myefo_actual_history(withholding$history_id)
  expect_equal(values$value[values$period_end == as.Date("2019-06-30")], 204764)
  expect_equal(nrow(values), 22L)
})

test_that("unit-only current account rows retain their parent and units", {
  cells <- read_au_budget_myefo_table(
    "treasury-budget-2000-01-final", "domestic_economy_detailed_forecasts"
  )
  current <- cells[cells$series_id == "current-account-balance", ]
  dollars <- current[current$unit == "AUD billion", ]
  percent <- current[current$unit == "% of GDP", ]
  expect_equal(dollars$value, c(-32.8, -33.5, -31.5))
  expect_equal(dollars$unit_multiplier, rep(1e9, 3))
  expect_equal(percent$value, c(-5.5, -5.5, -4.75))
  expect_true(all(dollars$row_label == "$billion"))
  expect_true(all(grepl("Current account balance", dollars$row_path, fixed = TRUE)))
})

test_that("financial-statement sectors and accounting bases do not collapse", {
  cells <- read_au_budget_myefo_table(
    "treasury-budget-2022-23-october-final", "formal_financial_statements"
  )
  borrowing <- cells[cells$series_id == "borrowing" &
    cells$consolidation == "public_financial_corporations_sector", ]
  expect_equal(borrowing$value, c(12923, 13485))
  expect_true(all(borrowing$is_preferred))
  expect_true(all(borrowing$accounting_basis == "accrual"))
  expect_identical(borrowing$actual_forecast, c("actual", "forecast"))
  expect_setequal(unique(cells$consolidation), c(
    "general_government_sector", "public_non_financial_corporations_sector",
    "total_non_financial_public_sector", "public_financial_corporations_sector"
  ))
  cash <- grepl("cash flow", cells$source_table, ignore.case = TRUE)
  expect_true(all(cells$accounting_basis[cash] == "cash"))
  fiscal <- cells[cells$series_id == "fiscal-balance" &
    cells$consolidation == "general_government_sector" &
    cells$actual_forecast == "actual", ]
  expect_equal(fiscal$value, -35061)
})

test_that("Final Budget Outcome fills only real missing observations", {
  histories <- list_au_budget_myefo_actual_histories()
  borrowing <- histories[histories$series_id == "borrowing" &
    histories$consolidation == "public_financial_corporations_sector", ]
  expect_equal(nrow(borrowing), 1L)
  expect_identical(borrowing$coverage_type, "continuous")
  expect_identical(borrowing$missing_year_count, 0L)
  expect_identical(borrowing$observation_count, 17L)
  values <- read_au_budget_myefo_actual_history(borrowing$history_id)
  expect_identical(values$period_end, as.Date(sprintf("%d-06-30", 2009:2025)))
  # Independently read from the official Outcome balance-sheet columns.
  expect_equal(values$value, c(4191,6966,4811,5167,7596,9532,6968,11170,
    6955,5399,5170,5224,6693,12923,20182,8220,9952))
  expect_identical(values$publication[values$period_end == as.Date("2021-06-30")], "fbo")
  expect_identical(values$publication[values$period_end == as.Date("2022-06-30")], "budget")
  expect_true(all(histories$coverage_type[histories$observation_count == 1L] == "single_observation"))
  expect_true(all(histories$missing_year_count[histories$coverage_type == "continuous"] == 0L))
})
