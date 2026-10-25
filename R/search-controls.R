#' List the package's fiscal-data search controls
#'
#' Returns the stable, display-ready chartable table families and data types
#' supported by the package. The rows are a deliberately small application-metadata
#' projection: this function does not parse the bundled observation panels,
#' calculate series coverage, contact a publisher, or add UI-specific choices
#' such as "All tables" and "All data types".
#'
#' Table controls identify the canonical values accepted by the package's
#' series-profile coverage/readers (including the 19 Treasury Budget/MYEFO
#' history families with dated series, 3 PBO forecast-history table IDs, the
#' 11 PBO historical-actual table IDs, and monthly fiscal history).
#' Complete-table-only families remain available through the table APIs but
#' are omitted because they cannot populate a dated chart.
#' Data-type controls
#' identify the units used in the bundled panels and table-history snapshot;
#' their labels are values, not promises that every table has every unit.
#'
#' @returns A tibble with exactly four character columns, in this order:
#'   `control`, `group`, `value`, and `label`. There are 42 rows: 34 rows with
#'   `control == "table"` followed by 8 rows with `control == "data_type"`.
#'   Table rows have non-empty `group`, `value`, and `label` values; data-type
#'   rows intentionally use `group == ""`. Row order is stable and is part of
#'   the display contract.
#' @details The values are package metadata maintained with the supported
#'   schema families and bundled panels. The function reads no files, makes no
#'   network requests, does not use the source cache, and has no persistent
#'   side effects. It does not validate a user-supplied table or unit; readers
#'   and coverage functions perform those checks when called.
#' @seealso [list_au_budget_myefo_series_profiles()],
#'   [list_au_pbo_budget_myefo_profiles()],
#'   [list_au_pbo_historical_tables()], [read_au_fiscal_release()]
#' @family application-metadata
#' @examples
#' controls <- list_au_fiscal_search_controls()
#' print(controls[controls$control == "table", c("value", "label")][1:4, ],
#'       row.names = FALSE)
#' print(controls[controls$control == "data_type", c("value", "label")],
#'       row.names = FALSE)
#' stopifnot(
#'   nrow(controls) == 42L,
#'   identical(names(controls), c("control", "group", "value", "label")),
#'   all(vapply(controls, is.character, logical(1)))
#' )
#' @export
list_au_fiscal_search_controls <- function() {
  tibble::tibble(
    control = c(
      rep("table", 34L),
      rep("data_type", 8L)
    ),
    group = c(
      rep("Treasury Budget and MYEFO with Final Budget Outcome actuals", 19L),
      rep("PBO Budget and MYEFO forecast vintages", 3L),
      rep("PBO historical actuals", 11L),
      "Monthly history",
      rep("", 8L)
    ),
    value = c(
      "international_gdp_forecasts",
      "domestic_economy_detailed_forecasts",
      "major_economic_parameters",
      "ggs_budget_aggregates",
      "underlying_cash_balance_reconciliation",
      "underlying_to_headline_cash_bridge",
      "ggs_accrual_aggregates",
      "fiscal_balance_reconciliation",
      "detailed_cash_receipts",
      "tax_receipts_economic_parameters",
      "cash_receipts_reconciliation",
      "expenses_by_function_subfunction",
      "net_capital_investment_reconciliation",
      "forecast_ags_on_issue",
      "net_debt_components",
      "net_debt_reconciliation",
      "cash_interest",
      "accrual_interest",
      "formal_financial_statements",
      "pbo-economic-forecasts",
      "pbo-budget-estimates",
      "pbo-medium-term-projections",
      paste0("pbo_historical_table_", sprintf("%02d", 1:11)),
      "monthly_fiscal_history",
      "AUD million",
      "AUD billion",
      "% of GDP",
      "Per cent change",
      "Per cent",
      "Percentage points",
      "Persons",
      "Average staffing level"
    ),
    label = c(
      "International GDP growth forecasts",
      "Domestic economy - detailed forecasts",
      "Major economic parameters",
      "GGS Budget aggregates",
      "Underlying cash balance reconciliation",
      "Underlying-to-headline cash bridge",
      "GGS accrual aggregates",
      "Fiscal-balance reconciliation",
      "Detailed cash receipts",
      "Economic parameters for tax receipts",
      "Cash-receipts reconciliations",
      "Expenses by function and sub-function",
      "Net-capital-investment reconciliation",
      "Forecast AGS on issue",
      "Components of net debt",
      "Net-debt reconciliation",
      "Cash interest payments and receipts",
      "Accrual interest expense and income",
      "Formal financial statements",
      "Economic forecasts",
      "Budget estimates",
      "Medium-term projections",
      "Cash flow aggregates",
      "Debt, net interest payments and net worth",
      "Fiscal (accrual) aggregates",
      "Institutional sectors \u2014 cash flow and operating aggregates",
      "Revenue by head of revenue (accrual accounting)",
      "Receipts by head of revenue (cash accounting)",
      "Expenses by function and sub-function",
      "Historical cash flow data",
      "Historical debt data",
      "Medium-term projections data",
      "General government financial statements",
      "Monthly fiscal history",
      "AUD million",
      "AUD billion",
      "% of GDP",
      "Per cent change",
      "Per cent",
      "Percentage points",
      "Persons",
      "Average staffing level"
    )
  )
}
