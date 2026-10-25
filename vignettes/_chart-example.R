## ----treasury-chart, eval=FALSE
library(readfiscalau)

treasury_releases <- list_au_fiscal_releases(
  source = "treasury",
  release_type = "budget"
)
treasury_release <- treasury_releases[
  treasury_releases$release_id == "treasury-budget-2024-25-final",
  ,
  drop = FALSE
]
stopifnot(nrow(treasury_release) == 1L)

treasury_budget <- read_au_budget(
  treasury_release,
  table_id = "Table 1",
  cache_dir = tempfile("readfiscalau-chart-cache-")
)

# Select one published series and convert AUD million to AUD billions.
chart_data <- treasury_budget[
  treasury_budget$series == "Total receipts",
  c("period_end", "value"),
  drop = FALSE
]
chart_data$value_billions <- chart_data$value / 1000

if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(
    chart_data,
    ggplot2::aes(x = period_end, y = value_billions)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Australian Government total receipts",
      subtitle = "Annual published observations",
      x = "Financial year ending",
      y = "AUD billions",
      caption = "Source: Australian Treasury, Budget 2024–25. © Commonwealth of Australia"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}

## ----budget-balance-vintages
selected_vintages <- c(
  "2004-05", "2008-09", "2012-13", "2016-17",
  "2020-21", "2024-25", "2026-27"
)

budget_balance <- read_au_budget_myefo_series(
  "underlying-cash-balance",
  publication = "budget",
  unit = "% of GDP"
)
chart_data <- budget_balance[
  budget_balance$budget_vintage %in% selected_vintages &
    budget_balance$family_id == "ggs_budget_aggregates" &
    budget_balance$profile_type == "budget" &
    !is.na(budget_balance$period_end),
  c("period_end", "value", "budget_vintage"),
  drop = FALSE
]
chart_data$series <- chart_data$budget_vintage

actual_data <- read_au_pbo_historical("pbo_historical_table_01")
actual_data <- actual_data[
  actual_data$series_id ==
    "pbo-hfd--table-01--row-024--underlying-cash-balance--of-gdp" &
    actual_data$period_end >= min(chart_data$period_end),
  c("period_end", "value"),
  drop = FALSE
]

if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = actual_data,
      ggplot2::aes(x = period_end, y = value, linetype = "Actuals"),
      colour = "black"
    ) +
    ggplot2::geom_line(
      data = chart_data,
      linetype = "dotted",
      ggplot2::aes(
        x = period_end, y = value, colour = series, group = series
      )
    ) +
    ggplot2::scale_linetype_manual(values = c("Actuals" = "solid")) +
    ggplot2::labs(
      title = "Underlying cash balance: actuals and Budget vintages",
      subtitle = "Published outcomes and selected forecast paths",
      x = "Financial year ending",
      y = "% of GDP",
      colour = "Budget vintage",
      linetype = NULL,
      caption = "Sources: Treasury Budget Paper 1; PBO Historical Fiscal Data"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}
