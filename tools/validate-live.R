#!/usr/bin/env Rscript

if (!identical(Sys.getenv("READFISCALAU_RUN_LIVE"), "true")) {
  message(
    "Live validation is opt-in. Set READFISCALAU_RUN_LIVE=true to enable it."
  )
  quit(status = 0L)
}

fail <- function(...) {
  message(...)
  quit(status = 1L)
}

tryCatch({
  manifest <- readfiscalau::list_au_fiscal_releases()
  evidence <- utils::read.csv(
    system.file(
      "evidence",
      "value-crosscheck.csv",
      package = "readfiscalau"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expected_budget_ids <- c(
    "treasury-budget-2026-27-final",
    "treasury-budget-2025-26-final",
    "treasury-budget-2024-25-final",
    "treasury-budget-2023-24-final",
    "treasury-budget-2022-23-october-final",
    "treasury-budget-2022-23-march-final",
    "treasury-budget-2021-22-final",
    "treasury-budget-2020-21-final",
    "treasury-budget-2019-20-final",
    "treasury-budget-2018-19-final",
    "treasury-budget-2017-18-final",
    "treasury-budget-2016-17-final",
    "treasury-budget-2015-16-final",
    "treasury-budget-2014-15-final",
    "treasury-budget-2013-14-final",
    "treasury-budget-2012-13-final",
    "treasury-budget-2011-12-final",
    "treasury-budget-2010-11-final"
  )
  expected_ids <- c(
    expected_budget_ids,
    paste0(
      "treasury-budget-2024-25-final-table-",
      1:4
    ),
    "treasury-fbo-2024-25-final-history",
    "finance-mfs-2026-05-actual-aggregates",
    "finance-mfs-history-snapshot-2026-06-may-2008"
  )
  stopifnot(
    identical(sort(manifest$release_id), sort(expected_ids)),
    manifest$release_status[
      match(
        "finance-mfs-history-snapshot-2026-06-may-2008",
        manifest$release_id
      )
    ] == "snapshot",
    all(
      manifest$release_status[
        manifest$release_id !=
          "finance-mfs-history-snapshot-2026-06-may-2008"
      ] == "final"
    ),
    all(grepl("^https://", manifest$landing_url)),
    all(grepl("^https://", manifest$resource_url)),
    all(grepl("^[a-f0-9]{64}$", manifest$expected_sha256))
  )

  treasury <- readfiscalau::discover_au_fiscal_releases("treasury")
  finance <- readfiscalau::discover_au_fiscal_releases("finance")
  stopifnot(nrow(treasury) > 0L, nrow(finance) > 0L)
  message(
    "Official discovery passed: treasury_candidates=", nrow(treasury),
    "; finance_candidates=", nrow(finance)
  )

  budget_2026 <- readfiscalau::read_au_budget(
    "treasury-budget-2026-27-final",
    table_id = "Table 1"
  )
  budget_2025 <- readfiscalau::read_au_budget(
    "treasury-budget-2025-26-final",
    table_id = "Table 1"
  )
  budget_2010 <- readfiscalau::read_au_budget(
    "treasury-budget-2010-11-final",
    table_id = "Table 1"
  )
  budget_expected <- evidence[evidence$chart == "budget_receipts", ]
  budget_actual <- budget_2026[
    budget_2026$series == "Total receipts" &
      budget_2026$fiscal_year %in% budget_expected$period,
  ]
  budget_actual <- budget_actual[
    match(budget_expected$period, budget_actual$fiscal_year),
  ]
  budget_provenance <- attr(
    budget_2026, "readfiscalau_provenance", exact = TRUE
  )
  stopifnot(
    all(budget_2026$accounting_basis == "cash"),
    all(budget_2025$accounting_basis == "cash"),
    identical(budget_actual$value, budget_expected$value),
    identical(
      budget_actual$actual_forecast,
      budget_expected$actual_forecast
    ),
    identical(budget_actual$unit, budget_expected$unit),
    identical(
      budget_provenance$raw_content_sha256,
      unique(budget_expected$raw_sha256)
    ),
    budget_2025$value[
      budget_2025$series == "Total receipts" &
        budget_2025$fiscal_year == "2025-26"
    ] == 735353,
    budget_2010$value[
      budget_2010$series == "Total receipts" &
        budget_2010$fiscal_year == "2010-11"
    ] == 314417,
    identical(
      unique(budget_2010$raw_content_sha256),
      "a08c31df5bd296e81a6be4730244c288879cf6fce0cc6afe5bb58d60db9f4c36"
    )
  )
  message(
    "Budget live replay passed: ",
    paste(
      budget_expected$period,
      budget_expected$value,
      budget_expected$actual_forecast,
      sep = "=",
      collapse = ", "
    ),
    "; raw_sha256=", budget_provenance$raw_content_sha256
  )

  finance <- readfiscalau::read_au_monthly_financials(
    "finance-mfs-2026-05-actual-aggregates"
  )
  finance_expected <- evidence[evidence$chart == "finance_may", ]
  finance_actual <- finance[
    finance$period_end == as.Date("2026-05-31"),
  ]
  finance_actual <- finance_actual[
    match(finance_expected$series, finance_actual$series),
  ]
  finance_provenance <- attr(
    finance, "readfiscalau_provenance", exact = TRUE
  )
  stopifnot(
    all(finance$actual_forecast == "actual"),
    isTRUE(all.equal(
      finance_actual$value,
      finance_expected$value,
      tolerance = 1e-9
    )),
    identical(finance_actual$unit, finance_expected$unit),
    identical(
      finance_provenance$raw_content_sha256,
      unique(finance_expected$raw_sha256)
    )
  )
  finance_raw <- finance_provenance$raw_bytes
  history <- readfiscalau::read_au_monthly_financials(
    "finance-mfs-history-snapshot-2026-06-may-2008",
    raw = finance_raw
  )
  history_provenance <- attr(
    history, "readfiscalau_provenance", exact = TRUE
  )
  stopifnot(
    history$value[
      history$series == "Revenue" &
        history$period_end == as.Date("2008-05-31")
    ] == 283403,
    all(history$release_date == as.Date("2026-06-26")),
    min(history$period_end) == as.Date("2007-07-31"),
    max(history$period_end) == as.Date("2008-05-31"),
    !identical(
      finance_provenance$raw_snapshot_path,
      history_provenance$raw_snapshot_path
    ),
    !identical(
      finance_provenance$cache_namespace_sha256,
      history_provenance$cache_namespace_sha256
    ),
    identical(
      finance_provenance$contract_revision,
      readfiscalau:::.rf_contract_revision
    ),
    identical(
      finance_provenance$parser_revision,
      readfiscalau:::.rf_parser_revision
    )
  )
  message(
    "Finance live replay passed: ",
    paste(
      finance_expected$series,
      finance_expected$value,
      sep = "=",
      collapse = ", "
    ),
    "; raw_sha256=", finance_provenance$raw_content_sha256,
    "; historical_cache_isolated=true"
  )

  fbo <- readfiscalau::read_au_fbo(
    "treasury-fbo-2024-25-final-history"
  )
  fbo_provenance <- attr(fbo, "readfiscalau_provenance", exact = TRUE)
  monthly_history <- readfiscalau::read_au_monthly_financial_history(
    monthly_raw = finance_raw,
    annual_raw = fbo_provenance$raw_bytes
  )
  coverage <- attr(
    monthly_history,
    "readfiscalau_monthly_history_provenance",
    exact = TRUE
  )$coverage
  stopifnot(
    fbo$value[
      fbo$fiscal_year == "2000-01" &
        fbo$series_id == "receipts"
    ] == 182996,
    fbo$value[
      fbo$fiscal_year == "2024-25" &
        fbo$series_id == "fiscal_balance"
    ] == -44806,
    nrow(monthly_history) == 3617L,
    length(unique(monthly_history$fiscal_year)) == 26L,
    sum(monthly_history$is_june) == 218L,
    sum(is.na(monthly_history$value)) == 28L,
    sum(coverage$complete_fiscal_year) == 211L,
    max(coverage$fiscal_year[coverage$complete_fiscal_year]) == "2024-25",
    isTRUE(all.equal(
      monthly_history$value[
        monthly_history$fiscal_year == "2024-25" &
          monthly_history$series_id == "receipts" &
          monthly_history$is_june
      ],
      64137.173,
      tolerance = 1e-9
    ))
  )
  message(
    "Monthly history live replay passed: rows=", nrow(monthly_history),
    "; June joins=", sum(monthly_history$is_june),
    "; complete series-years=", sum(coverage$complete_fiscal_year),
    "; published/adjacent-YTD gaps=", sum(is.na(monthly_history$value)),
    "; FBO raw_sha256=", fbo_provenance$raw_content_sha256
  )
  message("readfiscalau live validation passed.")
}, error = function(e) {
  fail("readfiscalau live validation failed: ", conditionMessage(e))
})
