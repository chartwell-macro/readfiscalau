history_release <- function(
    release_id,
    release_date,
    period_end,
    fiscal_year,
    resource_role = "budget_chart_csv_archive") {
  out <- fixture_release(
    release_id = release_id,
    release_date = release_date,
    period_end = period_end,
    fiscal_year = fiscal_year
  )
  out$accounting_basis <- "cash"
  out$resource_role <- resource_role
  out$resource_id <- paste0("resource-", release_id)
  out
}

history_fixture <- function(headers, values) {
  charToRaw(paste0(
    ",", paste(headers, collapse = ","), "\n",
    "Total receipts,", paste(values, collapse = ","), "\n",
    "Series with NA,", paste(c(values[-length(values)], ""), collapse = ","),
    "\n"
  ))
}

history_manifest <- function() {
  old <- history_release(
    "budget-2024", "2024-05-14", "2025-06-30", "2024-25",
    "budget_table_1_cash_receipts_csv"
  )
  middle <- history_release(
    "budget-2025", "2025-03-25", "2026-06-30", "2025-26"
  )
  latest <- history_release(
    "budget-2026", "2026-05-12", "2027-06-30", "2026-27"
  )
  rbind(old, middle, latest)
}

history_raw <- function() {
  list(
    "budget-2024" = history_fixture(
      c("2021-22 ($m)", "2022-23 ($m)", "2023-24 (est) ($m)"),
      c(100, 200, 300)
    ),
    "budget-2025" = history_fixture(
      c("2021-22 ($m)", "2022-23 ($m)", "2023-24 ($m)",
        "2024-25 (est) ($m)"),
      c(101, 200, 305, 400)
    ),
    "budget-2026" = history_fixture(
      c("2021-22 ($m)", "2022-23 ($m)", "2023-24 ($m)",
        "2024-25 ($m)", "2025-26 (est) ($m)"),
      c(101, 202, 306, 410, 500)
    )
  )
}

test_that("canonical Budget history resolves overlaps by latest actual vintage", {
  out <- read_au_budget_history(
    "budget-2026",
    table_id = "Table 1",
    series = "Total receipts",
    manifest = history_manifest(),
    raw = history_raw(),
    cache_dir = local_tempdir()
  )

  expect_s3_class(out, "readfiscalau_history")
  expect_equal(out$fiscal_year,
               c("2021-22", "2022-23", "2023-24", "2024-25"))
  expect_equal(out$value, c(101, 202, 306, 410))
  expect_equal(out$release_id,
               rep("budget-2026", 4L))
  expect_equal(out$revision_count, c(2L, 2L, 2L, 1L))
  expect_true(all(out$actual_forecast == "actual"))
  expect_true(all(out$accounting_basis == "cash"))
  expect_true(all(out$unit == "AUD million"))
  expect_true(all(out$consolidation == "general_government_sector"))
  expect_true(all(out$selected_vintage))

  provenance <- attr(out, "readfiscalau_history_provenance")
  expect_equal(provenance$earliest_period, as.Date("2022-06-30"))
  expect_equal(provenance$latest_period, as.Date("2025-06-30"))
  expect_equal(
    provenance$observation_period_coverage,
    "complete_as_published_by_terminal_release"
  )
  expect_false(provenance$release_vintage_coverage_complete)
  expect_equal(nrow(provenance$release_coverage), 3L)
  expect_true(all(provenance$release_coverage$observation_present))
})

test_that("history API preserves representative official Budget values", {
  release <- history_release(
    "budget-official-shaped", "2026-05-12", "2027-06-30", "2026-27"
  )
  out <- read_au_budget_history(
    "budget-official-shaped",
    table_id = "Australian Government (cash) receipts",
    series = "Total receipts",
    manifest = release,
    raw = list(
      "budget-official-shaped" = fixture_raw("budget-current.csv")
    ),
    cache_dir = local_tempdir()
  )
  expect_equal(
    out$value[out$fiscal_year == "2023-24"],
    688585
  )
  expect_equal(
    out$value[out$fiscal_year == "2024-25"],
    716951
  )
})

test_that("Budget history retains explicit vintages and never splices forecasts", {
  actual <- read_au_budget_history(
    "budget-2026", "cash_receipts", "total_receipts",
    vintages = "all",
    manifest = history_manifest(),
    raw = history_raw(),
    cache_dir = local_tempdir()
  )
  expect_setequal(unique(actual$release_id),
                  c("budget-2024", "budget-2025", "budget-2026"))
  expect_true(all(actual$actual_forecast == "actual"))
  expect_equal(
    actual$value[actual$fiscal_year == "2022-23"],
    c(200, 200, 202)
  )

  forecast <- read_au_budget_history(
    "budget-2026", "1", "Total receipts",
    observation = "forecast",
    manifest = history_manifest(),
    raw = history_raw(),
    cache_dir = local_tempdir()
  )
  expect_true(all(forecast$actual_forecast == "forecast"))
  expect_equal(forecast$fiscal_year,
               c("2023-24", "2024-25", "2025-26"))
  expect_equal(forecast$value, c(300, 400, 500))
})

test_that("Budget history preserves NA and records absent historical labels", {
  raw <- history_raw()
  raw[["budget-2024"]] <- charToRaw(
    ",2021-22 ($m),2022-23 ($m)\nOther name,1,2\n"
  )
  raw[["budget-2026"]] <- charToRaw(paste0(
    ",2021-22 ($m),2022-23 ($m),2023-24 ($m),2024-25 ($m),",
    "2025-26 (est) ($m)\n",
    "Total receipts,101,202,306,410,500\n",
    "Series with NA,101,202,,410,\n"
  ))
  out <- read_au_budget_history(
    "budget-2026", "Table 1", "Series with NA",
    vintages = "all",
    manifest = history_manifest(),
    raw = raw,
    cache_dir = local_tempdir()
  )
  expect_true(anyNA(out$value))
  expect_false(any(out$value == 0, na.rm = TRUE))
  coverage <- attr(out, "readfiscalau_history_provenance")$release_coverage
  expect_false(coverage$series_present[coverage$release_id == "budget-2024"])
})

test_that("period-row percentage tables preserve status, unit, and values", {
  raw <- charToRaw(paste0(
    ",Total receipts (%),Company tax (%)\n",
    "1978-79,22,2.6\n",
    "2022-23,25.7,5.3\n",
    "2024-25 (est),25.8,5.0\n"
  ))
  release <- fixture_release(
    release_id = "period-row",
    release_date = "2024-05-14",
    period_end = "2025-06-30",
    fiscal_year = "2024-25"
  )
  release$accounting_basis <- "cash"
  out <- read_au_budget(
    release,
    raw = raw,
    cache_dir = local_tempdir(),
    table_id = "Table 2"
  )
  expect_equal(nrow(out), 6L)
  expect_equal(range(out$period_end),
               as.Date(c("1979-06-30", "2025-06-30")))
  expect_equal(unique(out$unit), "% of GDP")
  expect_equal(unique(out$unit_multiplier), 0.01)
  expect_equal(
    out$value[out$series == "Total receipts" &
      out$fiscal_year == "1978-79"],
    22
  )
  expect_equal(sum(out$actual_forecast == "forecast"), 2L)
})

test_that("history fails closed across table, unit, and same-vintage conflicts", {
  expect_error(
    read_au_budget_history(
      "budget-2026", "an arbitrary chart", "Total receipts",
      manifest = history_manifest(), raw = history_raw(),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_table_required"
  )

  manifest <- history_manifest()
  manifest$accounting_basis[manifest$release_id == "budget-2025"] <- "accrual"
  expect_error(
    read_au_budget_history(
      "budget-2026", "Table 1", "Total receipts",
      manifest = manifest, raw = history_raw(),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_history_contract"
  )

  collision <- history_release(
    "budget-2026-copy", "2026-05-12", "2027-06-30", "2026-27",
    "budget_table_1_cash_receipts_csv"
  )
  manifest <- rbind(history_manifest(), collision)
  raw <- history_raw()
  raw[["budget-2026-copy"]] <- history_fixture(
    c("2024-25 ($m)", "2025-26 (est) ($m)"),
    c(999, 500)
  )
  expect_error(
    read_au_budget_history(
      "budget-2026", "Table 1", "Total receipts",
      manifest = manifest, raw = raw,
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_vintage_collision"
  )
})

test_that("new archive pins are exact and tampered bytes fail closed", {
  releases <- list_au_fiscal_releases()
  pinned <- releases[
    grepl("^treasury-budget-2024-25-final-table-", releases$release_id),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(pinned), 4L)
  expect_setequal(
    pinned$resource_role,
    paste0(
      "budget_table_", 1:4,
      c("_cash_receipts_csv", "_cash_receipts_gdp_csv",
        "_accrual_revenue_csv", "_accrual_revenue_gdp_csv")
    )
  )
  expect_true(all(grepl("^[a-f0-9]{64}$", pinned$expected_sha256)))
  expect_equal(
    pinned$resource_updated_at,
    rep("2025-03-11T04:38:16Z", 4L)
  )

  expect_error(
    read_au_budget(
      pinned[1, , drop = FALSE],
      raw = charToRaw(",2022-23 ($m)\nTotal receipts,1\n"),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_source_hash"
  )
})
