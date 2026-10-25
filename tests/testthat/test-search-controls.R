test_that("search controls are compact and chart-ready", {
  controls <- list_au_fiscal_search_controls()

  expect_s3_class(controls, "tbl_df")
  expect_identical(
    names(controls),
    c("control", "group", "value", "label")
  )
  expect_equal(nrow(controls), 42L)
  expect_false(anyNA(controls))
  expect_true(all(nzchar(controls$control)))
  expect_true(all(nzchar(controls$value)))
  expect_true(all(nzchar(controls$label)))
  expect_equal(anyDuplicated(controls[c("control", "value")]), 0L)
  expect_lt(as.numeric(object.size(controls)), 16 * 1024)

  history_families <- list_au_budget_myefo_families()
  history_series <- list_au_budget_myefo_series()
  history_profiles <- list_au_budget_myefo_series_profiles()
  pbo_forecasts <- list_au_pbo_budget_myefo_profiles()
  pbo <- list_au_pbo_historical_series()
  table_controls <- controls[controls$control == "table", ]
  data_type_controls <- controls[controls$control == "data_type", ]

  expect_setequal(
    table_controls$value,
    c(
      unlist(strsplit(
        history_profiles$family_ids,
        " | ",
        fixed = TRUE
      )),
      unlist(strsplit(
        pbo_forecasts$table_ids,
        " | ",
        fixed = TRUE
      )),
      pbo$table_id,
      "monthly_fiscal_history"
    )
  )
  expect_setequal(
    data_type_controls$value,
    c(history_series$unit, pbo_forecasts$unit, pbo$unit)
  )

  history_ids <- unique(unlist(strsplit(
    history_profiles$family_ids,
    " | ",
    fixed = TRUE
  )))
  history_labels <- history_families[
    history_families$family_id %in% history_ids,
    ,
    drop = FALSE
  ]
  coverage_labels <- unique(rbind(
    data.frame(
      value = history_labels$family_id,
      label = history_labels$family_label,
      stringsAsFactors = FALSE
    ),
    data.frame(
      value = pbo_forecasts$table_ids,
      label = pbo_forecasts$table_labels,
      stringsAsFactors = FALSE
    ),
    data.frame(
      value = pbo$table_id,
      label = pbo$table_label,
      stringsAsFactors = FALSE
    )
  ))
  package_rows <- match(coverage_labels$value, table_controls$value)
  expect_false(anyNA(package_rows))
  expect_identical(
    table_controls$label[package_rows],
    coverage_labels$label
  )

  unique_history <- history_labels
  history_rows <- match(unique_history$family_id, table_controls$value)
  expect_false(anyNA(history_rows))
  expect_identical(
    table_controls$label[history_rows],
    unique_history$family_label
  )
})

test_that("search controls do not materialize series coverage", {
  local_mocked_bindings(
    list_au_budget_series = function(...) stop("budget coverage loaded"),
    list_au_myefo_series = function(...) stop("MYEFO coverage loaded"),
    list_au_budget_myefo_families = function(...) {
      stop("Budget/MYEFO history loaded")
    },
    list_au_pbo_budget_myefo_profiles = function(...) {
      stop("PBO forecast coverage loaded")
    },
    list_au_pbo_historical_series = function(...) stop("PBO coverage loaded")
  )

  expect_no_error(list_au_fiscal_search_controls())
})
