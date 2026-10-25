test_that("PBO forecast snapshot is comprehensive and source-pinned", {
  profiles <- list_au_pbo_budget_myefo_profiles()

  expect_s3_class(profiles, "tbl_df")
  expect_equal(nrow(profiles), 1212L)
  expect_equal(sum(profiles$observation_count), 6057L)
  expect_equal(length(unique(profiles$release_id)), 53L)
  expect_equal(anyDuplicated(profiles$profile_id), 0L)
  expect_setequal(
    unique(profiles$publication),
    c("budget", "myefo", "pefo", "other")
  )
  expect_setequal(
    unique(profiles$table_ids),
    c(
      "pbo-budget-estimates",
      "pbo-economic-forecasts",
      "pbo-medium-term-projections"
    )
  )
  expect_true(all(
    profiles$snapshot_schema == "pbo_budget_myefo_history_v1"
  ))
  expect_true(all(profiles$actual_forecast == "forecast"))
  expect_true(all(profiles$first_period <= profiles$last_period))
  expect_true(all(profiles$observation_count >= 1L))
  expect_true(all(grepl(
    "^pbo-budget-myefo-profile-v1-[0-9a-f]{64}$",
    profiles$profile_id
  )))
  expect_true(all(
    profiles$source_sha256 ==
      "0fec89ace2845bc7c0c5c6712c16a3f75681ca082886068ccd81cbc64513aba0"
  ))
  expect_true(all(grepl("^https://www[.]pbo[.]gov[.]au/", profiles$source_url)))
})

test_that("one PBO profile replays exactly without app-side stitching", {
  profiles <- list_au_pbo_budget_myefo_profiles(
    publication = "myefo",
    series = "pbo-forecast--underlying-cash-balance"
  )
  selected <- profiles[
    profiles$update_label == "2025-26 MYEFO" &
      profiles$unit == "AUD million" &
      profiles$accounting_basis == "cash",
    ,
    drop = FALSE
  ]
  observations <- read_au_pbo_budget_myefo_profile(
    selected$profile_id[[1L]]
  )

  expect_equal(nrow(selected), 1L)
  expect_equal(nrow(observations), 4L)
  expect_identical(
    observations$period_end,
    as.Date(sprintf("%d-06-30", 2026:2029))
  )
  expect_equal(observations$value, c(-36764, -34303, -36189, -35993))
  expect_true(all(observations$actual_forecast == "forecast"))
  expect_true(all(observations$profile_type == "myefo"))
  expect_true(all(observations$source_table == "Table 2"))
  expect_equal(anyDuplicated(observations$period_end), 0L)

  provenance <- attr(
    observations,
    "readfiscalau_pbo_budget_myefo_provenance"
  )
  expect_identical(provenance$profile_id, selected$profile_id[[1L]])
  expect_identical(provenance$snapshot_schema, "pbo_budget_myefo_history_v1")
  expect_identical(provenance$update_label, "2025-26 MYEFO")
  expect_identical(provenance$source_sha256, selected$source_sha256[[1L]])
})

test_that("PBO forecast filters remain exact and separate from actual history", {
  pefo <- list_au_pbo_budget_myefo_profiles(publication = "pefo")
  other <- list_au_pbo_budget_myefo_profiles(publication = "other")
  debt <- list_au_pbo_budget_myefo_profiles(
    table = "pbo-medium-term-projections",
    series = "pbo-forecast--net-debt"
  )

  expect_equal(nrow(pefo), 25L)
  expect_equal(nrow(other), 10L)
  expect_gt(nrow(debt), 0L)
  expect_true(all(grepl(
    "pbo-medium-term-projections",
    debt$table_ids,
    fixed = TRUE
  )))
  expect_true(all(debt$actual_forecast == "forecast"))
  expect_error(
    list_au_pbo_budget_myefo_profiles(publication = "actual"),
    class = "readfiscalau_invalid_argument"
  )
  expect_error(
    read_au_pbo_budget_myefo_profile("not-a-profile"),
    class = "readfiscalau_invalid_argument"
  )
})
