# Contract tests for the durable Budget document archive.  These remain fully
# offline: production discovery must work from a captured index snapshot.

test_that("Budget archive discovery keeps every direct document link", {
  index_url <- "https://budget.gov.au/2025-26/content/downloads.htm"
  out <- discover_au_budget_archive_index(
    fixture_raw("budget-archive-index.html"),
    index_url = index_url,
    budget_vintage = "2025-26",
    index_snapshot_id = "budget-2025-26-index-2025-03-25"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 6L)
  expect_setequal(
    out$source_url,
    c(
      "https://budget.gov.au/2025-26/content/files/bp1-budget-strategy-and-outlook.pdf",
      "https://budget.gov.au/2025-26/content/files/bp2-budget-measures.pdf",
      "https://budget.gov.au/2025-26/content/data/budget-2025-26-chart-data.xlsx",
      "https://budget.gov.au/2025-26/content/data/budget-2025-26-data.csv?download=1",
      "https://budget.gov.au/2025-26/content/files/budget-2025-26-supplementary.zip",
      "https://budget.gov.au/2025-26/content/files/bp2-budget-measures-corrected.pdf"
    )
  )
  expect_true(all(out$index_url == index_url))
  expect_true(all(out$budget_vintage == "2025-26"))
  expect_true(all(out$index_snapshot_id == "budget-2025-26-index-2025-03-25"))
  expect_true(all(grepl("^https://", out$source_url)))
  expect_true(all(c("title", "document_role", "revision_status") %in% names(out)))
})

test_that("Budget archive discovery records replacement chains without dropping originals", {
  out <- discover_au_budget_archive_index(
    fixture_raw("budget-archive-index.html"),
    index_url = "https://budget.gov.au/2025-26/content/downloads.htm",
    budget_vintage = "2025-26",
    index_snapshot_id = "budget-2025-26-index-2025-03-25"
  )

  bp2 <- out[grepl("Budget Paper No. 2", out$title, fixed = TRUE), ]
  expect_equal(nrow(bp2), 2L)
  expect_setequal(bp2$revision_status, c("original", "replacement"))
  expect_true(any(grepl("corrected", bp2$source_url, ignore.case = TRUE)))
  expect_true(all(nzchar(bp2$document_role)))
})

test_that("Budget archive discovery records insecure links without following them", {
  out <- discover_au_budget_archive_index(
    fixture_raw("budget-archive-insecure.html"),
    index_url = "https://budget.gov.au/2025-26/content/downloads.htm",
    budget_vintage = "2025-26",
    index_snapshot_id = "fixture-insecure-index"
  )
  gaps <- attr(out, "budget_archive_link_gaps")

  expect_equal(nrow(out), 0L)
  expect_equal(nrow(gaps), 1L)
  expect_equal(gaps$gap_type, "insecure_first_party_link")
  expect_match(gaps$source_url, "^http://budget\\.gov\\.au/")
})

test_that("bundled Budget catalogue is internally reconciled", {
  catalogue <- read_au_budget_archive_catalogue()

  expect_named(
    catalogue,
    c("metadata", "candidates", "indexes", "gaps")
  )
  expect_equal(
    nrow(catalogue$candidates),
    catalogue$metadata$counts$candidates
  )
  expect_equal(nrow(catalogue$indexes), catalogue$metadata$counts$indexes)
  expect_equal(nrow(catalogue$gaps), catalogue$metadata$counts$explicit_gaps)
  expect_true(all(file.exists(catalogue$indexes$snapshot_path)))
  expect_equal(
    sum(file.info(unique(catalogue$indexes$snapshot_path))$size),
    catalogue$metadata$bundled_index_bytes
  )
  expect_equal(anyDuplicated(catalogue$candidates$source_url), 0L)
  expect_true(all(c("1901-02", "2000-01", "2026-27") %in%
                    catalogue$candidates$budget_vintage))

  unavailable <- catalogue$candidates$source_url[
    catalogue$candidates$availability != "available"
  ]
  expect_true(all(unavailable %in% catalogue$gaps$source_url))
  filtered <- list_au_budget_archive_candidates("2000-01")
  expect_true(nrow(filtered) > 0L)
  expect_true(all(filtered$budget_vintage == "2000-01"))
})
