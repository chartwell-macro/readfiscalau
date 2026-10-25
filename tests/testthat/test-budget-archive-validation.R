test_that("archive inventory fails closed on missing metadata and duplicate source vintages", {
  inventory <- tibble::tibble(
    source_url = "https://budget.gov.au/2025-26/content/files/bp1.pdf",
    archive_path = "/archive/objects/abc",
    sha256 = paste(rep("a", 64L), collapse = ""),
    byte_size = 3,
    retrieved_at = "2026-07-28 00:00:00 UTC",
    media_type = "application/pdf",
    budget_vintage = "2025-26",
    revision_status = "original",
    license = "Copyright",
    attribution = "© Commonwealth of Australia"
  )
  expect_silent(validate_au_budget_archive_inventory(inventory))

  incomplete <- inventory
  incomplete$sha256 <- NA_character_
  expect_error(validate_au_budget_archive_inventory(incomplete), class = "readfiscalau_schema")

  duplicate <- rbind(inventory, inventory)
  expect_error(validate_au_budget_archive_inventory(duplicate), class = "readfiscalau_schema")
})

test_that("archive inventory rejects disconnected replacement cycles", {
  base <- tibble::tibble(
    archive_file_id = c("root", "revision-b", "revision-c"),
    replaces_archive_file_id = c("", "revision-c", "revision-b"),
    source_url = rep(
      "https://budget.gov.au/2025-26/content/files/bp1.pdf",
      3L
    ),
    archive_path = paste0("/archive/objects/", c("a", "b", "c")),
    sha256 = vapply(c("a", "b", "c"), function(value) {
      paste(rep(value, 64L), collapse = "")
    }, character(1)),
    byte_size = c(1, 2, 3),
    retrieved_at = rep("2026-07-28 00:00:00 UTC", 3L),
    media_type = rep("application/pdf", 3L),
    budget_vintage = rep("2025-26", 3L),
    revision_status = c("original", "replacement", "replacement"),
    license = rep("Copyright", 3L),
    attribution = rep("© Commonwealth of Australia", 3L)
  )

  expect_error(
    validate_au_budget_archive_inventory(base),
    class = "readfiscalau_schema"
  )
})

test_that("archive validation reports complete indexes and explicit missing documents", {
  candidates <- archive_fixture_candidates()
  payloads <- stats::setNames(
    list(
      charToRaw("%PDF-1.4 object 1"),
      charToRaw("%PDF-1.4 object 2"),
      charToRaw("year,value\\n2025-26,1\\n")
    ),
    candidates$source_url
  )
  root <- local_tempdir()
  inventory <- archive_au_budget_documents(
    candidates, root, fetcher = archive_fixture_fetcher(payloads)
  )
  indexes <- tibble::tibble(
    budget_vintage = "2025-26",
    index_url = "https://budget.gov.au/2025-26/content/downloads.htm",
    index_snapshot_id = "fixture-index"
  )
  complete <- validate_au_budget_archive(root, inventory, indexes, candidates)

  expect_true(isTRUE(complete$complete))
  expect_equal(nrow(complete$missing), 0L)
  expect_equal(nrow(complete$unexpected), 0L)
  expect_equal(nrow(complete$gaps), 0L)

  partial <- inventory[-1, ]
  gap <- validate_au_budget_archive(root, partial, indexes, candidates)
  expect_false(isTRUE(gap$complete))
  expect_equal(gap$missing$source_url, candidates$source_url[[1]])
})

test_that("archive validation detects a tampered stored object", {
  candidates <- archive_fixture_candidates()[1, ]
  payloads <- stats::setNames(
    list(charToRaw("%PDF-1.4 official bytes")), candidates$source_url
  )
  root <- local_tempdir()
  inventory <- archive_au_budget_documents(
    candidates, root, fetcher = archive_fixture_fetcher(payloads)
  )
  writeBin(charToRaw("tampered bytes"), inventory$archive_path[[1]])
  indexes <- tibble::tibble(
    budget_vintage = "2025-26",
    index_url = "https://budget.gov.au/2025-26/content/downloads.htm",
    index_snapshot_id = "fixture-index"
  )

  expect_error(
    validate_au_budget_archive(root, inventory, indexes, candidates),
    class = "readfiscalau_archive_corrupt"
  )
})

test_that("continue_on_error records a durable explicit gap and admits later candidates", {
  candidates <- archive_fixture_candidates()[1:2, ]
  payloads <- list(
    "https://budget.gov.au/2025-26/content/files/bp1.pdf" = list(
      raw = raw(), status = 404L, content_type = "text/html",
      final_url = "https://budget.gov.au/2025-26/content/files/bp1.pdf",
      retrieved_at = "2026-07-28 00:00:00 UTC"
    ),
    "https://budget.gov.au/2025-26/content/files/bp1-replacement.pdf" = list(
      raw = charToRaw("%PDF-1.4 available corrected file"), status = 200L,
      content_type = "application/pdf",
      final_url = "https://budget.gov.au/2025-26/content/files/bp1-replacement.pdf",
      retrieved_at = "2026-07-28 00:00:00 UTC"
    )
  )
  fetcher <- function(url) payloads[[url]]
  root <- local_tempdir()
  admitted <- archive_au_budget_documents(
    candidates, root, fetcher = fetcher, continue_on_error = TRUE
  )
  gaps <- attr(admitted, "budget_archive_gaps")
  persisted_gaps <- utils::read.csv(
    file.path(root, "inventory", "budget-archive-gaps.csv"),
    stringsAsFactors = FALSE
  )

  expect_equal(nrow(admitted), 1L)
  expect_equal(admitted$source_url, candidates$source_url[[2]])
  expect_equal(nrow(gaps), 1L)
  expect_equal(gaps$source_url, candidates$source_url[[1]])
  expect_equal(gaps$gap_type, "readfiscalau_http")
  expect_match(gaps$gap_reason, "HTTP 404")
  expect_equal(persisted_gaps$source_url, candidates$source_url[[1]])

  validation <- validate_au_budget_archive(
    root, inventory = admitted, candidates = candidates
  )
  expect_true(validation$complete)
  expect_equal(nrow(validation$missing), 0L)
})
