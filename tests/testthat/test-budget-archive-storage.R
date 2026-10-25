test_that("archive store is hash addressed and never overwrites a replacement", {
  candidates <- archive_fixture_candidates()
  payloads <- list(
    "https://budget.gov.au/2025-26/content/files/bp1.pdf" = charToRaw("%PDF-1.4 original PDF bytes"),
    "https://budget.gov.au/2025-26/content/files/bp1-replacement.pdf" = charToRaw("%PDF-1.4 corrected PDF bytes"),
    "https://budget.gov.au/2025-26/content/data/chart-data.csv" = charToRaw("year,value\\n2025-26,1\\n")
  )
  root <- local_tempdir()
  inventory <- archive_au_budget_documents(
    candidates, archive_dir = root, fetcher = archive_fixture_fetcher(payloads)
  )

  expect_equal(nrow(inventory), 3L)
  expect_true(all(grepl("^[a-f0-9]{64}$", inventory$sha256)))
  expect_true(all(file.exists(inventory$archive_path)))
  expect_equal(inventory$byte_size, as.numeric(file.info(inventory$archive_path)$size))
  expect_equal(length(unique(inventory$sha256)), 3L)
  expect_setequal(inventory$revision_status, c("original", "replacement"))
  expect_true(all(c("retrieved_at", "media_type", "license", "attribution") %in% names(inventory)))
})

test_that("archive deduplicates equal bytes while retaining both source vintages", {
  candidates <- archive_fixture_candidates()[1:2, ]
  repeated <- charToRaw("%PDF-1.4 same official bytes")
  payloads <- stats::setNames(
    list(repeated, repeated),
    candidates$source_url
  )
  inventory <- archive_au_budget_documents(
    candidates, archive_dir = local_tempdir(),
    fetcher = archive_fixture_fetcher(payloads)
  )

  expect_equal(nrow(inventory), 2L)
  expect_equal(length(unique(inventory$sha256)), 1L)
  expect_equal(length(unique(inventory$archive_path)), 1L)
  expect_setequal(inventory$source_url, candidates$source_url)
})

test_that("a later retrieval appends a replacement admission without mutating history", {
  candidates <- archive_fixture_candidates()[1, ]
  root <- local_tempdir()
  original <- archive_au_budget_documents(
    candidates, root,
    fetcher = archive_fixture_fetcher(stats::setNames(
      list(charToRaw("%PDF-1.4 original issued bytes")), candidates$source_url
    ))
  )
  replacement <- archive_au_budget_documents(
    candidates, root,
    fetcher = archive_fixture_fetcher(stats::setNames(
      list(charToRaw("%PDF-1.4 corrected issued bytes")), candidates$source_url
    ))
  )
  ledger <- utils::read.csv(
    file.path(root, "inventory", "budget-archive-inventory.csv"),
    stringsAsFactors = FALSE
  )

  expect_equal(nrow(original), 1L)
  expect_equal(nrow(replacement), 1L)
  expect_equal(replacement$revision_status, "replacement")
  expect_equal(replacement$replaces_archive_file_id, original$archive_file_id)
  expect_true(file.exists(original$archive_path))
  expect_true(file.exists(replacement$archive_path))
  expect_false(identical(original$sha256, replacement$sha256))
  expect_equal(nrow(ledger), 2L)
  expect_setequal(ledger$archive_file_id,
                  c(original$archive_file_id, replacement$archive_file_id))
})

test_that("archive capacity preflight fails before any document fetch", {
  candidates <- archive_fixture_candidates()[1, ]
  fetch_count <- 0L
  fetcher <- function(url) {
    fetch_count <<- fetch_count + 1L
    stop("capacity validation should run before fetch", call. = FALSE)
  }

  expect_error(
    archive_au_budget_documents(
      candidates, archive_dir = local_tempdir(), fetcher = fetcher,
      required_bytes = 20, reserve_bytes = 5, available_bytes = 24
    ),
    class = "readfiscalau_archive_capacity"
  )
  expect_equal(fetch_count, 0L)
  expect_error(
    archive_au_budget_documents(
      candidates, archive_dir = local_tempdir(), fetcher = fetcher,
      required_bytes = 20, reserve_bytes = 5, available_bytes = Inf
    ),
    class = "readfiscalau_archive_capacity"
  )
  expect_equal(fetch_count, 0L)
})

test_that("archive object writer verifies immutable content hashes", {
  root <- local_tempdir()
  first <- readfiscalau:::.rf_archive_store_object(charToRaw("one"), root)
  second <- readfiscalau:::.rf_archive_store_object(charToRaw("two"), root)

  expect_true(file.exists(first$archive_path))
  expect_match(first$sha256, "^[a-f0-9]{64}$")
  expect_false(identical(first$archive_path, second$archive_path))
  expect_equal(
    digest::digest(file = first$archive_path, algo = "sha256", serialize = FALSE),
    first$sha256
  )
})

test_that("archive terms are not inferred for external official hosts", {
  budget_terms <- readfiscalau:::.rf_archive_terms(
    "https://archive.budget.gov.au/2025-26/bp1.pdf"
  )
  external_terms <- readfiscalau:::.rf_archive_terms(
    "https://www.dss.gov.au/publications/budget-paper.pdf"
  )

  expect_match(budget_terms$license, "Creative Commons Attribution 4.0")
  expect_equal(
    external_terms$license,
    "Not stated in captured official Budget index"
  )
  expect_match(external_terms$attribution, "www.dss.gov.au", fixed = TRUE)
})
