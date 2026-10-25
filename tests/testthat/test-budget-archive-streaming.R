# Streaming admission contract: an injected downloader may provide only a
# completed local file.  The archive must validate and hash that file directly,
# rather than materialising a second full raw vector in R.

streaming_archive_candidate <- function(url = "https://budget.gov.au/2025-26/content/files/bp1.pdf") {
  tibble::tibble(
    source_url = url,
    title = "Budget Paper No. 1",
    document_role = "budget_paper_1",
    budget_vintage = "2025-26",
    index_url = "https://budget.gov.au/2025-26/content/downloads.htm",
    index_snapshot_id = "streaming-fixture-index",
    revision_status = "original",
    media_type = "application/pdf"
  )
}

streaming_fixture_file <- function(bytes) {
  path <- tempfile("readfiscalau-streamed-")
  writeBin(bytes, path)
  path
}

test_that("archive admits a path-only streamed download with exact file hash", {
  source <- streaming_fixture_file(charToRaw("%PDF-1.4 streamed source bytes"))
  expected_hash <- digest::digest(
    file = source, algo = "sha256", serialize = FALSE
  )
  expected_size <- as.numeric(file.info(source)$size)
  fetcher <- function(url) list(
    path = source,
    status = 200L,
    content_type = "application/pdf",
    final_url = url,
    retrieved_at = "2026-07-28 00:00:00 UTC"
  )

  admitted <- archive_au_budget_documents(
    streaming_archive_candidate(), archive_dir = local_tempdir(),
    fetcher = fetcher
  )

  expect_equal(nrow(admitted), 1L)
  expect_equal(admitted$sha256, expected_hash)
  expect_equal(admitted$byte_size, expected_size)
  expect_true(file.exists(admitted$archive_path))
  expect_true(file.exists(source))
  expect_equal(
    digest::digest(file = admitted$archive_path, algo = "sha256", serialize = FALSE),
    expected_hash
  )
})

test_that("streamed file admission validates signatures before publishing objects", {
  source <- streaming_fixture_file(charToRaw("not a PDF document"))
  root <- local_tempdir()
  fetcher <- function(url) list(
    path = source,
    status = 200L,
    content_type = "application/pdf",
    final_url = url,
    retrieved_at = "2026-07-28 00:00:00 UTC"
  )

  expect_error(
    archive_au_budget_documents(
      streaming_archive_candidate(), archive_dir = root, fetcher = fetcher,
      continue_on_error = FALSE
    ),
    class = "readfiscalau_unsupported_media"
  )
  object_root <- file.path(root, "objects", "sha256")
  expect_false(dir.exists(object_root) && length(list.files(
    object_root, recursive = TRUE, all.files = FALSE
  )))
})

test_that("file-object admission atomically preserves existing object bytes", {
  root <- local_tempdir()
  first_source <- streaming_fixture_file(charToRaw("%PDF-1.4 first file"))
  second_source <- streaming_fixture_file(charToRaw("%PDF-1.4 second file"))

  first <- readfiscalau:::.rf_archive_store_file(first_source, root)
  second <- readfiscalau:::.rf_archive_store_file(second_source, root)

  expect_true(file.exists(first$archive_path))
  expect_true(file.exists(second$archive_path))
  expect_false(identical(first$archive_path, second$archive_path))
  expect_equal(first$byte_size, as.numeric(file.info(first$archive_path)$size))
  expect_equal(second$sha256, digest::digest(
    file = second$archive_path, algo = "sha256", serialize = FALSE
  ))
})

test_that("streaming admission checkpoints each successful document", {
  first_url <- "https://budget.gov.au/2025-26/content/files/bp1.pdf"
  second_url <- "https://budget.gov.au/2025-26/content/files/bp2.pdf"
  candidates <- rbind(
    streaming_archive_candidate(first_url),
    streaming_archive_candidate(second_url)
  )
  candidates$title[[2]] <- "Budget Paper No. 2"
  candidates$document_role[[2]] <- "budget_paper_2"
  fetcher <- function(url) list(
    path = streaming_fixture_file(if (identical(url, first_url)) {
      charToRaw("%PDF-1.4 checkpointed source")
    } else {
      charToRaw("malformed second source")
    }),
    status = 200L,
    content_type = "application/pdf",
    final_url = url,
    retrieved_at = "2026-07-28 00:00:00 UTC"
  )
  root <- local_tempdir()

  expect_error(
    archive_au_budget_documents(
      candidates, archive_dir = root, fetcher = fetcher,
      continue_on_error = FALSE
    ),
    class = "readfiscalau_unsupported_media"
  )
  ledger <- list_au_budget_archive(archive_dir = root)
  expect_equal(nrow(ledger), 1L)
  expect_equal(ledger$source_url, first_url)
  expect_true(file.exists(ledger$archive_path))
})

test_that("injected streaming paths reject symlinks and preserve source files", {
  source <- streaming_fixture_file(charToRaw("%PDF-1.4 immutable source"))
  link <- tempfile("readfiscalau-stream-link-")
  linked <- suppressWarnings(file.symlink(source, link))
  skip_if_not(linked, "Symbolic links are not available")
  on.exit(if (file.exists(link)) unlink(link), add = TRUE)
  fetcher <- function(url) list(
    path = link,
    status = 200L,
    content_type = "application/pdf",
    final_url = url,
    retrieved_at = "2026-07-28 00:00:00 UTC"
  )

  expect_error(
    archive_au_budget_documents(
      streaming_archive_candidate(),
      archive_dir = local_tempdir(),
      fetcher = fetcher,
      continue_on_error = FALSE
    ),
    class = "readfiscalau_transport"
  )
  expect_true(file.exists(source))
})

test_that("archive writer lock rejects concurrent mutations", {
  root <- local_tempdir()
  lock <- readfiscalau:::.rf_archive_acquire_lock(root, "fixture")
  on.exit(readfiscalau:::.rf_archive_release_lock(lock), add = TRUE)

  expect_error(
    archive_au_budget_documents(
      streaming_archive_candidate(),
      archive_dir = root,
      fetcher = function(url) stop("must not fetch")
    ),
    class = "readfiscalau_archive_locked"
  )
})
