test_that("cache paths are hash addressed and corrupt targets are repaired", {
  cache <- local_tempdir()
  bytes <- charToRaw("exact release bytes")
  first <- readfiscalau:::.rf_write_atomic(bytes, cache, "csv")

  expect_match(basename(first$path), "^[a-f0-9]{64}\\.csv$")
  writeBin(charToRaw("corrupt"), first$path)
  expect_false(
    identical(
      digest::digest(file = first$path, algo = "sha256", serialize = FALSE),
      first$sha256
    )
  )

  repaired <- readfiscalau:::.rf_write_atomic(bytes, cache, "csv")
  expect_equal(repaired$path, first$path)
  expect_identical(
    digest::digest(file = repaired$path, algo = "sha256", serialize = FALSE),
    first$sha256
  )
})

test_that("cache pruning bounds age, bytes, and file count", {
  cache <- local_tempdir()
  for (i in 1:5) {
    readfiscalau:::.rf_write_atomic(
      charToRaw(paste(rep(as.character(i), 20), collapse = "")),
      cache, "bin"
    )
  }
  readfiscalau_cache_prune(cache, max_bytes = Inf, max_files = 2L)
  files <- list.files(cache, pattern = "^[a-f0-9]{64}\\.bin$")
  expect_lte(length(files), 2L)

  readfiscalau_cache_prune(cache, max_bytes = 1, max_files = 100L)
  sizes <- file.info(list.files(cache, full.names = TRUE))$size
  expect_lte(sum(sizes), 1)

  old <- readfiscalau:::.rf_write_atomic(charToRaw("old"), cache, "bin")
  Sys.setFileTime(old$path, Sys.time() - 3600)
  fresh <- readfiscalau:::.rf_write_atomic(charToRaw("fresh"), cache, "bin")
  readfiscalau_cache_prune(
    cache,
    max_bytes = Inf,
    max_files = 100L,
    max_age_seconds = 60,
    preserve = fresh$path
  )
  expect_false(file.exists(old$path))
  expect_true(file.exists(fresh$path))
})

test_that("fixture transport enforces HTTPS and response byte limits", {
  fetcher <- function(url) {
    list(
      raw = charToRaw("12345"),
      status = 200L,
      content_type = "text/csv",
      final_url = url,
      retrieved_at = "2026-07-26 00:00:00 UTC"
    )
  }
  expect_error(
    readfiscalau:::.rf_fetch(
      "http://example.gov.au/data.csv",
      cache_dir = local_tempdir(),
      fetcher = fetcher
    ),
    class = "readfiscalau_transport"
  )
  expect_error(
    readfiscalau:::.rf_fetch(
      "https://example.gov.au/data.csv",
      cache_dir = local_tempdir(),
      max_bytes = 4L,
      fetcher = fetcher
    ),
    class = "readfiscalau_too_large"
  )

  unavailable <- function(url) {
    list(
      raw = charToRaw("unavailable"),
      status = 503L,
      content_type = "text/plain",
      final_url = url,
      retrieved_at = "2026-07-26 00:00:00 UTC"
    )
  }
  expect_error(
    readfiscalau:::.rf_fetch(
      "https://example.gov.au/data.csv",
      cache_dir = local_tempdir(),
      fetcher = unavailable
    ),
    class = "readfiscalau_http"
  )
})

test_that("pinned hashes and declared media fail before cache publication", {
  cache <- local_tempdir()
  release <- fixture_release()
  release$expected_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    read_au_budget(
      release,
      raw = fixture_raw("budget-current.csv"),
      cache_dir = cache
    ),
    class = "readfiscalau_source_hash"
  )
  expect_length(list.files(cache), 0L)

  expect_error(
    read_au_budget(
      fixture_release(),
      raw = charToRaw("<html><body>not csv</body></html>"),
      cache_dir = cache
    ),
    class = "readfiscalau_unsupported_media"
  )
  expect_length(list.files(cache), 0L)
})

test_that("release snapshots and cache keys cannot cross release boundaries", {
  cache <- local_tempdir()
  bytes <- fixture_raw("budget-current.csv")
  first_release <- fixture_release(release_id = "fixture-budget-first")
  second_release <- fixture_release(
    release_id = "fixture-budget-second",
    release_date = "2026-05-13"
  )

  first <- read_au_budget(
    first_release,
    raw = bytes,
    cache_dir = cache
  )
  second <- read_au_budget(
    second_release,
    raw = bytes,
    cache_dir = cache
  )
  first_provenance <- attr(first, "readfiscalau_provenance")
  second_provenance <- attr(second, "readfiscalau_provenance")

  expect_equal(
    first_provenance$raw_content_sha256,
    second_provenance$raw_content_sha256
  )
  expect_false(identical(
    first_provenance$raw_snapshot_path,
    second_provenance$raw_snapshot_path
  ))
  expect_false(identical(
    first_provenance$cache_namespace_sha256,
    second_provenance$cache_namespace_sha256
  ))
  expect_true(file.exists(first_provenance$raw_snapshot_path))
  expect_true(file.exists(second_provenance$raw_snapshot_path))
})

test_that("verified release snapshots are reused without another fetch", {
  cache <- local_tempdir()
  bytes <- fixture_raw("budget-current.csv")
  release <- fixture_release()
  release$expected_sha256 <- digest::digest(
    bytes,
    algo = "sha256",
    serialize = FALSE
  )
  fetch_count <- 0L
  fetcher <- function(url) {
    fetch_count <<- fetch_count + 1L
    list(
      raw = bytes,
      status = 200L,
      content_type = "text/csv",
      final_url = url,
      retrieved_at = "2026-07-28 00:00:00 UTC"
    )
  }

  first <- read_au_budget(
    release,
    fetcher = fetcher,
    cache_dir = cache
  )
  first_provenance <- attr(first, "readfiscalau_provenance")
  expect_equal(fetch_count, 1L)
  expect_false(first_provenance$cache_hit)
  expect_true(file.exists(
    paste0(first_provenance$raw_snapshot_path, ".metadata.json")
  ))

  second <- read_au_budget(
    release,
    fetcher = function(url) {
      stop("warm cache unexpectedly called the fetcher")
    },
    cache_dir = cache
  )
  second_provenance <- attr(second, "readfiscalau_provenance")
  expect_equal(fetch_count, 1L)
  expect_true(second_provenance$cache_hit)
  attr(second, "readfiscalau_provenance") <- NULL
  attr(first, "readfiscalau_provenance") <- NULL
  expect_equal(second, first)
  expect_identical(
    second_provenance$raw_content_sha256,
    first_provenance$raw_content_sha256
  )
  expect_identical(
    second_provenance$raw_snapshot_path,
    first_provenance$raw_snapshot_path
  )
  expect_identical(
    second_provenance$retrieved_at,
    first_provenance$retrieved_at
  )

  writeLines(
    "{not valid json",
    paste0(first_provenance$raw_snapshot_path, ".metadata.json")
  )
  metadata_repaired <- read_au_budget(
    release,
    fetcher = fetcher,
    cache_dir = cache
  )
  expect_equal(fetch_count, 2L)
  expect_false(
    attr(metadata_repaired, "readfiscalau_provenance")$cache_hit
  )

  writeBin(charToRaw("corrupt"), first_provenance$raw_snapshot_path)
  repaired <- read_au_budget(
    release,
    fetcher = fetcher,
    cache_dir = cache
  )
  repaired_provenance <- attr(repaired, "readfiscalau_provenance")
  expect_equal(fetch_count, 3L)
  expect_false(repaired_provenance$cache_hit)
  attr(repaired, "readfiscalau_provenance") <- NULL
  expect_equal(repaired, first)
  expect_identical(
    digest::digest(
      file = repaired_provenance$raw_snapshot_path,
      algo = "sha256",
      serialize = FALSE
    ),
    release$expected_sha256
  )
})

test_that("path-based cached fetches keep hashes without retaining source bytes", {
  cache <- local_tempdir()
  bytes <- c(as.raw(c(0x50, 0x4b, 0x03, 0x04)), charToRaw("fixture"))
  expected_hash <- digest::digest(
    bytes,
    algo = "sha256",
    serialize = FALSE
  )
  fetch_count <- 0L
  fetcher <- function(url) {
    fetch_count <<- fetch_count + 1L
    list(
      raw = bytes,
      status = 200L,
      content_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      final_url = url,
      retrieved_at = "2026-07-29 00:00:00 UTC"
    )
  }
  arguments <- list(
    url = "https://example.gov.au/source.xlsx",
    cache_dir = cache,
    expected_media_type =
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    expected_sha256 = expected_hash,
    cache_namespace = "path-based-fixture",
    materialize_raw = FALSE
  )

  first <- do.call(
    readfiscalau:::.rf_fetch,
    c(arguments, list(fetcher = fetcher))
  )
  second <- do.call(
    readfiscalau:::.rf_fetch,
    c(arguments, list(fetcher = function(url) {
      stop("warm cache unexpectedly called the fetcher")
    }))
  )

  expect_equal(fetch_count, 1L)
  expect_length(first$raw, 0L)
  expect_length(second$raw, 0L)
  expect_true(second$cache_hit)
  expect_identical(first$sha256, expected_hash)
  expect_identical(second$sha256, expected_hash)
  expect_true(file.exists(second$path))
})

test_that("partial and length-mismatched responses fail before publication", {
  cache <- local_tempdir()
  partial <- function(url) {
    list(
      raw = charToRaw("complete-looking,csv\nvalue,1\n"),
      status = 206L,
      content_type = "text/csv",
      final_url = url,
      retrieved_at = "2026-07-27 00:00:00 UTC"
    )
  }
  expect_error(
    readfiscalau:::.rf_fetch(
      "https://example.gov.au/data.csv",
      cache_dir = cache,
      expected_media_type = "text/csv",
      fetcher = partial
    ),
    class = "readfiscalau_truncated"
  )

  wrong_length <- function(url) {
    list(
      raw = charToRaw("complete-looking,csv\nvalue,1\n"),
      status = 200L,
      content_type = "text/csv",
      final_url = url,
      retrieved_at = "2026-07-27 00:00:00 UTC",
      content_length = 999L
    )
  }
  expect_error(
    readfiscalau:::.rf_fetch(
      "https://example.gov.au/data.csv",
      cache_dir = cache,
      expected_media_type = "text/csv",
      fetcher = wrong_length
    ),
    class = "readfiscalau_truncated"
  )
  expect_length(list.files(cache, recursive = TRUE), 0L)
})
