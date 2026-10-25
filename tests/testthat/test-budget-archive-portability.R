test_that("archive capacity preflight tolerates unavailable platform probes", {
  result <- testthat::with_mocked_bindings(
    readfiscalau:::.rf_archive_assert_capacity(
      archive_dir = tempfile("readfiscalau-capacity-"),
      required_bytes = 1,
      reserve_bytes = 0
    ),
    .rf_archive_free_bytes = function(path) NA_real_,
    .package = "readfiscalau"
  )

  expect_type(result, "list")
  expect_identical(result$required_bytes, 1)
  expect_identical(result$reserve_bytes, 0)
  expect_true(is.na(result$available_bytes))
})

test_that("an unavailable archive probe is warning-free and advisory", {
  old_path <- Sys.getenv("PATH", unset = NA_character_)
  on.exit(
    if (is.na(old_path)) Sys.unsetenv("PATH") else Sys.setenv(PATH = old_path),
    add = TRUE
  )
  Sys.setenv(PATH = tempfile("readfiscalau-empty-path-"))

  expect_silent(
    available <- readfiscalau:::.rf_archive_free_bytes(tempdir())
  )
  expect_true(is.na(available))
})

test_that("explicit archive capacity overrides remain deterministic", {
  expect_error(
    readfiscalau:::.rf_archive_assert_capacity(
      archive_dir = tempfile("readfiscalau-capacity-"),
      required_bytes = 10,
      reserve_bytes = 0,
      available_bytes = 9
    ),
    class = "readfiscalau_archive_capacity"
  )
  expect_error(
    readfiscalau:::.rf_archive_assert_capacity(
      archive_dir = tempfile("readfiscalau-capacity-"),
      required_bytes = 1,
      reserve_bytes = 0,
      available_bytes = NA_real_
    ),
    class = "readfiscalau_archive_capacity"
  )
})

test_that("archive HTTP options leave protocol negotiation to libcurl", {
  implementation <- paste(
    deparse(body(readfiscalau:::.rf_archive_fetch_file)),
    collapse = "\n"
  )
  expect_false(grepl("http_version = 2L", implementation, fixed = TRUE))
})

test_that("archive URL validation rejects hostless HTTPS spellings", {
  result <- tryCatch(
    readfiscalau:::.rf_archive_canonical_url(
      "https:///budget.gov.au/example.pdf"
    ),
    error = identity
  )

  expect_s3_class(result, "readfiscalau_schema")
  expect_match(
    conditionMessage(result),
    "credential-free HTTPS URLs",
    fixed = TRUE
  )
  expect_false("url" %in% names(result))
  expect_false(grepl("budget.gov.au/example.pdf", conditionMessage(result),
                     fixed = TRUE))
  expect_identical(
    readfiscalau:::.rf_archive_canonical_url(
      "https://budget.gov.au/example.pdf"
    ),
    "https://budget.gov.au/example.pdf"
  )
})

test_that("malformed archive URL errors are sanitized", {
  malformed <- "https://%"
  result <- tryCatch(
    readfiscalau:::.rf_archive_canonical_url(malformed),
    error = identity
  )

  expect_s3_class(result, "readfiscalau_schema")
  expect_false("url" %in% names(result))
  expect_false(grepl(malformed, conditionMessage(result), fixed = TRUE))
  expect_false(grepl("parse", conditionMessage(result), fixed = TRUE))
})

test_that("archive URL errors do not retain credentials recursively", {
  secret <- "archive-url-secret"
  result <- tryCatch(
    readfiscalau:::.rf_archive_canonical_url(
      paste0("https://archive-user:", secret,
             "@budget.gov.au/example.pdf")
    ),
    error = identity
  )

  contains_secret <- function(value) {
    if (is.character(value)) {
      return(any(grepl(secret, value, fixed = TRUE)))
    }
    if (is.list(value) || is.pairlist(value)) {
      return(any(vapply(as.list(value), contains_secret, logical(1))))
    }
    FALSE
  }

  expect_s3_class(result, "readfiscalau_schema")
  expect_match(
    conditionMessage(result),
    "credential-free HTTPS URLs",
    fixed = TRUE
  )
  expect_false(contains_secret(result))
  expect_false("url" %in% names(result))
})
