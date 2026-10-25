test_that("transport rejects HTTPS userinfo before any fetch", {
  secret <- "not-for-error-output"
  calls <- 0L
  result <- tryCatch(
    readfiscalau:::.rf_fetch(
      paste0("https://fixture-user:", secret, "@example.gov.au/data.csv"),
      cache_dir = local_tempdir(),
      fetcher = function(url) {
        calls <<- calls + 1L
        stop("the credential check must run before the fetcher", call. = FALSE)
      }
    ),
    error = identity
  )
  expect_s3_class(result, "readfiscalau_transport")
  expect_match(
    conditionMessage(result),
    "Only HTTPS source URLs are accepted.",
    fixed = TRUE
  )
  expect_false(grepl(secret, conditionMessage(result), fixed = TRUE))
  expect_identical(calls, 0L)
})

test_that("transport leaves HTTP version negotiation to libcurl", {
  implementation <- paste(
    deparse(body(readfiscalau:::.rf_fetch)),
    collapse = "\n"
  )
  expect_false(grepl("http_version = 2L", implementation, fixed = TRUE))
})

test_that("transport validates HTTPS authority without narrowing valid URLs", {
  calls <- 0L
  hostless <- tryCatch(
    readfiscalau:::.rf_fetch(
      "https:///path?query=value#fragment",
      cache_dir = local_tempdir(),
      fetcher = function(url) {
        calls <<- calls + 1L
        stop("the authority check must run before the fetcher", call. = FALSE)
      }
    ),
    error = identity
  )
  expect_s3_class(hostless, "readfiscalau_transport")
  expect_match(
    conditionMessage(hostless),
    "Only HTTPS source URLs are accepted.",
    fixed = TRUE
  )
  expect_identical(calls, 0L)

  valid <- readfiscalau:::.rf_fetch(
    "https://example.gov.au:8443/data.csv?query=value#fragment",
    cache_dir = local_tempdir(),
    fetcher = function(url) {
      list(
        raw = charToRaw("series,2026-27 ($m)\nTotal,1\n"),
        status = 200L,
        content_type = "text/csv",
        final_url = url,
        retrieved_at = "2026-08-24 00:00:00 UTC"
      )
    }
  )
  expect_true(is.list(valid))
  expect_identical(valid$source_url,
                   "https://example.gov.au:8443/data.csv?query=value#fragment")
})

test_that("transport rejects credential-bearing final URLs before persistence", {
  secret <- "final-url-secret"
  cache <- local_tempdir()
  result <- tryCatch(
    readfiscalau:::.rf_fetch(
      "https://example.gov.au/data.csv",
      cache_dir = cache,
      fetcher = function(url) {
        list(
          raw = charToRaw("series,2026-27 ($m)\nTotal,1\n"),
          status = 200L,
          content_type = "text/csv",
          final_url = paste0("https://user:", secret, "@example.gov.au/data.csv"),
          retrieved_at = "2026-08-24 00:00:00 UTC"
        )
      }
    ),
    error = identity
  )
  expect_s3_class(result, "readfiscalau_transport")
  expect_match(
    conditionMessage(result),
    "credential-free HTTPS URL",
    fixed = TRUE
  )
  expect_false(grepl(secret, conditionMessage(result), fixed = TRUE))
  expect_length(list.files(cache, all.files = FALSE), 0L)
})

test_that("discovery does not return credential-bearing URLs", {
  secret <- "discovery-secret"
  html <- charToRaw(paste0(
    "<html><body><a href='https://user:", secret,
    "@example.gov.au/budget-2026-27.csv'>Budget 2026-27</a></body></html>"
  ))
  result <- tryCatch(
    readfiscalau::discover_au_fiscal_releases(
      source = "treasury", raw = html,
      url = "https://budget.gov.au/content/downloads.htm"
    ),
    error = identity
  )
  expect_s3_class(result, "readfiscalau_schema")
  expect_false(grepl(secret, conditionMessage(result), fixed = TRUE))

  payload <- jsonlite::toJSON(
    list(
      success = TRUE,
      result = list(
        license_title = "fixture",
        resources = list(list(
          id = "fixture-resource", name = "Fixture", format = "CSV",
          url = paste0("https://user:", secret, "@example.gov.au/data.csv"),
          created = "2026-01-01", last_modified = "2026-01-02"
        ))
      )
    ),
    auto_unbox = TRUE
  )
  result <- tryCatch(
    readfiscalau::discover_finance_ckan_resources(charToRaw(payload)),
    error = identity
  )
  expect_s3_class(result, "readfiscalau_schema")
  expect_false(grepl(secret, conditionMessage(result), fixed = TRUE))
})
