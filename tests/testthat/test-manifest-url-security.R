test_that("manifest rejects credentials in every URL field without redaction leaks", {
  secrets <- c("manifest-user", "manifest-secret")
  credential_urls <- c(
    landing_url = "https://manifest-user:manifest-secret@example.gov.au/release",
    resource_url = "https://manifest-user:manifest-secret@example.gov.au/resource.csv",
    terms_url = "https://manifest-user:manifest-secret@example.gov.au/terms"
  )

  for (field in names(credential_urls)) {
    manifest <- fixture_release()
    manifest[[field]] <- unname(credential_urls[[field]])
    error <- tryCatch(
      readfiscalau:::.rf_validate_manifest(manifest),
      error = identity
    )

    expect_s3_class(error, "readfiscalau_schema")
    expect_false(any(vapply(
      secrets,
      grepl,
      logical(1),
      x = conditionMessage(error),
      fixed = TRUE
    )))
  }
})

test_that("manifest rejects malformed and hostless URLs", {
  malformed_urls <- c(
    landing_url = "https:///release",
    resource_url = "https://example.gov.au/resource file.csv",
    terms_url = "https://?terms"
  )

  for (field in names(malformed_urls)) {
    manifest <- fixture_release()
    manifest[[field]] <- unname(malformed_urls[[field]])
    expect_error(
      readfiscalau:::.rf_validate_manifest(manifest),
      class = "readfiscalau_schema"
    )
  }
})

test_that("valid custom HTTPS manifest URLs remain accepted", {
  manifest <- fixture_release(
    resource_url =
      "https://files.example.org:8443/path/resource.csv?download=1#v1"
  )

  validated <- readfiscalau:::.rf_validate_manifest(manifest)
  expect_identical(validated$resource_url, manifest$resource_url)
})

test_that("built-in manifests are not redundantly revalidated", {
  built_in <- readfiscalau:::.rf_builtin_manifest()
  validation_calls <- 0L

  resolved <- testthat::with_mocked_bindings(
    readfiscalau:::.rf_resolve_manifest(NULL),
    .rf_validate_manifest = function(x) {
      validation_calls <<- validation_calls + 1L
      x
    },
    .package = "readfiscalau"
  )

  expect_identical(resolved, built_in)
  expect_identical(validation_calls, 0L)

  custom <- built_in[1L, , drop = FALSE]
  resolved_custom <- testthat::with_mocked_bindings(
    readfiscalau:::.rf_resolve_manifest(custom),
    .rf_validate_manifest = function(x) {
      validation_calls <<- validation_calls + 1L
      x
    },
    .package = "readfiscalau"
  )

  expect_identical(resolved_custom, custom)
  expect_identical(validation_calls, 1L)
})
