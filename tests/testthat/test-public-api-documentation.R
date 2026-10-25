public_doc_history_release <- function() {
  release <- fixture_release(
    release_type = "budget",
    release_id = "public-doc-history",
    release_date = "2025-05-13",
    period_end = "2025-06-30",
    fiscal_year = "2024-25",
    resource_url = "https://example.gov.au/public-doc-history.csv"
  )
  release$accounting_basis <- "cash"
  release$resource_role <- "budget_chart_csv_archive"
  release$resource_id <- "resource-public-doc-history"
  release
}

public_doc_monthly_release <- function() {
  release <- fixture_release(
    release_type = "monthly_financial_statement",
    release_id = "public-doc-monthly",
    release_date = "2025-06-27",
    period_end = "2025-05-31",
    fiscal_year = "2024-25",
    media_type = paste0(
      "application/vnd.openxmlformats-officedocument.",
      "spreadsheetml.sheet"
    ),
    resource_url = "https://example.gov.au/public-doc-monthly.xlsx",
    schema_family = "finance_mfs_post_2008_v2",
    source_id = "department_finance"
  )
  release$actual_forecast <- "actual"
  release
}

public_doc_fbo_release <- function() {
  release <- fixture_release(
    release_type = "fbo",
    release_id = "public-doc-fbo",
    release_date = "2025-09-29",
    period_end = "2025-06-30",
    fiscal_year = "2024-25",
    media_type = paste0(
      "application/vnd.openxmlformats-officedocument.",
      "wordprocessingml.document"
    ),
    resource_url = "https://example.gov.au/public-doc-fbo.docx",
    schema_family = "treasury_fbo_history_docx_v1"
  )
  release$actual_forecast <- "actual"
  release$resource_role <- "fbo_historical_aggregates_docx"
  release
}

test_that("default path helpers are side-effect free and user-scoped", {
  old_archive <- Sys.getenv(
    "READFISCALAU_BUDGET_ARCHIVE_DIR",
    unset = NA_character_
  )
  on.exit({
    if (is.na(old_archive)) {
      Sys.unsetenv("READFISCALAU_BUDGET_ARCHIVE_DIR")
    } else {
      Sys.setenv(READFISCALAU_BUDGET_ARCHIVE_DIR = old_archive)
    }
  }, add = TRUE)

  Sys.unsetenv("READFISCALAU_BUDGET_ARCHIVE_DIR")
  expected_cache <- tools::R_user_dir("readfiscalau", which = "cache")
  expected_archive <- file.path(
    tools::R_user_dir("readfiscalau", which = "data"),
    "budget-archive"
  )
  cache_before <- dir.exists(expected_cache)
  archive_before <- dir.exists(expected_archive)

  expect_identical(readfiscalau_default_cache(), expected_cache)
  expect_identical(readfiscalau_default_archive_dir(), expected_archive)
  expect_identical(dir.exists(expected_cache), cache_before)
  expect_identical(dir.exists(expected_archive), archive_before)

  configured <- file.path(
    tempdir(),
    paste0("readfiscalau-public-doc-archive-", Sys.getpid())
  )
  if (dir.exists(configured)) {
    unlink(configured, recursive = TRUE, force = TRUE)
  }
  Sys.setenv(READFISCALAU_BUDGET_ARCHIVE_DIR = configured)
  expect_identical(readfiscalau_default_archive_dir(), configured)
  expect_false(dir.exists(configured))
})

test_that("generic release reader has a stable public contract", {
  raw <- fixture_raw("budget-current.csv")
  release <- fixture_release()
  out <- read_au_fiscal_release(
    release,
    raw = raw,
    cache_dir = local_tempdir(),
    table_id = "cash_receipts"
  )

  expect_s3_class(out, "readfiscalau_data")
  expect_gt(nrow(out), 0L)
  expect_true(all(c(
    "release_id", "period_end", "value", "raw_content_sha256"
  ) %in% names(out)))
  expect_identical(unique(out$release_id), release$release_id)
  expect_true(all(grepl("^[a-f0-9]{64}$", out$raw_content_sha256)))

  provenance <- attr(out, "readfiscalau_provenance")
  expect_type(provenance, "list")
  expect_identical(provenance$release$release_id, release$release_id)
  expect_identical(
    provenance$raw_content_sha256,
    digest::digest(raw, algo = "sha256", serialize = FALSE)
  )

  expect_error(
    read_au_fiscal_release(
      release,
      raw = charToRaw("<html><body>not a CSV</body></html>"),
      cache_dir = local_tempdir()
    ),
    class = "readfiscalau_unsupported_media"
  )
})

test_that("all registered print methods remain callable from documented objects", {
  data <- read_au_fiscal_release(
    fixture_release(),
    raw = fixture_raw("budget-current.csv"),
    cache_dir = local_tempdir(),
    table_id = "cash_receipts"
  )
  data_output <- capture.output(print(data))
  expect_gt(length(data_output), 0L)
  expect_match(data_output[[1L]], "<readfiscalau_data>", fixed = TRUE)

  history_release <- public_doc_history_release()
  history_raw <- charToRaw(
    ",2023-24 ($m)\nTotal receipts,1\n"
  )
  history <- read_au_budget_history(
    history_release$release_id,
    table_id = "Table 1",
    series = "Total receipts",
    manifest = history_release,
    raw = setNames(list(history_raw), history_release$release_id),
    cache_dir = local_tempdir()
  )
  history_output <- capture.output(print(history))
  expect_gt(length(history_output), 0L)
  expect_match(history_output[[1L]], "<readfiscalau_history>", fixed = TRUE)

  monthly <- read_au_monthly_financial_history(
    monthly_release = public_doc_monthly_release(),
    annual_release = public_doc_fbo_release(),
    start_fiscal_year = "2024-25",
    output = "monthly",
    manifest = rbind(
      public_doc_monthly_release(),
      public_doc_fbo_release()
    ),
    monthly_raw = fixture_raw("finance-history.xlsx"),
    annual_raw = fixture_raw("fbo-history.docx"),
    cache_dir = local_tempdir()
  )
  monthly_output <- capture.output(print(monthly))
  expect_gt(length(monthly_output), 0L)
  expect_match(
    monthly_output[[1L]],
    "<readfiscalau_monthly_history>",
    fixed = TRUE
  )
})

test_that("every exported alias has a nonempty generated Rd examples section", {
  package_root <- testthat::test_path("..", "..")
  man_dir <- file.path(package_root, "man")
  if (!dir.exists(man_dir)) {
    skip("source man/ is unavailable in an installed-package check")
  }

  namespace_path <- file.path(package_root, "NAMESPACE")
  namespace <- readLines(namespace_path, warn = FALSE)
  exports <- sub(
    "^export\\(([^)]+)\\).*$",
    "\\1",
    grep("^export\\(", namespace, value = TRUE)
  )
  expect_gt(length(exports), 0L)

  rd_files <- list.files(
    man_dir,
    pattern = "\\.Rd$",
    full.names = TRUE
  )
  expect_gt(length(rd_files), 0L)

  alias_has_examples <- list()
  for (path in rd_files) {
    lines <- readLines(path, warn = FALSE)
    alias_lines <- lines[startsWith(lines, "\\alias{")]
    aliases <- sub("^\\\\alias\\{", "", alias_lines)
    aliases <- sub("\\}.*$", "", aliases)
    if (!length(aliases)) next

    example_starts <- which(grepl("^\\\\examples\\{", lines))
    has_examples <- FALSE
    for (start in example_starts) {
      if (start >= length(lines)) next
      body <- lines[seq.int(start + 1L, length(lines))]
      closing <- which(trimws(body) == "}")[1L]
      if (is.na(closing)) closing <- length(body) + 1L
      if (closing <= 1L) next
      content <- body[seq_len(closing - 1L)]
      content <- content[nzchar(trimws(content))]
      content <- content[!grepl("^\\s*#", content)]
      if (length(content)) {
        has_examples <- TRUE
        break
      }
    }
    for (alias in aliases) {
      alias_has_examples[[alias]] <- isTRUE(
        alias_has_examples[[alias]]
      ) || has_examples
    }
  }

  missing_aliases <- setdiff(exports, names(alias_has_examples))
  no_examples <- exports[vapply(
    exports,
    function(alias) !isTRUE(alias_has_examples[[alias]]),
    logical(1)
  )]
  expect_true(
    length(missing_aliases) == 0L,
    info = paste(
      "Exported symbols missing from generated Rd aliases:",
      paste(missing_aliases, collapse = ", ")
    )
  )
  expect_true(
    length(no_examples) == 0L,
    info = paste(
      "Exported aliases missing nonempty \\\\examples sections:",
      paste(no_examples, collapse = ", ")
    )
  )
})
