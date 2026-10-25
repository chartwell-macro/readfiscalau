fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

local_tempdir <- function() {
  path <- tempfile("readfiscalau-test-")
  dir.create(path)
  path
}

fixture_raw <- function(name) {
  path <- fixture_path(name)
  con <- file(path, open = "rb")
  on.exit(close(con))
  readBin(con, what = "raw", n = file.info(path)$size)
}

fixture_release <- function(
    release_type = "budget",
    release_id = paste0("fixture-", release_type),
    release_date = "2026-05-12",
    period_end = "2027-06-30",
    fiscal_year = "2026-27",
    media_type = "text/csv",
    resource_url = "https://example.gov.au/chart.csv",
    schema_family = "treasury_chart_csv_v1",
    source_id = "australian_treasury") {
  if (identical(schema_family, "treasury_chart_csv_v1") &&
      grepl("zip", media_type, ignore.case = TRUE)) {
    schema_family <- "treasury_chart_zip_csv_v2"
  }
  data.frame(
    release_id = release_id,
    source_id = source_id,
    release_type = release_type,
    release_status = "final",
    release_date = release_date,
    period_end = period_end,
    fiscal_year = fiscal_year,
    frequency = if (release_type == "monthly_financial_statement") {
      "monthly"
    } else if (release_type %in% c("fbo", "cfs")) {
      "annual"
    } else {
      "release"
    },
    accounting_basis = "mixed",
    consolidation = if (release_type == "cfs") {
      "whole_of_government"
    } else {
      "general_government_sector"
    },
    actual_forecast = "mixed",
    revision_semantics = paste("Fixture vintage", release_id),
    landing_url = "https://example.gov.au/release",
    resource_id = paste0("resource-", release_id),
    resource_role = paste0("fixture_", gsub("-", "_", release_type)),
    resource_url = resource_url,
    resource_updated_at = paste0(release_date, "T00:00:00Z"),
    expected_sha256 = NA_character_,
    media_type = media_type,
    schema_family = schema_family,
    parser_version = "0.1.0",
    terms_url = if (source_id == "australian_treasury") {
      "https://budget.gov.au/legal-notices.htm#h1"
    } else {
      "https://www.finance.gov.au/about-us/copyright-notice"
    },
    license = "Creative Commons Attribution 4.0 International",
    attribution = if (source_id == "australian_treasury") {
      "\u00a9 Commonwealth of Australia"
    } else {
      "\u00a9 Commonwealth of Australia, Department of Finance"
    },
    stringsAsFactors = FALSE
  )
}

archive_fixture_fetcher <- function(payloads) {
  force(payloads)
  function(url) {
    raw <- payloads[[url]]
    if (is.null(raw)) stop("Unexpected fixture URL: ", url, call. = FALSE)
    list(
      raw = raw,
      status = 200L,
      content_type = if (grepl("\\.pdf(?:$|\\?)", url)) {
        "application/pdf"
      } else {
        "text/csv"
      },
      final_url = url,
      retrieved_at = "2026-07-28 00:00:00 UTC"
    )
  }
}

archive_fixture_candidates <- function() {
  tibble::tibble(
    source_url = c(
      "https://budget.gov.au/2025-26/content/files/bp1.pdf",
      "https://budget.gov.au/2025-26/content/files/bp1-replacement.pdf",
      "https://budget.gov.au/2025-26/content/data/chart-data.csv"
    ),
    title = c(
      "Budget Paper No. 1",
      "Budget Paper No. 1 corrected",
      "Chart data"
    ),
    document_role = c("budget_paper_1", "budget_paper_1", "chart_data"),
    budget_vintage = "2025-26",
    index_url = "https://budget.gov.au/2025-26/content/downloads.htm",
    index_snapshot_id = "fixture-index",
    revision_status = c("original", "replacement", "original")
  )
}
