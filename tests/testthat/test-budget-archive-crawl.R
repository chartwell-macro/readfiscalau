test_that("Budget archive crawl replays indexes offline without fetching documents", {
  root_url <- "https://archive.budget.gov.au/index.htm"
  year_url <- "https://archive.budget.gov.au/2025-26/index.htm"
  document_from_root <- "https://archive.budget.gov.au/2001-02/bp1.pdf"
  document_from_year <- "https://archive.budget.gov.au/2025-26/bp1.pdf"
  payloads <- c(
    setNames(
      list(charToRaw(paste0(
        "<html><body>",
        "<a href='2025-26/index.htm'>Budget 2025-26 index</a>",
        "<a href='2001-02/bp1.pdf'>Budget Paper No. 1 2001-02</a>",
        "</body></html>"
      ))),
      root_url
    ),
    setNames(
      list(charToRaw(paste0(
        "<html><body>",
        "<a href='bp1.pdf'>Budget Paper No. 1</a>",
        "</body></html>"
      ))),
      year_url
    )
  )
  calls <- character()
  fetcher <- function(url) {
    calls <<- c(calls, url)
    raw <- payloads[[url]]
    if (is.null(raw)) {
      stop("Document bytes must not be fetched: ", url, call. = FALSE)
    }
    list(
      raw = raw,
      status = 200L,
      content_type = "text/html",
      final_url = url,
      retrieved_at = "2026-07-28 00:00:00 UTC"
    )
  }
  archive_dir <- tempfile("readfiscalau-crawl-")
  on.exit(unlink(archive_dir, recursive = TRUE, force = TRUE), add = TRUE)

  out <- crawl_au_budget_archive(
    root_url = root_url,
    current_url = "https://budget.gov.au/content/downloads.htm",
    current_vintage = "2026-27",
    include_current = FALSE,
    archive_dir = archive_dir,
    fetcher = fetcher,
    max_index_pages = 5L
  )

  expect_named(out, c("documents", "indexes", "gaps", "root_entries"))
  expect_s3_class(out$documents, "tbl_df")
  expect_s3_class(out$indexes, "data.frame")
  expect_s3_class(out$gaps, "data.frame")
  expect_s3_class(out$root_entries, "data.frame")
  expect_setequal(calls, c(root_url, year_url))
  expect_false(any(calls %in% c(document_from_root, document_from_year)))
  expect_true(all(c(document_from_root, document_from_year) %in%
                    out$documents$source_url))
  expect_true(all(c(root_url, year_url) %in% out$indexes$index_url))
  expect_true(file.exists(
    file.path(archive_dir, "inventory", "budget-archive-indexes.csv")
  ))
  expect_true(file.exists(
    file.path(archive_dir, "inventory", "budget-archive-gaps.csv")
  ))
  expect_false(dir.exists(file.path(archive_dir, "documents")))
})
