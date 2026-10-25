usage <- function(status = 0L) {
  message(
    "Usage: Rscript tools/build-myefo-vintage-observations.R ",
    "--archive-dir=PATH [--output=PATH]"
  )
  quit(status = status)
}

fail <- function(...) {
  message(...)
  quit(status = 1L)
}

parse_args <- function(args) {
  out <- list(
    archive_dir = NULL,
    output = file.path(
      "inst", "extdata", "myefo-vintage-observations.csv"
    )
  )
  for (arg in args) {
    if (identical(arg, "--help")) usage()
    if (startsWith(arg, "--archive-dir=")) {
      out$archive_dir <- sub("^--archive-dir=", "", arg)
    } else if (startsWith(arg, "--output=")) {
      out$output <- sub("^--output=", "", arg)
    } else {
      fail("Unknown argument: ", arg)
    }
  }
  if (is.null(out$archive_dir) || !nzchar(out$archive_dir)) {
    fail("--archive-dir=PATH is required")
  }
  out
}

release_sources <- data.frame(
  release_id = paste0(
    "treasury-myefo-",
    c(
      "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
      "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
      "2020-21", "2021-22", "2023-24", "2024-25", "2025-26"
    ),
    "-final"
  ),
  budget_vintage = c(
    "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
    "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
    "2020-21", "2021-22", "2023-24", "2024-25", "2025-26"
  ),
  release_date = as.Date(c(
    "2010-11-09", "2011-11-09", "2012-10-22", "2013-12-17",
    "2014-12-15", "2015-12-15", "2016-12-19", "2017-12-18",
    "2018-12-17", "2019-12-16", "2020-12-17", "2021-12-16",
    "2023-12-13", "2024-12-18", "2025-12-17"
  )),
  source_url = c(
    "https://archive.budget.gov.au/2010-11/myefo/myefo_2010-11.pdf",
    "https://archive.budget.gov.au/2011-12/myefo/2011-12_MYEFO.pdf",
    "https://archive.budget.gov.au/2012-13/myefo/2012-13_MYEFO.pdf",
    "https://archive.budget.gov.au/2013-14/myefo/2013_14_MYEFO.pdf",
    "https://archive.budget.gov.au/2014-15/myefo/MYEFO_2014-15.pdf",
    "https://archive.budget.gov.au/2015-16/myefo/MYEFO_2015-16_Final.pdf",
    paste0(
      "https://archive.budget.gov.au/2016-17/myefo/",
      "2016-17-MYEFO-combined.pdf"
    ),
    "https://archive.budget.gov.au/2017-18/myefo/MYEFO_2017-18.pdf",
    "https://archive.budget.gov.au/2018-19/myefo/myefo_2018-19.pdf",
    paste0(
      "https://archive.budget.gov.au/2019-20/myefo/download/",
      "MYEFO_2019-20.pdf"
    ),
    paste0(
      "https://archive.budget.gov.au/2020-21/myefo/download/",
      "myefo-2020-21.pdf"
    ),
    paste0(
      "https://archive.budget.gov.au/2021-22/myefo/download/",
      "myefo-2021-22.pdf"
    ),
    paste0(
      "https://archive.budget.gov.au/2023-24/myefo/download/",
      "myefo2023-24.pdf"
    ),
    paste0(
      "https://archive.budget.gov.au/2024-25/myefo/download/",
      "myefo2024-25.pdf"
    ),
    paste0(
      "https://archive.budget.gov.au/2025-26/myefo/download/",
      "myefo-2025-26.pdf"
    )
  ),
  stringsAsFactors = FALSE
)

series_contract <- data.frame(
  series_id = c(
    "gross-income-tax-withholding",
    "company-tax",
    "total-receipts"
  ),
  series = c(
    "Gross income tax withholding",
    "Company tax",
    "Total receipts"
  ),
  row_pattern = c(
    "^\\s*Gross income tax withholding\\s+",
    "^\\s*Company tax\\s+",
    "^\\s*Total receipts(?:\\([a-z]\\))?\\s+"
  ),
  stringsAsFactors = FALSE
)

extract_matches <- function(text, pattern) {
  hit <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  if (identical(hit[[1L]], -1L)) return(character())
  regmatches(text, list(hit))[[1L]]
}

parse_number <- function(value) {
  value <- trimws(value)
  if (identical(value, "-")) return(NA_real_)
  negative <- grepl("^\\(.*\\)$", value)
  value <- gsub("[(),]", "", value)
  out <- suppressWarnings(as.numeric(value))
  if (is.na(out)) fail("Could not parse MYEFO table value: ", value)
  if (negative) -out else out
}

parse_pdf_table <- function(path, release) {
  text_path <- tempfile("myefo-vintage-", fileext = ".txt")
  on.exit(unlink(text_path), add = TRUE)
  status <- system2(
    "pdftotext",
    c("-layout", shQuote(path), shQuote(text_path)),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!identical(attr(status, "status") %||% 0L, 0L) ||
      !file.exists(text_path)) {
    fail("pdftotext failed for ", release$release_id)
  }
  lines <- readLines(text_path, warn = FALSE, encoding = "UTF-8")
  lines <- gsub("\\bw ithholding\\b", "withholding", lines)
  title <- grep(
    paste0(
      "Australian Government general government(?: sector)? ",
      "(?:(?:\\(cash\\)|cash) )?receipts"
    ),
    lines,
    perl = TRUE
  )
  if (!length(title)) {
    fail("No canonical cash-receipts table in ", release$release_id)
  }
  candidate_blocks <- lapply(title, function(start) {
    lines[start:min(length(lines), start + 90L)]
  })
  candidate_blocks <- candidate_blocks[vapply(
    candidate_blocks,
    function(block) all(vapply(
      series_contract$row_pattern,
      function(pattern) any(grepl(pattern, block, perl = TRUE)),
      logical(1)
    )),
    logical(1)
  )]
  if (!length(candidate_blocks)) {
    fail("No complete canonical cash-receipts table in ", release$release_id)
  }
  block <- candidate_blocks[[length(candidate_blocks)]]
  table_end <- grep(
    "^\\s*Total receipts(?:\\([a-z]\\))?\\s+",
    block,
    perl = TRUE
  )
  if (!length(table_end)) {
    fail("No total-receipts row in ", release$release_id)
  }
  block <- block[seq_len(table_end[[length(table_end)]])]
  years <- unique(gsub(
    "\u2013", "-",
    extract_matches(
      paste(block[seq_len(min(12L, length(block)))], collapse = " "),
      "(?:19|20)[0-9]{2}[-\u2013][0-9]{2}"
    )
  ))
  if (length(years) < 4L || anyDuplicated(years)) {
    fail("Could not identify unique fiscal-year columns in ",
         release$release_id, ": ", paste(years, collapse = ", "))
  }
  rows <- lapply(seq_len(nrow(series_contract)), function(index) {
    contract <- series_contract[index, , drop = FALSE]
    row <- grep(contract$row_pattern, block, value = TRUE, perl = TRUE)
    values_per_row <- vapply(row, function(line) {
      length(extract_matches(
        sub(contract$row_pattern, "", line, perl = TRUE),
        "(?:-?[0-9][0-9,]*|\\([0-9][0-9,]*\\)|-)"
      ))
    }, integer(1))
    row <- row[values_per_row == length(years)]
    if (length(row) != 1L) {
      fail(
        "Expected one ", contract$series, " row with ", length(years),
        " values in ", release$release_id, "; found ", length(row)
      )
    }
    values <- extract_matches(
      sub(contract$row_pattern, "", row, perl = TRUE),
      "(?:-?[0-9][0-9,]*|\\([0-9][0-9,]*\\)|-)"
    )
    period_end <- as.Date(paste0(
      as.integer(substr(years, 1L, 4L)) + 1L,
      "-06-30"
    ))
    data.frame(
      release_id = release$release_id,
      source_url = release$source_url,
      source_sha256 = release$sha256,
      table_id = "Table 1",
      series_id = contract$series_id,
      series = contract$series,
      fiscal_year = years,
      period_end = period_end,
      value = vapply(values, parse_number, numeric(1)),
      unit = "AUD million",
      unit_multiplier = 1e6,
      accounting_basis = "cash",
      consolidation = "general_government_sector",
      actual_forecast = ifelse(
        period_end < release$release_date,
        "actual",
        "forecast"
      ),
      profile_type = ifelse(
        period_end < release$release_date,
        "actual",
        "myefo"
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

options <- parse_args(commandArgs(trailingOnly = TRUE))
inventory_path <- file.path(
  options$archive_dir,
  "inventory",
  "budget-archive-inventory.csv"
)
if (!file.exists(inventory_path)) {
  fail("Archive inventory does not exist: ", inventory_path)
}
inventory <- utils::read.csv(
  inventory_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
needed <- c("source_url", "archive_path", "sha256")
if (length(setdiff(needed, names(inventory)))) {
  fail("Archive inventory lacks required provenance fields.")
}
source_row <- match(release_sources$source_url, inventory$source_url)
if (anyNA(source_row)) {
  fail(
    "Archive is missing configured MYEFO source(s): ",
    paste(release_sources$source_url[is.na(source_row)], collapse = ", ")
  )
}
release_sources$archive_path <- inventory$archive_path[source_row]
release_sources$sha256 <- inventory$sha256[source_row]
if (any(!file.exists(release_sources$archive_path)) ||
    any(!grepl("^[a-f0-9]{64}$", release_sources$sha256))) {
  fail("Configured MYEFO source objects failed archive admission checks.")
}

observations <- do.call(rbind, lapply(
  seq_len(nrow(release_sources)),
  function(index) {
    parse_pdf_table(
      release_sources$archive_path[[index]],
      release_sources[index, , drop = FALSE]
    )
  }
))
key <- paste(
  observations$release_id,
  observations$series_id,
  observations$fiscal_year,
  observations$actual_forecast,
  sep = "\r"
)
if (anyDuplicated(key) ||
    anyNA(observations$value) ||
    any(observations$value < 0) ||
    !setequal(unique(observations$release_id), release_sources$release_id)) {
  fail("Generated MYEFO release panel failed its observation contract.")
}

parent <- dirname(options$output)
if (!dir.exists(parent) &&
    !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
  fail("Could not create output directory: ", parent)
}
utils::write.csv(observations, options$output, row.names = FALSE, na = "")
message(
  "Wrote ", nrow(observations), " observations across ",
  length(unique(observations$release_id)), " MYEFO releases: ",
  options$output
)
