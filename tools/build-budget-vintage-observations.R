usage <- function(status = 0L) {
  message(
    "Usage: Rscript tools/build-budget-vintage-observations.R ",
    "--archive-dir=PATH [--output=PATH]"
  )
  quit(status = status)
}

fail <- function(...) {
  message(...)
  quit(status = 1L)
}

parse_args <- function(args) {
  out <- list(archive_dir = NULL, output = file.path(
    "inst", "extdata", "budget-vintage-observations.csv"
  ))
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
  release_id = c(
    "treasury-budget-2010-11-final",
    "treasury-budget-2011-12-final",
    "treasury-budget-2012-13-final",
    "treasury-budget-2013-14-final",
    "treasury-budget-2014-15-final",
    "treasury-budget-2015-16-final",
    "treasury-budget-2016-17-final",
    "treasury-budget-2017-18-final",
    "treasury-budget-2018-19-final",
    "treasury-budget-2019-20-final",
    "treasury-budget-2020-21-final",
    "treasury-budget-2021-22-final",
    "treasury-budget-2022-23-march-final",
    "treasury-budget-2022-23-october-final",
    "treasury-budget-2023-24-final"
  ),
  budget_vintage = c(
    "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
    "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
    "2020-21", "2021-22", "2022-23", "2022-23-october", "2023-24"
  ),
  release_date = as.Date(c(
    "2010-05-11", "2011-05-10", "2012-05-08", "2013-05-14",
    "2014-05-13", "2015-05-12", "2016-05-03", "2017-05-09",
    "2018-05-08", "2019-04-02", "2020-10-06", "2021-05-11",
    "2022-03-29", "2022-10-25", "2023-05-09"
  )),
  source_url = c(
    "https://archive.budget.gov.au/2010-11/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2011-12/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2012-13/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2013-14/bp1/bp1_consolidated.pdf",
    "https://archive.budget.gov.au/2014-15/bp1/BP1_combined.pdf",
    "https://archive.budget.gov.au/2015-16/bp1/Budget_Paper_No_1.pdf",
    "https://archive.budget.gov.au/2016-17/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2017-18/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2018-19/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2019-20/bp1/download/bp1.pdf",
    "https://archive.budget.gov.au/2020-21/bp1/download/bp1_w.pdf",
    "https://archive.budget.gov.au/2021-22/bp1/download/bp1_2021-22.pdf",
    "https://archive.budget.gov.au/2022-23/bp1/download/bp1_2022-23.pdf",
    paste0(
      "https://archive.budget.gov.au/2022-23-october/",
      "bp1/download/bp1_2022-23.pdf"
    ),
    "https://archive.budget.gov.au/2023-24/bp1/download/bp1_2023-24.pdf"
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
  if (is.na(out)) fail("Could not parse Budget table value: ", value)
  if (negative) -out else out
}

parse_pdf_table <- function(path, release) {
  text_path <- tempfile("budget-vintage-", fileext = ".txt")
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
  title <- grep(
    "Australian Government general government (cash) receipts",
    lines,
    fixed = TRUE
  )
  if (!length(title)) {
    fail("No canonical cash-receipts table in ", release$release_id)
  }
  block <- lines[title[[1L]]:min(length(lines), title[[1L]] + 90L)]
  table_end <- grep(
    "^\\s*Total receipts(?:\\([a-z]\\))?\\s+",
    block,
    perl = TRUE
  )
  if (!length(table_end)) {
    fail("No total-receipts row in ", release$release_id)
  }
  block <- block[seq_len(table_end[[1L]])]
  years <- unique(gsub(
    "\u2013", "-",
    extract_matches(paste(block[seq_len(min(8L, length(block)))],
                          collapse = " "), "(?:19|20)[0-9]{2}[-\u2013][0-9]{2}")
  ))
  if (length(years) < 4L || anyDuplicated(years)) {
    fail("Could not identify unique fiscal-year columns in ",
         release$release_id)
  }
  rows <- lapply(seq_len(nrow(series_contract)), function(index) {
    contract <- series_contract[index, , drop = FALSE]
    row <- grep(contract$row_pattern, block, value = TRUE, perl = TRUE)
    if (length(row) != 1L) {
      fail(
        "Expected one ", contract$series,
        " row in ", release$release_id, "; found ", length(row)
      )
    }
    values_text <- sub(contract$row_pattern, "", row, perl = TRUE)
    values <- extract_matches(
      values_text,
      "(?:-?[0-9][0-9,]*|\\([0-9][0-9,]*\\)|-)"
    )
    if (length(values) != length(years)) {
      fail(
        contract$series, " in ", release$release_id, " has ",
        length(values), " values for ", length(years), " periods."
      )
    }
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
        "budget"
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
    "Archive is missing configured Budget source(s): ",
    paste(release_sources$source_url[is.na(source_row)], collapse = ", ")
  )
}
release_sources$archive_path <- inventory$archive_path[source_row]
release_sources$sha256 <- inventory$sha256[source_row]
if (any(!file.exists(release_sources$archive_path)) ||
    any(!grepl("^[a-f0-9]{64}$", release_sources$sha256))) {
  fail("Configured Budget source objects failed archive admission checks.")
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
  fail("Generated Budget release panel failed its observation contract.")
}

parent <- dirname(options$output)
if (!dir.exists(parent) &&
    !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
  fail("Could not create output directory: ", parent)
}
utils::write.csv(observations, options$output, row.names = FALSE, na = "")
message(
  "Wrote ", nrow(observations), " observations across ",
  length(unique(observations$release_id)), " Budget releases: ",
  options$output
)
