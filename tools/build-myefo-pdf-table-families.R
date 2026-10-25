`%||%` <- function(x, y) if (is.null(x)) y else x

fail <- function(...) {
  message(...)
  quit(status = 1L)
}

usage <- function(status = 0L) {
  message(
    "Usage: Rscript tools/build-myefo-pdf-table-families.R ",
    "[--publication=myefo --pdf-dir=PATH | ",
    "--publication=budget (--archive-dir=PATH | --pdf-dir=PATH) ",
    "[--chart-cache-dir=PATH]] ",
    "[--from-vintage=YYYY-YY] [--through-vintage=YYYY-YY] ",
    "[--input=PATH] [--output=PATH]"
  )
  quit(status = status)
}

parse_args <- function(args) {
  out <- list(
    publication = "myefo",
    pdf_dir = NULL,
    archive_dir = NULL,
    chart_cache_dir = NULL,
    from_vintage = NULL,
    through_vintage = NULL,
    input = NULL,
    output = NULL
  )
  for (arg in args) {
    if (identical(arg, "--help")) usage()
    if (startsWith(arg, "--publication=")) {
      out$publication <- sub("^--publication=", "", arg)
    } else if (startsWith(arg, "--pdf-dir=")) {
      out$pdf_dir <- sub("^--pdf-dir=", "", arg)
    } else if (startsWith(arg, "--archive-dir=")) {
      out$archive_dir <- sub("^--archive-dir=", "", arg)
    } else if (startsWith(arg, "--chart-cache-dir=")) {
      out$chart_cache_dir <- sub("^--chart-cache-dir=", "", arg)
    } else if (startsWith(arg, "--from-vintage=")) {
      out$from_vintage <- sub("^--from-vintage=", "", arg)
    } else if (startsWith(arg, "--through-vintage=")) {
      out$through_vintage <- sub("^--through-vintage=", "", arg)
    } else if (startsWith(arg, "--input=")) {
      out$input <- sub("^--input=", "", arg)
    } else if (startsWith(arg, "--output=")) {
      out$output <- sub("^--output=", "", arg)
    } else {
      fail("Unknown argument: ", arg)
    }
  }
  if (!out$publication %in% c("myefo", "budget")) {
    fail("--publication must be `myefo` or `budget`")
  }
  if (identical(out$publication, "myefo") &&
      (is.null(out$pdf_dir) || !nzchar(out$pdf_dir))) {
    fail("--pdf-dir=PATH is required for MYEFO")
  }
  if (identical(out$publication, "budget") &&
      (is.null(out$archive_dir) || !nzchar(out$archive_dir)) &&
      (is.null(out$pdf_dir) || !nzchar(out$pdf_dir))) {
    fail("--archive-dir=PATH or --pdf-dir=PATH is required for Budget")
  }
  default_panel <- file.path(
    "inst",
    "extdata",
    paste0(out$publication, "-vintage-observations.csv")
  )
  out$input <- out$input %||% default_panel
  out$output <- out$output %||% default_panel
  out
}

treasury_pdf_release_dates <- function(publication, archive_dir = NULL) {
  if (identical(publication, "myefo")) {
    vintages <- c(
      "2000-01", "2001-02", "2002-03", "2003-04", "2004-05",
      "2005-06", "2006-07", "2007-08", "2008-09", "2009-10",
      "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
      "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
      "2020-21", "2021-22", "2023-24", "2024-25", "2025-26"
    )
    return(data.frame(
      release_id = paste0("treasury-myefo-", vintages, "-final"),
      budget_vintage = vintages,
      release_date = as.Date(c(
        "2000-11-15", "2001-10-16", "2002-11-27", "2003-12-08",
        "2004-12-17", "2005-12-14", "2006-12-20", "2007-10-15",
        "2008-11-05", "2009-11-02",
        "2010-11-09", "2011-11-09", "2012-10-22", "2013-12-17",
        "2014-12-15", "2015-12-15", "2016-12-19", "2017-12-18",
        "2018-12-17", "2019-12-16", "2020-12-17", "2021-12-16",
        "2023-12-13", "2024-12-18", "2025-12-17"
      )),
      profile_type = "myefo",
      stringsAsFactors = FALSE
    ))
  }

  vintages <- c(
    "2000-01", "2001-02", "2002-03", "2003-04", "2004-05",
    "2005-06", "2006-07", "2007-08", "2008-09", "2009-10",
    "2010-11", "2011-12", "2012-13", "2013-14", "2014-15",
    "2015-16", "2016-17", "2017-18", "2018-19", "2019-20",
    "2020-21", "2021-22", "2022-23", "2022-23-october",
    "2023-24", "2024-25", "2025-26", "2026-27"
  )
  release_ids <- paste0("treasury-budget-", vintages, "-final")
  release_ids[[23L]] <- "treasury-budget-2022-23-march-final"
  pdf_urls <- c(
    "https://archive.budget.gov.au/2000-01/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2001-02/bp1/bp1.pdf",
    paste0(
      "https://archive.budget.gov.au/2002-03/bp1/",
      "bp1_combined_pdf.zip"
    ),
    "https://archive.budget.gov.au/2003-04/bp1/bp1_pdf.zip",
    "https://archive.budget.gov.au/2004-05/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2005-06/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2006-07/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2007-08/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2008-09/bp1/bp1.pdf",
    "https://archive.budget.gov.au/2009-10/bp1/bp_1.pdf",
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
    "https://archive.budget.gov.au/2023-24/bp1/download/bp1_2023-24.pdf",
    "https://archive.budget.gov.au/2024-25/bp1/download/bp1_2024-25.pdf",
    "https://archive.budget.gov.au/2025-26/bp1/download/bp1_2025-26.pdf",
    "https://budget.gov.au/content/bp1/download/bp1_2026-27.pdf"
  )
  out <- data.frame(
    release_id = release_ids,
    budget_vintage = vintages,
    release_date = as.Date(c(
      "2000-05-09", "2001-05-22", "2002-05-14", "2003-05-13",
      "2004-05-11", "2005-05-10", "2006-05-09", "2007-05-08",
      "2008-05-13", "2009-05-12",
      "2010-05-11", "2011-05-10", "2012-05-08", "2013-05-14",
      "2014-05-13", "2015-05-12", "2016-05-03", "2017-05-09",
      "2018-05-08", "2019-04-02", "2020-10-06", "2021-05-11",
      "2022-03-29", "2022-10-25", "2023-05-09", "2024-05-14",
      "2025-03-25", "2026-05-12"
    )),
    profile_type = "budget",
    observation_source_url = pdf_urls,
    parsing_sha256 = c(
      NA, NA,
      "6e6adb8364b509772536fc6fddcc6d68cd0f147457a6d4ea3d264ea0e43b0eb8",
      "f935a2113750518ca56f04d63aa4bcc78de3a261313745f8d997860e11e93744",
      rep(NA, length(vintages) - 4L)
    ),
    stringsAsFactors = FALSE
  )
  if (is.null(archive_dir) || !nzchar(archive_dir)) return(out)
  inventory_path <- file.path(
    archive_dir,
    "inventory",
    "budget-archive-inventory.csv"
  )
  if (!file.exists(inventory_path)) {
    inventory_path <- file.path(
      archive_dir,
      "budget-archive-inventory.csv"
    )
  }
  if (!file.exists(inventory_path)) {
    fail("Budget archive inventory does not exist: ", inventory_path)
  }
  inventory <- utils::read.csv(
    inventory_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  inventory_rows <- match(pdf_urls, inventory$source_url)
  if (anyNA(inventory_rows)) {
    fail("Budget archive is missing a configured Budget Paper No. 1 PDF.")
  }
  out$observation_source_sha256 <- inventory$sha256[inventory_rows]
  out$pdf_path <- inventory$archive_path[inventory_rows]
  out
}

slug <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-|-$", "", x)
}

decode_legacy_font_line <- function(x) {
  hint_tokens <- c(
    "7DEOH", "5HDO", "0DMRU", ")RUHFDVWV", "3URMHFWLRQV",
    "&RPPRQZHDOWK", "$XVWUDOLDQ", "8QGHUO", "(PSOR",
    ":DJHV", "&3,", "*'3", "+RXVHKROG", "'ZHOOLQJ", "*URVV"
  )
  encoded_hint <- any(utf8ToInt(x) %in% 1:31) ||
    any(vapply(
      hint_tokens,
      function(token) grepl(token, x, fixed = TRUE),
      logical(1)
    ))
  if (!encoded_hint) return(x)
  pieces <- regmatches(x, gregexpr("\\S+|\\s+", x, perl = TRUE))[[1L]]
  pieces <- vapply(pieces, function(piece) {
    if (grepl("^\\s+$", piece, perl = TRUE) ||
        (!grepl("[A-Z]", piece) &&
          !any(utf8ToInt(piece) %in% 1:31) &&
          !piece %in% hint_tokens)) {
      return(piece)
    }
    code <- utf8ToInt(piece)
    shift <- code < 127L & !code %in% c(9L, 10L, 13L, 32L)
    code[shift] <- code[shift] + 29L
    intToUtf8(code)
  }, character(1), USE.NAMES = FALSE)
  paste0(pieces, collapse = "")
}

normalize_line <- function(x) {
  x <- vapply(x, decode_legacy_font_line, character(1), USE.NAMES = FALSE)
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", x)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("\\bdom estic\\b", "domestic", x, ignore.case = TRUE)
  x <- gsub("\\bDw elling\\b", "Dwelling", x, ignore.case = TRUE)
  x <- gsub("\\bw ages\\b", "wages", x, ignore.case = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

extract_years <- function(lines) {
  text <- paste(normalize_line(lines), collapse = " ")
  hit <- gregexpr("(?:19|20)[0-9]{2}-[0-9]{2}", text, perl = TRUE)[[1L]]
  if (identical(hit[[1L]], -1L)) return(character())
  unique(regmatches(text, list(hit))[[1L]])
}

number_pattern <- paste0(
  "(?<![A-Za-z0-9])",
  "(?:-\\s*)?",
  "(?:",
  "[0-9]+/[0-9]+",
  "|(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+(?:\\.[0-9]+)?)",
  "(?:\\s+[0-9]+/[0-9]+)?",
  ")"
)

extract_number_tokens <- function(x) {
  hit <- gregexpr(number_pattern, x, perl = TRUE)[[1L]]
  if (identical(hit[[1L]], -1L)) return(character())
  regmatches(x, list(hit))[[1L]]
}

first_number_start <- function(x) {
  hit <- regexpr(number_pattern, x, perl = TRUE)
  if (identical(hit[[1L]], -1L)) return(NA_integer_)
  as.integer(hit[[1L]])
}

parse_number <- function(x) {
  x <- gsub(",", "", trimws(x), fixed = TRUE)
  negative <- startsWith(x, "-")
  x <- trimws(sub("^-\\s*", "", x))
  parts <- strsplit(x, "[[:space:]]+", perl = TRUE)[[1L]]
  value <- 0
  for (part in parts) {
    if (grepl("/", part, fixed = TRUE)) {
      fraction <- strsplit(part, "/", fixed = TRUE)[[1L]]
      value <- value + as.numeric(fraction[[1L]]) / as.numeric(fraction[[2L]])
    } else {
      value <- value + as.numeric(part)
    }
  }
  if (negative) -value else value
}

period_end <- function(years) {
  as.Date(paste0(
    as.integer(substr(years, 1L, 4L)) + 1L,
    "-06-30"
  ))
}

release_profile <- function(release) {
  if ("profile_type" %in% names(release) &&
      !is.na(release$profile_type[[1L]]) &&
      nzchar(release$profile_type[[1L]])) {
    return(release$profile_type[[1L]])
  }
  "myefo"
}

panel_row <- function(
    release,
    table_family,
    table_label,
    source_table,
    series_id,
    series,
    fiscal_year,
    value,
    unit,
    unit_multiplier,
    accounting_basis,
    consolidation,
    actual_forecast,
    profile_type) {
  data.frame(
    release_id = release$release_id,
    source_url = release$source_url,
    source_sha256 = release$source_sha256,
    observation_source_url =
      release$observation_source_url %||% release$source_url,
    observation_source_sha256 =
      release$observation_source_sha256 %||% release$source_sha256,
    table_id = table_family,
    table_family = table_family,
    table_label = table_label,
    source_table = source_table,
    series_id = series_id,
    series = series,
    fiscal_year = fiscal_year,
    period_end = period_end(fiscal_year),
    value = value,
    unit = unit,
    unit_multiplier = unit_multiplier,
    accounting_basis = accounting_basis,
    consolidation = consolidation,
    actual_forecast = actual_forecast,
    profile_type = profile_type,
    stringsAsFactors = FALSE
  )
}

major_contract <- data.frame(
  pattern = c(
    "^Real GDP\\b",
    "^Employment\\b",
    "^Unemployment rate\\b",
    "^Consumer price index\\b|^CPI\\b",
    "^Wage price index\\b",
    "^Wages?\\b",
    "^Nominal GDP\\b"
  ),
  aggregate = c(
    "Real GDP growth",
    "Employment growth",
    "Unemployment rate",
    "Consumer price index growth",
    "Wage price index growth",
    "Wages growth",
    "Nominal GDP growth"
  ),
  series = c(
    "Real GDP",
    "Employment",
    "Unemployment rate",
    "Consumer price index",
    "Wage price index",
    "Wages",
    "Nominal GDP"
  ),
  unit = c(
    "Per cent change", "Per cent change", "Per cent",
    "Per cent change", "Per cent change", "Per cent change",
    "Per cent change"
  ),
  stringsAsFactors = FALSE
)

parse_major <- function(lines, release) {
  title <- grep(
    "^Table [A-Z]?[0-9.]+: Major economic parameters",
    normalize_line(lines),
    ignore.case = TRUE
  )
  if (!length(title)) return(NULL)
  start <- title[[1L]]
  block <- lines[start:min(length(lines), start + 16L)]
  years <- extract_years(block[seq_len(min(5L, length(block)))])
  if (length(years) < 4L) {
    fail("Could not identify major-parameter years in ", release$release_id)
  }
  rows <- list()
  cursor <- 0L
  clean <- normalize_line(block)
  for (i in seq_len(nrow(major_contract))) {
    hit <- grep(
      major_contract$pattern[[i]],
      clean,
      ignore.case = TRUE,
      perl = TRUE
    )
    if (!length(hit)) next
    tokens <- extract_number_tokens(sub(
      major_contract$pattern[[i]],
      "",
      clean[hit[[1L]]],
      ignore.case = TRUE,
      perl = TRUE
    ))
    if (length(tokens) != length(years)) {
      fail(
        "Unexpected major-parameter width for ",
        major_contract$series[[i]], " in ", release$release_id
      )
    }
    ends <- period_end(years)
    statuses <- ifelse(
      ends < release$release_date,
      "actual",
      "forecast"
    )
    cursor <- cursor + 1L
    rows[[cursor]] <- panel_row(
      release,
      "major_economic_parameters",
      "Major economic parameters",
      normalize_line(lines[start]),
      paste(
        "major_economic_parameters",
        slug(major_contract$aggregate[[i]]),
        if (identical(major_contract$unit[[i]], "Per cent")) {
          "per-cent"
        } else {
          "per-cent-change"
        },
        sep = "--"
      ),
      major_contract$series[[i]],
      years,
      vapply(tokens, parse_number, numeric(1)),
      major_contract$unit[[i]],
      0.01,
      "not_applicable",
      "australian_economy",
      statuses,
      ifelse(statuses == "actual", "actual", release_profile(release))
    )
  }
  if (length(rows) < 4L) {
    fail("Major-parameter table is incomplete in ", release$release_id)
  }
  do.call(rbind, rows)
}

domestic_contract <- data.frame(
  pattern = c(
    "^Real gross domestic product\\b",
    "^Household consumption\\b",
    "^Dwellings?\\b|^Dwelling investment\\b",
    "^Total business investment\\b",
    "^Mining investment\\b",
    "^Non-mining investment\\b",
    "^Private final demand\\b",
    "^Public final demand\\b",
    "^Total final demand\\b",
    "^Change in inventories\\b",
    "^Gross national expenditure\\b",
    "^Exports of goods and services\\b",
    "^Imports of goods and services\\b",
    "^Net exports\\b",
    "^Non-farm product\\b",
    "^Farm product\\b",
    "^Nominal gross domestic product\\b",
    "^Terms of trade\\b",
    "^Current account balance\\b",
    "^Employment(?: \\(labour force survey basis\\))?\\b",
    "^Unemployment rate \\(per cent\\)\\b",
    "^Participation rate \\(per cent\\)\\b",
    "^Consumer price index\\b",
    "^Gross non-farm product deflator\\b",
    "^Wage price index\\b",
    "^GDP deflator\\b",
    "^Net overseas migration\\b"
  ),
  series = c(
    "Real gross domestic product",
    "Household consumption",
    "Dwelling investment",
    "Total business investment",
    "Mining investment",
    "Non-mining investment",
    "Private final demand",
    "Public final demand",
    "Total final demand",
    "Change in inventories",
    "Gross national expenditure",
    "Exports of goods and services",
    "Imports of goods and services",
    "Net exports",
    "Non-farm product",
    "Farm product",
    "Nominal gross domestic product",
    "Terms of trade",
    "Current account balance",
    "Employment",
    "Unemployment rate",
    "Participation rate",
    "Consumer price index",
    "Gross non-farm product deflator",
    "Wage price index",
    "GDP deflator",
    "Net overseas migration"
  ),
  stringsAsFactors = FALSE
)

domestic_unit <- function(series) {
  if (series %in% c("Change in inventories", "Net exports")) {
    return(list(unit = "Percentage points", multiplier = 0.01))
  }
  if (series %in% c("Unemployment rate", "Participation rate")) {
    return(list(unit = "Per cent", multiplier = 0.01))
  }
  if (identical(series, "Current account balance")) {
    return(list(unit = "% of GDP", multiplier = 0.01))
  }
  if (identical(series, "Net overseas migration")) {
    return(list(unit = "Persons", multiplier = 1))
  }
  list(unit = "Per cent change", multiplier = 0.01)
}

parse_domestic <- function(lines, release) {
  clean_all <- normalize_line(lines)
  title <- grep(
    "^Table [A-Z]?[0-9.]+: Domestic economy.*forecasts",
    clean_all,
    ignore.case = TRUE
  )
  if (!length(title)) {
    return(NULL)
  }
  start <- title[[1L]]
  block <- lines[start:min(length(lines), start + 70L)]
  clean <- normalize_line(block)
  footnote <- grep("^(?:\\(?a\\)|a\\)) Percentage change", clean)
  if (length(footnote)) block <- block[seq_len(footnote[[1L]] - 1L)]
  clean <- normalize_line(block)
  years <- extract_years(block[seq_len(min(8L, length(block)))])
  publication_profile <- release_profile(release)
  is_budget <- identical(publication_profile, "budget")
  if ((!is_budget && !length(years) %in% 2:4) ||
      (is_budget && !length(years) %in% 3:6)) {
    fail(
      "Unexpected domestic-economy fiscal columns in ",
      release$release_id, "; found ", paste(years, collapse = ", ")
    )
  }
  combined <- c(clean, paste(clean[-length(clean)], clean[-1L]))
  rows <- list()
  cursor <- 0L
  for (i in seq_len(nrow(domestic_contract))) {
    candidates <- grep(
      domestic_contract$pattern[[i]],
      combined,
      ignore.case = TRUE,
      perl = TRUE,
      value = TRUE
    )
    if (!length(candidates)) next
    widths <- vapply(candidates, function(candidate) {
      tail <- sub(
        domestic_contract$pattern[[i]],
        "",
        candidate,
        ignore.case = TRUE,
        perl = TRUE
      )
      length(extract_number_tokens(tail))
    }, integer(1))
    if (is_budget) {
      candidates <- candidates[
        widths %in% c(length(years), length(years) + 1L)
      ]
    } else {
      expected_width <- min(widths[widths >= length(years)], na.rm = TRUE)
      candidates <- candidates[widths == expected_width]
    }
    if (!length(candidates)) next
    candidate <- candidates[[1L]]
    tail <- sub(
      domestic_contract$pattern[[i]],
      "",
      candidate,
      ignore.case = TRUE,
      perl = TRUE
    )
    values <- vapply(extract_number_tokens(tail), parse_number, numeric(1))
    if (is_budget) {
      selected_values <- values[seq_along(years)]
      statuses <- ifelse(
        period_end(years) < release$release_date,
        "actual",
        "forecast"
      )
    } else {
      n_years <- length(years)
      selected_values <- if (length(values) == n_years) {
        values
      } else if (length(values) == 2L * n_years - 1L) {
        values[c(1L, seq.int(3L, length(values), by = 2L))]
      } else if (length(values) == 3L * n_years - 2L) {
        values[c(1L, seq.int(4L, length(values), by = 3L))]
      } else {
        next
      }
      statuses <- ifelse(
        period_end(years) < release$release_date,
        "actual",
        "forecast"
      )
    }
    contract <- domestic_unit(domestic_contract$series[[i]])
    cursor <- cursor + 1L
    rows[[cursor]] <- panel_row(
      release,
      "domestic_economy_forecasts",
      "Domestic economy — detailed forecasts",
      normalize_line(lines[start]),
      paste0(
        "domestic-economy-forecasts--",
        slug(domestic_contract$series[[i]])
      ),
      domestic_contract$series[[i]],
      years,
      selected_values,
      contract$unit,
      contract$multiplier,
      "not_applicable",
      "australian_economy",
      statuses,
      ifelse(statuses == "actual", "actual", publication_profile)
    )
  }
  if (length(rows) < 8L) {
    fail(
      "Domestic-economy table yielded only ", length(rows),
      " series in ", release$release_id
    )
  }
  do.call(rbind, rows)
}

parse_budget_aggregates <- function(lines, release) {
  clean <- normalize_line(lines)
  title <- grep(
    "^Table [A-Z]?[0-9.]+: Summary of budget aggregates",
    clean,
    ignore.case = TRUE
  )
  if (!length(title)) return(NULL)
  start <- title[[1L]]
  block <- clean[start:min(length(clean), start + 45L)]
  years <- extract_years(block)
  if (length(years) < 2L) return(NULL)
  metrics <- data.frame(
    pattern = c(
      "^Underlying cash balance\\s*(?:\\(\\$b\\)|\\$b)",
      "^Fiscal balance\\s*(?:\\(\\$b\\)|\\$b)"
    ),
    series = c("Underlying cash balance", "Fiscal balance"),
    stringsAsFactors = FALSE
  )
  rows <- list()
  cursor <- 0L
  for (i in seq_len(nrow(metrics))) {
    hits <- grep(
      metrics$pattern[[i]],
      block,
      ignore.case = TRUE,
      perl = TRUE
    )
    if (!length(hits)) next
    year_cursor <- 1L
    for (hit in hits) {
      absolute <- extract_number_tokens(sub(
        metrics$pattern[[i]],
        "",
        block[[hit]],
        ignore.case = TRUE,
        perl = TRUE
      ))
      if (length(absolute) < 2L) next
      group_width <- if (length(absolute) %% 2L == 0L) 2L else 1L
      variants <- length(absolute) / group_width
      if (!variants %in% c(2, 3)) next
      selected <- absolute[
        seq.int(as.integer(variants), length(absolute), by = variants)
      ]
      group_years <- years[
        year_cursor:min(length(years), year_cursor + length(selected) - 1L)
      ]
      if (length(group_years) != length(selected)) next
      percent_hit <- hit + 1L
      percent <- if (
          percent_hit <= length(block) &&
          grepl("^Per cent of GDP", block[[percent_hit]], ignore.case = TRUE)
      ) {
        extract_number_tokens(block[[percent_hit]])
      } else {
        character()
      }
      selected_percent <- if (length(percent) == length(absolute)) {
        percent[
          seq.int(as.integer(variants), length(percent), by = variants)
        ]
      } else {
        character()
      }
      statuses <- ifelse(
        period_end(group_years) < release$release_date,
        "actual",
        "forecast"
      )
      cursor <- cursor + 1L
      rows[[cursor]] <- panel_row(
        release,
        "budget_aggregates",
        "Budget aggregates",
        clean[[start]],
        paste(
          "budget_aggregates",
          slug(metrics$series[[i]]),
          "aud-million",
          sep = "--"
        ),
        paste0(metrics$series[[i]], " — AUD million"),
        group_years,
        vapply(selected, parse_number, numeric(1)) * 1000,
        "AUD million",
        1e6,
        "cash",
        "general_government_sector",
        statuses,
        ifelse(statuses == "actual", "actual", release_profile(release))
      )
      if (length(selected_percent) == length(group_years)) {
        cursor <- cursor + 1L
        rows[[cursor]] <- panel_row(
          release,
          "budget_aggregates",
          "Budget aggregates",
          clean[[start]],
          paste(
            "budget_aggregates",
            slug(metrics$series[[i]]),
            "percent-of-gdp",
            sep = "--"
          ),
          paste0(metrics$series[[i]], " — % of GDP"),
          group_years,
          vapply(selected_percent, parse_number, numeric(1)),
          "% of GDP",
          0.01,
          "cash",
          "general_government_sector",
          statuses,
          ifelse(statuses == "actual", "actual", release_profile(release))
        )
      }
      year_cursor <- year_cursor + length(group_years)
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

parse_cash_interest <- function(lines, release) {
  clean <- normalize_line(lines)
  title <- grep(
    "^Table [A-Z]?[0-9.]+: Interest payments, interest receipts and net interest payments?",
    clean,
    ignore.case = TRUE
  )
  if (!length(title)) return(NULL)
  start <- title[[1L]]
  block <- clean[start:min(length(clean), start + 22L)]
  years <- extract_years(block[seq_len(min(6L, length(block)))])
  if (length(years) < 4L || length(years) > 5L) {
    fail("Unexpected cash-interest year width in ", release$release_id)
  }
  metric_patterns <- c(
    "^Interest payments on (?:AGS|CGS)\\b",
    "^Interest payments(?:\\([a-z]\\))?(?=\\s+[0-9-])",
    "^Interest receipts\\b",
    "^Net interest payments\\b"
  )
  metric_names <- c(
    "Interest payments on Australian Government Securities",
    "Interest payments",
    "Interest receipts",
    "Net interest payments"
  )
  rows <- list()
  cursor <- 0L
  for (i in seq_along(metric_patterns)) {
    hit <- grep(metric_patterns[[i]], block, ignore.case = TRUE, perl = TRUE)
    hit <- hit[!grepl("^Per cent", block[hit], ignore.case = TRUE)]
    if (length(hit) != 1L) {
      fail("Missing cash-interest metric in ", release$release_id)
    }
    absolute <- extract_number_tokens(sub(
      metric_patterns[[i]],
      "",
      block[hit],
      ignore.case = TRUE,
      perl = TRUE
    ))
    percent_hit <- hit + 1L
    percent <- extract_number_tokens(block[percent_hit])
    if (
      length(absolute) != length(years) ||
        length(percent) != length(years)
    ) {
      fail("Unexpected cash-interest row width in ", release$release_id)
    }
    for (variant in c("aud-million", "percent-of-gdp")) {
      is_percent <- identical(variant, "percent-of-gdp")
      statuses <- ifelse(
        period_end(years) < release$release_date,
        "actual",
        "forecast"
      )
      cursor <- cursor + 1L
      rows[[cursor]] <- panel_row(
        release,
        "cash_interest",
        "Cash interest",
        clean[start],
        paste(
          "cash-interest",
          slug(metric_names[[i]]),
          variant,
          sep = "--"
        ),
        paste0(
          metric_names[[i]],
          " — ",
          if (is_percent) "% of GDP" else "AUD million"
        ),
        years,
        vapply(
          if (is_percent) percent else absolute,
          parse_number,
          numeric(1)
        ),
        if (is_percent) "% of GDP" else "AUD million",
        if (is_percent) 0.01 else 1e6,
        "cash",
        "general_government_sector",
        statuses,
        ifelse(statuses == "actual", "actual", release_profile(release))
      )
    }
  }
  do.call(rbind, rows)
}

clean_receipt_label <- function(x) {
  x <- normalize_line(x)
  x <- gsub("\\([a-z]\\)", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(x)
  x <- sub(
    "^Total individuals and other withholding taxation$",
    "Total individuals and other withholding tax",
    x,
    ignore.case = TRUE
  )
  x <- sub(
    "^Superannuation funds$",
    "Superannuation fund taxes",
    x,
    ignore.case = TRUE
  )
  x
}

receipt_series_id <- function(label) {
  key <- tolower(label)
  if (identical(key, "gross income tax withholding")) {
    return("gross-income-tax-withholding")
  }
  if (identical(key, "company tax")) return("company-tax")
  if (identical(key, "total receipts")) return("total-receipts")
  slug(label)
}

receipt_data_lines <- function(block, needed, single_year = FALSE) {
  out <- list()
  cursor <- 0L
  pending <- ""
  section <- ""
  section_names <- c(
    "Income taxation", "Excise and customs", "Excise duty",
    "Customs duty", "Indirect taxation", "Other taxation",
    "Non-taxation", "Sales taxes"
  )
  for (raw_line in block) {
    line <- normalize_line(raw_line)
    if (!nzchar(line)) next
    start <- first_number_start(line)
    if (is.na(start)) {
      section_hit <- section_names[
        tolower(section_names) == tolower(line)
      ]
      if (length(section_hit)) section <- section_hit[[1L]]
      if (!grepl(
        "^(Table |Actual|Estimates|Projections|Change on |Budget |PEFO |MYEFO|\\$m|Memorandum:|Individuals and other withholding taxes$|Sales taxes$|Excise duty$|Customs duty$|Other customs duty$|Excise and customs duty$|Other indirect taxation$|Income taxation receipts$|Indirect taxation receipts$|Taxation receipts$|Private investment$)",
        line,
        ignore.case = TRUE
      )) {
        pending <- line
      } else {
        pending <- ""
      }
      next
    }
    prefix <- trimws(substr(line, 1L, start - 1L))
    tokens <- extract_number_tokens(substr(line, start, nchar(line)))
    if ((single_year && length(tokens) < 2L) ||
        (!single_year && length(tokens) < needed)) {
      next
    }
    if (nzchar(pending) && grepl("^[a-z]", prefix)) {
      prefix <- paste(pending, prefix)
    }
    pending <- ""
    label <- clean_receipt_label(prefix)
    if (tolower(label) %in% c("other taxes", "other") &&
        nzchar(section)) {
      label <- paste0(label, " — ", section)
    }
    if (!nzchar(label) ||
        grepl("^(Per cent|Page )", label, ignore.case = TRUE)) {
      next
    }
    cursor <- cursor + 1L
    out[[cursor]] <- list(
      label = label,
      tokens = if (single_year) tokens[[2L]] else tokens[seq_len(needed)]
    )
  }
  out
}

parse_cash_receipts <- function(lines, release) {
  clean <- normalize_line(lines)
  title <- grep(
    paste0(
      "^Table [A-Z]?[0-9.]+: Australian Government general government",
      "(?: sector)? (?:(?:\\(cash\\)|cash) receipts",
      "|receipts(?: \\(cash basis\\)| - [0-9]{4}-[0-9]{2}| — [0-9]{4}-[0-9]{2}))"
    ),
    clean,
    ignore.case = TRUE,
    perl = TRUE
  )
  if (identical(release_profile(release), "myefo")) {
    title <- sort(unique(c(
      title,
      grep(
        paste0(
          "^Table [A-Z]?[0-9.]+: Australian Government general government",
          "(?: sector)? receipts(?:\\([a-z]\\))?$"
        ),
        clean,
        ignore.case = TRUE,
        perl = TRUE
      )
    )))
  }
  if (!length(title)) {
    return(NULL)
  }
  rows <- list()
  cursor <- 0L
  for (start in title) {
    next_title <- title[title > start]
    stop <- if (length(next_title)) {
      min(next_title[[1L]] - 1L, start + 100L)
    } else {
      min(length(lines), start + 100L)
    }
    block <- lines[start:stop]
    footnote <- grep(
      "^(?:\\(?[a-z]\\)|[a-z]\\))\\s",
      normalize_line(block),
      ignore.case = TRUE
    )
    if (length(footnote)) {
      block <- block[seq_len(footnote[[1L]] - 1L)]
    }
    header_years <- extract_years(block[seq_len(min(8L, length(block)))])
    consolidated <- length(header_years) >= 4L
    if (consolidated) {
      years <- header_years
      parsed <- receipt_data_lines(block, length(years), FALSE)
    } else {
      title_years <- extract_years(block[seq_len(min(3L, length(block)))])
      if (length(title_years) != 1L) next
      years <- title_years
      parsed <- receipt_data_lines(block, 1L, TRUE)
    }
    if (!length(parsed)) next
    for (item in parsed) {
      values <- vapply(item$tokens, parse_number, numeric(1))
      statuses <- if (consolidated) {
        ifelse(
          period_end(years) < release$release_date,
          "actual",
          "forecast"
        )
      } else {
        "forecast"
      }
      cursor <- cursor + 1L
      rows[[cursor]] <- panel_row(
        release,
        "cash_receipts",
        "Cash receipts",
        clean[start],
        receipt_series_id(item$label),
        item$label,
        years,
        values,
        "AUD million",
        1e6,
        "cash",
        "general_government_sector",
        statuses,
        ifelse(statuses == "actual", "actual", release_profile(release))
      )
    }
  }
  if (!length(rows)) {
    fail("No cash-receipts rows parsed in ", release$release_id)
  }
  out <- do.call(rbind, rows)
  key <- paste(
    out$series_id,
    out$period_end,
    out$actual_forecast,
    out$profile_type,
    sep = "\r"
  )
  duplicate_groups <- split(seq_len(nrow(out)), key)
  conflicts <- vapply(duplicate_groups, function(index) {
    length(unique(out$value[index])) > 1L
  }, logical(1))
  if (any(conflicts)) {
    first_conflict <- duplicate_groups[[which(conflicts)[[1L]]]]
    fail(
      "Conflicting cash-receipts duplicates in ", release$release_id,
      ": ", out$series_id[first_conflict[[1L]]],
      " / ", out$period_end[first_conflict[[1L]]],
      " = ", paste(unique(out$value[first_conflict]), collapse = ", ")
    )
  }
  out <- out[!duplicated(key), ]
  if (length(unique(out$series_id)) < 10L ||
      !"total-receipts" %in% out$series_id) {
    fail(
      "Cash-receipts table yielded only ",
      length(unique(out$series_id)), " series in ", release$release_id
    )
  }
  out
}

read_pdf_lines <- function(path, vintage) {
  text_path <- file.path(
    tempdir(),
    paste0(
      "readfiscalau-",
      slug(vintage),
      "-",
      substr(digest::digest(path, algo = "sha256"), 1L, 12L),
      ".txt"
    )
  )
  if (!file.exists(text_path)) {
    status <- system2(
      "pdftotext",
      c("-layout", shQuote(path), shQuote(text_path)),
      stdout = TRUE,
      stderr = TRUE
    )
    if (!identical(attr(status, "status") %||% 0L, 0L) ||
        !file.exists(text_path)) {
      fail("pdftotext failed for ", path)
    }
  }
  readLines(text_path, warn = FALSE, encoding = "UTF-8")
}

parse_budget_current_charts <- function(chart_cache_dir) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    fail("Package `pkgload` is required to rebuild current Budget charts.")
  }
  pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
  release_ids <- paste0(
    "treasury-budget-",
    c("2024-25", "2025-26", "2026-27"),
    "-final"
  )
  manifest <- readfiscalau::list_au_fiscal_releases(
    release_type = "budget"
  )
  files <- list.files(
    chart_cache_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  rows <- lapply(release_ids, function(release_id) {
    release <- manifest[
      manifest$release_id == release_id,
      ,
      drop = FALSE
    ]
    if (nrow(release) != 1L) {
      fail("No unique manifest row for ", release_id)
    }
    candidates <- files[
      grepl(release$expected_sha256[[1L]], files, fixed = TRUE) &
        !grepl("metadata", files, fixed = TRUE)
    ]
    if (!length(candidates)) {
      fail("No cached chart resource for ", release_id)
    }
    source_path <- candidates[[1L]]
    source_hash <- digest::digest(
      file = source_path,
      algo = "sha256",
      serialize = FALSE
    )
    if (!identical(source_hash, release$expected_sha256[[1L]])) {
      fail("Cached chart resource hash mismatch for ", release_id)
    }
    chart <- if (grepl("zip", release$media_type[[1L]])) {
      readfiscalau:::.rf_parse_chart_zip(
        source_path,
        release,
        "Table 1"
      )
    } else {
      bytes <- readBin(
        source_path,
        "raw",
        n = as.numeric(file.info(source_path)$size)
      )
      readfiscalau:::.rf_parse_chart_csv(
        bytes,
        release,
        "Table 1"
      )
    }
    member <- attr(chart, "readfiscalau_member")
    data.frame(
      release_id = release_id,
      source_url = release$resource_url[[1L]],
      source_sha256 = release$expected_sha256[[1L]],
      table_id = "cash_receipts",
      series_id = vapply(chart$series, slug, character(1)),
      series = chart$series,
      fiscal_year = chart$fiscal_year,
      period_end = as.character(chart$period_end),
      value = chart$value,
      unit = chart$unit,
      unit_multiplier = chart$unit_multiplier,
      accounting_basis = chart$accounting_basis,
      consolidation = chart$consolidation,
      actual_forecast = chart$actual_forecast,
      profile_type = ifelse(
        chart$actual_forecast == "actual",
        "actual",
        "budget"
      ),
      observation_source_url = release$resource_url[[1L]],
      observation_source_sha256 = release$expected_sha256[[1L]],
      table_family = "cash_receipts",
      table_label = "Cash receipts",
      source_table = if (
        !is.null(member) &&
          nzchar(as.character(member$name %||% ""))
      ) {
        as.character(member$name)
      } else {
        "Table 1: Australian Government general government (cash) receipts"
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

options <- parse_args(commandArgs(trailingOnly = TRUE))
if (!file.exists(options$input)) fail("Input panel does not exist.")
release_dates <- treasury_pdf_release_dates(
  options$publication,
  options$archive_dir
)
vintage_start <- as.integer(substr(release_dates$budget_vintage, 1L, 4L))
if (!is.null(options$from_vintage) && nzchar(options$from_vintage)) {
  vintage_start_min <- as.integer(substr(options$from_vintage, 1L, 4L))
  release_dates <- release_dates[vintage_start >= vintage_start_min, ]
  vintage_start <- as.integer(substr(release_dates$budget_vintage, 1L, 4L))
}
if (!is.null(options$through_vintage) && nzchar(options$through_vintage)) {
  vintage_start_max <- as.integer(substr(options$through_vintage, 1L, 4L))
  release_dates <- release_dates[vintage_start <= vintage_start_max, ]
}
if (!nrow(release_dates)) fail("No configured releases match the vintage range.")
publication_label <- if (identical(options$publication, "myefo")) {
  "MYEFO"
} else {
  "Budget"
}
panel <- utils::read.csv(
  options$input,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
manifest_path <- file.path("inst", "extdata", "releases.csv")
if (!file.exists(manifest_path)) {
  fail("Release manifest does not exist: ", manifest_path)
}
manifest <- utils::read.csv(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
manifest_rows <- match(release_dates$release_id, manifest$release_id)
if (anyNA(manifest_rows)) {
  fail("The release manifest does not include every configured release.")
}
release_key <- release_dates
release_key$source_url <- manifest$resource_url[manifest_rows]
release_key$source_sha256 <- manifest$expected_sha256[manifest_rows]
if (!"observation_source_url" %in% names(release_key)) {
  release_key$observation_source_url <- release_key$source_url
}
missing_observation_url <- is.na(release_key$observation_source_url) |
  !nzchar(release_key$observation_source_url)
release_key$observation_source_url[missing_observation_url] <-
  release_key$source_url[missing_observation_url]
if (!"observation_source_sha256" %in% names(release_key)) {
  release_key$observation_source_sha256 <- release_key$source_sha256
}
missing_observation_hash <- is.na(release_key$observation_source_sha256) |
  !nzchar(release_key$observation_source_sha256)
release_key$observation_source_sha256[missing_observation_hash] <-
  release_key$source_sha256[missing_observation_hash]

generated <- list()
cursor <- 0L
append_generated <- function(x) {
  if (is.null(x) || !nrow(x)) return(invisible(NULL))
  cursor <<- cursor + 1L
  generated[[cursor]] <<- x
  invisible(NULL)
}
for (i in seq_len(nrow(release_key))) {
  release <- release_key[i, , drop = FALSE]
  pdf_path <- if ("pdf_path" %in% names(release) &&
      !is.na(release$pdf_path[[1L]]) &&
      nzchar(release$pdf_path[[1L]])) {
    release$pdf_path[[1L]]
  } else {
    file.path(
      options$pdf_dir,
      paste0(release$budget_vintage, ".pdf")
    )
  }
  if (!file.exists(pdf_path)) fail("Missing PDF: ", pdf_path)
  sha <- digest::digest(
    file = pdf_path,
    algo = "sha256",
    serialize = FALSE
  )
  expected_pdf_sha <- if (
      "parsing_sha256" %in% names(release) &&
      !is.na(release$parsing_sha256[[1L]]) &&
      nzchar(release$parsing_sha256[[1L]])
  ) {
    release$parsing_sha256[[1L]]
  } else if (
      "observation_source_sha256" %in% names(release) &&
      !is.na(release$observation_source_sha256[[1L]]) &&
      nzchar(release$observation_source_sha256[[1L]])
  ) {
    release$observation_source_sha256[[1L]]
  } else {
    release$source_sha256[[1L]]
  }
  if (!identical(sha, expected_pdf_sha)) {
    fail("PDF hash mismatch for ", release$release_id)
  }
  lines <- read_pdf_lines(pdf_path, release$budget_vintage)

  has_major <- any(
    panel$release_id == release$release_id &
      panel$table_family == "major_economic_parameters"
  )
  if (!has_major) {
    append_generated(parse_major(lines, release))
  }
  has_aggregates <- any(
    panel$release_id == release$release_id &
      panel$table_family == "budget_aggregates"
  )
  if (!has_aggregates) {
    append_generated(parse_budget_aggregates(lines, release))
  }
  append_generated(parse_domestic(lines, release))
  append_generated(parse_cash_interest(lines, release))
  append_generated(parse_cash_receipts(lines, release))
}

if (!length(generated)) {
  fail("No table-family observations were parsed.")
}
generated <- do.call(rbind, generated)
if (identical(options$publication, "budget")) {
  current_release_ids <- paste0(
    "treasury-budget-",
    c("2024-25", "2025-26", "2026-27"),
    "-final"
  )
  selected_current <- intersect(
    current_release_ids,
    release_key$release_id
  )
  current_receipt_charts <- NULL
  if (length(selected_current)) {
    if (is.null(options$chart_cache_dir) ||
        !nzchar(options$chart_cache_dir)) {
      fail("--chart-cache-dir=PATH is required for current Budget releases")
    }
    current_receipt_charts <- parse_budget_current_charts(
      options$chart_cache_dir
    )
    current_receipt_charts <- current_receipt_charts[
      current_receipt_charts$release_id %in% selected_current,
      ,
      drop = FALSE
    ]
  }
  current_receipt_pdf <- generated[
    generated$release_id %in% selected_current &
      generated$table_family == "cash_receipts",
    ,
    drop = FALSE
  ]
  receipt_key_fields <- c(
    "release_id", "series_id", "period_end",
    "actual_forecast", "profile_type"
  )
  if (!is.null(current_receipt_charts) &&
      nrow(current_receipt_charts)) {
    current_receipt_charts <- current_receipt_charts[!(
      do.call(paste, c(
        current_receipt_charts[receipt_key_fields],
        sep = "\r"
      )) %in%
        do.call(paste, c(
          current_receipt_pdf[receipt_key_fields],
          sep = "\r"
        ))
    ), , drop = FALSE]
    generated <- rbind(
      generated,
      current_receipt_charts
    )
  }
}
replace_families <- c(
  "budget_aggregates",
  "domestic_economy_forecasts",
  "cash_interest",
  "cash_receipts"
)
replace <- rep(FALSE, nrow(panel))
for (family in c("major_economic_parameters", replace_families)) {
  family_releases <- unique(generated$release_id[
    generated$table_family == family
  ])
  replace <- replace | (
    panel$table_family == family &
      panel$release_id %in% family_releases
  )
}
panel <- panel[!replace, ]
panel$period_end <- as.character(period_end(panel$fiscal_year))
generated$period_end <- as.character(period_end(generated$fiscal_year))

all_fields <- unique(c(names(panel), names(generated)))
add_missing <- function(x) {
  for (field in setdiff(all_fields, names(x))) x[[field]] <- NA
  x[all_fields]
}
observations <- rbind(add_missing(panel), add_missing(generated))
observations <- observations[order(
  observations$release_id,
  observations$table_family,
  observations$series_id,
  observations$period_end,
  observations$profile_type
), ]

key <- paste(
  observations$release_id,
  observations$table_id,
  observations$series_id,
  observations$period_end,
  observations$actual_forecast,
  observations$profile_type,
  sep = "\r"
)
if (anyDuplicated(key) ||
    anyNA(observations$value) ||
    anyNA(observations$period_end) ||
    any(!observations$actual_forecast %in% c("actual", "forecast"))) {
  fail(
    "Generated ", publication_label,
    " table-family panel failed its observation contract."
  )
}

parent <- dirname(options$output)
if (!dir.exists(parent) &&
    !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
  fail("Could not create output directory: ", parent)
}
utils::write.csv(observations, options$output, row.names = FALSE, na = "")
message(
  "Wrote ", nrow(observations), " observations across ",
  length(unique(observations$release_id)), " ", publication_label,
  " releases and ",
  length(unique(observations$table_family)), " table families: ",
  options$output
)
