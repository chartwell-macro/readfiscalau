.rf_docx_inline_text <- function(paragraph, namespace) {
  nodes <- xml2::xml_find_all(
    paragraph,
    paste(
      ".//w:t | .//w:noBreakHyphen | .//w:softHyphen |",
      ".//w:tab | .//w:br"
    ),
    namespace
  )
  paste0(vapply(nodes, function(node) {
    name <- xml2::xml_name(node)
    if (identical(name, "t")) {
      xml2::xml_text(node)
    } else if (name %in% c("noBreakHyphen", "softHyphen")) {
      "-"
    } else {
      " "
    }
  }, character(1)), collapse = "")
}

.rf_docx_cell_text <- function(cell, namespace) {
  paragraphs <- xml2::xml_find_all(cell, ".//w:p", namespace)
  text <- vapply(
    paragraphs, .rf_docx_inline_text, character(1),
    namespace = namespace
  )
  trimws(paste(text[nzchar(trimws(text))], collapse = " "))
}

.rf_docx_table_rows <- function(table, namespace) {
  rows <- xml2::xml_find_all(table, "./w:tr", namespace)
  lapply(rows, function(row) {
    cells <- xml2::xml_find_all(row, "./w:tc", namespace)
    vapply(
      cells, .rf_docx_cell_text, character(1),
      namespace = namespace
    )
  })
}

.rf_fbo_fiscal_year <- function(x) {
  x <- gsub(
    "\u2010|\u2011|\u2012|\u2013|\u2014|\u2212",
    "-", trimws(as.character(x))
  )
  x <- gsub("[^0-9-]", "", x)
  compact <- grepl("^(?:19|20)[0-9]{4}$", x)
  x[compact] <- paste0(
    substr(x[compact], 1L, 4L), "-", substr(x[compact], 5L, 6L)
  )
  valid <- grepl("^(?:19|20)[0-9]{2}-[0-9]{2}$", x)
  x[!valid] <- NA_character_
  x
}

.rf_fbo_number <- function(x) {
  x <- gsub(
    "\u2010|\u2011|\u2012|\u2013|\u2014|\u2212",
    "-", trimws(as.character(x))
  )
  missing <- is.na(x) | !nzchar(x) |
    tolower(x) %in% c("-", "na", "n/a", "..")
  x[missing] <- NA_character_
  .rf_parse_numeric(x, "FBO historical table")
}

.rf_fbo_table_kind <- function(rows) {
  header <- tolower(paste(
    unlist(rows[seq_len(min(6L, length(rows)))], use.names = FALSE),
    collapse = " "
  ))
  if (grepl("net future", header) &&
      grepl("underlying", header) &&
      grepl("receipts", header) &&
      grepl("payments", header)) {
    return("b1")
  }
  if (grepl("headline", header) &&
      grepl("financial assets for", header) &&
      grepl("receipts", header) &&
      grepl("payments", header)) {
    return("b2")
  }
  if (grepl("net debt", header) &&
      grepl("net interest payments", header)) {
    return("b4")
  }
  if (grepl("net operating balance", header) &&
      grepl("net capital investment", header) &&
      grepl("fiscal balance", header)) {
    return("b6")
  }
  if (grepl("net worth", header) &&
      grepl("net financial worth", header)) {
    return("b7")
  }
  NA_character_
}

.rf_fbo_table_contracts <- list(
  b1 = list(
    positions = c(2L, 5L, 9L, 11L),
    series_id = c(
      "receipts", "payments", "net_future_fund_earnings",
      "underlying_cash_balance"
    ),
    series = c(
      "Receipts", "Payments", "Net Future Fund earnings",
      "Underlying cash balance"
    ),
    accounting_basis = rep("cash", 4L)
  ),
  b2 = list(
    positions = 9L,
    series_id = "headline_cash_balance",
    series = "Headline cash balance",
    accounting_basis = "cash"
  ),
  b4 = list(
    positions = 2L,
    series_id = "net_debt",
    series = "Net debt",
    accounting_basis = "accrual"
  ),
  b6 = list(
    positions = c(2L, 5L, 8L, 11L, 14L),
    series_id = c(
      "revenue", "expenses", "net_operating_balance",
      "net_capital_investment", "fiscal_balance"
    ),
    series = c(
      "Revenue", "Expenses", "Net operating balance",
      "Net capital investment", "Fiscal balance"
    ),
    accounting_basis = rep("accrual", 5L)
  ),
  b7 = list(
    positions = 2L,
    series_id = "net_worth",
    series = "Net worth",
    accounting_basis = "accrual"
  )
)

.rf_parse_fbo_history_rows <- function(rows, kind, release) {
  contract <- .rf_fbo_table_contracts[[kind]]
  parsed <- list()
  for (row in rows) {
    fiscal_year <- .rf_fbo_fiscal_year(row[[1L]])
    if (is.na(fiscal_year)) next
    if (length(row) < max(contract$positions)) {
      .rf_abort(
        sprintf("FBO Table %s data row has too few cells.", toupper(kind)),
        "schema",
        fiscal_year = fiscal_year
      )
    }
    count <- length(contract$series_id)
    period_end <- as.Date(paste0(
      as.integer(substr(fiscal_year, 1L, 4L)) + 1L, "-06-30"
    ))
    parsed[[length(parsed) + 1L]] <- tibble::tibble(
      table_id = rep(paste0("fbo_table_", kind), count),
      series_id = contract$series_id,
      series = contract$series,
      fiscal_year = rep(fiscal_year, count),
      period_end = rep(period_end, count),
      value = .rf_fbo_number(row[contract$positions]),
      unit = rep("AUD million", count),
      unit_multiplier = rep(1e6, count),
      accounting_basis = contract$accounting_basis,
      consolidation = rep("general_government_sector", count),
      actual_forecast = rep("actual", count),
      profile_type = rep("actual", count),
      cumulative = rep(TRUE, count)
    )
  }
  .rf_bind_rows(parsed)
}

.rf_parse_fbo_history_xml <- function(document, release) {
  namespace <- xml2::xml_ns(document)
  tables <- xml2::xml_find_all(document, ".//w:tbl", namespace)
  if (!length(tables)) {
    .rf_abort("FBO DOCX contains no tables.", "schema")
  }
  found <- stats::setNames(integer(length(.rf_fbo_table_contracts)),
                           names(.rf_fbo_table_contracts))
  parsed <- list()
  for (table in tables) {
    rows <- .rf_docx_table_rows(table, namespace)
    kind <- .rf_fbo_table_kind(rows)
    if (is.na(kind)) next
    found[[kind]] <- found[[kind]] + 1L
    values <- .rf_parse_fbo_history_rows(rows, kind, release)
    if (nrow(values)) parsed[[length(parsed) + 1L]] <- values
  }
  if (any(found == 0L)) {
    .rf_abort(
      "FBO DOCX lacks one or more required historical aggregate tables.",
      "schema",
      missing_tables = paste0("B.", sub("^b", "", names(found)[found == 0L]))
    )
  }
  out <- .rf_bind_rows(parsed)
  out <- out[!is.na(out$value), , drop = FALSE]
  if (!nrow(out)) {
    .rf_abort("FBO historical tables contain no numeric observations.",
              "schema")
  }
  if (max(out$period_end) != release$period_end) {
    .rf_abort(
      "FBO historical tables do not end at the declared release period.",
      "schema",
      declared_period = release$period_end,
      latest_period = max(out$period_end)
    )
  }
  .rf_assert_unique_observations(out)
}

.rf_parse_fbo_history_docx <- function(
    path, release, max_member_bytes = 10 * 1024^2) {
  listing <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) .rf_abort("Source is not a readable DOCX archive.",
                                  "schema")
  )
  unsafe <- startsWith(listing$Name, "/") |
    grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", listing$Name)
  if (any(unsafe)) {
    .rf_abort("FBO DOCX contains an unsafe member path.", "schema")
  }
  selected <- listing[listing$Name == "word/document.xml", , drop = FALSE]
  if (nrow(selected) != 1L) {
    .rf_abort("FBO DOCX must contain one word/document.xml member.",
              "schema")
  }
  if (selected$Length > max_member_bytes) {
    .rf_abort("FBO document XML exceeds the byte limit.", "too_large")
  }
  extract_dir <- tempfile("readfiscalau-docx-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  utils::unzip(
    path, files = selected$Name, exdir = extract_dir, junkpaths = FALSE
  )
  member_path <- file.path(extract_dir, selected$Name)
  size <- file.info(member_path)$size
  if (is.na(size) || size > max_member_bytes) {
    .rf_abort("Extracted FBO document XML exceeds the byte limit.",
              "too_large")
  }
  if (!identical(as.numeric(size), as.numeric(selected$Length))) {
    .rf_abort(
      "Extracted FBO document length disagrees with the ZIP directory.",
      "truncated"
    )
  }
  document <- tryCatch(
    xml2::read_xml(member_path),
    error = function(e) .rf_abort(
      paste("Could not parse FBO document XML:", conditionMessage(e)),
      "schema"
    )
  )
  data <- .rf_parse_fbo_history_xml(document, release)
  con <- file(member_path, open = "rb")
  member_raw <- tryCatch(
    readBin(con, what = "raw", n = size),
    finally = close(con)
  )
  attr(data, "readfiscalau_member") <- list(
    name = selected$Name,
    sha256 = .rf_sha256(member_raw),
    bytes = length(member_raw)
  )
  data
}
