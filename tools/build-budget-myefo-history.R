#!/usr/bin/env Rscript

# Maintainer-only builder for the static historical Budget/MYEFO table store.
# It consumes a verified official-PDF inventory and pdftotext -layout output.
# Installed-package readers never parse or download PDFs.

`%||%` <- function(x, y) if (is.null(x)) y else x

fail <- function(...) stop(paste0(...), call. = FALSE)

parse_args <- function(args) {
  out <- list(inventory = NULL, text_dir = NULL, output = NULL)
  for (arg in args) {
    if (identical(arg, "--args")) {
      next
    } else if (startsWith(arg, "--inventory=")) {
      out$inventory <- sub("^--inventory=", "", arg)
    } else if (startsWith(arg, "--text-dir=")) {
      out$text_dir <- sub("^--text-dir=", "", arg)
    } else if (startsWith(arg, "--output=")) {
      out$output <- sub("^--output=", "", arg)
    } else {
      fail("Unknown argument: ", arg)
    }
  }
  if (is.null(out$inventory) || is.null(out$text_dir) || is.null(out$output)) {
    fail("Required: --inventory=CSV --text-dir=PATH --output=RDS")
  }
  out
}

family_spec <- data.frame(
  family_id = c(
    "major_economic_parameters", "international_gdp_forecasts",
    "domestic_economy_detailed_forecasts", "ggs_budget_aggregates",
    "underlying_cash_balance_reconciliation",
    "underlying_to_headline_cash_bridge", "ggs_balance_sheet_aggregates",
    "ggs_accrual_aggregates", "fiscal_balance_reconciliation",
    "detailed_cash_receipts", "tax_receipts_economic_parameters",
    "cash_receipts_reconciliation", "accrual_revenue_reconciliation",
    "expense_estimates_reconciliation",
    "expenses_by_function_subfunction",
    "net_capital_investment_reconciliation", "forecast_ags_on_issue",
    "net_debt_components", "net_debt_reconciliation", "cash_interest",
    "accrual_interest", "formal_financial_statements"
  ),
  family_label = c(
    "Major economic parameters", "International GDP growth forecasts",
    "Domestic economy - detailed forecasts", "GGS Budget aggregates",
    "Underlying cash balance reconciliation",
    "Underlying-to-headline cash bridge", "GGS balance-sheet aggregates",
    "GGS accrual aggregates", "Fiscal-balance reconciliation",
    "Detailed cash receipts", "Economic parameters for tax receipts",
    "Cash-receipts reconciliations", "Accrual-revenue reconciliations",
    "Expense-estimate reconciliation",
    "Expenses by function and sub-function",
    "Net-capital-investment reconciliation", "Forecast AGS on issue",
    "Components of net debt", "Net-debt reconciliation",
    "Cash interest payments and receipts",
    "Accrual interest expense and income", "Formal financial statements"
  ),
  query_mode = c(
    "series_and_table", "series_and_table", "series_and_table",
    "series_and_table", "hybrid", "hybrid", "series_and_table",
    "series_and_table", "hybrid", "series_and_table", "series_and_table",
    "hybrid", "hybrid", "table_only", "series_and_table", "hybrid",
    "series_and_table", "series_and_table", "hybrid", "series_and_table",
    "series_and_table", "series_and_table"
  ),
  pattern = c(
    "^Table [^:]+: Major Economic Parameters",
    "^Table [^:]+: International (?:GDP growth|economy) forecasts",
    "^Table [^:]+: Domestic economy.*(?:detailed )?forecasts",
    "^Table [^:]+: Australian Government general government(?: sector)? budget aggregates",
    "^Table [^:]+: Reconciliation of (?:Australian Government )?general government sector underlying cash balance(?: estimates)?(?:\\([a-z]\\))?$",
    "^Table [^:]+: Reconciliation of (?:Australian Government )?general government sector underlying and headline cash balance",
    "^Table [^:]+: Australian Government general government sector balance sheet aggregates",
    "^Table [^:]+: Australian Government general government sector accrual aggregates",
    "^Table [^:]+: Reconciliation of (?:Australian Government )?general government sector fiscal balance estimates",
    paste0(
      "^Table [^:]+: Australian Government general government(?: sector)? ",
      "(?:(?:\\(cash\\)|cash) receipts|receipts(?: \\(cash basis\\)| ",
      "[—-] [0-9]{4}-[0-9]{2}|(?:\\([a-z]\\))? (?=Actual|Estimates|Budget|MYEFO)))"
    ),
    "^Table [^:]+: (?:Key )?Economic parameters for tax receipts",
    "^Table [^:]+: Reconciliation of [0-9]{4}-[0-9]{2} general government (?:\\(cash\\) |cash )?receipts",
    "^Table [^:]+: Reconciliation of [0-9]{4}-[0-9]{2} general government (?:\\(accrual\\) )?revenue",
    "^Table [^:]+: Reconciliation of (?:Australian Government )?(?:general government sector )?expense estimates",
    "^Table [^:]+: .*expenses by function and sub-function",
    "^Table [^:]+: Reconciliation of (?:Australian Government )?(?:general government sector )?net capital investment(?: estimates)?",
    "^Table [^:]+: Estimates of (?:Australian Government Securities|AGS|CGS) on issue subject to the Treasurer",
    "^Table [^:]+: Liabilities and assets included in net debt",
    "^Table [^:]+: Net debt - reconciliation",
    "^Table [^:]+: Interest payments(?:, interest receipts and net interest payments| and interest expense)",
    "^Table [^:]+: Interest expense, interest income and net interest expense",
    paste0(
      "^Table [^:]+: Australian Government ",
      "(?:general government|public non-financial corporations|total non-financial public|public financial corporations) sector ",
      "(?:operating statement|balance sheet|cash flow statement)"
    )
  ),
  stringsAsFactors = FALSE
)

slug <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-|-$", "", x)
}

decode_legacy_font_line <- function(x) {
  hint_tokens <- c(
    "7DEOH", "5HDO", "0DMRU", ")RUHFDVWV", "3URMHFWLRQV",
    "&RPPRQZHDOWK", "$XVWUDOLDQ", "8QGHUO", "(PSOR",
    ":DJHV", "&3,", "*'3", "+RXVHKROG", "'ZHOOLQJ", "*URVV",
    "<HDU", "$YHUDJH", "$FWXDO", ")RUHFDVW", "2(&'", ":RUOG",
    "7RWDO", "8QLWHG", "-DSDQ", "(XURSHDQ", "1RQ", "3DQHO",
    "'HPDQG", "6WDWHV", "3ULYDWH", "3XEOLF", "3ULFHV", "/DERXU",
    "([WHUQDO", "&KDQJH", "+HDOWK", "6RFLDO", "(GXFDWLRQ",
    "*HQHUDO", "5HYHQXH", "([SHQVHV"
  )
  encoded_hint <- any(utf8ToInt(x) %in% setdiff(1:31, c(9L, 10L, 12L, 13L))) ||
    any(vapply(hint_tokens, function(token) grepl(token, x, fixed = TRUE), logical(1)))
  if (!encoded_hint) return(x)
  pieces <- regmatches(x, gregexpr("\\S+|\\s+", x, perl = TRUE))[[1L]]
  pieces <- vapply(pieces, function(piece) {
    if (grepl("^\\s+$", piece, perl = TRUE) ||
        (!grepl("[A-Z]", piece) && !any(utf8ToInt(piece) %in% 1:31) &&
         !piece %in% hint_tokens)) return(piece)
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
  trimws(gsub("[[:space:]]+", " ", x))
}

mask_publication_vintages <- function(x) {
  # Publication names frequently form part of a row label (for example,
  # "Change since 2023-24 Budget"). Those digits are identifiers, not table
  # values. Replace them with spaces while retaining the layout positions used
  # to align genuine values with their columns.
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", x)
  hit <- gregexpr(
    "(?:19|20)[0-9]{2}(?:-[0-9]{2})?(?=[[:space:]]+(?:Budget|MYEFO|PEFO)\\b)",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )[[1L]]
  if (identical(hit[[1L]], -1L)) return(x)
  lengths <- attr(hit, "match.length")
  for (i in rev(seq_along(hit))) {
    substr(x, hit[[i]], hit[[i]] + lengths[[i]] - 1L) <-
      strrep(" ", lengths[[i]])
  }
  x
}

number_pattern <- paste0(
  "(?<![A-Za-z0-9])(?:\\(?-?[ ]{0,2})?",
  "(?:[0-9]+/[0-9]+|",
  "(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?",
  "(?:[ ]{1,2}[0-9]+/[0-9]+)?)(?:\\))?"
)

token_info <- function(x) {
  hit <- gregexpr(number_pattern, x, perl = TRUE)[[1L]]
  if (identical(hit[[1L]], -1L)) {
    return(data.frame(text = character(), start = integer(), end = integer()))
  }
  length <- attr(hit, "match.length")
  data.frame(
    text = regmatches(x, list(hit))[[1L]],
    start = as.integer(hit),
    end = as.integer(hit + length - 1L),
    stringsAsFactors = FALSE
  )
}

parse_number <- function(x) {
  x <- trimws(x)
  if (tolower(x) %in% c("-", "na", "n/a")) return(NA_real_)
  paren_negative <- startsWith(x, "(") && endsWith(x, ")")
  x <- gsub("[(),]", "", x)
  negative <- startsWith(x, "-") || paren_negative
  x <- trimws(sub("^-\\s*", "", x))
  parts <- strsplit(x, "[[:space:]]+", perl = TRUE)[[1L]]
  value <- sum(vapply(parts, function(part) {
    if (grepl("/", part, fixed = TRUE)) {
      z <- strsplit(part, "/", fixed = TRUE)[[1L]]
      as.numeric(z[[1L]]) / as.numeric(z[[2L]])
    } else as.numeric(part)
  }, numeric(1)))
  if (negative) -value else value
}

clean_label <- function(x) {
  x <- normalize_line(x)
  x <- gsub("\\([a-z]+\\)", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

clean_title <- function(x) {
  x <- normalize_line(x)
  x <- gsub("\\([a-z]\\)", "", x, ignore.case = TRUE)
  x <- sub("\\s*\\(continued\\).*$", "", x, ignore.case = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

canonical_series_id <- function(label, parent = "") {
  label <- clean_label(label)
  label <- gsub("\\(\\$(?:m|b|bn)\\)", "", label, ignore.case = TRUE, perl = TRUE)
  label <- sub("\\s*\\$(?:m|b|bn)$", "", label, ignore.case=TRUE, perl=TRUE)
  label <- sub("\\s+-\\s+(?:AUD million|AUD billion|% of GDP)$", "", label,
               ignore.case = TRUE)
  key <- tolower(gsub("[\u2018\u2019]", "'", label))
  key <- gsub("w ithholding", "withholding", key, fixed = TRUE)
  key <- gsub("incom e", "income", key, fixed = TRUE)
  key <- gsub("dw elling", "dwelling", key, fixed = TRUE)
  key <- gsub("excisablebeverages", "excisable beverages", key, fixed = TRUE)
  key <- gsub("footw ear", "footwear", key, fixed = TRUE)
  key <- gsub("dom estic", "domestic", key, fixed = TRUE)
  key <- gsub("draw backs", "drawbacks", key, fixed = TRUE)
  key <- sub("\\s*\\(per cent(?: of gdp)?\\)$", "", key, perl = TRUE)
  key <- sub("^equals\\s+", "", key, perl = TRUE)
  aliases <- c(
    "interest on commonwealth behalf" = "interest-on-australian-government-s-behalf",
    "interest on commonwealth's behalf" = "interest-on-australian-government-s-behalf",
    "interest on australian government's behalf" = "interest-on-australian-government-s-behalf",
    "interest on government's behalf" = "interest-on-australian-government-s-behalf",
    "mining and mineral resources manufacturing and construction" = "mining-manufacturing-and-construction",
    "mining and mineral resources (other than fuels), manufacturing and construction" = "mining-manufacturing-and-construction",
    "mining, manufacturing and construction" = "mining-manufacturing-and-construction",
    "mining, manufacturing & construction" = "mining-manufacturing-and-construction",
    "plus net future fund earnings" = "net-future-fund-earnings",
    "less net future fund earnings" = "net-future-fund-earnings",
    "pharmaceutical services and benefits" = "pharmaceutical-benefits-and-services",
    "national health care specific purpose payment" = "national-healthcare-specific-purpose-payment",
    "total customs duty receipts" = "total-customs-duty",
    "total excise duty receipts" = "total-excise-duty",
    "total individuals and other withholding tax" = "total-individuals-and-other-withholding-taxation",
    "total individuals and other withholding" = "total-individuals-and-other-withholding-taxation",
    "total individuals and withholding taxation" = "total-individuals-and-other-withholding-taxation",
    "interest received on commonwealth government stock" = "interest-received-on-australian-government-stock",
    "interest received on commonwealth stock" = "interest-received-on-australian-government-stock",
    "payment for goods and services" = "payments-for-goods-and-services",
    "gst payment to taxation authority" = "gst-payments-to-taxation-authority",
    "land and fixed assets" = "land-and-other-fixed-assets",
    "total cash payments from financing activities" = "total-cash-payments-for-financing-activities",
    "interest received" = "interest-receipts",
    "companies" = "company-tax",
    "total superannuation funds" = "superannuation-fund-tax",
    "superannuation funds" = "superannuation-fund-tax",
    "total superannuation tax" = "superannuation-fund-tax",
    "total superannuation taxation" = "superannuation-fund-tax",
    "superannuation fund taxes" = "superannuation-fund-tax",
    "customs duty" = "total-customs-duty",
    "wage cost index" = "wage-price-index",
    "republic of korea" = "south-korea",
    "average earnings g" = "average-earnings",
    "average earnings national accounts basis" = "average-earnings",
    "farm product at basic prices" = "farm-product",
    "unemployment rate per cent" = "unemployment-rate",
    "participation rate per cent" = "participation-rate",
    "memorandum: capital gains tax" = "capital-gains-tax",
    "school eduation - specific funding" = "school-education-specific-funding",
    "general purpose inter-government transaction" = "general-purpose-inter-government-transactions",
    "other welfare programmes" = "other-welfare-programs",
    "assistance to veterans and dependents" = "assistance-to-veterans-and-dependants",
    "revenue assistance to the states and terri" = "revenue-assistance-to-the-states-and-territories",
    "dividend income" = "dividend-and-distribution-income",
    "dividends and income tax equivalents" = "dividends-distributions-and-income-tax-equivalents",
    "plus net cash flows from financing activities for leases" = "plus-principal-payments-of-lease-liabilities",
    "real gdp" = "real-gross-domestic-product",
    "real gdp growth" = "real-gross-domestic-product",
    "nominal gdp" = "nominal-gross-domestic-product",
    "nominal gdp growth" = "nominal-gross-domestic-product",
    "consumer price index growth" = "consumer-price-index",
    "consumer price index - headline" = "consumer-price-index",
    "wage price index growth" = "wage-price-index",
    "employment growth" = "employment",
    "underlying cash balance - aud million" = "underlying-cash-balance",
    "underlying cash balance - % of gdp" = "underlying-cash-balance",
    "fiscal balance - aud million" = "fiscal-balance",
    "fiscal balance - % of gdp" = "fiscal-balance"
  )
  if (key %in% names(aliases)) return(unname(aliases[[key]]))
  for (concept in c(
    "underlying cash balance", "headline cash balance", "fiscal balance",
    "total receipts", "total payments", "total revenue", "total expenses",
    "net operating balance", "net capital investment", "gross debt",
    "net debt", "net worth", "net financial worth"
  )) {
    if (grepl(paste0("^", concept, "(?:$|\\b)"), key, perl = TRUE)) {
      return(slug(concept))
    }
  }
  if (nzchar(parent) && key %in% c("other", "total", "of which")) {
    return(slug(paste(parent, key)))
  }
  slug(key)
}

infer_unit <- function(family, label, title, header = "") {
  text <- tolower(paste(label, title, header))
  if (grepl("per cent of gdp|% of gdp", tolower(label))) {
    return(c(unit = "% of GDP", multiplier = "0.01"))
  }
  if (grepl("^\\$b(?:illion|n)?$", tolower(label), perl = TRUE)) {
    return(c(unit = "AUD billion", multiplier = "1e9"))
  }
  if (grepl("percentage of gdp", tolower(label), fixed = TRUE)) {
    return(c(unit = "% of GDP", multiplier = "0.01"))
  }
  if (family == "international_gdp_forecasts") {
    return(c(unit = "Per cent change", multiplier = "0.01"))
  }
  if (family %in% c(
      "major_economic_parameters", "domestic_economy_detailed_forecasts",
      "tax_receipts_economic_parameters")) {
    if (grepl("unemployment rate|participation rate", tolower(label))) {
      return(c(unit = "Per cent", multiplier = "0.01"))
    }
    if (grepl("net overseas migration", tolower(label))) {
      return(c(unit = "Persons", multiplier = "1"))
    }
    if (grepl("change in inventories|net exports", tolower(label))) {
      return(c(unit = "Percentage points", multiplier = "0.01"))
    }
    return(c(unit = "Per cent change", multiplier = "0.01"))
  }
  if (grepl("\\$b|billion", text, perl = TRUE)) {
    return(c(unit = "AUD billion", multiplier = "1e9"))
  }
  if (grepl("per cent|percentage|rate", tolower(label))) {
    return(c(unit = "Per cent", multiplier = "0.01"))
  }
  c(unit = "AUD million", multiplier = "1e6")
}

infer_basis <- function(family, label, title) {
  if (family == "formal_financial_statements") {
    return(if (grepl("cash flow", title, ignore.case = TRUE)) "cash" else "accrual")
  }
  if (family %in% c(
      "underlying_cash_balance_reconciliation",
      "underlying_to_headline_cash_bridge", "detailed_cash_receipts",
      "cash_receipts_reconciliation", "cash_interest")) return("cash")
  if (family %in% c(
      "ggs_accrual_aggregates", "ggs_balance_sheet_aggregates",
      "fiscal_balance_reconciliation",
      "accrual_revenue_reconciliation", "expense_estimates_reconciliation",
      "expenses_by_function_subfunction", "net_capital_investment_reconciliation",
      "net_debt_components", "net_debt_reconciliation", "accrual_interest"
  )) return("accrual")
  text <- tolower(paste(label, title))
  if (grepl("underlying cash|headline cash|cash receipts|(?:tax|non-tax) receipts|payments|^receipts|future fund earnings", text)) return("cash")
  if (grepl("fiscal balance|revenue|expenses|net operating|net capital investment|net debt", text)) return("accrual")
  "not_applicable"
}

infer_consolidation <- function(family, title) {
  if (family %in% c(
    "major_economic_parameters", "international_gdp_forecasts",
    "domestic_economy_detailed_forecasts", "tax_receipts_economic_parameters"
  )) return("australian_economy")
  title <- tolower(title)
  if (grepl("public non-financial corporations", title, fixed = TRUE)) {
    return("public_non_financial_corporations_sector")
  }
  if (grepl("total non-financial public sector", title, fixed = TRUE)) {
    return("total_non_financial_public_sector")
  }
  if (grepl("public financial corporations", title, fixed = TRUE)) {
    return("public_financial_corporations_sector")
  }
  "general_government_sector"
}

series_eligible <- function(mode, label) {
  if (mode == "table_only") return(FALSE)
  if (!grepl("[[:alpha:]]", label) ||
      grepl("^(?:change since|month of peak|as at\\b)", label,
            ignore.case = TRUE, perl = TRUE)) return(FALSE)
  if (tolower(label) %in% c(
    "budget", "myefo", "actual", "actuals", "outcome", "outcomes",
    "estimate", "estimates", "forecast", "forecasts", "projections"
  )) return(FALSE)
  if (mode == "series_and_table") return(TRUE)
  canonical_series_id(label) %in% c(
    "underlying-cash-balance", "headline-cash-balance", "fiscal-balance",
    "total-receipts", "total-revenue", "total-expenses", "total-payments",
    "net-capital-investment", "net-debt"
  )
}

is_prose_or_heading_label <- function(label) {
  label <- trimws(label)
  if (!nzchar(label) || tolower(label) %in% c(
    "part", "page", "statement", "table"
  )) return(TRUE)
  if (nchar(label) > 100L) return(TRUE)
  if (grepl("^[a-j][[:space:]]+The[[:space:]]", label,
            ignore.case = TRUE, perl = TRUE)) return(TRUE)
  grepl(
    paste0(
      "(?:\\b(?:is|are|was|were)[[:space:]]+",
      "(?:forecast|estimated|projected|expected|shown|allocated|associated)|",
      "\\brepresents\\b|\\bcontributing to\\b|\\bcompared with\\b|",
      "\\bincreased in\\b|\\bdecreased in\\b)"
    ),
    label,
    ignore.case = TRUE,
    perl = TRUE
  )
}

table_number <- function(title) {
  sub("^Table\\s+([^:]+):.*$", "\\1", title, ignore.case = TRUE, perl = TRUE)
}

period_info <- function(label, release_date) {
  label <- trimws(label)
  four_quarters <- regmatches(
    label,
    regexpr("Four quarters to June (?:19|20)[0-9]{2}", label,
            ignore.case = TRUE, perl = TRUE)
  )
  if (length(four_quarters) && nzchar(four_quarters)) {
    finish <- as.integer(sub(".*((?:19|20)[0-9]{2})$", "\\1",
                             four_quarters, perl = TRUE))
    return(list(
      fiscal_year = sprintf("%04d-%02d", finish - 1L, finish %% 100L),
      calendar_year = NA_integer_,
      period_end = as.Date(sprintf("%d-06-30", finish)),
      period_type = "four_quarters_to_june",
      actual_forecast = if (isTRUE(as.Date(sprintf("%d-06-30", finish)) < release_date))
        "actual" else "forecast"
    ))
  }
  fy_long <- regmatches(
    label,
    regexpr("(?:19|20)[0-9]{2}-(?:19|20)[0-9]{2}", label, perl = TRUE)
  )
  if (length(fy_long) && nzchar(fy_long)) {
    start <- as.integer(substr(fy_long, 1L, 4L))
    finish <- as.integer(substr(fy_long, 6L, 9L))
    if (finish == start + 1L) {
      fy <- sprintf("%04d-%02d", start, finish %% 100L)
      return(list(
        fiscal_year = fy, calendar_year = NA_integer_,
        period_end = as.Date(sprintf("%d-06-30", finish)),
        period_type = "fiscal_year",
        actual_forecast = if (isTRUE(as.Date(sprintf("%d-06-30", finish)) < release_date))
          "actual" else "forecast"
      ))
    }
  }
  fy <- regmatches(
    label,
    regexpr("(?:19|20)[0-9]{2}-(?:(?:19|20)[0-9]{2}|[0-9]{2})(?![0-9])", label, perl = TRUE)
  )
  if (length(fy) && nzchar(fy)) {
    end <- as.Date(paste0(as.integer(substr(fy, 1L, 4L)) + 1L, "-06-30"))
    return(list(
      fiscal_year = paste0(substr(fy, 1L, 5L), substr(format(end, "%Y"), 3L, 4L)), calendar_year = NA_integer_, period_end = end,
      period_type = "fiscal_year",
      actual_forecast = if (isTRUE(end < release_date)) "actual" else "forecast"
    ))
  }
  cy <- regmatches(label, regexpr("(?:19|20)[0-9]{2}", label, perl = TRUE))
  if (length(cy) && nzchar(cy)) {
    year <- as.integer(cy)
    end <- as.Date(sprintf("%d-12-31", year))
    return(list(
      fiscal_year = NA_character_, calendar_year = year, period_end = end,
      period_type = "calendar_year",
      actual_forecast = if (isTRUE(end < release_date)) "actual" else "forecast"
    ))
  }
  list(
    fiscal_year = NA_character_, calendar_year = NA_integer_,
    period_end = as.Date(NA), period_type = "table_column",
    actual_forecast = "not_applicable"
  )
}

header_columns <- function(block, data_start_hint = 2L) {
  layout_block <- vapply(
    block, decode_legacy_font_line, character(1), USE.NAMES = FALSE
  )
  # Treasury PDFs use several Unicode dash glyphs in fiscal-year headings.
  # Normalize them before locating year spans; otherwise `2023–24` is
  # truncated to calendar year `2023` and receives an incorrect December date.
  layout_block <- gsub(
    "[\u2010\u2011\u2012\u2013\u2014\u2212]",
    "-",
    layout_block
  )
  limit <- seq_len(min(length(layout_block), 16L))
  year_pattern <- "(?:19|20)[0-9]{2}(?:-(?:(?:19|20)[0-9]{2}|[0-9]{2}))?"
  year_hits <- lapply(layout_block[limit], function(x) {
    hit <- gregexpr(year_pattern, x, perl = TRUE)[[1L]]
    if (identical(hit[[1L]], -1L)) return(data.frame())
    len <- attr(hit, "match.length")
    data.frame(
      label = regmatches(x, list(hit))[[1L]], start = hit,
      end = hit + len - 1L, stringsAsFactors = FALSE
    )
  })
  year_counts <- vapply(year_hits, nrow, integer(1))
  primary_header <- if (max(year_counts) >= 2L) which.max(year_counts) else NA_integer_
  header_lines <- integer()
  if (!is.na(primary_header)) {
    header_lines <- primary_header
    adjacent <- which(
      year_counts > 0L & seq_along(year_counts) != 1L &
        abs(seq_along(year_counts) - primary_header) <= 2L &
        (year_counts >= 2L | grepl(
          "Actual|Outcome|Estimate|Forecast|Projection",
          layout_block[limit],
          ignore.case = TRUE
        )) &
        grepl(
          "Budget|MYEFO|PEFO|Actual|Outcome|Estimate|Forecast|Projection",
          layout_block[limit],
          ignore.case = TRUE
        )
    )
    header_lines <- sort(unique(c(header_lines, adjacent)))
  }
  header_at <- if (length(header_lines)) max(header_lines) else NA_integer_

  candidate_start <- if (is.na(header_at)) data_start_hint else header_at + 1L
  candidate_end <- min(
    length(layout_block),
    if (is.na(header_at)) 60L else header_at + 70L
  )
  if (candidate_start > candidate_end) {
    return(list(
      header_at = header_at, labels = character(), ends = numeric(),
      profiles = character(), units = character()
    ))
  }
  candidate_at <- seq.int(candidate_start, candidate_end)
  token_rows <- lapply(candidate_at, function(i) {
    token_line <- mask_publication_vintages(layout_block[[i]])
    info <- token_info(token_line)
    prefix <- if (nrow(info)) {
      trimws(substr(token_line, 1L, info$start[[1L]] - 1L))
    } else ""
    if (!nrow(info) || !nzchar(prefix) ||
        grepl("^(?:a|b|c|d|e|f|g|h|i|j)\\)|^(?:Note|Source):", prefix,
              ignore.case = TRUE)) {
      return(NULL)
    }
    info
  })
  data_tokens <- token_rows[!vapply(token_rows, is.null, logical(1))]
  data_tokens <- data_tokens[vapply(data_tokens, nrow, integer(1)) >= 2L]

  header_marks <- data.frame(
    label = character(), start = integer(), end = integer(),
    stringsAsFactors = FALSE
  )
  if (length(header_lines)) {
    header_marks <- do.call(rbind, year_hits[header_lines])
  }
  header_scope <- layout_block[limit]
  total_scope <- layout_block[seq_len(
    if (is.na(header_at)) min(4L, length(layout_block)) else header_at
  )]
  total_hits <- lapply(total_scope, function(x) {
    hit <- gregexpr("\\bTotal(?:\\([a-z]\\))?", x, ignore.case = TRUE,
                    perl = TRUE)[[1L]]
    if (identical(hit[[1L]], -1L)) return(data.frame())
    len <- attr(hit, "match.length")
    data.frame(
      label = "Total", start = hit, end = hit + len - 1L,
      stringsAsFactors = FALSE
    )
  })
  total_hits <- total_hits[vapply(total_hits, nrow, integer(1)) > 0L]
  if (length(total_hits)) header_marks <- rbind(header_marks, total_hits[[1L]])
  if (nrow(header_marks)) {
    header_marks <- header_marks[order(header_marks$end), , drop = FALSE]
    header_marks <- header_marks[!duplicated(header_marks$end), , drop = FALSE]
  }

  if (!nrow(header_marks) || !length(data_tokens)) {
    return(list(
      header_at = header_at, labels = character(), ends = numeric(),
      profiles = character(), units = character()
    ))
  }

  unit_pattern <- "\\$(?:m|b|bn)\\b"
  unit_rows <- lapply(header_scope, function(x) {
    hit <- gregexpr(unit_pattern, x, ignore.case = TRUE, perl = TRUE)[[1L]]
    if (identical(hit[[1L]], -1L)) return(integer())
    hit + attr(hit, "match.length") - 1L
  })
  unit_counts <- vapply(unit_rows, length, integer(1))
  data_counts <- vapply(data_tokens, nrow, integer(1))
  count_frequency <- table(data_counts)
  modal_count <- as.integer(names(count_frequency)[
    count_frequency == max(count_frequency)
  ])
  modal_count <- max(modal_count)
  # Numeric note references are left of the dated monetary columns. They
  # must never enlarge the column count or become the first observation.
  has_note_column <- any(grepl("\\bNote\\b", header_scope, perl = TRUE))
  column_count <- if (has_note_column && max(unit_counts) >= 2L) {
    max(unit_counts)
  } else max(nrow(header_marks), modal_count, max(unit_counts))
  complete_rows <- data_tokens[
    vapply(data_tokens, nrow, integer(1)) == column_count
  ]
  ends <- numeric()
  matching_unit_rows <- unit_rows[unit_counts == column_count]
  if (length(matching_unit_rows)) {
    ends <- as.numeric(matching_unit_rows[[1L]])
  } else if (nrow(header_marks) == column_count) {
    ends <- as.numeric(header_marks$end)
  } else if (length(complete_rows)) {
    ends <- vapply(seq_len(column_count), function(column) {
      stats::median(vapply(
        complete_rows,
        function(info) info$end[[column]],
        numeric(1)
      ))
    }, numeric(1))
  }
  if (length(ends) != column_count || any(diff(ends) <= 0)) {
    return(list(
      header_at = header_at, labels = character(), ends = numeric(),
      profiles = character(), units = character()
    ))
  }

  header_text <- paste(header_scope, collapse = " ")
  header_text <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", header_text)
  header_text <- trimws(gsub("[[:space:]]+", " ", header_text))
  june_year <- regmatches(
    header_text,
    regexpr("June[[:space:]]+(?:19|20)[0-9]{2}", header_text,
            ignore.case = TRUE, perl = TRUE)
  )
  has_four_quarters <- grepl("Four", header_text, ignore.case = TRUE) &&
    grepl("Quarters", header_text, ignore.case = TRUE) &&
    length(june_year) && nzchar(june_year)
  labels <- if (nrow(header_marks) == column_count && has_four_quarters) {
    c(
      as.character(header_marks$label[seq_len(column_count - 1L)]),
      paste("Four quarters to", june_year)
    )
  } else if (nrow(header_marks) == column_count) {
    as.character(header_marks$label)
  } else if (nrow(header_marks) == column_count - 1L && has_four_quarters) {
    c(
      as.character(header_marks$label),
      paste("Four quarters to", june_year)
    )
  } else {
    vapply(ends, function(column_end) {
      header_marks$label[[which.min(abs(header_marks$end - column_end))]]
    }, character(1))
  }

  token_rows_present <- !vapply(token_rows, is.null, logical(1))
  data_row <- candidate_at[token_rows_present]
  data_row <- data_row[vapply(
    token_rows[token_rows_present],
    nrow,
    integer(1)
  ) >= 2L]
  profile_scope_end <- if (length(data_row)) {
    min(data_row) - 1L
  } else {
    min(length(layout_block), if (is.na(header_at)) 6L else header_at + 3L)
  }
  profile_hits <- lapply(layout_block[seq_len(profile_scope_end)], function(x) {
    hit <- gregexpr("\\b(?:Budget|MYEFO|PEFO)\\b", x, perl = TRUE)[[1L]]
    if (identical(hit[[1L]], -1L)) return(data.frame())
    len <- attr(hit, "match.length")
    data.frame(
      label = tolower(regmatches(x, list(hit))[[1L]]),
      end = hit + len - 1L,
      stringsAsFactors = FALSE
    )
  })
  profile_hits <- profile_hits[vapply(profile_hits, nrow, integer(1)) > 0L]
  profile_marks <- if (length(profile_hits)) do.call(rbind, profile_hits) else NULL
  profiles <- rep(NA_character_, column_count)
  if (!is.null(profile_marks) && nrow(profile_marks) >= 2L &&
      nrow(profile_marks) <= column_count) {
    profile_marks <- profile_marks[order(profile_marks$end), , drop = FALSE]
    target <- seq.int(column_count - nrow(profile_marks) + 1L, column_count)
    profiles[target] <- profile_marks$label
    profile_group_size <- length(unique(profile_marks$label))
    forecast_group_count <- nrow(profile_marks) / profile_group_size
    actual_mark_count <- nrow(header_marks) - forecast_group_count
    if (profile_group_size >= 2L &&
        forecast_group_count == as.integer(forecast_group_count) &&
        actual_mark_count == column_count - nrow(profile_marks) &&
        actual_mark_count >= 1L) {
      labels <- c(
        as.character(header_marks$label[seq_len(actual_mark_count)]),
        rep(
          as.character(header_marks$label[seq.int(
            actual_mark_count + 1L, nrow(header_marks)
          )]),
          each = profile_group_size
        )
      )
    }
  }
  unit_marks <- lapply(header_scope, function(x) {
    hit <- gregexpr(
      "\\$(?:m|b|bn)\\b|%[[:space:]]*(?:of[[:space:]]*)?GDP\\b",
      x,
      ignore.case = TRUE,
      perl = TRUE
    )[[1L]]
    if (identical(hit[[1L]], -1L)) return(data.frame())
    len <- attr(hit, "match.length")
    value <- tolower(regmatches(x, list(hit))[[1L]])
    data.frame(
      unit = ifelse(
        grepl("%", value, fixed = TRUE),
        "% of GDP",
        ifelse(grepl("b", value, fixed = TRUE),
               "AUD billion", "AUD million")
      ),
      end = hit + len - 1L,
      stringsAsFactors = FALSE
    )
  })
  unit_counts_full <- vapply(unit_marks, nrow, integer(1))
  units <- rep(NA_character_, column_count)
  if (length(unit_counts_full) && max(unit_counts_full) >= column_count) {
    marks <- unit_marks[[which.max(unit_counts_full)]]
    units <- if (nrow(marks) == column_count) {
      marks$unit
    } else {
      vapply(ends, function(column_end) {
        marks$unit[[which.min(abs(marks$end - column_end))]]
      }, character(1))
    }
  }
  list(
    header_at = header_at, labels = labels, ends = ends,
    profiles = profiles, units = units
  )
}

parse_table_part <- function(block, table_row, release, columns = NULL) {
  if (is.null(columns)) columns <- header_columns(block)
  if (!length(columns$ends)) return(NULL)
  start <- if (is.na(columns$header_at)) 2L else columns$header_at + 1L
  stop <- length(block)
  footnote <- grep(
    "^(?:\\(?[a-z]\\)|Note:|Source:)",
    normalize_line(block),
    ignore.case = TRUE,
    perl = TRUE
  )
  footnote <- footnote[footnote > start]
  if (length(footnote)) stop <- min(stop, footnote[[1L]] - 1L)
  blank <- !nzchar(normalize_line(block))
  double_blank <- which(blank[-length(blank)] & blank[-1L]) + 1L
  possible_data <- vapply(seq_along(block), function(i) {
    token_line <- mask_publication_vintages(
      decode_legacy_font_line(block[[i]])
    )
    info <- token_info(token_line)
    if (nrow(info) < min(2L, length(columns$ends))) return(FALSE)
    prefix <- trimws(substr(token_line, 1L, info$start[[1L]] - 1L))
    if (!nzchar(prefix) || !grepl("[[:alpha:]]", prefix)) return(FALSE)
    if (nrow(info) == length(columns$ends)) return(TRUE)
    aligned <- vapply(info$end, function(token_end) {
      min(abs(columns$ends - token_end)) <= 10L
    }, logical(1))
    sum(aligned) >= min(2L, length(columns$ends))
  }, logical(1))
  first_data <- which(possible_data & seq_along(block) >= start)
  blank_floor <- if (length(first_data)) first_data[[1L]] + 1L else start + 1L
  double_blank <- double_blank[double_blank > blank_floor]
  if (length(double_blank)) {
    closes_table <- vapply(double_blank, function(blank_at) {
      if (blank_at >= length(block)) return(TRUE)
      lookahead <- seq.int(
        blank_at + 1L,
        min(length(block), blank_at + 8L)
      )
      !any(possible_data[lookahead])
    }, logical(1))
    double_blank <- double_blank[closes_table]
  }
  if (length(double_blank)) {
    stop <- min(stop, double_blank[[1L]] - 2L)
  }
  if (stop < start) return(NULL)

  cells <- list()
  cursor <- 0L
  row_index <- 0L
  section <- ""
  section_depth <- Inf
  pending <- ""
  previous_series <- ""
  primary_function <- ""
  cash_direction <- ""
  receipt_group <- ""
  continuation_lines <- integer()
  adjusted_expenditure <- FALSE
  equity_flows <- FALSE
  for (i in seq.int(start, stop)) {
    if (i %in% continuation_lines) next
    raw <- block[[i]]
    decoded_raw <- decode_legacy_font_line(raw)
    clean <- normalize_line(decoded_raw)
    if (identical(clean, "&XUUHQW DFFRXQW EDODQFH")) {
      clean <- "Current account balance"
    }
    if (!nzchar(clean)) next
    if (grepl(
      paste0(
        "^(?:(?:Year|Average|Actual|Actuals|Outcome|Outcomes|Estimate|",
        "Estimates|Forecast|Forecasts|Projection|Projections|Four|",
        "Quarters|to|June)[[:space:]]*)+(?:[0-9]{4})?$"
      ),
      clean,
      ignore.case = TRUE,
      perl = TRUE
    )) next
    if (grepl(
      "climate spending is shown on an aggregated basis in Table",
      clean,
      ignore.case = TRUE
    )) next
    # Some bridge-table row labels begin with the publication vintage (for
    # example, "2025-26 MYEFO underlying cash balance"). Keep that vintage as
    # label text rather than mistaking it for the first numeric observation.
    layout_raw <- gsub(
      "[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", decoded_raw
    )
    label_raw <- layout_raw
    leading_vintage <- regexpr(
      "^\\s*(?:19|20)[0-9]{2}-[0-9]{2}(?=\\s+[[:alpha:]])",
      label_raw,
      perl = TRUE
    )
    if (leading_vintage[[1L]] != -1L) {
      match_length <- attr(leading_vintage, "match.length")
      substr(label_raw, leading_vintage[[1L]],
             leading_vintage[[1L]] + match_length - 1L) <-
        strrep(" ", match_length)
    }
    token_raw <- mask_publication_vintages(layout_raw)
    info <- token_info(token_raw)
    if (table_row$family_id == "formal_financial_statements" && nrow(info)) {
      info <- info[info$end >= min(columns$ends) - 10L, , drop = FALSE]
    }
    if (nrow(info) && nrow(info) != length(columns$ends)) {
      # Numeric text inside a row label (for example "COVID-19" or
      # "100,000 Homes") is not a table observation. Published values are
      # right-aligned with the detected data columns. If the row already has
      # exactly one token per column, all tokens are observations even where a
      # PDF has shifted the whole row left relative to the header.
      aligned <- vapply(info$end, function(token_end) {
        min(abs(columns$ends - token_end)) <= 10L
      }, logical(1))
      info <- info[aligned, , drop = FALSE]
    }
    if (!nrow(info)) {
      primary_functions <- c("General public services", "Defence", "Public order and safety",
        "Education", "Health", "Social security and welfare", "Housing and community amenities",
        "Recreation and culture", "Fuel and energy", "Agriculture, forestry and fishing",
        "Mining, manufacturing and construction", "Transport and communication",
        "Other economic affairs", "Other purposes")
      function_label <- clean_label(clean)
      if (tolower(function_label) %in% tolower(primary_functions)) primary_function <- tolower(function_label)
      if (grepl("^Cash receipts from financing", clean, ignore.case = TRUE)) cash_direction <- "financing receipts"
      if (grepl("^Cash payments (?:from|for) financing", clean, ignore.case = TRUE)) cash_direction <- "financing payments"
      if (grepl("^Non-owner movements in equity$", clean, ignore.case = TRUE)) equity_flows <- TRUE
      if (grepl("^Panel . - Expenditure Excluding", clean, ignore.case = TRUE)) adjusted_expenditure <- TRUE
      else if (grepl("^Panel . -", clean, ignore.case = TRUE)) adjusted_expenditure <- FALSE
      if (tolower(clean) %in% c("income taxation", "indirect taxation", "other indirect taxation", "other taxation", "non-taxation")) receipt_group <- tolower(clean)
      if (!grepl("^(Actual|Outcome|Estimate|Forecast|Projection|\\$[mb])", clean,
                 ignore.case = TRUE)) {
        if (identical(clean, "(other than fuels),") &&
            identical(pending, "Mining and mineral resources")) {
          clean <- paste(pending, clean)
        }
        if (table_row$family_id == "expenses_by_function_subfunction" &&
            grepl("^Mining and mineral resources", pending, ignore.case = TRUE) &&
            grepl("^(?:\\(other than|manufacturing|MANUFACTURING|CONSTRUCTION|\\(OTHER THAN)", clean)) clean <- paste(pending, clean)
        if (nzchar(pending) && grepl("(?: and| -|resources|MANUFACTURING AND|FUELS\\),)$", pending, ignore.case = TRUE) &&
            grepl("^(?:\\(other than|manufacturing|MANUFACTURING|CONSTRUCTION|\\(OTHER THAN)", clean)) clean <- paste(pending, clean)
        section <- clean_label(sub(
          paste0(
            "^(?:(?:19|20)[0-9]{2}-[0-9]{2}[[:space:]]+)?",
            "(?:Budget|MYEFO|PEFO)[[:space:]]+"
          ),
          "",
          clean,
          ignore.case = TRUE,
          perl = TRUE
        ))
        section_depth <- nchar(raw) - nchar(sub("^\\s+", "", raw))
        pending <- section
      }
      next
    }
    raw_label <- trimws(substr(label_raw, 1L, info$start[[1L]] - 1L))
    if (grepl("^(?:to[[:space:]]+)?\\$$", raw_label, ignore.case = TRUE)) next
    label <- raw_label
    label <- clean_label(label)
    if (!nzchar(label) && nzchar(pending)) {
      label <- pending
      pending <- ""
    }
    if (!nzchar(label) || grepl("^(Actual|Outcome|Estimate|Forecast|Projection|\\$(?:m|b|bn)?)$",
                                label, ignore.case = TRUE)) next
    if (nzchar(pending) && nchar(label) < 45L &&
        (grepl("^[a-z]", label, ignore.case = FALSE) ||
         grepl("^(?:Government|States|Territories|CONSTRUCTION|Manufacturing|Islander|Australians)\\b", label))) {
      label <- clean_label(paste(pending, label))
    }
    # Some labels wrap after the values rather than before them.
    if (i < stop) {
      suffix <- clean_label(decode_legacy_font_line(block[[i+1L]]))
      wrapped_suffix <- suffix %in% c("behalf", "and Territories", "and territories", "stock", "partners", "taxation", "activities", "fishing", "affairs", "Australians nec", "Islander health", "investments")
      if (wrapped_suffix && !nrow(token_info(block[[i+1L]]))) {
        label <- clean_label(paste(label, suffix))
        continuation_lines <- c(continuation_lines, i+1L)
      }
    }
    label <- sub(
      "^(?:Budget|MYEFO|PEFO)\\s+",
      "",
      label,
      ignore.case = TRUE,
      perl = TRUE
    )
    if (table_row$family_id == "formal_financial_statements" &&
        grepl("[[:alpha:]]", label)) {
      label <- sub("\\s+[0-9]{1,2}$", "", label, perl = TRUE)
    }
    if (table_row$family_id %in% c(
      "major_economic_parameters", "international_gdp_forecasts",
      "domestic_economy_detailed_forecasts",
      "tax_receipts_economic_parameters"
    ) && grepl("[[:alpha:]]", label)) {
      label <- sub("\\s+[a-j]$", "", label, ignore.case = TRUE, perl = TRUE)
    }
    if (is_prose_or_heading_label(label) || endsWith(label, "$")) next
    if (!grepl("[[:alpha:]]", label) ||
        grepl("^(?:month of peak|as at\\b)", label,
              ignore.case = TRUE, perl = TRUE)) next
    pending <- ""
    row_index <- row_index + 1L
    row_depth <- nchar(raw) - nchar(sub("^\\s+", "", raw))
    if (nzchar(section) && row_depth <= section_depth) {
      section <- ""
      section_depth <- Inf
    }
    parent <- if (nzchar(section) && !identical(section, label)) section else ""
    row_path <- if (nzchar(parent)) paste(parent, label, sep = " > ") else label
    series_label <- label
    if (table_row$family_id == "domestic_economy_detailed_forecasts" &&
        grepl("^Unemployment rate", label, ignore.case=TRUE)) {
      reference <- regmatches(raw_label, regexpr("\\([a-z]\\)$", raw_label))
      if (length(reference) && nzchar(reference)) {
        notes <- normalize_line(block)
        at <- which(startsWith(notes, reference))
        if (length(at) && grepl("June quarter", paste(notes[at[[1L]]:min(at[[1L]]+1L,length(notes))],collapse=" "),fixed=TRUE)) {
          series_label <- "Unemployment rate — June quarter"
        }
      }
    }
    if (table_row$family_id == "expenses_by_function_subfunction" &&
        label == "General administration" && nzchar(primary_function)) {
      series_label <- paste(tools::toTitleCase(primary_function), label, sep = " — ")
    }
    if (table_row$family_id == "expenses_by_function_subfunction" && label == "Other health services") {
      series_label <- if (row_depth > 0L) "Other health services — other component" else "Other health services — total"
    }
    if (table_row$family_id == "formal_financial_statements" && label == "Actuarial revaluations") {
      series_label <- paste(label, if (equity_flows) "included in equity" else "included in operating result", sep = " — ")
    }
    if (table_row$family_id == "domestic_economy_detailed_forecasts" && adjusted_expenditure) {
      series_label <- paste(label, "excluding transfers and one-off transactions", sep = " — ")
    }
    if (table_row$family_id == "formal_financial_statements" &&
        infer_basis(table_row$family_id, label, table_row$source_table) == "cash" &&
        tolower(label) %in% c("borrowing", "other financing") && nzchar(cash_direction)) {
      series_label <- paste(label, cash_direction, sep = " — ")
    }
    if (table_row$family_id == "detailed_cash_receipts" && tolower(label) == "other" && receipt_group == "non-taxation") {
      series_label <- "Other non-taxation receipts"
    }
    if (table_row$family_id == "detailed_cash_receipts" && tolower(label) == "income taxation receipts") receipt_group <- "indirect taxation"
    if (table_row$family_id == "detailed_cash_receipts" && tolower(label) == "taxation receipts") receipt_group <- "non-taxation"
    if (table_row$family_id == "detailed_cash_receipts" && tolower(label) == "surcharge" && grepl("superannuation", parent, ignore.case = TRUE)) {
      series_label <- "Superannuation surcharge"
    }
    if (table_row$family_id == "detailed_cash_receipts" && tolower(label)=="other taxes" &&
        !nzchar(receipt_group) && any(grepl("^Indirect taxation receipts",normalize_line(block)))) receipt_group <- "indirect taxation"
    if (table_row$family_id == "detailed_cash_receipts" &&
        tolower(label) == "other taxes" && nzchar(receipt_group)) {
      series_label <- paste(label, receipt_group, sep = " — ")
    }
    if (grepl("^\\$b(?:illion|n)?$|^Percentage of GDP$", label,
              ignore.case = TRUE, perl = TRUE) &&
        identical(parent, "Current account balance")) {
      series_label <- "Current account balance"
    }
    if (identical(label, "(Net lending/borrowing)") &&
        identical(parent, "Fiscal balance")) series_label <- "Fiscal balance"
    row_profile <- NA_character_
    if (tolower(label) %in% c(
      "budget", "myefo", "pefo", "actual", "actuals", "outcome", "outcomes"
    ) && nzchar(parent)) {
      row_profile <- if (tolower(label) %in% c(
        "actual", "actuals", "outcome", "outcomes"
      )) "actual" else tolower(label)
      series_label <- parent
    }
    if (grepl("^\\(?(?:Per cent|%)(?: of GDP)?\\)?$", label,
              ignore.case = TRUE) &&
        nzchar(previous_series)) {
      series_label <- previous_series
    } else {
      previous_series <- series_label
    }
    unit <- infer_unit(
      table_row$family_id, label, table_row$source_table,
      paste(block[seq_len(min(16L,length(block)))], collapse = " ")
    )
    column_indices <- integer(nrow(info))
    total_column <- match("Total", columns$labels)
    if (nrow(info) == length(columns$labels)) {
      column_indices <- seq_len(nrow(info))
    } else if (!is.na(total_column) &&
               nrow(info) <= total_column && total_column > 1L) {
      ordinary_count <- total_column - 1L
      if (nrow(info) <= ordinary_count) {
        column_indices <- seq_len(nrow(info))
      } else {
        column_indices[seq_len(ordinary_count)] <- seq_len(ordinary_count)
        optional <- seq.int(total_column, length(columns$labels))
        column_indices[[nrow(info)]] <- optional[[which.min(abs(
          columns$ends[optional] - info$end[[nrow(info)]]
        ))]]
      }
    } else {
      column_indices <- vapply(info$end, function(token_end) {
        which.min(abs(columns$ends - token_end))
      }, integer(1))
    }
    # A legacy PDF may space digits inside one printed integer (e.g. "73 2"
    # for 732). Two values cannot occupy the same observation column. Join
    # integer fragments only within that column and preserve the source text.
    if (nrow(info)>length(columns$ends) && anyDuplicated(column_indices) && unname(unit[["unit"]]) == "AUD million") {
      groups <- split(seq_len(nrow(info)), column_indices)
      merged <- lapply(groups, function(index) {
        row <- info[index[[1L]],,drop=FALSE]
        if (length(index)>1L) {
          if (!all(grepl("^[0-9,]+$",trimws(info$text[index])))) fail("Ambiguous numeric column in ", table_row$table_instance_id, ": ", label, " [", paste(info$text[index],collapse=" | "), "]")
          row$start <- min(info$start[index]); row$end <- max(info$end[index])
          row$text <- gsub("[[:space:]]+", "", substr(token_raw,row$start,row$end))
        }
        row
      })
      info <- do.call(rbind,merged); column_indices <- as.integer(names(groups))
    }
    for (j in seq_len(nrow(info))) {
      column_index <- column_indices[[j]]
      column_label <- columns$labels[[column_index]]
      period <- period_info(column_label, as.Date(release$release_date))
      # These two Treasury international tables explicitly classify the
      # World/MTP historical column as estimates, despite its Outcome heading.
      if (table_row$family_id == "international_gdp_forecasts" &&
          release$release_id %in% c("treasury-budget-2020-21-final", "treasury-budget-2021-22-final") &&
          canonical_series_id(series_label) %in% c("world", "major-trading-partners") &&
          identical(period$actual_forecast, "actual")) period$actual_forecast <- "forecast"
      if (!is.null(columns$is_actual) && isTRUE(columns$is_actual[[column_index]])) {
        period$actual_forecast <- "actual"
      }
      if (canonical_series_id(series_label)=="capital-gains-tax" &&
          identical(period$actual_forecast,"actual") && !is.na(period$fiscal_year) &&
          grepl(paste0(period$fiscal_year," (?:reported |outcome )?figure is an? estimate"),
            paste(normalize_line(block),collapse=" "),ignore.case=TRUE,perl=TRUE)) {
        period$actual_forecast <- "forecast"
      }
      cell_series <- series_label
      if (table_row$family_id == "international_gdp_forecasts" &&
          release$release_id == "treasury-budget-2025-26-final" &&
          canonical_series_id(series_label) == "india" &&
          identical(period$actual_forecast,"actual") &&
          identical(period$calendar_year,2024L) &&
          grepl("fiscal year ending",paste(normalize_line(block),collapse=" "),fixed=TRUE)) {
        # The published note identifies this one outcome as FY to March,
        # despite the calendar-year column heading. Do not move or combine it
        # with the ordinary calendar-year India GDP history.
        cell_series <- "India — fiscal year ending March"
        period$calendar_year <- NA_integer_; period$fiscal_year <- "2024-25"
        period$period_end <- as.Date("2025-03-31"); period$period_type <- "fiscal_year"
      }
      cell_unit <- unit
      if (unname(unit[["unit"]]) %in% c("AUD million", "AUD billion") &&
          length(columns$units) == length(columns$labels) &&
          !is.na(columns$units[[column_index]])) {
        cell_unit <- switch(
          columns$units[[column_index]],
          "AUD billion" = c(unit = "AUD billion", multiplier = "1e9"),
          "% of GDP" = c(unit = "% of GDP", multiplier = "0.01"),
          c(unit = "AUD million", multiplier = "1e6")
        )
      }
      cursor <- cursor + 1L
      cells[[cursor]] <- data.frame(
        release_id = release$release_id,
        publication = release$publication,
        budget_vintage = release$budget_vintage,
        release_date = as.Date(release$release_date),
        family_id = table_row$family_id,
        family_label = table_row$family_label,
        query_mode = table_row$query_mode,
        table_instance_id = table_row$table_instance_id,
        source_table = table_row$source_table,
        source_page = table_row$source_page,
        table_part = table_row$table_part,
        row_index = row_index,
        row_label = label,
        row_path = row_path,
        row_depth = row_depth,
        column_index = column_index,
        column_label = column_label,
        value_text = trimws(info$text[[j]]),
        value = parse_number(info$text[[j]]),
        unit = unname(cell_unit[["unit"]]),
        unit_multiplier = as.numeric(cell_unit[["multiplier"]]),
        accounting_basis = infer_basis(
          table_row$family_id, series_label, table_row$source_table
        ),
        consolidation = infer_consolidation(
          table_row$family_id, table_row$source_table
        ),
        series_id = canonical_series_id(cell_series, parent),
        series = cell_series,
        series_eligible = series_eligible(table_row$query_mode, series_label) &&
          !is.na(period$period_end),
        fiscal_year = period$fiscal_year,
        calendar_year = period$calendar_year,
        period_end = period$period_end,
        period_type = period$period_type,
        actual_forecast = period$actual_forecast,
        profile_type = if (!is.na(row_profile)) {
          row_profile
        } else if (identical(period$actual_forecast, "actual")) {
          "actual"
        } else if (!is.null(columns$profiles) &&
                   !is.na(columns$profiles[[column_index]])) {
          columns$profiles[[column_index]]
        } else if (identical(period$actual_forecast, "forecast")) {
          release$publication
        } else "not_applicable",
        source_url = release$source_url,
        source_sha256 = release$pdf_sha256,
        raw_line = raw,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(cells)) NULL else do.call(rbind, cells)
}

extract_release <- function(release, text_dir) {
  text_path <- file.path(
    text_dir, paste0(release$publication, "-", release$budget_vintage, ".txt")
  )
  if (!file.exists(text_path)) fail("Missing pdftotext output: ", text_path)
  raw <- readLines(text_path, warn = FALSE, encoding = "UTF-8")
  clean <- normalize_line(raw)
  form_feeds <- nchar(raw, type = "bytes") -
    nchar(gsub("\\f", "", raw), type = "bytes")
  page <- cumsum(form_feeds) + 1L
  title_search <- vapply(seq_along(clean), function(i) {
    following <- seq.int(i + 1L, min(i + 4L, length(clean) + 1L))
    following <- following[following <= length(clean)]
    following <- following[nzchar(clean[following])]
    following <- following[!grepl("^\\([a-z]\\)$", clean[following])]
    paste(clean[[i]], if (length(following)) clean[[following[[1L]]]] else "")
  }, character(1))
  title_lines <- grepl("^Table [^:]+:", clean, ignore.case = TRUE, perl = TRUE)
  tables <- list()
  cells <- list()
  table_cursor <- 0L
  cell_cursor <- 0L
  part_count <- list()
  for (s in seq_len(nrow(family_spec))) {
    hits <- grep(
      family_spec$pattern[[s]], title_search,
      ignore.case = TRUE, perl = TRUE
    )
    if (!length(hits)) next
    for (hit in hits) {
      # A contents entry is not a source table. Wrapped headings may have a
      # blank line between title fragments in legacy PDFs.
      if (grepl("\\.{3,}", title_search[[hit]])) next
      title <- if (grepl(family_spec$pattern[[s]], clean[[hit]], ignore.case = TRUE, perl = TRUE)) clean[[hit]] else title_search[[hit]]
      number <- table_number(title)
      instance <- paste(
        release$release_id, family_spec$family_id[[s]], slug(number), sep = "--"
      )
      part_count[[instance]] <- (part_count[[instance]] %||% 0L) + 1L
      next_page <- which(seq_along(raw) > hit & grepl("\\f", raw))
      next_title <- which(seq_along(raw) > hit & title_lines)
      stop_at <- min(c(
        if (length(next_page)) next_page[[1L]] - 1L else length(raw),
        if (length(next_title)) next_title[[1L]] - 1L else length(raw),
        hit + 250L
      ))
      block <- raw[hit:stop_at]
      source_table <- clean_title(title)
      table_cursor <- table_cursor + 1L
      tables[[table_cursor]] <- data.frame(
        release_id = release$release_id,
        publication = release$publication,
        budget_vintage = release$budget_vintage,
        release_date = as.Date(release$release_date),
        family_id = family_spec$family_id[[s]],
        family_label = family_spec$family_label[[s]],
        query_mode = family_spec$query_mode[[s]],
        table_instance_id = instance,
        source_table = source_table,
        source_page = page[[hit]],
        table_part = part_count[[instance]],
        source_url = release$source_url,
        source_sha256 = release$pdf_sha256,
        raw_text = paste(block, collapse = "\n"),
        stringsAsFactors = FALSE
      )
      table_row <- tables[[table_cursor]]
      parsed <- parse_table_part(block, table_row, release)
      if (!is.null(parsed) && nrow(parsed)) {
        cell_cursor <- cell_cursor + 1L
        cells[[cell_cursor]] <- parsed
      }
    }
  }
  list(
    tables = if (length(tables)) do.call(rbind, tables) else NULL,
    cells = if (length(cells)) do.call(rbind, cells) else NULL
  )
}

seed_panel_observations <- function(package_dir) {
  paths <- file.path(
    package_dir, "inst", "extdata",
    c("budget-vintage-observations.csv", "myefo-vintage-observations.csv")
  )
  panels <- lapply(paths, utils::read.csv, stringsAsFactors = FALSE,
                   check.names = FALSE, na.strings = "")
  panel <- do.call(rbind, panels)
  family_map <- c(
    major_economic_parameters = "major_economic_parameters",
    domestic_economy_forecasts = "domestic_economy_detailed_forecasts",
    budget_aggregates = "ggs_budget_aggregates",
    cash_receipts = "detailed_cash_receipts",
    cash_interest = "cash_interest"
  )
  panel <- panel[panel$table_family %in% names(family_map), ]
  label <- sub("\\s+[—-]\\s+(?:AUD million|% of GDP)$", "", panel$series,
               ignore.case = TRUE)
  data.frame(
    release_id = panel$release_id,
    publication = ifelse(grepl("-myefo-", panel$release_id), "myefo", "budget"),
    budget_vintage = sub("^treasury-(?:budget|myefo)-(.+)-final$", "\\1",
                         panel$release_id),
    release_date = as.Date(NA),
    family_id = unname(family_map[panel$table_family]),
    family_label = unname(family_spec$family_label[
      match(unname(family_map[panel$table_family]), family_spec$family_id)
    ]),
    query_mode = "series_and_table",
    table_instance_id = paste(
      panel$release_id, unname(family_map[panel$table_family]),
      slug(panel$source_table), sep = "--"
    ),
    source_table = panel$source_table,
    source_page = NA_integer_,
    table_part = NA_integer_,
    row_index = NA_integer_,
    row_label = label,
    row_path = label,
    row_depth = NA_integer_,
    column_index = NA_integer_,
    column_label = panel$fiscal_year,
    value_text = as.character(panel$value),
    value = panel$value,
    unit = panel$unit,
    unit_multiplier = panel$unit_multiplier,
    accounting_basis = panel$accounting_basis,
    consolidation = panel$consolidation,
    series_id = vapply(label, canonical_series_id, character(1)),
    series = label,
    series_eligible = TRUE,
    fiscal_year = panel$fiscal_year,
    calendar_year = NA_integer_,
    period_end = as.Date(panel$period_end),
    period_type = "fiscal_year",
    actual_forecast = panel$actual_forecast,
    profile_type = panel$profile_type,
    source_url = panel$observation_source_url,
    source_sha256 = panel$observation_source_sha256,
    raw_line = NA_character_,
    stringsAsFactors = FALSE
  )
}

build_catalogue <- function(observations) {
  eligible <- observations[observations$series_eligible, ]
  keys <- unique(eligible[c(
    "publication", "series_id", "family_id", "unit", "unit_multiplier",
    "accounting_basis", "consolidation"
  )])
  keys$series <- ""
  keys$first_period <- as.Date(NA)
  keys$last_period <- as.Date(NA)
  keys$first_release <- as.Date(NA)
  keys$last_release <- as.Date(NA)
  keys$profile_types <- ""
  keys$release_count <- 0L
  keys$observation_count <- 0L
  for (i in seq_len(nrow(keys))) {
    hit <- eligible$publication == keys$publication[[i]] &
      eligible$series_id == keys$series_id[[i]] &
      eligible$family_id == keys$family_id[[i]] &
      eligible$unit == keys$unit[[i]] &
      eligible$unit_multiplier == keys$unit_multiplier[[i]] &
      eligible$accounting_basis == keys$accounting_basis[[i]] &
      eligible$consolidation == keys$consolidation[[i]]
    labels <- sort(table(eligible$series[hit]), decreasing = TRUE)
    keys$series[[i]] <- names(labels)[[1L]]
    periods <- eligible$period_end[hit & !is.na(eligible$period_end)]
    if (length(periods)) {
      keys$first_period[[i]] <- min(periods)
      keys$last_period[[i]] <- max(periods)
    }
    releases <- eligible$release_date[hit & !is.na(eligible$release_date)]
    if (length(releases)) {
      keys$first_release[[i]] <- min(releases)
      keys$last_release[[i]] <- max(releases)
    }
    keys$profile_types[[i]] <- paste(
      sort(unique(eligible$profile_type[hit])), collapse = ","
    )
    keys$release_count[[i]] <- length(unique(eligible$release_id[hit]))
    keys$observation_count[[i]] <- sum(hit)
  }
  keys[order(keys$publication, keys$series_id, keys$family_id, keys$unit), ]
}

options <- parse_args(commandArgs(trailingOnly = TRUE))
package_dir <- normalizePath(".", mustWork = TRUE)
inventory <- utils::read.csv(
  options$inventory, stringsAsFactors = FALSE, check.names = FALSE
)
required <- c(
  "release_id", "publication", "budget_vintage", "release_date",
  "source_url", "container_sha256", "archive_member",
  "assembled_from_members", "pdf_sha256"
)
if (!all(required %in% names(inventory))) fail("Inventory schema is incomplete.")
if (nrow(inventory) != 53L || anyDuplicated(inventory$release_id)) {
  fail("Expected the frozen 53-release official PDF inventory.")
}
inventory$release_date <- as.Date(inventory$release_date)
parent <- dirname(options$output)
if (!dir.exists(parent)) dir.create(parent, recursive = TRUE)
cache_dir <- paste0(options$output, ".extraction-cache-v32")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

extract_one <- function(i) {
  cache_path <- file.path(cache_dir, paste0(inventory$release_id[[i]], ".rds"))
  if (file.exists(cache_path)) {
    message("Loading ", inventory$release_id[[i]], " [", i, "/", nrow(inventory), "]")
    return(readRDS(cache_path))
  }
  message("Extracting ", inventory$release_id[[i]], " [", i, "/", nrow(inventory), "]")
  value <- extract_release(inventory[i, , drop = FALSE], options$text_dir)
  cache_temp <- tempfile("snapshot-", tmpdir = cache_dir, fileext = ".rds")
  on.exit(unlink(cache_temp), add = TRUE)
  saveRDS(value, cache_temp, version = 3L, compress = "gzip")
  if (!file.rename(cache_temp, cache_path)) {
    fail("Could not publish extraction cache entry: ", cache_path)
  }
  value
}
# Deterministic single-worker extraction avoids fork-specific PDF-text/parser
# failures and keeps this maintainer build within CRAN's two-core ceiling.
workers <- 1L
extracted <- if (workers > 1L) {
  parallel::mclapply(seq_len(nrow(inventory)), extract_one, mc.cores = workers)
} else {
  lapply(seq_len(nrow(inventory)), extract_one)
}
tables <- do.call(rbind, lapply(extracted, `[[`, "tables"))
generic <- do.call(rbind, lapply(extracted, `[[`, "cells"))

# Some documents include a short receipts summary immediately before the full
# detailed cash-receipts table. Keep the table instance(s) with the detailed
# row set for each release; the narrower summary is represented elsewhere and
# must not be mislabeled as the detailed family.
cash <- generic[generic$family_id == "detailed_cash_receipts", ]
if (nrow(cash)) {
  cash_counts <- aggregate(
    row_label ~ release_id + table_instance_id,
    cash,
    function(x) length(unique(x))
  )
  names(cash_counts)[[3L]] <- "row_count"
  release_max <- ave(cash_counts$row_count, cash_counts$release_id, FUN = max)
  keep_cash <- cash_counts$table_instance_id[
    cash_counts$row_count >= pmax(10L, 0.6 * release_max)
  ]
  remove_cash <- setdiff(
    unique(generic$table_instance_id[generic$family_id == "detailed_cash_receipts"]),
    keep_cash
  )
  if (length(remove_cash)) {
    generic <- generic[!generic$table_instance_id %in% remove_cash, ]
    tables <- tables[!tables$table_instance_id %in% remove_cash, ]
  }
}
table_headers <- tapply(
  tables$raw_text,
  tables$table_instance_id,
  function(x) {
    lines <- unlist(strsplit(paste(x, collapse = "\n"), "\n", fixed = TRUE))
    paste(utils::head(lines, 16L), collapse = "\n")
  }
)
billion_instances <- names(table_headers)[grepl(
  "\\$(?:b|bn)\\b|billion",
  table_headers,
  ignore.case = TRUE,
  perl = TRUE
)]
billion_rows <- generic$table_instance_id %in% billion_instances &
  generic$unit == "AUD million"
generic$unit[billion_rows] <- "AUD billion"
generic$unit_multiplier[billion_rows] <- 1e9
seeded <- seed_panel_observations(package_dir)
release_dates <- setNames(inventory$release_date, inventory$release_id)
seeded$release_date <- as.Date(release_dates[seeded$release_id])

# The new store is a record of published PDF tables. Existing panels are used
# below as an independent validation reference, not mixed into the new output.
observations <- generic
series_parent <- ifelse(
  grepl(" > ", observations$row_path, fixed = TRUE),
  sub(" > [^>]+$", "", observations$row_path),
  ""
)
observations$series_id <- mapply(
  canonical_series_id,
  observations$series,
  series_parent,
  USE.NAMES = FALSE
)
observations$source_kind <- c(
  rep("pdf_table_cell", nrow(generic))
)

# A canonical concept may occur in several source tables. Retain every source
# occurrence, but identify one preferred observation per release/period/unit.
observations$is_preferred <- FALSE
eligible <- which(observations$series_eligible & !is.na(observations$period_end))
priority <- c(pdf_table_cell = 1L)
family_priority <- setNames(
  seq_along(family_spec$family_id),
  family_spec$family_id
)
family_priority[c(
  "ggs_budget_aggregates",
  "underlying_to_headline_cash_bridge",
  "formal_financial_statements"
)] <- c(1L, 2L, 3L)
key <- paste(
  observations$release_id[eligible], observations$series_id[eligible],
  observations$period_end[eligible], observations$unit[eligible],
  observations$accounting_basis[eligible], observations$consolidation[eligible],
  observations$profile_type[eligible],
  sep = "\r"
)
groups <- split(eligible, key)
for (index in groups) {
  order_index <- order(
    unname(priority[observations$source_kind[index]]),
    unname(family_priority[observations$family_id[index]]),
    observations$table_instance_id[index]
  )
  observations$is_preferred[index[order_index[[1L]]]] <- TRUE
}

catalogue <- build_catalogue(observations)

seed_parent <- ifelse(
  grepl(" > ", seeded$row_path, fixed = TRUE),
  sub(" > [^>]+$", "", seeded$row_path),
  ""
)
seeded$series_id <- mapply(
  canonical_series_id, seeded$series, seed_parent, USE.NAMES = FALSE
)
comparison_fields <- c(
  "release_id", "family_id", "series_id", "period_end", "accounting_basis"
)
generic_key <- do.call(paste, c(observations[comparison_fields], sep = "\r"))
seed_key <- do.call(paste, c(seeded[comparison_fields], sep = "\r"))
common_keys <- intersect(unique(generic_key), unique(seed_key))
comparison <- data.frame(
  key = common_keys,
  minimum_base_unit_difference = numeric(length(common_keys)),
  tolerance = numeric(length(common_keys)),
  within_published_rounding = logical(length(common_keys)),
  stringsAsFactors = FALSE
)
for (i in seq_along(common_keys)) {
  key_i <- common_keys[[i]]
  gi <- which(generic_key == key_i)
  si <- which(seed_key == key_i)
  gv <- observations$value[gi] * observations$unit_multiplier[gi]
  sv <- seeded$value[si] * seeded$unit_multiplier[si]
  difference <- min(abs(outer(gv, sv, "-")))
  generic_tolerance <- ifelse(
    observations$unit[gi] == "AUD billion", 5e7,
    ifelse(observations$unit[gi] == "AUD million", 5e5, 5e-4)
  )
  tolerance <- max(generic_tolerance)
  comparison$minimum_base_unit_difference[[i]] <- difference
  comparison$tolerance[[i]] <- tolerance
  comparison$within_published_rounding[[i]] <- difference <= tolerance
}
comparison$classification <- ifelse(
  comparison$within_published_rounding,
  "matches_within_published_rounding",
  ifelse(
    grepl("\\rdomestic_economy_detailed_forecasts\\r", comparison$key),
    "legacy_panel_column_alignment_differs_from_published_pdf",
    ifelse(
      startsWith(comparison$key, "treasury-budget-2020-21-final\rggs_budget_aggregates\r"),
      "later_pbo_actual_differs_from_budget_paper_vintage",
      "unclassified"
    )
  )
)
unclassified <- comparison$classification == "unclassified"
if (any(unclassified)) {
  saveRDS(
    list(
      observations = observations,
      seeded = seeded,
      comparison = comparison[unclassified, , drop = FALSE]
    ),
    paste0(options$output, ".validation-fail.rds"),
    version = 3L,
    compress = "xz"
  )
  fail(
    "Audited panel comparison has ", sum(unclassified),
    " unclassified value difference(s); see ",
    paste0(options$output, ".validation-fail.rds"), "."
  )
}
validation <- list(
  audited_panel_common_keys = nrow(comparison),
  audited_panel_keys_within_published_rounding =
    sum(comparison$within_published_rounding),
  audited_panel_key_mismatches = sum(!comparison$within_published_rounding),
  audited_panel_key_mismatches_classified = sum(
    comparison$classification != "unclassified" &
      !comparison$within_published_rounding
  ),
  audited_panel_classification_counts = table(comparison$classification),
  audited_panel_comparison = comparison
)
coverage <- unique(tables[c(
  "release_id", "publication", "budget_vintage", "release_date",
  "family_id", "family_label", "query_mode", "table_instance_id",
  "source_table", "source_page", "table_part", "source_url", "source_sha256"
)])
coverage <- coverage[order(
  coverage$family_id, coverage$publication, coverage$release_date,
  coverage$table_instance_id
), ]

if (!all(c(
  "major_economic_parameters", "domestic_economy_detailed_forecasts",
  "detailed_cash_receipts", "cash_interest"
) %in% unique(observations$family_id[observations$series_eligible]))) {
  fail("Required audited series families are missing.")
}
missing_families <- setdiff(family_spec$family_id, unique(tables$family_id))
if (length(missing_families)) {
  saveRDS(
    list(tables = tables, generic = generic, seeded = seeded),
    paste0(options$output, ".partial.rds"),
    version = 3L,
    compress = "xz"
  )
  fail(
    "Approved table families with no extracted snapshot: ",
    paste(missing_families, collapse = ", ")
  )
}
if (anyNA(observations$value[observations$series_eligible])) {
  fail("Series-eligible observations contain missing values.")
}
if (any(observations$series_eligible & is.na(observations$period_end))) {
  fail("Series-eligible observations must have a dated period.")
}
if (any(observations$series_eligible & observations$column_label == "Total")) {
  fail("Non-time Total columns must not be series-queryable.")
}
if (any(observations$series_eligible & observations$query_mode == "table_only")) {
  fail("Table-only families must not expose component series.")
}
if (!all(c("underlying-cash-balance", "headline-cash-balance") %in%
         observations$series_id[observations$series_eligible])) {
  fail("Underlying and headline cash balances must be canonical series.")
}
preferred_key <- paste(
  observations$release_id[eligible], observations$series_id[eligible],
  observations$period_end[eligible], observations$unit[eligible],
  observations$accounting_basis[eligible], observations$consolidation[eligible],
  observations$profile_type[eligible],
  sep = "\r"
)
if (any(table(preferred_key[observations$is_preferred[eligible]]) != 1L)) {
  fail("Preferred observations are not unique on the public series key.")
}
if (any(grepl("^Chart ", tables$source_table, ignore.case = TRUE))) {
  fail("Chart content is outside this table-only dataset.")
}
if (!all(grepl("^https://", inventory$source_url)) ||
    !all(grepl("^[0-9a-f]{64}$", inventory$pdf_sha256)) ||
    !all(grepl("^[0-9a-f]{64}$", inventory$container_sha256)) ||
    any(inventory$assembled_from_members & !nzchar(inventory$archive_member))) {
  fail("Every source must have a valid HTTPS URL and SHA-256.")
}

store <- list(
  schema_version = "budget_myefo_history_v2",
  built_from = list(
    release_count = nrow(inventory),
    budget_release_count = sum(inventory$publication == "budget"),
    myefo_release_count = sum(inventory$publication == "myefo"),
    first_release = min(inventory$release_date),
    last_release = max(inventory$release_date),
    source = "Official Australian Treasury Budget Paper No. 1 and MYEFO PDFs",
    extraction = "Maintainer-time pdftotext -layout; no runtime PDF parsing"
  ),
  families = family_spec,
  sources = inventory[required],
  coverage = coverage,
  tables = tables,
  observations = observations,
  series = catalogue,
  validation = validation
)
saveRDS(store, options$output, version = 3L, compress = "xz")
message(
  "Wrote ", nrow(tables), " table parts, ", nrow(observations),
  " observation cells, and ", nrow(catalogue), " series catalogue rows to ",
  options$output
)
