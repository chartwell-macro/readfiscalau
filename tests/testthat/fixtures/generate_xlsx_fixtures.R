# Rebuild the tiny, offline Finance workbook fixtures.
#
# This development-only script uses writexl. The generated workbooks are
# committed, so writexl is not a package dependency and the test suite never
# invokes this script.
script_arg <- commandArgs(trailingOnly = FALSE)
script_arg <- script_arg[grep("^--file=", script_arg)][1L]
script_path <- sub("^--file=", "", script_arg)
fixture_dir <- dirname(normalizePath(script_path, mustWork = TRUE))

make_table <- function(title, headers, rows) {
  width <- length(headers) + 1L
  out <- matrix(NA_character_, nrow = 3L + length(rows), ncol = width)
  out[1L, 1L] <- title
  out[2L, ] <- c("AGGREGATES", headers)
  for (i in seq_along(rows)) {
    out[i + 2L, ] <- c(rows[[i]][[1L]], as.character(rows[[i]][[2L]]))
  }
  out[nrow(out), 1L] <- "* Fixture footnote."
  as.data.frame(out, stringsAsFactors = FALSE)
}

write_fixture <- function(filename, sheet, table) {
  sheets <- setNames(list(table), sheet)
  path <- file.path(fixture_dir, filename)
  writexl::write_xlsx(sheets, path, col_names = FALSE)

  unpacked <- tempfile("readfiscalau-xlsx-")
  dir.create(unpacked)
  on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = unpacked)
  core_path <- file.path(unpacked, "docProps", "core.xml")
  core <- readLines(core_path, warn = FALSE, encoding = "UTF-8")
  core <- gsub(
    "<dcterms:created[^>]*>[^<]+</dcterms:created>",
    paste0(
      "<dcterms:created xsi:type=\"dcterms:W3CDTF\">",
      "2026-07-27T00:00:00Z</dcterms:created>"
    ),
    core
  )
  core <- gsub(
    "<dcterms:modified[^>]*>[^<]+</dcterms:modified>",
    paste0(
      "<dcterms:modified xsi:type=\"dcterms:W3CDTF\">",
      "2026-07-27T00:00:00Z</dcterms:modified>"
    ),
    core
  )
  writeLines(core, core_path, useBytes = TRUE)
  members <- sort(list.files(unpacked, recursive = TRUE, all.files = TRUE))
  members <- members[file.info(file.path(unpacked, members))$isdir %in% FALSE]
  Sys.setFileTime(
    file.path(unpacked, members),
    as.POSIXct("2026-07-27 00:00:00", tz = "UTC")
  )
  file.remove(path)
  old <- setwd(unpacked)
  on.exit(setwd(old), add = TRUE)
  status <- utils::zip(path, members, flags = "-X -q")
  setwd(old)
  if (!identical(status, 0L) || !file.exists(path)) {
    stop("Could not create deterministic XLSX fixture: ", filename)
  }
}

write_fixture(
  "finance-current.xlsx", "2025-26",
  make_table(
    paste(
      "2025-26 Australian Government general government sector",
      "- Aggregates"
    ),
    c(
      "ACTUAL\n2025-2026\nYTD April\n$b",
      "ACTUAL\n2025-2026\nYTD May\n$b"
    ),
    list(
      list("Receipts(b)", c(615.432515, 693.756160)),
      list("Payments(c)", c(638.374578, 704.703472)),
      list("Underlying cash balance", c(-22.942063, -10.947312))
    )
  )
)

write_fixture(
  "finance-historical.xlsx", "2007-08",
  make_table(
    paste(
      "2007-2008 Australian Government general government sector",
      "- Aggregates"
    ),
    c(
      "ACTUAL\n2007-2008\nYTD April\n$m",
      "ACTUAL\n2007-2008\nYTD May\n$m"
    ),
    list(
      list("Revenue", c(256543, 283403)),
      list("Expenses", c(229200, 253155)),
      list("Fiscal balance", c(26236, 28771))
    )
  )
)

write_fixture(
  "finance-june.xlsx", "2025-26",
  make_table(
    paste(
      "2025-26 Australian Government general government sector",
      "- Aggregates"
    ),
    "ACTUAL\n2025-2026\nYTD June\n$b",
    list(list("Receipts(b)", 750))
  )
)

write_fixture(
  "finance-unit-drift.xlsx", "2025-26",
  make_table(
    paste(
      "2025-26 Australian Government general government sector",
      "- Aggregates"
    ),
    "ACTUAL\n2025-2026\nYTD May\n$m",
    list(list("Receipts(b)", 693756.160))
  )
)

write_fixture(
  "finance-non-ytd.xlsx", "2025-26",
  make_table(
    paste(
      "2025-26 Australian Government general government sector",
      "- Aggregates"
    ),
    "ACTUAL\n2025-2026\nMay\n$b",
    list(list("Receipts(b)", 693.756160))
  )
)

history_headers <- c(
  "ACTUAL\n2024-2025\nYTD July\n$b",
  "ACTUAL\n2024-2025\nAugust\n$b",
  paste0(
    "ACTUAL\n2024-2025\nYTD ",
    c("September", "October", "November", "December",
      "January", "February", "March", "April", "May"),
    "\n$b"
  )
)

write_fixture(
  "finance-history.xlsx", "2024-25",
  make_table(
    paste(
      "2024-25 Australian Government general government sector",
      "- Aggregates"
    ),
    history_headers,
    list(
      list("Receipts(b)", c(10, 10, seq(30, 110, by = 10))),
      list("Payments(c)", c(8, 8, seq(24, 88, by = 8))),
      list("Underlying cash balance",
           c(2, 2, seq(6, 22, by = 2))),
      list("Revenue", c(12, 12, seq(36, 132, by = 12))),
      list("Expenses", c(9, 9, seq(27, 99, by = 9))),
      list("Net operating balance", c(3, 3, seq(9, 33, by = 3))),
      list("Net capital investment", c(1, 1, seq(3, 11, by = 1))),
      list("Fiscal balance", c(2, 2, seq(6, 22, by = 2))),
      list("Total assets", seq(1000, 1010)),
      list("Net worth(d)", -seq(100, 110))
    )
  )
)

write_fixture(
  "finance-post-2008-millions.xlsx", "2008-09",
  make_table(
    paste(
      "2008-2009 Australian Government general government sector",
      "- Aggregates"
    ),
    "ACTUAL\n2008-2009\nYTD May\n$m",
    list(list("Revenue", 280000))
  )
)
