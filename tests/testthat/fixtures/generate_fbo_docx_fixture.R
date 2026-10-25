# Rebuild the tiny, offline FBO DOCX fixture.
#
# Only word/document.xml is needed by the package parser. The fixture is a
# deterministic ZIP container with the five audited historical aggregate table
# shapes used by the official Appendix B document.
script_arg <- commandArgs(trailingOnly = FALSE)
script_arg <- script_arg[grep("^--file=", script_arg)][1L]
script_path <- sub("^--file=", "", script_arg)
fixture_dir <- dirname(normalizePath(script_path, mustWork = TRUE))

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

run <- function(x) {
  if (startsWith(x, "-")) {
    paste0(
      "<w:r><w:noBreakHyphen/></w:r><w:r><w:t>",
      xml_escape(sub("^-", "", x)),
      "</w:t></w:r>"
    )
  } else {
    paste0("<w:r><w:t>", xml_escape(x), "</w:t></w:r>")
  }
}

cell <- function(x = "") {
  paste0("<w:tc><w:p>", run(as.character(x)), "</w:p></w:tc>")
}

row <- function(values) {
  paste0("<w:tr>", paste0(vapply(values, cell, character(1)),
                           collapse = ""), "</w:tr>")
}

table <- function(header, values) {
  paste0("<w:tbl>", row(header), row(values), "</w:tbl>")
}

blank <- function(n) rep("", n)

b1_header <- blank(12)
b1_header[c(2, 5, 9, 11)] <- c(
  "Receipts", "Payments", "Net Future Fund earnings",
  "Underlying cash balance"
)
b1_values <- blank(12)
b1_values[c(1, 2, 5, 9, 11)] <- c(
  "2024-25", "125,000", "100,000", "2,000", "23,000"
)

b2_header <- blank(10)
b2_header[c(2, 4, 6, 9)] <- c(
  "Receipts", "Payments", "financial assets for policy purposes",
  "Headline cash balance"
)
b2_values <- blank(10)
b2_values[c(1, 2, 4, 6, 9)] <- c(
  "2024-25", "125,000", "100,000", "1,000", "24,000"
)

b4_header <- blank(6)
b4_header[c(2, 5)] <- c("Net debt", "Net interest payments")
b4_values <- blank(6)
b4_values[c(1, 2, 5)] <- c("2024-25", "150,000", "5,000")

b6_header <- blank(15)
b6_header[c(2, 5, 8, 11, 14)] <- c(
  "Revenue", "Expenses", "Net operating balance",
  "Net capital investment", "Fiscal balance"
)
b6_values <- blank(15)
b6_values[c(1, 2, 5, 8, 11, 14)] <- c(
  "2024-25", "150,000", "120,000", "30,000", "12,000", "18,000"
)

b7_header <- blank(6)
b7_header[c(2, 5)] <- c("Net worth", "Net financial worth")
b7_values <- blank(6)
b7_values[c(1, 2, 5)] <- c("2024-25", "-120,000", "-140,000")

document <- paste0(
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
  "<w:document xmlns:w=\"http://schemas.openxmlformats.org/",
  "wordprocessingml/2006/main\"><w:body>",
  table(b1_header, b1_values),
  table(b2_header, b2_values),
  table(b4_header, b4_values),
  table(b6_header, b6_values),
  table(b7_header, b7_values),
  "</w:body></w:document>"
)

unpacked <- tempfile("readfiscalau-docx-")
dir.create(file.path(unpacked, "word"), recursive = TRUE)
on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
writeLines(
  document,
  file.path(unpacked, "word", "document.xml"),
  useBytes = TRUE
)
Sys.setFileTime(
  file.path(unpacked, "word", "document.xml"),
  as.POSIXct("2026-07-28 00:00:00", tz = "UTC")
)

path <- file.path(fixture_dir, "fbo-history.docx")
if (file.exists(path)) file.remove(path)
old <- setwd(unpacked)
on.exit(setwd(old), add = TRUE)
status <- utils::zip(
  path,
  "word/document.xml",
  flags = "-X -q"
)
setwd(old)
if (!identical(status, 0L) || !file.exists(path)) {
  stop("Could not create deterministic FBO DOCX fixture.")
}
