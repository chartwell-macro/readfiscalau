#!/usr/bin/env Rscript

# Operator CLI for the Budget-document archive.  The archive APIs are kept in
# the package so that downloading, hashing and validation remain testable; this
# script only wires them into a repeatable command-line run.

usage <- function(status = 0L) {
  message(
    "Usage: Rscript tools/archive-budget-documents.R [options]\n\n",
    "Options:\n",
    "  --archive-dir=PATH       Directory for immutable source documents (required)\n",
    "  --inventory-output=PATH  Inventory CSV path (default: ARCHIVE_DIR/inventory.csv)\n",
    "  --discover-only          Discover and export; do not download documents\n",
    "  --refresh-existing       Re-fetch admitted URLs to detect replacements\n",
    "  --include-current=false  Exclude the currently published Budget\n",
    "  --help                   Show this help"
  )
  quit(status = status)
}

fail <- function(...) {
  message(...)
  quit(status = 1L)
}

parse_args <- function(args) {
  options <- list(
    archive_dir = NULL,
    inventory_output = NULL,
    discover_only = FALSE,
    refresh_existing = FALSE,
    include_current = TRUE
  )
  for (arg in args) {
    if (identical(arg, "--help")) usage()
    if (identical(arg, "--discover-only")) {
      options$discover_only <- TRUE
    } else if (identical(arg, "--refresh-existing")) {
      options$refresh_existing <- TRUE
    } else if (startsWith(arg, "--archive-dir=")) {
      options$archive_dir <- sub("^--archive-dir=", "", arg)
    } else if (startsWith(arg, "--inventory-output=")) {
      options$inventory_output <- sub("^--inventory-output=", "", arg)
    } else if (startsWith(arg, "--include-current=")) {
      value <- tolower(sub("^--include-current=", "", arg))
      if (!value %in% c("true", "false")) {
        fail("--include-current must be true or false")
      }
      options$include_current <- identical(value, "true")
    } else {
      fail("Unknown argument: ", arg, "\nUse --help for usage.")
    }
  }
  if (is.null(options$archive_dir) || !nzchar(options$archive_dir)) {
    fail("--archive-dir=PATH is required")
  }
  options$archive_dir <- normalizePath(options$archive_dir, mustWork = FALSE)
  if (is.null(options$inventory_output)) {
    options$inventory_output <- file.path(options$archive_dir, "inventory.csv")
  }
  options$inventory_output <- normalizePath(options$inventory_output, mustWork = FALSE)
  options
}

as_table <- function(value) {
  if (is.null(value)) return(data.frame())
  if (is.data.frame(value)) return(as.data.frame(value, stringsAsFactors = FALSE))
  if (is.list(value) && !is.null(value$inventory) && is.data.frame(value$inventory)) {
    return(as.data.frame(value$inventory, stringsAsFactors = FALSE))
  }
  if (is.list(value)) return(as.data.frame(value, stringsAsFactors = FALSE))
  data.frame(value = value, stringsAsFactors = FALSE)
}

component_table <- function(value, names) {
  if (!is.list(value)) return(data.frame())
  for (name in names) {
    if (!is.null(value[[name]])) return(as_table(value[[name]]))
  }
  data.frame()
}

write_csv <- function(data, path) {
  parent <- dirname(path)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    fail("Could not create output directory: ", parent)
  }
  utils::write.csv(data, path, row.names = FALSE, na = "")
  if (!file.exists(path)) fail("CSV export was not created: ", path)
  message("Wrote ", nrow(data), " rows: ", path)
}

require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    fail(label, " is missing required columns: ",
         paste(missing, collapse = ", "))
  }
  invisible(data)
}

options <- parse_args(commandArgs(trailingOnly = TRUE))

if (!requireNamespace("readfiscalau", quietly = TRUE)) {
  fail(
    "The readfiscalau package is not installed. Install this checkout first, ",
    "for example: R CMD INSTALL packages/readfiscalau"
  )
}

discovery <- readfiscalau::crawl_au_budget_archive(
  archive_dir = options$archive_dir,
  include_current = options$include_current
)
require_columns(
  discovery$documents,
  c("source_url", "budget_vintage", "index_snapshot_id"),
  "Discovery candidates"
)
require_columns(
  discovery$indexes,
  c("index_snapshot_id", "index_url", "sha256", "archive_path"),
  "Captured index ledger"
)

output_base <- sub("\\.csv$", "", options$inventory_output, ignore.case = TRUE)
if (options$discover_only) {
  write_csv(discovery$documents, options$inventory_output)
  write_csv(discovery$indexes, paste0(output_base, "-index.csv"))
  write_csv(discovery$gaps, paste0(output_base, "-gaps.csv"))
  message(
    "Budget archive discovery completed (candidates=",
    nrow(discovery$documents), ", indexes=", nrow(discovery$indexes),
    ", explicit_gaps=", nrow(discovery$gaps),
    "). No document bytes were downloaded."
  )
  quit(status = 0L)
}

# `archive_au_budget_documents()` returns rows admitted in this run, whereas
# the operator export and validator must use the complete durable ledger.
catalogue <- readfiscalau::read_au_budget_archive_catalogue()$candidates
catalogue_row <- match(discovery$documents$source_url, catalogue$source_url)
existing_inventory <- readfiscalau::list_au_budget_archive(
  archive_dir = options$archive_dir
)
pending <- if (options$refresh_existing) {
  rep(TRUE, nrow(discovery$documents))
} else {
  !discovery$documents$source_url %in% existing_inventory$source_url
}
pending_candidates <- discovery$documents[pending, , drop = FALSE]
pending_candidates$advertised_bytes <-
  catalogue$advertised_bytes[catalogue_row[pending]]
matched_available <- !is.na(catalogue_row) &
  catalogue$availability[catalogue_row] == "available"
known_available <- pending & matched_available &
  is.finite(catalogue$advertised_bytes[catalogue_row])
required_bytes <- sum(
  catalogue$advertised_bytes[catalogue_row[known_available]],
  na.rm = TRUE
)
unknown_size <- pending & (
  is.na(catalogue_row) |
    (matched_available &
       !is.finite(catalogue$advertised_bytes[catalogue_row]))
)
unknown_count <- sum(unknown_size)
if (unknown_count) {
  required_bytes <- required_bytes + unknown_count * 250 * 1024^2
  message(
    "Capacity estimate reserves the per-document maximum for ",
    unknown_count, " candidate(s) absent from the bundled catalogue."
  )
}
invisible(
  readfiscalau::archive_au_budget_documents(
    candidates = pending_candidates,
    archive_dir = options$archive_dir,
    discovery = discovery,
    required_bytes = required_bytes,
    progress = TRUE
  )
)

inventory <- readfiscalau::list_au_budget_archive(
  archive_dir = options$archive_dir
)
validation <- readfiscalau::validate_au_budget_archive(
  archive_dir = options$archive_dir,
  inventory = inventory,
  indexes = discovery$indexes,
  candidates = discovery$documents,
  discovery = discovery
)
inventory <- as_table(validation$inventory)
index <- as_table(validation$indexes)
gaps <- as_table(validation$gaps)
require_columns(inventory, c("source_url", "sha256", "archive_path"),
                "Archive inventory")
require_columns(index, c("index_snapshot_id", "index_url", "sha256"),
                "Archive index export")
require_columns(gaps, c("gap_type", "gap_reason"), "Archive gap export")

write_csv(inventory, options$inventory_output)
write_csv(index, paste0(output_base, "-index.csv"))
write_csv(gaps, paste0(output_base, "-gaps.csv"))

unresolved <- nrow(as_table(validation$missing)) +
  nrow(as_table(validation$unexpected))
if (!isTRUE(validation$complete) || unresolved > 0L) {
  message(
    "Budget archive validation is incomplete (missing=",
    nrow(as_table(validation$missing)), ", unexpected=",
    nrow(as_table(validation$unexpected)), ", documented_gaps=", nrow(gaps),
    "); see the gap export above."
  )
  quit(status = 1L)
}
message(sprintf(
  "Budget archive validation passed with %d explicit documented gap(s).",
  nrow(gaps)
))
