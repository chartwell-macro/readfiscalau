.rf_budget_archive_hosts <- c(
  "archive.budget.gov.au",
  "budget.gov.au",
  "www.budget.gov.au"
)

.rf_budget_archive_preservation_hosts <- c(
  "web.archive.org.au",
  "webarchive.nla.gov.au"
)

.rf_budget_archive_document_extensions <- c(
  "csv", "doc", "docx", "json", "pdf", "rtf", "xls", "xlsx", "xml", "zip"
)

.rf_budget_archive_inventory_fields <- c(
  "archive_file_id", "logical_file_id", "budget_vintage",
  "publication_type", "document_role", "title", "revision_status",
  "replaces_archive_file_id", "index_snapshot_id", "index_url",
  "source_url", "final_url", "media_type", "byte_size", "sha256",
  "retrieved_at", "archive_path", "license", "terms_url", "attribution"
)

.rf_budget_archive_index_fields <- c(
  "index_snapshot_id", "budget_vintage", "index_url", "retrieved_at",
  "media_type", "byte_size", "sha256", "archive_path"
)

.rf_budget_archive_gap_fields <- c(
  "budget_vintage", "index_snapshot_id", "index_url", "source_url",
  "title", "gap_type", "checked_at", "gap_reason"
)

#' Default durable Budget archive directory
#'
#' Resolve the directory used by the durable Budget-document archive. Unlike
#' [readfiscalau_default_cache()], this is an append-only evidence store and
#' is never pruned by the package. The directory is not created by this helper;
#' it is created by archive writers when they need it.
#'
#' The `READFISCALAU_BUDGET_ARCHIVE_DIR` environment variable takes precedence
#' when it is non-empty. Otherwise the path is
#' `tools::R_user_dir("readfiscalau", which = "data")/budget-archive`, which
#' is outside the package source tree. Set the variable only to a directory
#' whose ownership and retention policy are understood by the caller.
#'
#' @returns A length-one character vector containing the configured archive
#'   root. No files, directories, network requests, or locks are created.
#'
#' @examples
#' old_archive_dir <- Sys.getenv(
#'   "READFISCALAU_BUDGET_ARCHIVE_DIR", unset = NA_character_
#' )
#' Sys.setenv(
#'   READFISCALAU_BUDGET_ARCHIVE_DIR =
#'     file.path(tempdir(), "readfiscalau-example-archive")
#' )
#' example_archive_dir <- readfiscalau_default_archive_dir()
#' if (is.na(old_archive_dir)) {
#'   Sys.unsetenv("READFISCALAU_BUDGET_ARCHIVE_DIR")
#' } else {
#'   Sys.setenv(READFISCALAU_BUDGET_ARCHIVE_DIR = old_archive_dir)
#' }
#' stopifnot(
#'   identical(
#'     example_archive_dir,
#'     file.path(tempdir(), "readfiscalau-example-archive")
#'   ),
#'   !dir.exists(example_archive_dir)
#' )
#'
#' @seealso [archive_au_budget_documents()], [list_au_budget_archive()],
#'   [readfiscalau_default_cache()]
#' @family budget-archive
#' @export
readfiscalau_default_archive_dir <- function() {
  configured <- Sys.getenv("READFISCALAU_BUDGET_ARCHIVE_DIR", unset = "")
  if (nzchar(configured)) return(configured)
  file.path(
    tools::R_user_dir("readfiscalau", which = "data"),
    "budget-archive"
  )
}

.rf_archive_hash_text <- function(x) {
  digest::digest(charToRaw(enc2utf8(as.character(x))),
                 algo = "sha256", serialize = FALSE)
}

.rf_archive_canonical_url <- function(url, official_only = TRUE) {
  .rf_assert_scalar_string(url, "url")
  if (!.rf_is_safe_https_url(url)) {
    .rf_abort(
      "Budget archive URLs must be credential-free HTTPS URLs on an official .gov.au host.",
      "schema"
    )
  }
  parsed <- tryCatch(
    httr2::url_parse(url),
    error = function(error) .rf_abort(
      "Budget archive URL is malformed.",
      "schema"
    )
  )
  hostname <- tolower(parsed$hostname %||% "")
  credentials <- nzchar(parsed$username %||% "") ||
    nzchar(parsed$password %||% "")
  bad_port <- !is.null(parsed$port) &&
    !identical(as.integer(parsed$port), 443L)
  ip_literal <- grepl(
    "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$|^\\[.*\\]$",
    hostname
  )
  official <- hostname == "gov.au" ||
    grepl("\\.gov\\.au$", hostname) ||
    hostname %in% .rf_budget_archive_preservation_hosts
  if (!identical(tolower(parsed$scheme %||% ""), "https") ||
      !nzchar(hostname) ||
      credentials ||
      bad_port ||
      ip_literal ||
      (official_only && !official)) {
    .rf_abort(
      "Budget archive URLs must be credential-free HTTPS URLs on an official .gov.au host.",
      "schema"
    )
  }
  parsed$fragment <- NULL
  unname(httr2::url_build(parsed))
}

.rf_archive_first_party_url <- function(url) {
  parsed <- tryCatch(httr2::url_parse(url), error = function(error) NULL)
  !is.null(parsed) &&
    tolower(parsed$hostname %||% "") %in% .rf_budget_archive_hosts
}

.rf_archive_url_extension <- function(url) {
  parsed <- httr2::url_parse(url)
  tolower(tools::file_ext(parsed$path %||% ""))
}

.rf_archive_media_type <- function(url, content_type = "") {
  extension <- .rf_archive_url_extension(url)
  types <- c(
    csv = "text/csv",
    doc = "application/msword",
    docx = paste0(
      "application/vnd.openxmlformats-officedocument.",
      "wordprocessingml.document"
    ),
    htm = "text/html",
    html = "text/html",
    json = "application/json",
    pdf = "application/pdf",
    rtf = "application/rtf",
    xls = "application/vnd.ms-excel",
    xlsx = paste0(
      "application/vnd.openxmlformats-officedocument.",
      "spreadsheetml.sheet"
    ),
    xml = "application/xml",
    zip = "application/zip"
  )
  if (extension %in% names(types)) return(unname(types[[extension]]))
  cleaned <- trimws(strsplit(content_type %||% "", ";", fixed = TRUE)[[1]][[1]])
  if (nzchar(cleaned)) cleaned else "application/octet-stream"
}

.rf_archive_terms <- function(source_url) {
  parsed <- httr2::url_parse(.rf_archive_canonical_url(source_url))
  hostname <- tolower(parsed$hostname %||% "")
  if (hostname %in% .rf_budget_archive_hosts ||
      identical(hostname, "treasury.gov.au") ||
      identical(hostname, "www.treasury.gov.au")) {
    return(.rf_terms_contracts$australian_treasury)
  }
  list(
    terms_url = source_url,
    license = "Not stated in captured official Budget index",
    attribution = paste("Official source host:", hostname)
  )
}

.rf_archive_validate_payload <- function(raw, media_type) {
  if (!is.raw(raw) || !length(raw)) {
    .rf_abort("Budget archive source returned no bytes.", "transport")
  }
  media <- tolower(media_type)
  if (grepl("pdf", media)) {
    magic <- rawToChar(raw[seq_len(min(5L, length(raw)))], multiple = FALSE)
    if (!identical(magic, "%PDF-")) {
      .rf_abort("PDF resource does not have a PDF file signature.",
                "unsupported_media")
    }
  } else if (grepl("wordprocessingml|spreadsheetml|zip", media)) {
    .rf_validate_payload(raw, media_type)
  } else if (grepl("msword|ms-excel", media)) {
    expected <- as.raw(c(0xd0, 0xcf, 0x11, 0xe0))
    magic <- raw[seq_len(min(4L, length(raw)))]
    if (!identical(magic, expected)) {
      .rf_abort("Legacy Office resource does not have an OLE signature.",
                "unsupported_media")
    }
  } else if (grepl("csv", media)) {
    .rf_validate_payload(raw, media_type)
  } else if (grepl("rtf", media)) {
    magic <- rawToChar(raw[seq_len(min(5L, length(raw)))], multiple = FALSE)
    if (!startsWith(magic, "{\\rtf")) {
      .rf_abort("RTF resource does not have an RTF file signature.",
                "unsupported_media")
    }
  } else if (grepl("html", media)) {
    text <- tolower(rawToChar(
      raw[seq_len(min(1024L, length(raw)))],
      multiple = FALSE
    ))
    if (!grepl("<(?:!doctype\\s+html|html|head|body|main)\\b",
               text, perl = TRUE)) {
      .rf_abort("HTML index does not contain an HTML document.",
                "unsupported_media")
    }
  }
  invisible(raw)
}

.rf_archive_validate_file <- function(path, media_type) {
  .rf_assert_scalar_string(path, "path")
  if (!file.exists(path) || dir.exists(path)) {
    .rf_abort("Budget archive download file does not exist.",
              "transport", path = path)
  }
  size <- as.numeric(file.info(path)$size)
  if (!is.finite(size) || size <= 0) {
    .rf_abort("Budget archive source returned no bytes.", "transport")
  }
  connection <- file(path, open = "rb")
  prefix <- tryCatch(
    readBin(connection, what = "raw", n = min(size, 1024)),
    finally = close(connection)
  )
  .rf_archive_validate_payload(prefix, media_type)
  invisible(path)
}

.rf_archive_revision_status <- function(text) {
  text <- tolower(text)
  if (grepl("correct|replacement|revised|updated|amend|errata", text)) {
    "replacement"
  } else {
    "original"
  }
}

.rf_archive_document_role <- function(text, url) {
  value <- tolower(paste(text, url))
  if (grepl("chart.?data|data\\.zip|\\.csv|\\.xlsx", value)) {
    return("chart_data")
  }
  if (grepl("budget paper|\\bbp[1-9]\\b|bp[1-9]_", value)) {
    number <- regmatches(value, regexpr(
      "(?:budget paper(?: no\\.)?\\s*|\\bbp)([1-9])",
      value, perl = TRUE
    ))
    number <- gsub("[^1-9]", "", number)
    return(if (length(number) && nzchar(number)) {
      paste0("budget_paper_", number)
    } else {
      "budget_paper"
    })
  }
  if (grepl("appendix|annex", value)) return("appendix")
  if (grepl("mid.?year|myefo", value)) return("myefo")
  if (grepl("final budget|\\bfbo\\b", value)) return("final_budget_outcome")
  if (grepl("pre-election|pefo", value)) return("pefo")
  if (grepl("appropriation|supply bill", value)) return("appropriation")
  if (grepl("portfolio.*statement|\\bpbs\\b|\\bpaes\\b", value)) {
    return("portfolio_statement")
  }
  if (grepl("women", value)) return("womens_budget_statement")
  if (grepl("overview", value)) return("overview")
  if (grepl("fact.?sheet", value)) return("fact_sheet")
  if (grepl("supplement|additional|online only", value)) {
    return("supplementary")
  }
  "budget_document"
}

.rf_archive_publication_type <- function(text, url) {
  value <- tolower(paste(text, url))
  if (grepl("mid.?year|myefo", value)) return("myefo")
  if (grepl("final budget|\\bfbo\\b", value)) return("fbo")
  if (grepl("pre-election|pefo", value)) return("pefo")
  if (grepl("updated economic|uefo", value)) return("uefo")
  "budget"
}

.rf_archive_vintage <- function(text, url = "") {
  value <- paste(url, text)
  october <- grepl(
    "2022[-\u2013]23.*october|2022-23-october",
    value,
    ignore.case = TRUE
  )
  pattern <- "(?:19|20)[0-9]{2}[-\u2013](?:[0-9]{2}|(?:19|20)[0-9]{2})"
  url_hit <- regmatches(url, regexpr(pattern, url, perl = TRUE))
  hit <- if (length(url_hit) && nzchar(url_hit)) {
    url_hit
  } else {
    regmatches(text, regexpr(pattern, text, perl = TRUE))
  }
  if (!length(hit) || !nzchar(hit)) return(NA_character_)
  fiscal_year <- .rf_normalize_fiscal_year(hit)
  if (october) paste0(fiscal_year, "-october") else fiscal_year
}

.rf_archive_anchor_title <- function(anchor) {
  link_text <- trimws(xml2::xml_text(anchor))
  row <- xml2::xml_find_first(anchor, "ancestor::tr[1]")
  row_text <- if (inherits(row, "xml_missing")) "" else
    trimws(xml2::xml_text(row))
  item <- xml2::xml_find_first(anchor, "ancestor::li[1]")
  item_text <- if (inherits(item, "xml_missing")) "" else
    trimws(xml2::xml_text(item))
  title <- if (nzchar(row_text)) row_text else
    if (nzchar(item_text)) item_text else link_text
  trimws(gsub("\\s+", " ", title))
}

.rf_archive_anchor_heading <- function(anchor) {
  heading <- xml2::xml_find_first(
    anchor,
    "preceding::*[self::h1 or self::h2 or self::h3 or self::h4][1]"
  )
  if (inherits(heading, "xml_missing")) "" else
    trimws(gsub("\\s+", " ", xml2::xml_text(heading)))
}

#' Discover documents from one official Budget index snapshot
#'
#' Parse one captured official Australian Government Budget index into direct
#' document candidates. This function is deliberately separate from both
#' [crawl_au_budget_archive()] and [archive_au_budget_documents()]: it parses
#' the supplied bytes only, does not download any linked document, and does
#' not admit anything into the durable archive.
#'
#' Relative links are resolved against `index_url`, canonicalised, and limited
#' to credential-free HTTPS URLs. Only the supported document extensions are
#' returned. HTTP links are excluded and recorded in the
#' `budget_archive_link_gaps` attribute; no warning is emitted. The candidate
#' rows retain the source URL, title/context-derived document role and
#' publication type, Budget vintage, referring index identity, ordinal,
#' revision status, and inferred media type. Duplicate source URLs are kept
#' once, in first-seen order.
#'
#' @param raw A raw vector containing the exact captured HTML bytes. It must
#'   contain an HTML document; character strings are not accepted.
#' @param index_url A scalar official HTTPS index URL used to resolve relative
#'   links. It must be on an allowed `.gov.au` host and cannot contain
#'   credentials or a non-443 port.
#' @param budget_vintage A non-empty scalar fiscal-year/vintage label attached
#'   to every returned candidate and gap row.
#' @param index_snapshot_id A non-empty scalar stable identifier for the
#'   captured index bytes.
#' @returns A `tbl_df`/`data.frame` with columns `source_url`, `title`,
#'   `document_role`, `publication_type`, `budget_vintage`, `index_url`,
#'   `index_snapshot_id`, `index_ordinal`, `revision_status`, and `media_type`.
#'   The `budget_archive_link_gaps` attribute is a tibble with columns
#'   `budget_vintage`, `index_snapshot_id`, `index_url`, `source_url`, `title`,
#'   `gap_type`, `checked_at`, and `gap_reason`; it is empty when no insecure
#'   document links were present. An index with no supported anchors returns a
#'   zero-row, zero-column tibble with the usual `tbl_df`/`data.frame` classes;
#'   it does not fabricate the candidate columns.
#'
#' @details Invalid raw bytes, malformed URLs, non-official index URLs, and
#'   malformed document links raise typed `readfiscalau_invalid_argument` or
#'   `readfiscalau_schema` errors. HTML/media validation failures raise typed
#'   `readfiscalau_unsupported_media`/`readfiscalau_schema` errors. The
#'   function performs no network, cache, lock, or durable filesystem writes.
#'
#' @examples
#' catalogue <- read_au_budget_archive_catalogue()
#' snapshot <- catalogue$indexes[1, , drop = FALSE]
#' connection <- file(snapshot$snapshot_path, open = "rb")
#' snapshot_raw <- readBin(
#'   connection,
#'   what = "raw",
#'   n = as.numeric(file.info(snapshot$snapshot_path)$size)
#' )
#' close(connection)
#' candidates <- discover_au_budget_archive_index(
#'   snapshot_raw,
#'   index_url = snapshot$index_url,
#'   budget_vintage = snapshot$budget_vintage,
#'   index_snapshot_id = snapshot$index_snapshot_id
#' )
#' head(candidates[c("source_url", "document_role", "budget_vintage")])
#' attr(candidates, "budget_archive_link_gaps")
#'
#' @seealso [read_au_budget_archive_catalogue()],
#'   [list_au_budget_archive_candidates()], [crawl_au_budget_archive()]
#' @family budget-archive-discovery
#' @export
discover_au_budget_archive_index <- function(
    raw,
    index_url,
    budget_vintage,
    index_snapshot_id) {
  if (!is.raw(raw)) {
    .rf_abort("`raw` must contain exact Budget index HTML bytes.",
              "invalid_argument")
  }
  .rf_assert_scalar_string(index_url, "index_url")
  .rf_assert_scalar_string(budget_vintage, "budget_vintage")
  .rf_assert_scalar_string(index_snapshot_id, "index_snapshot_id")
  index_url <- .rf_archive_canonical_url(index_url)
  if (!.rf_archive_first_party_url(index_url)) {
    .rf_abort("Budget index URLs must use budget.gov.au.",
              "schema", index_url = index_url)
  }
  .rf_archive_validate_payload(raw, "text/html")
  document <- xml2::read_html(rawToChar(raw))
  content <- xml2::xml_find_first(document, ".//main")
  if (inherits(content, "xml_missing")) {
    content <- xml2::xml_find_first(document, ".//body")
  }
  anchors <- xml2::xml_find_all(content, ".//a[@href]")
  if (!length(anchors)) return(tibble::tibble())
  link_gaps <- list()
  rows <- lapply(seq_along(anchors), function(index) {
    anchor <- anchors[[index]]
    href <- trimws(xml2::xml_attr(anchor, "href") %||% "")
    if (!nzchar(href) || startsWith(href, "#")) return(NULL)
    encoded_href <- utils::URLencode(href, reserved = FALSE)
    resolved <- tryCatch(
      xml2::url_absolute(encoded_href, index_url),
      error = function(error) NA_character_
    )
    if (is.na(resolved) || !nzchar(resolved)) {
      .rf_abort("Budget index contains a malformed document URL.",
                "schema", href = href)
    }
    parsed <- tryCatch(
      httr2::url_parse(resolved),
      error = function(error) NULL
    )
    extension <- if (is.null(parsed)) "" else
      tolower(tools::file_ext(parsed$path %||% ""))
    if (!extension %in% .rf_budget_archive_document_extensions) {
      return(NULL)
    }
    scheme <- tolower(parsed$scheme %||% "")
    hostname <- tolower(parsed$hostname %||% "")
    if (!identical(scheme, "https")) {
      link_gaps[[length(link_gaps) + 1L]] <<- data.frame(
        budget_vintage = budget_vintage,
        index_snapshot_id = index_snapshot_id,
        index_url = index_url,
        source_url = resolved,
        title = .rf_archive_anchor_title(anchor),
        gap_type = if (hostname %in% .rf_budget_archive_hosts) {
          "insecure_first_party_link"
        } else {
          "insecure_external_official_link"
        },
        checked_at = "",
        gap_reason = paste(
          "The official index exposes this document over non-HTTPS;",
          "the archive did not downgrade transport security."
        ),
        stringsAsFactors = FALSE
      )
      return(NULL)
    }
    canonical <- .rf_archive_canonical_url(resolved)
    title <- .rf_archive_anchor_title(anchor)
    heading <- .rf_archive_anchor_heading(anchor)
    context <- trimws(paste(heading, title))
    data.frame(
      source_url = canonical,
      title = title,
      document_role = .rf_archive_document_role(context, canonical),
      publication_type = .rf_archive_publication_type(context, canonical),
      budget_vintage = budget_vintage,
      index_url = index_url,
      index_snapshot_id = index_snapshot_id,
      index_ordinal = as.integer(index),
      revision_status = .rf_archive_revision_status(context),
      media_type = .rf_archive_media_type(canonical),
      stringsAsFactors = FALSE
    )
  })
  out <- .rf_bind_rows(Filter(Negate(is.null), rows))
  if (nrow(out)) {
    out <- out[!duplicated(out$source_url), , drop = FALSE]
  }
  attr(out, "budget_archive_link_gaps") <-
    .rf_bind_rows(link_gaps)
  row.names(out) <- NULL
  out
}

.rf_archive_store_file <- function(path, archive_dir) {
  .rf_assert_scalar_string(path, "path")
  .rf_assert_scalar_string(archive_dir, "archive_dir")
  if (!file.exists(path) || dir.exists(path)) {
    .rf_abort("Archive object source file does not exist.",
              "invalid_argument", path = path)
  }
  if (nzchar(Sys.readlink(path))) {
    .rf_abort("Archive object source files must not be symbolic links.",
              "invalid_argument", path = path)
  }
  staging_dir <- file.path(archive_dir, ".staging")
  if (!dir.exists(staging_dir) &&
      !dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)) {
    .rf_abort("Could not create the Budget archive staging directory.",
              "archive_write", path = staging_dir)
  }
  if (nzchar(Sys.readlink(staging_dir))) {
    .rf_abort("Budget archive staging must not be a symbolic link.",
              "archive_corrupt", path = staging_dir)
  }
  source_path <- normalizePath(path, mustWork = TRUE)
  staging_path <- normalizePath(staging_dir, mustWork = TRUE)
  archive_owned <- startsWith(
    source_path,
    paste0(staging_path, .Platform$file.sep)
  )
  if (!archive_owned) {
    owned_path <- tempfile("readfiscalau-archive-owned-", tmpdir = staging_dir)
    if (!file.copy(path, owned_path, overwrite = FALSE, copy.mode = FALSE)) {
      .rf_abort("Could not copy source bytes into archive-owned staging.",
                "archive_write", path = path)
    }
    path <- owned_path
    on.exit(if (file.exists(owned_path)) unlink(owned_path), add = TRUE)
  }
  byte_size <- as.numeric(file.info(path)$size)
  if (!is.finite(byte_size) || byte_size <= 0) {
    .rf_abort("Archive objects must contain non-empty bytes.",
              "invalid_argument", path = path)
  }
  sha256 <- digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
  object_dir <- file.path(
    archive_dir,
    "objects",
    "sha256",
    substr(sha256, 1L, 2L)
  )
  if (!dir.exists(object_dir) &&
      !dir.create(object_dir, recursive = TRUE, showWarnings = FALSE)) {
    .rf_abort("Could not create the Budget archive object directory.",
              "archive_write", path = object_dir)
  }
  if (nzchar(Sys.readlink(object_dir))) {
    .rf_abort("Budget archive object directories must not be symbolic links.",
              "archive_corrupt", path = object_dir)
  }
  target <- file.path(object_dir, sha256)
  if (file.exists(target)) {
    if (nzchar(Sys.readlink(target))) {
      .rf_abort("Budget archive objects must not be symbolic links.",
                "archive_corrupt", path = target)
    }
    existing <- tryCatch(
      digest::digest(file = target, algo = "sha256", serialize = FALSE),
      error = function(error) NA_character_
    )
    if (!identical(existing, sha256)) {
      .rf_abort(
        "A durable Budget archive object is corrupt; it will not be overwritten.",
        "archive_corrupt", path = target, expected_sha256 = sha256,
        actual_sha256 = existing
      )
    }
    if (!identical(normalizePath(path, mustWork = TRUE),
                   normalizePath(target, mustWork = TRUE))) {
      unlink(path)
    }
    return(list(archive_path = normalizePath(target, mustWork = TRUE),
                sha256 = sha256, byte_size = byte_size))
  }
  published <- file.rename(path, target)
  if (!published) {
    temporary <- tempfile("readfiscalau-archive-", tmpdir = object_dir)
    on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
    if (!file.copy(path, temporary, overwrite = FALSE, copy.mode = FALSE)) {
      .rf_abort("Could not stage the Budget archive object.",
                "archive_write", path = path)
    }
    written <- digest::digest(
      file = temporary, algo = "sha256", serialize = FALSE
    )
    if (!identical(written, sha256)) {
      .rf_abort("Temporary Budget archive object failed SHA-256 validation.",
                "archive_corrupt", path = temporary)
    }
    published <- file.rename(temporary, target)
  }
  if (!published) {
    competing <- if (file.exists(target)) tryCatch(
      digest::digest(file = target, algo = "sha256", serialize = FALSE),
      error = function(error) NA_character_
    ) else NA_character_
    if (!identical(competing, sha256)) {
      .rf_abort("Could not atomically publish the Budget archive object.",
                "archive_write", path = target)
    }
  }
  published_hash <- digest::digest(
    file = target, algo = "sha256", serialize = FALSE
  )
  if (!identical(published_hash, sha256) ||
      !identical(as.numeric(file.info(target)$size), byte_size)) {
    .rf_abort("Published Budget archive object failed integrity validation.",
              "archive_corrupt", path = target)
  }
  if (file.exists(path) &&
      !identical(normalizePath(path, mustWork = TRUE),
                 normalizePath(target, mustWork = TRUE))) {
    unlink(path)
  }
  list(
    archive_path = normalizePath(target, mustWork = TRUE),
    sha256 = sha256,
    byte_size = byte_size
  )
}

.rf_archive_store_object <- function(raw, archive_dir) {
  if (!is.raw(raw) || !length(raw)) {
    .rf_abort("Archive objects must contain non-empty raw bytes.",
              "invalid_argument")
  }
  .rf_assert_scalar_string(archive_dir, "archive_dir")
  staging_dir <- file.path(archive_dir, ".staging")
  if (!dir.exists(staging_dir) &&
      !dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)) {
    .rf_abort("Could not create the Budget archive staging directory.",
              "archive_write", path = staging_dir)
  }
  if (nzchar(Sys.readlink(staging_dir))) {
    .rf_abort("Budget archive staging must not be a symbolic link.",
              "archive_corrupt", path = staging_dir)
  }
  temporary <- tempfile("readfiscalau-archive-raw-", tmpdir = staging_dir)
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  connection <- file(temporary, open = "wb")
  tryCatch(writeBin(raw, connection), finally = close(connection))
  .rf_archive_store_file(temporary, archive_dir)
}

.rf_archive_empty_inventory <- function() {
  out <- stats::setNames(
    replicate(
      length(.rf_budget_archive_inventory_fields),
      character(),
      simplify = FALSE
    ),
    .rf_budget_archive_inventory_fields
  )
  out$byte_size <- numeric()
  tibble::as_tibble(out)
}

.rf_archive_empty_indexes <- function() {
  out <- stats::setNames(
    replicate(
      length(.rf_budget_archive_index_fields),
      character(),
      simplify = FALSE
    ),
    .rf_budget_archive_index_fields
  )
  out$byte_size <- numeric()
  tibble::as_tibble(out)
}

.rf_archive_empty_gaps <- function() {
  tibble::as_tibble(stats::setNames(
    replicate(
      length(.rf_budget_archive_gap_fields),
      character(),
      simplify = FALSE
    ),
    .rf_budget_archive_gap_fields
  ))
}

.rf_archive_read_csv <- function(path, empty) {
  if (!file.exists(path)) return(empty())
  out <- utils::read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = character()
  )
  tibble::as_tibble(out)
}

.rf_archive_write_csv_atomic <- function(data, path) {
  parent <- dirname(path)
  if (!dir.exists(parent) &&
      !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    .rf_abort("Could not create a Budget archive inventory directory.",
              "archive_write", path = parent)
  }
  temporary <- tempfile("readfiscalau-ledger-", tmpdir = parent)
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  if (file.rename(temporary, path)) return(invisible(path))
  if (!file.exists(path)) {
    .rf_abort("Could not atomically publish the Budget archive ledger.",
              "archive_write", path = path)
  }
  backup <- tempfile("readfiscalau-ledger-previous-", tmpdir = parent)
  if (!file.rename(path, backup)) {
    .rf_abort("Could not preserve the previous Budget archive ledger.",
              "archive_write", path = path)
  }
  if (!file.rename(temporary, path)) {
    restored <- file.rename(backup, path)
    .rf_abort(
      paste(
        "Could not publish the Budget archive ledger;",
        if (restored) "the previous ledger was restored." else
          paste("the previous ledger remains at", backup)
      ),
      "archive_write",
      path = path,
      recovery_path = if (restored) path else backup
    )
  }
  if (file.exists(backup)) unlink(backup)
  invisible(path)
}

.rf_archive_inventory_path <- function(archive_dir) {
  file.path(archive_dir, "inventory", "budget-archive-inventory.csv")
}

.rf_archive_indexes_path <- function(archive_dir) {
  file.path(archive_dir, "inventory", "budget-archive-indexes.csv")
}

.rf_archive_gaps_path <- function(archive_dir) {
  file.path(archive_dir, "inventory", "budget-archive-gaps.csv")
}

.rf_archive_acquire_lock <- function(archive_dir, operation) {
  .rf_assert_scalar_string(archive_dir, "archive_dir")
  .rf_assert_scalar_string(operation, "operation")
  if (!dir.exists(archive_dir) &&
      !dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)) {
    .rf_abort("Could not create the Budget archive directory.",
              "archive_write", path = archive_dir)
  }
  lock_dir <- file.path(archive_dir, ".write-lock")
  if (!dir.create(lock_dir, showWarnings = FALSE)) {
    owner_path <- file.path(lock_dir, "owner.txt")
    owner <- if (file.exists(owner_path)) {
      paste(readLines(owner_path, warn = FALSE), collapse = "; ")
    } else {
      "owner metadata unavailable"
    }
    .rf_abort(
      paste("The Budget archive is locked by another writer:", owner),
      "archive_locked",
      archive_dir = archive_dir,
      lock_dir = lock_dir
    )
  }
  owner <- c(
    paste0("pid=", Sys.getpid()),
    paste0("operation=", operation),
    paste0("started_at=", format(Sys.time(), tz = "UTC", usetz = TRUE))
  )
  writeLines(owner, file.path(lock_dir, "owner.txt"), useBytes = TRUE)
  lock_dir
}

.rf_archive_release_lock <- function(lock_dir) {
  if (dir.exists(lock_dir)) unlink(lock_dir, recursive = TRUE)
  invisible(lock_dir)
}

.rf_archive_validate_time <- function(x, field) {
  text <- sub(" UTC$", "", as.character(x))
  parsed <- suppressWarnings(as.POSIXct(
    text,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ))
  if (anyNA(parsed)) {
    .rf_abort(
      sprintf("Budget archive `%s` values must be UTC timestamps.", field),
      "schema", field = field
    )
  }
  invisible(x)
}

#' Validate the durable Budget archive inventory contract
#'
#' Validate the schema and provenance metadata of an archive inventory before
#' it is used by [list_au_budget_archive()] or
#' [validate_au_budget_archive()]. The validator accepts the complete
#' 21-column inventory emitted by [archive_au_budget_documents()] and also
#' accepts a minimal inventory containing the required fields. It does not
#' inspect the bytes at `archive_path`; use
#' [validate_au_budget_archive()] for on-disk SHA-256 and size verification.
#'
#' Replacement rows for one source URL must form one explicit chain: one root
#' row without `replaces_archive_file_id`, followed by rows that point to an
#' earlier archive file ID. This preserves corrections and changed official
#' bytes instead of collapsing them. Empty inventories are returned as empty
#' tibbles after required-column validation.
#'
#' @param inventory A data frame or tibble. Required columns are `source_url`,
#'   `archive_path`, `sha256`, `byte_size`, `retrieved_at`, `media_type`,
#'   `budget_vintage`, `revision_status`, `license`, and `attribution`.
#'   The full archive schema additionally includes `archive_file_id`,
#'   `logical_file_id`, `publication_type`, `document_role`, `title`,
#'   `replaces_archive_file_id`, `index_snapshot_id`, `index_url`, `final_url`,
#'   and `terms_url`.
#' @returns The input converted to a `tbl_df`/`data.frame`, with all original
#'   columns and row order retained. No files are created or removed.
#'
#' @details Required hashes are 64 lowercase hexadecimal SHA-256 strings,
#' byte sizes are finite positive numbers, URLs are credential-free official
#' HTTPS URLs, retrieval times are UTC timestamps, and revision statuses are
#' one of `original`, `replacement`, `revised`, `superseded`, or `unknown`.
#' Empty required metadata, duplicate admissions/IDs, invalid replacement
#' chains, malformed URLs/times/hashes/sizes, and unsupported revisions raise
#' typed `readfiscalau_schema` or `readfiscalau_invalid_argument` errors. The
#' function emits no warnings and performs no network, locking, or durable
#' filesystem side effects.
#'
#' @examples
#' inventory_example <- tibble::tibble(
#'   source_url = "https://budget.gov.au/2025-26/example.csv",
#'   archive_path = file.path(tempdir(), "not-read-by-inventory-validator.csv"),
#'   sha256 = digest::digest(
#'     charToRaw("example"), algo = "sha256", serialize = FALSE
#'   ),
#'   byte_size = 7,
#'   retrieved_at = "2026-08-01 00:00:00 UTC",
#'   media_type = "text/csv",
#'   budget_vintage = "2025-26",
#'   revision_status = "original",
#'   license = "Creative Commons Attribution 4.0 International",
#'   attribution = "\u00a9 Commonwealth of Australia"
#' )
#' validated_inventory <- validate_au_budget_archive_inventory(
#'   inventory_example
#' )
#' class(validated_inventory)
#'
#' @seealso [archive_au_budget_documents()],
#'   [validate_au_budget_archive()]
#' @family budget-archive-validation
#' @export
validate_au_budget_archive_inventory <- function(inventory) {
  inventory <- tibble::as_tibble(inventory)
  required <- c(
    "source_url", "archive_path", "sha256", "byte_size", "retrieved_at",
    "media_type", "budget_vintage", "revision_status", "license",
    "attribution"
  )
  missing <- setdiff(required, names(inventory))
  if (length(missing)) {
    .rf_abort(
      paste("Budget archive inventory is missing:",
            paste(missing, collapse = ", ")),
      "schema", fields = missing
    )
  }
  if (!nrow(inventory)) return(inventory)
  character_required <- setdiff(required, "byte_size")
  incomplete <- vapply(inventory[character_required], function(value) {
    anyNA(value) || any(!nzchar(trimws(as.character(value))))
  }, logical(1))
  if (any(incomplete)) {
    .rf_abort("Budget archive inventory has empty required metadata.",
              "schema", fields = names(incomplete)[incomplete])
  }
  has_ids <- all(c(
    "archive_file_id", "replaces_archive_file_id"
  ) %in% names(inventory))
  if (("archive_file_id" %in% names(inventory) &&
       anyDuplicated(inventory$archive_file_id)) ||
      anyDuplicated(paste(
        inventory$source_url,
        inventory$sha256,
        sep = "\r"
      ))) {
    .rf_abort(
      "Budget archive inventory repeats an admission or archive ID.",
      "schema"
    )
  }
  duplicate_sources <- duplicated(inventory$source_url) |
    duplicated(inventory$source_url, fromLast = TRUE)
  if (any(duplicate_sources)) {
    if (!has_ids) {
      .rf_abort(
        "Budget archive inventory repeats a source URL without replacement metadata.",
        "schema"
      )
    }
    groups <- split(
      which(duplicate_sources),
      inventory$source_url[duplicate_sources]
    )
    invalid_chain <- vapply(groups, function(rows) {
      group_ids <- inventory$archive_file_id[rows]
      replacements <- inventory$replaces_archive_file_id[rows]
      replacements[is.na(replacements)] <- ""
      roots <- !nzchar(replacements)
      if (anyDuplicated(group_ids) ||
        sum(roots) != 1L ||
        any(!replacements[!roots] %in% group_ids) ||
        any(replacements == group_ids)) {
        return(TRUE)
      }
      root_id <- group_ids[[which(roots)]]
      for (id in group_ids[!roots]) {
        cursor <- id
        seen <- character()
        while (!identical(cursor, root_id)) {
          if (cursor %in% seen) return(TRUE)
          seen <- c(seen, cursor)
          cursor <- replacements[[match(cursor, group_ids)]]
        }
      }
      FALSE
    }, logical(1))
    if (any(invalid_chain)) {
      .rf_abort(
        "Repeated Budget source URLs must form one explicit replacement chain.",
        "schema",
        source_url = names(invalid_chain)[invalid_chain]
      )
    }
  }
  if (any(!grepl("^[a-f0-9]{64}$", inventory$sha256)) ||
      any(!is.finite(as.numeric(inventory$byte_size))) ||
      any(as.numeric(inventory$byte_size) <= 0)) {
    .rf_abort("Budget archive hashes and byte sizes are malformed.",
              "schema")
  }
  invisible(lapply(
    inventory$source_url,
    .rf_archive_canonical_url
  ))
  .rf_archive_validate_time(inventory$retrieved_at, "retrieved_at")
  valid_revisions <- c(
    "original", "replacement", "revised", "superseded", "unknown"
  )
  if (any(!inventory$revision_status %in% valid_revisions)) {
    .rf_abort("Budget archive revision status is unsupported.", "schema")
  }
  inventory
}

.rf_archive_fetch <- function(
    url,
    media_type,
    fetcher = NULL,
    max_bytes = 250 * 1024^2) {
  url <- .rf_archive_canonical_url(url)
  scratch <- tempfile("readfiscalau-archive-fetch-")
  on.exit(if (dir.exists(scratch)) unlink(scratch, recursive = TRUE), add = TRUE)
  fetched <- .rf_fetch(
    url,
    cache_dir = scratch,
    max_bytes = max_bytes,
    max_attempts = 3L,
    cache_max_bytes = max_bytes,
    cache_max_files = 2L,
    cache_max_age_seconds = Inf,
    fetcher = fetcher
  )
  final_url <- .rf_archive_canonical_url(fetched$final_url)
  detected <- .rf_archive_media_type(final_url, fetched$content_type)
  expected_extension <- .rf_archive_url_extension(url)
  expected <- if (expected_extension %in%
                  .rf_budget_archive_document_extensions) {
    .rf_archive_media_type(url)
  } else {
    media_type
  }
  if (!identical(expected, "application/octet-stream") &&
      !identical(detected, "application/octet-stream") &&
      !identical(expected, detected)) {
    compatible_zip <- grepl(
      "wordprocessingml|spreadsheetml|zip",
      expected
    ) && grepl(
      "wordprocessingml|spreadsheetml|zip|octet-stream",
      tolower(fetched$content_type)
    )
    if (!compatible_zip) {
      .rf_abort(
        "Budget archive response media type conflicts with its official link.",
        "unsupported_media", expected = expected, actual = detected,
        url = url
      )
    }
  }
  .rf_archive_validate_payload(fetched$raw, expected)
  fetched$final_url <- final_url
  fetched$archive_media_type <- expected
  fetched
}

.rf_archive_fetch_file <- function(
    url,
    media_type,
    archive_dir,
    fetcher = NULL,
    max_bytes = 250 * 1024^2,
    timeout_seconds = 300,
    max_attempts = 3L) {
  url <- .rf_archive_canonical_url(url)
  .rf_assert_scalar_string(archive_dir, "archive_dir")
  if (!is.numeric(max_bytes) || length(max_bytes) != 1L ||
      is.na(max_bytes) || !is.finite(max_bytes) || max_bytes <= 0) {
    .rf_abort("`max_bytes` must be positive and finite.",
              "invalid_argument")
  }
  staging_dir <- file.path(archive_dir, ".staging")
  if (!dir.exists(staging_dir) &&
      !dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)) {
    .rf_abort("Could not create the Budget archive staging directory.",
              "archive_write", path = staging_dir)
  }
  if (nzchar(Sys.readlink(staging_dir))) {
    .rf_abort("Budget archive staging must not be a symbolic link.",
              "archive_corrupt", path = staging_dir)
  }
  download_path <- tempfile("readfiscalau-budget-", tmpdir = staging_dir)
  keep_download <- FALSE
  on.exit({
    if (!keep_download && file.exists(download_path)) unlink(download_path)
  }, add = TRUE)

  if (is.null(fetcher)) {
    request <- httr2::request(url)
    request <- httr2::req_user_agent(
      request, "curl/8.7.1 readfiscalau/0.1 (+https://budget.gov.au/)"
    )
    request <- httr2::req_timeout(request, timeout_seconds)
    request <- httr2::req_options(
      request,
      connecttimeout = min(timeout_seconds, 10),
      maxfilesize = max_bytes,
      accept_encoding = "identity",
      fresh_connect = TRUE,
      forbid_reuse = TRUE
    )
    request <- httr2::req_retry(
      request,
      max_tries = as.integer(max_attempts),
      retry_on_failure = TRUE
    )
    request <- httr2::req_error(request, is_error = function(response) FALSE)
    response <- tryCatch(
      httr2::req_perform(request, path = download_path),
      error = function(error) .rf_abort(
        paste("Budget archive download failed:", conditionMessage(error)),
        "transport", url = url
      )
    )
    status <- httr2::resp_status(response)
    content_type <- httr2::resp_header(response, "content-type") %||% ""
    content_length <- httr2::resp_header(response, "content-length") %||% ""
    content_range <- httr2::resp_header(response, "content-range") %||% ""
    link <- httr2::resp_header(response, "link") %||% ""
    final_url <- httr2::resp_url(response)
    retrieved_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  } else {
    response <- fetcher(url)
    metadata <- c("status", "content_type", "final_url", "retrieved_at")
    has_path <- is.list(response) &&
      "path" %in% names(response) &&
      is.character(response$path) &&
      length(response$path) == 1L &&
      !is.na(response$path) &&
      nzchar(response$path)
    has_raw <- is.list(response) &&
      "raw" %in% names(response) &&
      is.raw(response$raw)
    if (!is.list(response) || any(!metadata %in% names(response)) ||
        (!has_path && !has_raw)) {
      .rf_abort(
        paste(
          "Injected Budget archive fetchers must return path or raw, plus",
          "status, content_type, final_url, and retrieved_at."
        ),
        "transport"
      )
    }
    if (has_path) {
      if (!file.exists(response$path) || dir.exists(response$path) ||
          nzchar(Sys.readlink(response$path))) {
        .rf_abort(
          "Injected Budget archive paths must be regular non-symlink files.",
          "transport",
          path = response$path
        )
      }
      if (!file.copy(
        response$path,
        download_path,
        overwrite = FALSE,
        copy.mode = FALSE
      )) {
        .rf_abort(
          "Could not copy injected bytes into archive-owned staging.",
          "archive_write",
          path = response$path
        )
      }
    } else {
      connection <- file(download_path, open = "wb")
      tryCatch(writeBin(response$raw, connection), finally = close(connection))
    }
    status <- response$status
    content_type <- response$content_type
    content_length <- response$content_length %||% ""
    content_range <- response$content_range %||% ""
    link <- response$link %||% ""
    final_url <- response$final_url
    retrieved_at <- response$retrieved_at
  }

  if (!is.numeric(status) || length(status) != 1L || is.na(status) ||
      status != as.integer(status) || status < 100L || status > 599L) {
    .rf_abort("Budget archive response status was malformed.", "transport")
  }
  if (!is.character(content_type) || length(content_type) != 1L ||
      is.na(content_type) ||
      !is.character(final_url) || length(final_url) != 1L ||
      is.na(final_url) || !nzchar(final_url) ||
      !is.character(retrieved_at) || length(retrieved_at) != 1L ||
      is.na(retrieved_at) || !nzchar(retrieved_at)) {
    .rf_abort("Budget archive response metadata was malformed.", "transport")
  }
  if (!file.exists(download_path) || dir.exists(download_path)) {
    .rf_abort("Budget archive download did not create a file.", "transport")
  }
  downloaded_bytes <- as.numeric(file.info(download_path)$size)
  if (!is.finite(downloaded_bytes) || downloaded_bytes > max_bytes) {
    .rf_abort("Budget archive source exceeded the configured byte limit.",
              "too_large", size = downloaded_bytes, max_bytes = max_bytes)
  }
  if (status < 200L || status >= 300L) {
    .rf_abort(sprintf("Source returned HTTP %s.", status), "http",
              status = status, url = url)
  }
  has_next_link <- is.character(link) && length(link) == 1L &&
    !is.na(link) && nzchar(link) &&
    grepl("rel[[:space:]]*=[[:space:]]*[\"']?next[\"']?",
          link, ignore.case = TRUE)
  if (identical(as.integer(status), 206L) ||
      (is.character(content_range) && length(content_range) == 1L &&
       !is.na(content_range) && nzchar(trimws(content_range))) ||
      has_next_link) {
    .rf_abort("Source returned a partial or paginated response.",
              "truncated", status = status, url = url)
  }
  final_url <- .rf_archive_canonical_url(final_url)
  announced_length <- suppressWarnings(as.numeric(content_length))
  if (length(announced_length) == 1L &&
      is.finite(announced_length) &&
      announced_length >= 0 &&
      announced_length != downloaded_bytes) {
    .rf_abort(
      "Source body length disagrees with the HTTP Content-Length.",
      "truncated",
      announced_bytes = announced_length,
      actual_bytes = downloaded_bytes,
      url = url
    )
  }

  detected <- .rf_archive_media_type(final_url, content_type)
  expected_extension <- .rf_archive_url_extension(url)
  expected <- if (expected_extension %in%
                  .rf_budget_archive_document_extensions) {
    .rf_archive_media_type(url)
  } else {
    media_type
  }
  if (!identical(expected, "application/octet-stream") &&
      !identical(detected, "application/octet-stream") &&
      !identical(expected, detected)) {
    compatible_zip <- grepl(
      "wordprocessingml|spreadsheetml|zip", expected
    ) && grepl(
      "wordprocessingml|spreadsheetml|zip|octet-stream",
      tolower(content_type)
    )
    if (!compatible_zip) {
      .rf_abort(
        "Budget archive response media type conflicts with its official link.",
        "unsupported_media", expected = expected, actual = detected,
        url = url
      )
    }
  }
  .rf_archive_validate_file(download_path, expected)
  keep_download <- TRUE
  list(
    path = download_path,
    source_url = url,
    final_url = final_url,
    retrieved_at = as.character(retrieved_at),
    content_type = content_type,
    status = as.integer(status),
    response_bytes = downloaded_bytes,
    announced_bytes = if (is.finite(announced_length)) {
      announced_length
    } else {
      NA_real_
    },
    archive_media_type = expected
  )
}

.rf_archive_candidate_contract <- function(candidates) {
  candidates <- tibble::as_tibble(candidates)
  required <- c(
    "source_url", "title", "document_role", "budget_vintage",
    "index_url", "index_snapshot_id", "revision_status"
  )
  missing <- setdiff(required, names(candidates))
  if (length(missing)) {
    .rf_abort(
      paste("Budget archive candidates are missing:",
            paste(missing, collapse = ", ")),
      "schema", fields = missing
    )
  }
  if (!nrow(candidates)) return(candidates)
  for (field in required) {
    if (anyNA(candidates[[field]]) ||
        any(!nzchar(trimws(as.character(candidates[[field]]))))) {
      .rf_abort("Budget archive candidates have empty required metadata.",
                "schema", field = field)
    }
  }
  candidates$source_url <- vapply(
    candidates$source_url,
    .rf_archive_canonical_url,
    character(1)
  )
  candidates$index_url <- vapply(
    candidates$index_url,
    .rf_archive_canonical_url,
    character(1)
  )
  if (anyDuplicated(candidates$source_url)) {
    .rf_abort("Budget archive candidates repeat a source URL.", "schema")
  }
  if (!"publication_type" %in% names(candidates)) {
    candidates$publication_type <- mapply(
      .rf_archive_publication_type,
      candidates$title,
      candidates$source_url,
      USE.NAMES = FALSE
    )
  }
  if (!"media_type" %in% names(candidates)) {
    candidates$media_type <- vapply(
      candidates$source_url,
      .rf_archive_media_type,
      character(1)
    )
  }
  candidates
}

.rf_archive_free_bytes <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  probe <- path
  while (!dir.exists(probe)) {
    parent <- dirname(probe)
    if (identical(parent, probe)) return(NA_real_)
    probe <- parent
  }
  output <- suppressWarnings(tryCatch(
    system2("df", c("-Pk", shQuote(probe)), stdout = TRUE, stderr = TRUE),
    error = function(error) character()
  ))
  if (length(output) < 2L) return(NA_real_)
  fields <- strsplit(trimws(output[[length(output)]]), "\\s+")[[1]]
  if (length(fields) < 4L) return(NA_real_)
  available_kib <- suppressWarnings(as.numeric(fields[[4L]]))
  if (!is.finite(available_kib) || available_kib < 0) return(NA_real_)
  available_kib * 1024
}

.rf_archive_assert_capacity <- function(
    archive_dir,
    required_bytes,
    reserve_bytes = 512 * 1024^2,
    available_bytes = NULL) {
  available_bytes_supplied <- !is.null(available_bytes)
  values <- list(
    required_bytes = required_bytes,
    reserve_bytes = reserve_bytes
  )
  for (field in names(values)) {
    value <- values[[field]]
    if (!is.numeric(value) || length(value) != 1L ||
        is.na(value) || !is.finite(value) || value < 0) {
      .rf_abort(
        sprintf("`%s` must be one finite non-negative byte count.", field),
        "invalid_argument", field = field
      )
    }
  }
  if (is.null(available_bytes)) {
    available_bytes <- .rf_archive_free_bytes(archive_dir)
  }
  if (!is.numeric(available_bytes) || length(available_bytes) != 1L ||
      is.na(available_bytes) || !is.finite(available_bytes) ||
      available_bytes < 0) {
    if (!available_bytes_supplied) {
      return(invisible(list(
        required_bytes = required_bytes,
        reserve_bytes = reserve_bytes,
        available_bytes = NA_real_
      )))
    }
    .rf_abort(
      "Could not determine available storage for the Budget archive.",
      "archive_capacity", archive_dir = archive_dir
    )
  }
  if (required_bytes + reserve_bytes > available_bytes) {
    .rf_abort(
      sprintf(
        paste(
          "Budget archive preflight needs %.2f GiB plus a %.2f GiB reserve,",
          "but only %.2f GiB is available."
        ),
        required_bytes / 1024^3,
        reserve_bytes / 1024^3,
        available_bytes / 1024^3
      ),
      "archive_capacity",
      required_bytes = required_bytes,
      reserve_bytes = reserve_bytes,
      available_bytes = available_bytes,
      archive_dir = archive_dir
    )
  }
  invisible(list(
    required_bytes = required_bytes,
    reserve_bytes = reserve_bytes,
    available_bytes = available_bytes
  ))
}

#' Archive exact Budget document bytes
#'
#' Download and preserve exact bytes for explicit official Budget-document
#' candidates. This is the admission step after
#' [discover_au_budget_archive_index()] or [crawl_au_budget_archive()]: it does
#' not crawl for undiscovered URLs. Each document is streamed to archive-owned
#' staging, validated against its advertised media type and size limit, hashed
#' with SHA-256, and atomically admitted to an immutable content-addressed
#' object store. The inventory ledger is checkpointed after each admission, so
#' an interrupted run can be resumed with the same candidates.
#'
#' Existing identical bytes are reused. A changed response for the same
#' logical source URL is retained as a replacement row linked through
#' `replaces_archive_file_id`; it is never silently overwritten. A failed
#' candidate is recorded in the durable gap ledger when `continue_on_error` is
#' `TRUE` (the default), or aborts immediately when it is `FALSE`.
#'
#' @param candidates Candidate data frame from archive discovery. Required
#'   columns are `source_url`, `title`, `document_role`, `budget_vintage`,
#'   `index_url`, `index_snapshot_id`, and `revision_status`. `publication_type`,
#'   `media_type`, and `advertised_bytes` are inferred/optional when absent.
#' @param archive_dir Durable append-only archive root. It defaults to
#'   [readfiscalau_default_archive_dir()] and is created when absent. Pass a
#'   temporary or explicitly managed directory for tests and examples.
#' @param fetcher Optional injected offline fetcher. It must return either a
#'   regular non-symlink `path` or raw `raw` bytes plus scalar `status`,
#'   `content_type`, `final_url`, and `retrieved_at`; omit it to use bounded
#'   HTTPS transport.
#' @param max_bytes Positive finite per-document byte limit; default is
#'   `250 * 1024^2` (250 MiB).
#' @param inventory Optional alias for `candidates`.
#' @param discovery Optional crawl result whose `documents` (or `inventory`)
#'   component supplies candidates.
#' @param continue_on_error Logical scalar. With `TRUE`, transport/media/size
#'   failures become durable gap rows and processing continues; with `FALSE`,
#'   the first failure is rethrown as its typed error.
#' @param required_bytes Optional finite non-negative aggregate byte estimate.
#'   When `NULL`, a complete non-negative `advertised_bytes` candidate column
#'   is summed. The aggregate preflight is checked before downloads.
#' @param reserve_bytes Finite non-negative bytes to leave free after the
#'   estimate; default is `512 * 1024^2`. If a platform cannot report free
#'   space, the safety preflight is best effort; pass `available_bytes` for
#'   deterministic enforcement.
#' @param available_bytes Optional finite non-negative free-space override.
#'   When supplied, it is always enforced and invalid/insufficient values raise
#'   `readfiscalau_archive_capacity`; it is useful for deterministic tests and
#'   cross-platform orchestration.
#' @param progress Logical scalar. With `TRUE`, emit one compact progress
#'   message for the first, every tenth, and final candidate; default `FALSE`.
#' @returns A `tbl_df`/`data.frame` containing admissions added by this call,
#'   with columns `archive_file_id`, `logical_file_id`, `budget_vintage`,
#'   `publication_type`, `document_role`, `title`, `revision_status`,
#'   `replaces_archive_file_id`, `index_snapshot_id`, `index_url`, `source_url`,
#'   `final_url`, `media_type`, `byte_size`, `sha256`, `retrieved_at`,
#'   `archive_path`, `license`, `terms_url`, and `attribution`. The
#'   `budget_archive_gaps` attribute is a tibble of gap-ledger rows with
#'   `budget_vintage`, `index_snapshot_id`, `index_url`, `source_url`, `title`,
#'   `gap_type`, `checked_at`, and `gap_reason`. An empty candidate input
#'   returns an empty inventory tibble with the same inventory columns.
#'
#' @details The function acquires an exclusive `.write-lock` below
#'   `archive_dir`, writes temporary download/object/ledger files beneath the
#'   archive, atomically publishes objects and CSV ledgers, and removes staging
#'   files and the lock on exit. It may create `objects/sha256/`, `inventory/`,
#'   `.staging/`, and `.write-lock/`; it never writes to the package checkout
#'   unless the caller explicitly supplies that path. URLs must be credential-
#'   free HTTPS official hosts, response status must be 2xx, partial/paginated
#'   responses and `Content-Length` mismatches are rejected, and PDF/Office/
#'   archive/CSV signatures are validated before admission. The operation uses
#'   at most one download at a time and applies no credentials.
#'
#' Typed failures include `readfiscalau_invalid_argument`,
#' `readfiscalau_schema`, `readfiscalau_transport`, `readfiscalau_http`,
#' `readfiscalau_truncated`, `readfiscalau_too_large`,
#' `readfiscalau_unsupported_media`, `readfiscalau_archive_capacity`,
#' `readfiscalau_archive_locked`, `readfiscalau_archive_write`, and
#' `readfiscalau_archive_corrupt`. No warning is used as a substitute for a
#' failed admission. The default path and all durable side effects are opt-out
#' by passing an explicit `archive_dir` and injected `fetcher`.
#'
#' @examples
#' archive_example_dir <- tempfile("readfiscalau-archive-example-")
#' archive_example_url <-
#'   "https://archive.budget.gov.au/2025-26/example.csv"
#' archive_example_candidates <- data.frame(
#'   source_url = archive_example_url,
#'   title = "Example chart data",
#'   document_role = "chart_data",
#'   budget_vintage = "2025-26",
#'   index_url = "https://archive.budget.gov.au/index.htm",
#'   index_snapshot_id = "example-index",
#'   revision_status = "original",
#'   stringsAsFactors = FALSE
#' )
#' archive_example_fetcher <- function(url) {
#'   stopifnot(identical(url, archive_example_url))
#'   list(
#'     raw = charToRaw("series,value\nExample,1\n"),
#'     status = 200L,
#'     content_type = "text/csv",
#'     final_url = url,
#'     retrieved_at = "2026-08-01 00:00:00 UTC"
#'   )
#' }
#' admitted <- archive_au_budget_documents(
#'   candidates = archive_example_candidates,
#'   archive_dir = archive_example_dir,
#'   fetcher = archive_example_fetcher,
#'   max_bytes = 1024,
#'   reserve_bytes = 0,
#'   available_bytes = 1024^2
#' )
#' admitted[c("source_url", "byte_size", "sha256")]
#' attr(admitted, "budget_archive_gaps")
#'
#' @seealso [discover_au_budget_archive_index()],
#'   [crawl_au_budget_archive()], [list_au_budget_archive()],
#'   [validate_au_budget_archive()]
#' @family budget-archive-storage
#' @export
archive_au_budget_documents <- function(
    candidates = NULL,
    archive_dir = readfiscalau_default_archive_dir(),
    fetcher = NULL,
    max_bytes = 250 * 1024^2,
    inventory = NULL,
    discovery = NULL,
    continue_on_error = TRUE,
    required_bytes = NULL,
    reserve_bytes = 512 * 1024^2,
    available_bytes = NULL,
    progress = FALSE) {
  candidates <- candidates %||% inventory %||%
    discovery$documents %||% discovery$inventory
  candidates <- .rf_archive_candidate_contract(candidates)
  if (!nrow(candidates)) return(.rf_archive_empty_inventory())
  archive_lock <- .rf_archive_acquire_lock(
    archive_dir,
    "archive_au_budget_documents"
  )
  on.exit(.rf_archive_release_lock(archive_lock), add = TRUE)
  existing <- .rf_archive_read_csv(
    .rf_archive_inventory_path(archive_dir),
    .rf_archive_empty_inventory
  )
  if (is.null(required_bytes) &&
      "advertised_bytes" %in% names(candidates)) {
    advertised <- suppressWarnings(as.numeric(candidates$advertised_bytes))
    if (all(is.finite(advertised)) &&
        all(advertised >= 0)) {
      required_bytes <- sum(advertised)
    }
  }
  if (!is.null(required_bytes)) {
    .rf_archive_assert_capacity(
      archive_dir = archive_dir,
      required_bytes = required_bytes,
      reserve_bytes = reserve_bytes,
      available_bytes = available_bytes
    )
  }
  ledger <- existing
  download_gaps <- list()
  run_bytes <- 0
  rows <- lapply(seq_len(nrow(candidates)), function(index) {
    candidate <- candidates[index, , drop = FALSE]
    candidate_bytes <- max_bytes
    if ("advertised_bytes" %in% names(candidate)) {
      advertised <- suppressWarnings(
        as.numeric(candidate$advertised_bytes[[1]])
      )
      if (length(advertised) == 1L &&
          is.finite(advertised) &&
          advertised >= 0) {
        candidate_bytes <- advertised
      }
    }
    .rf_archive_assert_capacity(
      archive_dir = archive_dir,
      required_bytes = candidate_bytes,
      reserve_bytes = reserve_bytes,
      available_bytes = available_bytes
    )
    fetched <- tryCatch(
      .rf_archive_fetch_file(
        candidate$source_url[[1]],
        candidate$media_type[[1]],
        archive_dir = archive_dir,
        fetcher = fetcher,
        max_bytes = max_bytes
      ),
      error = function(error) {
        if (!isTRUE(continue_on_error)) stop(error)
        gap <- data.frame(
          budget_vintage = candidate$budget_vintage[[1]],
          index_snapshot_id = candidate$index_snapshot_id[[1]],
          index_url = candidate$index_url[[1]],
          source_url = candidate$source_url[[1]],
          title = candidate$title[[1]],
          gap_type = class(error)[[1]],
          checked_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
          gap_reason = conditionMessage(error),
          stringsAsFactors = FALSE
        )
        download_gaps[[length(download_gaps) + 1L]] <<- gap
        existing_gaps <- .rf_archive_read_csv(
          .rf_archive_gaps_path(archive_dir),
          .rf_archive_empty_gaps
        )
        all_gaps <- .rf_bind_rows(list(existing_gaps, gap))
        gap_key <- paste(
          all_gaps$budget_vintage,
          all_gaps$source_url,
          all_gaps$gap_type,
          sep = "\r"
        )
        all_gaps <- all_gaps[
          !duplicated(gap_key, fromLast = TRUE),
          ,
          drop = FALSE
        ]
        .rf_archive_write_csv_atomic(
          all_gaps,
          .rf_archive_gaps_path(archive_dir)
        )
        if (isTRUE(progress)) {
          message(sprintf(
            "[%d/%d] gap %s: %s",
            index, nrow(candidates), candidate$budget_vintage[[1]],
            conditionMessage(error)
          ))
        }
        NULL
      }
    )
    if (is.null(fetched)) return(NULL)
    on.exit(if (file.exists(fetched$path)) unlink(fetched$path), add = TRUE)
    object <- .rf_archive_store_file(fetched$path, archive_dir)
    terms <- .rf_archive_terms(fetched$source_url)
    logical_id <- paste0(
      "budget-",
      .rf_slug(candidate$budget_vintage[[1]]),
      "-",
      substr(.rf_archive_hash_text(candidate$source_url[[1]]), 1L, 16L)
    )
    prior <- ledger[
      ledger$logical_file_id == logical_id &
        ledger$sha256 != object$sha256,
      ,
      drop = FALSE
    ]
    replaces <- if (nrow(prior)) {
      prior$archive_file_id[[nrow(prior)]]
    } else {
      ""
    }
    revision <- candidate$revision_status[[1]]
    if (nzchar(replaces) && identical(revision, "original")) {
      revision <- "replacement"
    }
    row <- data.frame(
      archive_file_id = paste0(
        logical_id, "-", substr(object$sha256, 1L, 12L)
      ),
      logical_file_id = logical_id,
      budget_vintage = candidate$budget_vintage[[1]],
      publication_type = candidate$publication_type[[1]],
      document_role = candidate$document_role[[1]],
      title = candidate$title[[1]],
      revision_status = revision,
      replaces_archive_file_id = replaces,
      index_snapshot_id = candidate$index_snapshot_id[[1]],
      index_url = candidate$index_url[[1]],
      source_url = fetched$source_url,
      final_url = fetched$final_url,
      media_type = fetched$archive_media_type,
      byte_size = object$byte_size,
      sha256 = object$sha256,
      retrieved_at = fetched$retrieved_at,
      archive_path = object$archive_path,
      license = terms$license,
      terms_url = terms$terms_url,
      attribution = terms$attribution,
      stringsAsFactors = FALSE
    )
    ledger <<- .rf_bind_rows(list(ledger, row))
    key <- paste(ledger$archive_file_id, ledger$sha256, sep = "\r")
    ledger <<- ledger[!duplicated(key, fromLast = TRUE), , drop = FALSE]
    ledger <<- ledger[order(
      ledger$budget_vintage,
      ledger$source_url,
      ledger$retrieved_at
    ), , drop = FALSE]
    validate_au_budget_archive_inventory(ledger)
    .rf_archive_write_csv_atomic(
      ledger,
      .rf_archive_inventory_path(archive_dir)
    )
    run_bytes <<- run_bytes + object$byte_size
    if (isTRUE(progress) &&
        (index == 1L || index %% 10L == 0L || index == nrow(candidates))) {
      message(sprintf(
        "[%d/%d] archived %.2f GiB this run",
        index,
        nrow(candidates),
        run_bytes / 1024^3
      ))
    }
    row
  })
  added <- .rf_bind_rows(Filter(Negate(is.null), rows))
  if (!nrow(added)) added <- .rf_archive_empty_inventory()
  new_gaps <- .rf_bind_rows(download_gaps)
  attr(added, "budget_archive_gaps") <- new_gaps
  added
}

.rf_archive_index_snapshot <- function(
    raw,
    index_url,
    budget_vintage,
    retrieved_at,
    archive_dir) {
  object <- .rf_archive_store_object(raw, archive_dir)
  snapshot_id <- paste0(
    "budget-index-",
    .rf_slug(budget_vintage),
    "-",
    substr(object$sha256, 1L, 16L)
  )
  data.frame(
    index_snapshot_id = snapshot_id,
    budget_vintage = budget_vintage,
    index_url = index_url,
    retrieved_at = retrieved_at,
    media_type = "text/html",
    byte_size = object$byte_size,
    sha256 = object$sha256,
    archive_path = object$archive_path,
    stringsAsFactors = FALSE
  )
}

.rf_archive_content_anchors <- function(raw) {
  document <- xml2::read_html(rawToChar(raw))
  content <- xml2::xml_find_first(document, ".//main")
  if (inherits(content, "xml_missing")) {
    content <- xml2::xml_find_first(document, ".//body")
  }
  xml2::xml_find_all(content, ".//a[@href]")
}

.rf_archive_root_entries <- function(raw, root_url, snapshot_id) {
  anchors <- .rf_archive_content_anchors(raw)
  rows <- lapply(seq_along(anchors), function(index) {
    anchor <- anchors[[index]]
    href <- trimws(xml2::xml_attr(anchor, "href") %||% "")
    title <- .rf_archive_anchor_title(anchor)
    vintage <- .rf_archive_vintage(title, href)
    if (is.na(vintage) || !nzchar(href) || startsWith(href, "#")) return(NULL)
    resolved <- xml2::url_absolute(
      utils::URLencode(href, reserved = FALSE),
      root_url
    )
    canonical <- .rf_archive_canonical_url(resolved)
    if (!.rf_archive_first_party_url(canonical)) return(NULL)
    extension <- .rf_archive_url_extension(canonical)
    entry_type <- if (extension %in%
                      .rf_budget_archive_document_extensions) {
      "document"
    } else {
      "index"
    }
    data.frame(
      budget_vintage = vintage,
      entry_type = entry_type,
      title = title,
      source_url = canonical,
      index_url = root_url,
      index_snapshot_id = snapshot_id,
      index_ordinal = as.integer(index),
      stringsAsFactors = FALSE
    )
  })
  out <- .rf_bind_rows(Filter(Negate(is.null), rows))
  if (!nrow(out)) return(out)
  out[!duplicated(paste(out$budget_vintage, out$source_url, sep = "\r")),
      , drop = FALSE]
}

.rf_archive_subindex_links <- function(raw, index_url, budget_vintage) {
  anchors <- .rf_archive_content_anchors(raw)
  links <- lapply(anchors, function(anchor) {
    href <- trimws(xml2::xml_attr(anchor, "href") %||% "")
    if (!nzchar(href) || startsWith(href, "#")) return(NULL)
    resolved <- tryCatch(
      xml2::url_absolute(
        utils::URLencode(href, reserved = FALSE),
        index_url
      ),
      error = function(error) NA_character_
    )
    if (is.na(resolved)) return(NULL)
    canonical <- tryCatch(
      .rf_archive_canonical_url(resolved),
      readfiscalau_error = function(error) NA_character_
    )
    if (is.na(canonical) ||
        !.rf_archive_first_party_url(canonical) ||
        identical(canonical, index_url)) {
      return(NULL)
    }
    extension <- .rf_archive_url_extension(canonical)
    if (!extension %in% c("", "htm", "html")) return(NULL)
    path <- tolower(httr2::url_parse(canonical)$path %||% "")
    vintage_path <- sub("-october$", "", budget_vintage)
    release_scoped <- grepl(
      paste0("/", gsub("-", "[-/]", vintage_path), "/"),
      path
    ) || grepl(
      paste0(
        "/content/(?:bp[1-9]|documents|downloads|myefo|fbo|pbs|paes|",
        "appropriation|womens)"
      ),
      path,
      perl = TRUE
    )
    if (!release_scoped ||
        grepl("/(?:help|contact|privacy|copyright|purchasing)", path)) {
      return(NULL)
    }
    canonical
  })
  unique(unlist(Filter(Negate(is.null), links), use.names = FALSE))
}

.rf_archive_fiscal_year_sequence <- function(first = 1901L, last) {
  sprintf(
    "%04d-%02d",
    seq.int(first, last),
    (seq.int(first, last) + 1L) %% 100L
  )
}

.rf_archive_gap_rows <- function(
    root_entries,
    root_snapshot,
    current_vintage,
    include_current = TRUE) {
  latest <- as.integer(substr(sub("-october$", "", current_vintage), 1L, 4L))
  expected <- .rf_archive_fiscal_year_sequence(1901L, latest)
  present <- unique(sub(
    "-october$",
    "",
    root_entries$budget_vintage
  ))
  if (isTRUE(include_current)) {
    present <- unique(c(present, sub("-october$", "", current_vintage)))
  }
  missing <- setdiff(expected, present)
  if (!length(missing)) return(.rf_archive_empty_gaps())
  data.frame(
    budget_vintage = missing,
    index_snapshot_id = root_snapshot$index_snapshot_id[[1]],
    index_url = root_snapshot$index_url[[1]],
    source_url = "",
    title = paste("Budget", missing),
    gap_type = "not_listed_by_official_archive",
    checked_at = root_snapshot$retrieved_at[[1]],
    gap_reason = paste(
      "No visible entry was present in the captured official Budget",
      "archive root index; no document URL was inferred."
    ),
    stringsAsFactors = FALSE
  )
}

.rf_archive_document_from_root <- function(entry) {
  context <- paste(entry$title[[1]], entry$source_url[[1]])
  data.frame(
    source_url = entry$source_url[[1]],
    title = entry$title[[1]],
    document_role = .rf_archive_document_role(
      context, entry$source_url[[1]]
    ),
    publication_type = "budget",
    budget_vintage = entry$budget_vintage[[1]],
    index_url = entry$index_url[[1]],
    index_snapshot_id = entry$index_snapshot_id[[1]],
    index_ordinal = entry$index_ordinal[[1]],
    revision_status = .rf_archive_revision_status(context),
    media_type = .rf_archive_media_type(entry$source_url[[1]]),
    stringsAsFactors = FALSE
  )
}

#' Crawl all official Australian Government Budget indexes
#'
#' Crawl the official Budget archive root and year/index pages to build a
#' reproducible discovery snapshot. The crawl downloads index HTML only;
#' candidate document bytes are left for
#' [archive_au_budget_documents()]. Missing fiscal years, failed index pages,
#' and insecure links are retained as explicit gap rows rather than inferred
#' documents. Repeated source URLs and index snapshots are de-duplicated.
#'
#' @param root_url Official historical archive root. It must be a credential-
#'   free HTTPS URL on an allowed Budget `.gov.au` host.
#' @param current_url Official current Budget downloads page, validated even
#'   when `include_current = FALSE`.
#' @param current_vintage Current fiscal-year identifier used for current-page
#'   discovery and the expected missing-year range; default is `"2026-27"`.
#' @param include_current Logical scalar. Include the current downloads/Budget
#'   subindex pages in the queue when `TRUE` (default); set `FALSE` for a
#'   bounded historical or deterministic fixture crawl.
#' @param archive_dir Durable archive root for index snapshots, index ledger,
#'   gap ledger, and the writer lock. Defaults to
#'   [readfiscalau_default_archive_dir()].
#' @param fetcher Optional injected offline fetcher with the same response
#'   contract as [archive_au_budget_documents()]. Omit it only when live
#'   bounded HTTPS index retrieval is explicitly intended.
#' @param max_index_pages Positive integer safety bound on visited index pages;
#'   default is `250L`.
#' @returns A list with four components: `documents` is a tibble of unique
#'   candidate rows (`source_url`, `title`, `document_role`,
#'   `publication_type`, `budget_vintage`, `index_url`, `index_snapshot_id`,
#'   `index_ordinal`, `revision_status`, `media_type`); `indexes` is a tibble
#'   with `index_snapshot_id`, `budget_vintage`, `index_url`, `retrieved_at`,
#'   `media_type`, `byte_size`, `sha256`, and `archive_path`; `gaps` is a tibble
#'   with the eight gap-ledger fields; and `root_entries` is a tibble with
#'   `budget_vintage`, `entry_type`, `title`, `source_url`, `index_url`,
#'   `index_snapshot_id`, and `index_ordinal`.
#'
#' `complete` is not returned by this discovery operation: gaps are evidence
#' for later [validate_au_budget_archive()] completeness accounting. The crawl
#' returns no candidate document bytes.
#'
#' @details The crawl acquires an exclusive `.write-lock`, writes each index
#' response as a SHA-256-addressed object, and atomically checkpoints the index
#' and gap CSV ledgers below `archive_dir`. Temporary fetch files and the lock
#' are removed on exit. The crawl is bounded to 20 MiB per index and
#' `max_index_pages` pages; it is HTTPS-only, credential-free, retry-limited,
#' and rejects partial/paginated responses, malformed HTML, unsafe links, and
#' media/signature mismatches with typed `readfiscalau_*` errors. A page-fetch
#' failure is normally recorded as an `index_fetch_failure` gap so later pages
#' can still be processed; lock, write, schema, transport, and scope failures
#' abort the operation. No warning is emitted in place of a gap or error.
#'
#' @examples
#' crawl_example_root <- "https://archive.budget.gov.au/index.htm"
#' crawl_example_year <-
#'   "https://archive.budget.gov.au/2025-26/index.htm"
#' crawl_example_fetcher <- function(url) {
#'   raw <- if (identical(url, crawl_example_root)) {
#'     charToRaw(paste0(
#'       "<html><main><a href=\"/2025-26/index.htm\">",
#'       "2025-26 Budget</a></main></html>"
#'     ))
#'   } else if (identical(url, crawl_example_year)) {
#'     charToRaw(
#'       "<html><main><a href=\"bp1.pdf\">Budget Paper No. 1</a></main></html>"
#'     )
#'   } else {
#'     stop("unexpected fixture index: ", url, call. = FALSE)
#'   }
#'   list(
#'     raw = raw,
#'     status = 200L,
#'     content_type = "text/html",
#'     final_url = url,
#'     retrieved_at = "2026-08-01 00:00:00 UTC"
#'   )
#' }
#' crawl_example <- crawl_au_budget_archive(
#'   root_url = crawl_example_root,
#'   current_url = crawl_example_root,
#'   current_vintage = "2025-26",
#'   include_current = FALSE,
#'   archive_dir = tempfile("readfiscalau-crawl-example-"),
#'   fetcher = crawl_example_fetcher,
#'   max_index_pages = 3L
#' )
#' c(
#'   documents = nrow(crawl_example$documents),
#'   indexes = nrow(crawl_example$indexes),
#'   gaps = nrow(crawl_example$gaps)
#' )
#'
#' @seealso [discover_au_budget_archive_index()],
#'   [archive_au_budget_documents()], [validate_au_budget_archive()]
#' @family budget-archive-discovery
#' @export
crawl_au_budget_archive <- function(
    root_url = "https://archive.budget.gov.au/index.htm",
    current_url = "https://budget.gov.au/content/downloads.htm",
    current_vintage = "2026-27",
    include_current = TRUE,
    archive_dir = readfiscalau_default_archive_dir(),
    fetcher = NULL,
    max_index_pages = 250L) {
  root_url <- .rf_archive_canonical_url(root_url)
  current_url <- .rf_archive_canonical_url(current_url)
  if (!.rf_archive_first_party_url(root_url) ||
      !.rf_archive_first_party_url(current_url)) {
    .rf_abort("Budget crawl roots must use budget.gov.au.", "schema")
  }
  if (!is.numeric(max_index_pages) ||
      length(max_index_pages) != 1L ||
      is.na(max_index_pages) ||
      max_index_pages < 1L ||
      max_index_pages != as.integer(max_index_pages)) {
    .rf_abort("`max_index_pages` must be a positive integer.",
              "invalid_argument")
  }
  archive_lock <- .rf_archive_acquire_lock(
    archive_dir,
    "crawl_au_budget_archive"
  )
  on.exit(.rf_archive_release_lock(archive_lock), add = TRUE)
  fetch_index <- function(url, vintage) {
    fetched <- .rf_archive_fetch(
      url,
      "text/html",
      fetcher = fetcher,
      max_bytes = 20 * 1024^2
    )
    snapshot <- .rf_archive_index_snapshot(
      fetched$raw,
      url,
      vintage,
      fetched$retrieved_at,
      archive_dir
    )
    list(fetched = fetched, snapshot = snapshot)
  }
  root <- fetch_index(root_url, "archive-root")
  root_entries <- .rf_archive_root_entries(
    root$fetched$raw,
    root_url,
    root$snapshot$index_snapshot_id[[1]]
  )
  queue <- root_entries[root_entries$entry_type == "index",
                        c("budget_vintage", "source_url"), drop = FALSE]
  if (isTRUE(include_current)) {
    queue <- .rf_bind_rows(list(
      queue,
      data.frame(
        budget_vintage = current_vintage,
        source_url = c(
          current_url,
          "https://budget.gov.au/content/bp1/index.htm",
          "https://budget.gov.au/content/bp2/index.htm",
          "https://budget.gov.au/content/bp3/index.htm",
          "https://budget.gov.au/content/bp4/index.htm",
          "https://budget.gov.au/content/womens-statement/index.htm",
          "https://budget.gov.au/content/pbs/index.htm",
          "https://budget.gov.au/content/appropriation-bills/index.htm",
          "https://budget.gov.au/content/myefo/index.htm",
          "https://budget.gov.au/content/fbo/index.htm"
        ),
        stringsAsFactors = FALSE
      )
    ))
  }
  queue <- queue[!duplicated(queue$source_url), , drop = FALSE]
  documents <- lapply(
    seq_len(nrow(root_entries[root_entries$entry_type == "document", ])),
    function(index) {
      direct <- root_entries[root_entries$entry_type == "document", ,
                             drop = FALSE]
      .rf_archive_document_from_root(direct[index, , drop = FALSE])
    }
  )
  indexes <- list(root$snapshot)
  crawl_gaps <- list()
  seen <- root_url
  cursor <- 1L
  while (cursor <= nrow(queue)) {
    if (length(seen) >= max_index_pages) {
      .rf_abort(
        "Budget archive crawl exceeded its index-page safety bound.",
        "archive_scope", max_index_pages = max_index_pages
      )
    }
    url <- queue$source_url[[cursor]]
    vintage <- queue$budget_vintage[[cursor]]
    cursor <- cursor + 1L
    if (url %in% seen) next
    seen <- c(seen, url)
    page <- tryCatch(
      fetch_index(url, vintage),
      error = function(error) {
        crawl_gaps[[length(crawl_gaps) + 1L]] <<- data.frame(
          budget_vintage = vintage,
          index_snapshot_id = "",
          index_url = url,
          source_url = url,
          title = paste("Budget index", vintage),
          gap_type = "index_fetch_failure",
          checked_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
          gap_reason = conditionMessage(error),
          stringsAsFactors = FALSE
        )
        NULL
      }
    )
    if (is.null(page)) next
    indexes[[length(indexes) + 1L]] <- page$snapshot
    discovered <- discover_au_budget_archive_index(
      page$fetched$raw,
      index_url = url,
      budget_vintage = vintage,
      index_snapshot_id = page$snapshot$index_snapshot_id[[1]]
    )
    page_gaps <- attr(discovered, "budget_archive_link_gaps")
    if (!is.null(page_gaps) && nrow(page_gaps)) {
      page_gaps$checked_at[!nzchar(page_gaps$checked_at)] <-
        page$snapshot$retrieved_at[[1]]
      crawl_gaps[[length(crawl_gaps) + 1L]] <- page_gaps
    }
    if (nrow(discovered)) {
      documents[[length(documents) + 1L]] <- discovered
    }
    subindexes <- .rf_archive_subindex_links(
      page$fetched$raw, url, vintage
    )
    if (length(subindexes)) {
      queue <- .rf_bind_rows(list(
        queue,
        data.frame(
          budget_vintage = vintage,
          source_url = subindexes,
          stringsAsFactors = FALSE
        )
      ))
      queue <- queue[!duplicated(queue$source_url), , drop = FALSE]
    }
  }
  index_ledger <- .rf_bind_rows(indexes)
  index_ledger <- index_ledger[
    !duplicated(index_ledger$index_snapshot_id),
    ,
    drop = FALSE
  ]
  document_ledger <- .rf_bind_rows(documents)
  if (nrow(document_ledger)) {
    document_ledger <- document_ledger[
      !duplicated(document_ledger$source_url),
      ,
      drop = FALSE
    ]
  }
  gaps <- .rf_bind_rows(c(list(.rf_archive_gap_rows(
    root_entries,
    root$snapshot,
    current_vintage,
    include_current = include_current
  )), crawl_gaps))
  .rf_archive_write_csv_atomic(
    index_ledger,
    .rf_archive_indexes_path(archive_dir)
  )
  existing_gaps <- .rf_archive_read_csv(
    .rf_archive_gaps_path(archive_dir),
    .rf_archive_empty_gaps
  )
  all_gaps <- .rf_bind_rows(list(existing_gaps, gaps))
  gap_key <- paste(
    all_gaps$budget_vintage,
    all_gaps$source_url,
    all_gaps$gap_type,
    sep = "\r"
  )
  all_gaps <- all_gaps[
    !duplicated(gap_key, fromLast = TRUE),
    ,
    drop = FALSE
  ]
  .rf_archive_write_csv_atomic(
    all_gaps,
    .rf_archive_gaps_path(archive_dir)
  )
  list(
    documents = document_ledger,
    indexes = index_ledger,
    gaps = all_gaps,
    root_entries = root_entries
  )
}

#' Read the bundled official Budget archive catalogue
#'
#' Read and integrity-check the version-controlled Budget discovery catalogue
#' shipped with the package. The catalogue describes official document links,
#' captured index snapshots, and explicit gaps; it does not bundle the
#' multi-gigabyte document objects themselves. Use
#' [archive_au_budget_documents()] to materialise selected candidates in an
#' operator-managed durable archive.
#'
#' Every captured index snapshot is hashed against the installed index ledger
#' before the list is returned. This makes the function a local evidence read,
#' not a live refresh operation.
#'
#' @returns A list with `metadata` (the parsed catalogue JSON object),
#'   `candidates` (a tibble with the 16 bundled candidate fields, including
#'   source URL, availability, advertised bytes, and snapshot identity),
#'   `indexes` (a tibble with the eight index-ledger fields, the catalogue's
#'   `snapshot_file`, and the resolved installed `snapshot_path`), and `gaps`
#'   (a tibble with
#'   `budget_vintage`, `index_snapshot_id`, `index_url`, `source_url`, `title`,
#'   `gap_type`, `checked_at`, and `gap_reason`). The tabular components have
#'   `tbl_df`/`data.frame` classes; `metadata` retains the structure encoded in
#'   the JSON file.
#'
#' @details Missing catalogue files or missing snapshot files raise typed
#' `readfiscalau_archive_catalogue` errors. A snapshot hash or catalogue
#' integrity mismatch raises `readfiscalau_archive_catalogue_corrupt`.
#' The function performs no network requests, cache writes, locks, or durable
#' filesystem writes and emits no warnings.
#'
#' @examples
#' catalogue <- read_au_budget_archive_catalogue()
#' c(
#'   candidates = nrow(catalogue$candidates),
#'   indexes = nrow(catalogue$indexes),
#'   gaps = nrow(catalogue$gaps)
#' )
#' head(catalogue$candidates[c("budget_vintage", "document_role", "availability")])
#'
#' @seealso [list_au_budget_archive_candidates()],
#'   [discover_au_budget_archive_index()], [archive_au_budget_documents()]
#' @family budget-archive-discovery
#' @export
read_au_budget_archive_catalogue <- function() {
  catalogue_file <- function(name) {
    path <- system.file("extdata", name, package = "readfiscalau")
    if (!nzchar(path) || !file.exists(path)) {
      .rf_abort(
        paste("The installed readfiscalau package is missing", name),
        "archive_catalogue", file = name
      )
    }
    path
  }
  read_table <- function(name) {
    tibble::as_tibble(utils::read.csv(
      catalogue_file(name),
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = character()
    ))
  }
  indexes <- read_table("budget-archive-indexes.csv")
  indexes$snapshot_path <- file.path(
    dirname(catalogue_file("budget-archive-indexes.csv")),
    indexes$snapshot_file
  )
  if (any(!file.exists(indexes$snapshot_path))) {
    .rf_abort(
      "The installed Budget archive catalogue is missing index evidence.",
      "archive_catalogue_corrupt"
    )
  }
  snapshot_hashes <- vapply(
    indexes$snapshot_path,
    digest::digest,
    character(1),
    algo = "sha256",
    serialize = FALSE,
    file = TRUE
  )
  if (!identical(unname(snapshot_hashes), as.character(indexes$sha256))) {
    .rf_abort(
      "A bundled Budget index snapshot failed SHA-256 validation.",
      "archive_catalogue_corrupt"
    )
  }
  list(
    metadata = jsonlite::fromJSON(
      catalogue_file("budget-archive-metadata.json"),
      simplifyVector = TRUE
    ),
    candidates = read_table("budget-archive-candidates.csv"),
    indexes = indexes,
    gaps = read_table("budget-archive-gaps.csv")
  )
}

#' List the bundled official Budget document catalogue
#'
#' Filter the bundled official Budget-document discovery snapshot without
#' network access. The result is a catalogue of candidate URLs and discovery
#' metadata, not a claim that the document bytes are installed. Use
#' [archive_au_budget_documents()] to materialise selected candidates in a
#' durable archive; unavailable or insecure links remain visible as catalogue
#' rows/gaps rather than being silently dropped.
#'
#' Filters use `%in%` matching, so each argument can be a character vector;
#' `NULL` leaves that dimension unfiltered. No ordering, network refresh, or
#' HEAD request is performed by this function.
#'
#' @param budget_vintage Optional character vector of fiscal-year/vintage
#'   labels, such as `"2025-26"` or `"2022-23-october"`.
#' @param document_role Optional character vector of inferred roles, such as
#'   `"budget_paper_1"`, `"chart_data"`, or `"appendix"`.
#' @param availability Optional character vector such as `"available"` or
#'   `"not_found"`; values are matched exactly against the bundled catalogue.
#' @returns A `tbl_df`/`data.frame` containing the matching bundled candidate
#'   rows. Columns are `budget_vintage`, `publication_type`, `document_role`,
#'   `title`, `revision_status`, `source_url`, `final_url`, `media_type`,
#'   `head_content_type`, `http_status`, `availability`, `advertised_bytes`,
#'   `checked_at`, `index_url`, `index_snapshot_id`, and `index_ordinal`.
#'   An unmatched filter returns a zero-row tibble with these columns.
#'
#' @details Catalogue corruption is reported by
#' [read_au_budget_archive_catalogue()] as a typed
#' `readfiscalau_archive_catalogue` or
#' `readfiscalau_archive_catalogue_corrupt` error. This filter itself performs
#' no writes, locking, network access, or warnings.
#'
#' @examples
#' current_candidates <- list_au_budget_archive_candidates(
#'   budget_vintage = "2025-26",
#'   availability = "available"
#' )
#' head(current_candidates[c("source_url", "document_role", "media_type")])
#'
#' @seealso [read_au_budget_archive_catalogue()],
#'   [discover_au_budget_archive_index()], [archive_au_budget_documents()]
#' @family budget-archive-discovery
#' @export
list_au_budget_archive_candidates <- function(
    budget_vintage = NULL,
    document_role = NULL,
    availability = NULL) {
  candidates <- read_au_budget_archive_catalogue()$candidates
  if (!is.null(budget_vintage)) {
    candidates <- candidates[
      candidates$budget_vintage %in% budget_vintage,
      ,
      drop = FALSE
    ]
  }
  if (!is.null(document_role)) {
    candidates <- candidates[
      candidates$document_role %in% document_role,
      ,
      drop = FALSE
    ]
  }
  if (!is.null(availability)) {
    candidates <- candidates[
      candidates$availability %in% availability,
      ,
      drop = FALSE
    ]
  }
  candidates
}

#' List archived Australian Government Budget documents
#'
#' Read the durable content-addressed inventory created by
#' [archive_au_budget_documents()] and optionally filter it locally. This is a
#' ledger read: it does not verify object bytes, refresh URLs, or discover new
#' candidates. Use [validate_au_budget_archive()] when completeness and byte
#' integrity must be checked.
#'
#' @param budget_vintage Optional character vector of fiscal-year/vintage
#'   labels to retain.
#' @param document_role Optional character vector of document roles to retain.
#' @param revision_status Optional character vector such as `"original"`,
#'   `"replacement"`, `"revised"`, or `"superseded"`.
#' @param archive_dir Durable archive root. Defaults to
#'   [readfiscalau_default_archive_dir()]; no directory is created when it is
#'   absent.
#' @returns A `tbl_df`/`data.frame` with the immutable 21-column inventory:
#'   `archive_file_id`, `logical_file_id`, `budget_vintage`,
#'   `publication_type`, `document_role`, `title`, `revision_status`,
#'   `replaces_archive_file_id`, `index_snapshot_id`, `index_url`, `source_url`,
#'   `final_url`, `media_type`, `byte_size`, `sha256`, `retrieved_at`,
#'   `archive_path`, `license`, `terms_url`, and `attribution`. A missing ledger
#'   yields a zero-row tibble with these columns. Filters use `%in%` matching.
#'
#' @details The function reads only the inventory CSV below `archive_dir`; it
#' performs no network, lock, hash, or durable-write operation and emits no
#' warnings. The default directory may be redirected with
#' `READFISCALAU_BUDGET_ARCHIVE_DIR` through
#' [readfiscalau_default_archive_dir()].
#'
#' @examples
#' list_example_dir <- tempfile("readfiscalau-list-example-")
#' list_example_url <-
#'   "https://archive.budget.gov.au/2025-26/example.csv"
#' list_example_candidate <- data.frame(
#'   source_url = list_example_url,
#'   title = "Example chart data",
#'   document_role = "chart_data",
#'   budget_vintage = "2025-26",
#'   index_url = "https://archive.budget.gov.au/index.htm",
#'   index_snapshot_id = "example-index",
#'   revision_status = "original",
#'   stringsAsFactors = FALSE
#' )
#' list_example_fetcher <- function(url) list(
#'   raw = charToRaw("series,value\nExample,1\n"),
#'   status = 200L,
#'   content_type = "text/csv",
#'   final_url = url,
#'   retrieved_at = "2026-08-01 00:00:00 UTC"
#' )
#' archive_au_budget_documents(
#'   list_example_candidate,
#'   archive_dir = list_example_dir,
#'   fetcher = list_example_fetcher,
#'   max_bytes = 1024,
#'   reserve_bytes = 0,
#'   available_bytes = 1024^2
#' )
#' list_au_budget_archive(
#'   archive_dir = list_example_dir,
#'   revision_status = "original"
#' )[, c("source_url", "byte_size", "sha256")]
#'
#' @seealso [archive_au_budget_documents()],
#'   [validate_au_budget_archive()], [readfiscalau_default_archive_dir()]
#' @family budget-archive-storage
#' @export
list_au_budget_archive <- function(
    budget_vintage = NULL,
    document_role = NULL,
    revision_status = NULL,
    archive_dir = readfiscalau_default_archive_dir()) {
  inventory <- .rf_archive_read_csv(
    .rf_archive_inventory_path(archive_dir),
    .rf_archive_empty_inventory
  )
  if (!is.null(budget_vintage)) {
    inventory <- inventory[
      inventory$budget_vintage %in% budget_vintage,
      ,
      drop = FALSE
    ]
  }
  if (!is.null(document_role)) {
    inventory <- inventory[
      inventory$document_role %in% document_role,
      ,
      drop = FALSE
    ]
  }
  if (!is.null(revision_status)) {
    inventory <- inventory[
      inventory$revision_status %in% revision_status,
      ,
      drop = FALSE
    ]
  }
  inventory
}

#' Validate completeness and byte integrity of the Budget archive
#'
#' Reconcile discovered Budget-document candidates, durable admissions, and
#' explicit gap records, while re-hashing every admitted object. This is the
#' completeness/integrity gate for an archive; it does not download missing
#' candidates or repair corrupt objects.
#'
#' A candidate is accounted for when its URL is present in the inventory or in
#' a gap record. `complete` is `TRUE` exactly when no candidate URL is missing
#' from both. An inventory URL absent from the candidate set is reported as
#' `unexpected`. Replacement rows are valid when their inventory chain passes
#' [validate_au_budget_archive_inventory()].
#'
#' @param archive_dir Durable archive root containing `inventory/`, `objects/`,
#'   and the durable gap ledger. Defaults to
#'   [readfiscalau_default_archive_dir()].
#' @param inventory Optional inventory tibble/data frame. When `NULL`, use
#'   `archive$inventory` or read the durable inventory ledger.
#' @param indexes Optional captured-index ledger. When `NULL`, use
#'   `index`, `discovery$indexes`, or the durable index ledger.
#' @param candidates Optional candidate tibble/data frame. When `NULL`, use
#'   `discovery$documents` or an empty candidate table.
#' @param index Optional alias for `indexes`.
#' @param discovery Optional result from [crawl_au_budget_archive()] supplying
#'   `documents`, `indexes`, and `gaps`.
#' @param archive Optional result from [archive_au_budget_documents()] supplying
#'   `inventory`.
#' @returns A list with `complete` (logical scalar), `missing` and
#'   `unexpected` candidate/inventory tibbles, `gaps` (the de-duplicated gap
#'   ledger), `summary` (one row per vintage with
#'   `budget_vintage`, `indexed_count`, `admitted_count`, `gap_count`,
#'   `unresolved_count`, and `complete`), `indexes` (the selected index ledger),
#'   and `inventory` (the validated inventory). `missing`, `unexpected`,
#'   `gaps`, `summary`, `indexes`, and `inventory` are tibble/data-frame
#'   objects where applicable.
#'
#' @details Each admitted `archive_path` must exist, be a regular file, and
#' match both its recorded byte size and SHA-256. Missing objects, changed
#' bytes, malformed inventory, or invalid replacement chains raise typed
#' `readfiscalau_archive_corrupt` or `readfiscalau_schema` errors. Missing
#' candidates are reported as data, not warnings; no network requests or
#' durable writes are performed. The function reads the durable ledgers and
#' may read every admitted object to hash it, so pass an explicit `archive_dir`
#' for controlled validation.
#'
#' @examples
#' validate_example_dir <- tempfile("readfiscalau-validate-example-")
#' validate_example_url <-
#'   "https://archive.budget.gov.au/2025-26/example.csv"
#' validate_example_candidate <- data.frame(
#'   source_url = validate_example_url,
#'   title = "Example chart data",
#'   document_role = "chart_data",
#'   budget_vintage = "2025-26",
#'   index_url = "https://archive.budget.gov.au/index.htm",
#'   index_snapshot_id = "example-index",
#'   revision_status = "original",
#'   stringsAsFactors = FALSE
#' )
#' validate_example_fetcher <- function(url) list(
#'   raw = charToRaw("series,value\nExample,1\n"),
#'   status = 200L,
#'   content_type = "text/csv",
#'   final_url = url,
#'   retrieved_at = "2026-08-01 00:00:00 UTC"
#' )
#' archive_au_budget_documents(
#'   validate_example_candidate,
#'   archive_dir = validate_example_dir,
#'   fetcher = validate_example_fetcher,
#'   max_bytes = 1024,
#'   reserve_bytes = 0,
#'   available_bytes = 1024^2
#' )
#' validation <- validate_au_budget_archive(
#'   archive_dir = validate_example_dir,
#'   candidates = validate_example_candidate
#' )
#' c(
#'   complete = validation$complete,
#'   missing = nrow(validation$missing),
#'   unexpected = nrow(validation$unexpected)
#' )
#'
#' @seealso [validate_au_budget_archive_inventory()],
#'   [list_au_budget_archive()], [crawl_au_budget_archive()],
#'   [archive_au_budget_documents()]
#' @family budget-archive-validation
#' @export
validate_au_budget_archive <- function(
    archive_dir = readfiscalau_default_archive_dir(),
    inventory = NULL,
    indexes = NULL,
    candidates = NULL,
    index = NULL,
    discovery = NULL,
    archive = NULL) {
  inventory <- inventory %||% archive$inventory %||%
    .rf_archive_read_csv(
      .rf_archive_inventory_path(archive_dir),
      .rf_archive_empty_inventory
    )
  indexes <- indexes %||% index %||% discovery$indexes %||%
    .rf_archive_read_csv(
      .rf_archive_indexes_path(archive_dir),
      .rf_archive_empty_indexes
    )
  candidates <- candidates %||% discovery$documents %||% tibble::tibble(
    source_url = character(),
    budget_vintage = character()
  )
  durable_gaps <- .rf_archive_read_csv(
    .rf_archive_gaps_path(archive_dir),
    .rf_archive_empty_gaps
  )
  gaps <- .rf_bind_rows(list(discovery$gaps, durable_gaps))
  if (nrow(gaps)) {
    gap_key <- paste(
      gaps$budget_vintage,
      gaps$source_url,
      gaps$gap_type,
      sep = "\r"
    )
    gaps <- gaps[
      !duplicated(gap_key, fromLast = TRUE),
      ,
      drop = FALSE
    ]
  }
  inventory <- validate_au_budget_archive_inventory(inventory)
  if (nrow(inventory)) {
    for (row in seq_len(nrow(inventory))) {
      path <- inventory$archive_path[[row]]
      if (!file.exists(path)) {
        .rf_abort("A Budget archive object is missing.",
                  "archive_corrupt", path = path)
      }
      actual_size <- as.numeric(file.info(path)$size)
      actual_hash <- digest::digest(
        file = path, algo = "sha256", serialize = FALSE
      )
      if (!identical(actual_hash, inventory$sha256[[row]]) ||
          !identical(actual_size, as.numeric(inventory$byte_size[[row]]))) {
        .rf_abort(
          "A Budget archive object failed immutable hash or size validation.",
          "archive_corrupt", path = path,
          expected_sha256 = inventory$sha256[[row]],
          actual_sha256 = actual_hash
        )
      }
    }
  }
  candidate_urls <- unique(as.character(candidates$source_url %||%
                                          character()))
  inventory_urls <- unique(as.character(inventory$source_url %||%
                                          character()))
  gap_urls <- unique(as.character(gaps$source_url %||% character()))
  gap_urls <- gap_urls[nzchar(gap_urls)]
  missing_urls <- setdiff(candidate_urls, union(inventory_urls, gap_urls))
  unexpected_urls <- setdiff(inventory_urls, candidate_urls)
  missing <- candidates[
    candidates$source_url %in% missing_urls,
    ,
    drop = FALSE
  ]
  unexpected <- inventory[
    inventory$source_url %in% unexpected_urls,
    ,
    drop = FALSE
  ]
  vintages <- sort(unique(c(
    as.character(candidates$budget_vintage %||% character()),
    as.character(inventory$budget_vintage %||% character())
  )))
  summary <- .rf_bind_rows(lapply(vintages, function(vintage) {
    discovered <- sum(candidates$budget_vintage == vintage)
    admitted <- sum(inventory$budget_vintage == vintage)
    unresolved <- sum(missing$budget_vintage == vintage)
    documented_gaps <- sum(
      gaps$budget_vintage == vintage &
        nzchar(gaps$source_url)
    )
    data.frame(
      budget_vintage = vintage,
      indexed_count = discovered,
      admitted_count = admitted,
      gap_count = documented_gaps,
      unresolved_count = unresolved,
      complete = unresolved == 0L,
      stringsAsFactors = FALSE
    )
  }))
  list(
    complete = nrow(missing) == 0L,
    missing = missing,
    unexpected = unexpected,
    gaps = gaps,
    summary = summary,
    indexes = indexes,
    inventory = inventory
  )
}
