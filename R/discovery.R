.rf_absolute_url <- function(href, base_url) {
  .rf_assert_scalar_string(href, "href")
  .rf_assert_scalar_string(base_url, "base_url")
  absolute <- tryCatch(
    xml2::url_absolute(href, base_url),
    error = function(e) .rf_abort(
      sprintf("Could not resolve discovered URL: %s", conditionMessage(e)),
      "schema", href = href
    )
  )
  .rf_validate_https_url(
    absolute,
    label = "Discovered resource URLs",
    subclass = "schema"
  )
  unname(absolute)
}

.rf_type_from_text <- function(x) {
  x <- tolower(x)
  if (grepl("mid.?year|myefo", x)) return("myefo")
  if (grepl("final budget|\\bfbo\\b", x)) return("fbo")
  if (grepl("consolidated financial|\\bcfs\\b", x)) return("cfs")
  if (grepl("monthly financial", x)) return("monthly_financial_statement")
  if (grepl("budget", x)) return("budget")
  NA_character_
}

.rf_fy_from_text <- function(x) {
  hit <- regmatches(x, regexpr("20[0-9]{2}[-\u2013/](20)?[0-9]{2}", x))
  if (!length(hit) || !nzchar(hit)) return(NA_character_)
  hit <- gsub("[\u2013/]", "-", hit)
  .rf_normalize_fiscal_year(hit)
}

.rf_html_candidates <- function(raw, base_url, source_id) {
  doc <- xml2::read_html(rawToChar(raw))
  anchors <- xml2::xml_find_all(doc, ".//a[@href]")
  text <- trimws(xml2::xml_text(anchors))
  href <- xml2::xml_attr(anchors, "href")
  combined <- paste(text, href)
  type <- vapply(combined, .rf_type_from_text, character(1))
  keep <- !is.na(type)
  if (!any(keep)) {
    .rf_abort("No supported fiscal release links were found.", "schema")
  }
  text <- text[keep]
  href <- href[keep]
  type <- type[keep]
  fiscal_year <- vapply(paste(text, href), .rf_fy_from_text, character(1))
  tibble::tibble(
    source_id = source_id,
    release_type = type,
    title = text,
    fiscal_year = fiscal_year,
    resource_url = unname(vapply(
      href, .rf_absolute_url, character(1), base_url = base_url
    )),
    discovery_status = ifelse(is.na(fiscal_year), "needs_metadata",
                              "candidate"),
    discovered_from = base_url
  )
}

#' Discover release candidates from official HTML
#'
#' Discovery is deliberately separate from the curated release manifest.  It
#' classifies links found on an official Treasury or Department of Finance
#' landing page but does not infer enough metadata to make a candidate
#' parseable.  Candidates without a fiscal year are marked `needs_metadata`.
#' Finance links whose title says June and whose inferred type is a monthly
#' financial statement are rejected: June is an FBO/CFS annual boundary.
#'
#' Supply `raw` for deterministic offline replay.  If `raw` is `NULL`, the
#' landing page is fetched through the package's HTTPS-only, bounded transport
#' and cache.  The function writes only cache files in `cache_dir`; it never
#' writes a manifest, downloads a candidate resource, or silently follows
#' credentials.  Malformed links, non-HTTPS URLs, absent supported links and
#' invalid fixture bytes raise typed `readfiscalau_schema` or
#' `readfiscalau_invalid_argument` conditions.
#'
#' @param source Which official landing-page family to inspect: `"treasury"`
#'   or `"finance"`.
#' @param url Official HTTPS landing page.  `NULL` selects the package default
#'   for `source` (`budget.gov.au/content/downloads.htm` or the Finance
#'   monthly-statements page).
#' @param raw Optional raw HTML bytes for offline replay.  It must be a raw
#'   vector; when supplied no network request is made.
#' @param cache_dir Cache directory used by the bounded transport when `raw` is
#'   `NULL`; defaults to [readfiscalau_default_cache()].
#' @param fetcher Optional injected fetcher for offline transport replay.  It
#'   must return `raw`, `status`, `content_type`, `final_url` and
#'   `retrieved_at`.
#' @returns A tibble in discovered anchor order with character columns
#'   `source_id`, `release_type`, `title`, `fiscal_year`, `resource_url`,
#'   `discovery_status` and `discovered_from`.  `release_type` is one of
#'   `budget`, `myefo`, `fbo`, `cfs` or `monthly_financial_statement`; rows
#'   without an inferable fiscal year have `NA` in `fiscal_year` and
#'   `discovery_status = "needs_metadata"`.
#' @seealso [list_au_fiscal_releases()],
#'   [discover_finance_ckan_resources()]
#' @family discovery
#' @examples
#' html_path <- system.file("extdata", "examples", "treasury-downloads.html",
#'                          package = "readfiscalau")
#' html <- readBin(html_path, what = "raw", n = file.info(html_path)$size)
#' candidates <- discover_au_fiscal_releases(
#'   source = "treasury", url = "https://budget.gov.au/content/downloads.htm",
#'   raw = html
#' )
#' stopifnot(
#'   inherits(candidates, "tbl_df"),
#'   all(grepl("^https://", candidates$resource_url)),
#'   any(candidates$discovery_status == "candidate")
#' )
#' @export
discover_au_fiscal_releases <- function(
    source = c("treasury", "finance"),
    url = NULL,
    raw = NULL,
    cache_dir = readfiscalau_default_cache(),
    fetcher = NULL) {
  source <- match.arg(source)
  defaults <- c(
    treasury = "https://budget.gov.au/content/downloads.htm",
    finance = paste0("https://www.finance.gov.au/publications/",
                     "commonwealth-monthly-financial-statements")
  )
  url <- url %||% unname(defaults[source])
  if (is.null(raw)) raw <- .rf_fetch(
    url, cache_dir = cache_dir, fetcher = fetcher
  )$raw
  if (!is.raw(raw)) .rf_abort("`raw` must contain raw HTML bytes.",
                              "invalid_argument")
  source_id <- if (source == "treasury") "australian_treasury" else
    "department_finance"
  candidates <- .rf_html_candidates(raw, url, source_id)
  if (source == "finance") {
    monthly <- candidates$release_type == "monthly_financial_statement"
    june <- monthly & grepl("\\bJune\\b", candidates$title, ignore.case = TRUE)
    if (any(june)) {
      .rf_abort(
        "Finance discovery found June labelled as monthly; classify it as FBO or CFS.",
        "june_is_annual", titles = candidates$title[june]
      )
    }
  }
  candidates
}

#' Discover Department of Finance CKAN resources
#'
#' Parses a CKAN `package_show` response without dropping historical resource
#' vintages.  This is a metadata-only operation: it does not fetch any
#' workbook.  The response must be successful, contain at least one resource,
#' and expose complete scalar IDs, names, HTTPS URLs and timestamps.
#'
#' @param raw Raw CKAN JSON bytes from a `package_show` response.  A raw vector
#' is required so the operation can be replayed without network access.
#' @param dataset_url Official dataset landing URL recorded unchanged in every
#'   returned row; it defaults to the Department of Finance dataset used by the
#'   built-in discovery workflow.
#' @returns A data-frame/tibble with one row per resource, preserving response
#'   order and the character columns `resource_id`, `name`, `format` (upper
#'   case), `resource_url`, `created`, `last_modified`, `dataset_url` and
#'   `license`.  Resource IDs are unique; missing fields, duplicate IDs,
#'   unsuccessful CKAN responses and non-HTTPS URLs raise
#'   `readfiscalau_schema`.
#' @seealso [discover_au_fiscal_releases()], [list_au_fiscal_releases()]
#' @family discovery
#' @examples
#' json_path <- system.file("extdata", "examples", "finance-ckan.json",
#'                          package = "readfiscalau")
#' payload <- readBin(json_path, what = "raw", n = file.info(json_path)$size)
#' resources <- discover_finance_ckan_resources(raw = payload)
#' stopifnot(
#'   inherits(resources, "tbl_df"),
#'   nrow(resources) == 2L,
#'   all(resources$format == "XLSX"),
#'   all(grepl("^https://", resources$resource_url))
#' )
#' @export
discover_finance_ckan_resources <- function(
    raw,
    dataset_url = paste0(
      "https://data.gov.au/data/dataset/",
      "2b690e28-8239-48c6-a71d-2658f37d51d7"
    )) {
  if (!is.raw(raw)) .rf_abort("`raw` must contain raw JSON bytes.",
                              "invalid_argument")
  .rf_validate_https_url(
    dataset_url,
    label = "CKAN dataset URL",
    subclass = "schema"
  )
  x <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
  if (!identical(x$success, TRUE) || !is.list(x$result$resources)) {
    .rf_abort("CKAN response is not a successful package_show result.",
              "schema")
  }
  resources <- x$result$resources
  if (!length(resources)) {
    .rf_abort("CKAN package_show result contains no resources.", "schema")
  }
  required <- c("id", "name", "url", "format", "created", "last_modified")
  rows <- lapply(resources, function(resource) {
    if (any(!required %in% names(resource))) {
      .rf_abort("A CKAN resource lacks required vintage metadata.", "schema")
    }
    scalar <- vapply(
      resource[required],
      function(value) is.character(value) && length(value) == 1L &&
        !is.na(value) && nzchar(trimws(value)),
      logical(1)
    )
    if (any(!scalar)) {
      .rf_abort("A CKAN resource has malformed vintage metadata.", "schema",
                fields = required[!scalar])
    }
    .rf_validate_https_url(
      resource$url,
      label = "CKAN resource URL",
      subclass = "schema"
    )
    data.frame(
      resource_id = resource$id,
      name = resource$name,
      format = toupper(resource$format),
      resource_url = resource$url,
      created = resource$created,
      last_modified = resource$last_modified,
      dataset_url = dataset_url,
      license = x$result$license_title %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  out <- .rf_bind_rows(rows)
  if (anyDuplicated(out$resource_id)) {
    .rf_abort("CKAN response contains duplicate resource IDs.", "schema")
  }
  out
}
