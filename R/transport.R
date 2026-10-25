.rf_sha256 <- function(raw) {
  digest::digest(raw, algo = "sha256", serialize = FALSE)
}

.rf_validate_payload <- function(raw, expected_media_type) {
  if (!is.raw(raw)) {
    .rf_abort("Source payload was not raw bytes.", "transport")
  }
  media <- tolower(expected_media_type %||% "")
  zip_like <- grepl("zip|spreadsheet|excel|wordprocessingml|openxml", media)
  magic <- if (length(raw) >= 4L) raw[seq_len(4L)] else raw
  zip_magic <- as.raw(c(0x50, 0x4b, 0x03, 0x04))
  if (zip_like && !identical(magic, zip_magic)) {
    .rf_abort("Source bytes do not match the declared ZIP/XLSX media type.",
              "unsupported_media")
  }
  if (grepl("csv", media)) {
    prefix <- tolower(trimws(rawToChar(
      raw[seq_len(min(length(raw), 512L))], multiple = FALSE
    )))
    if (grepl("^<(?:!doctype|html|head|body)", prefix, perl = TRUE)) {
      .rf_abort("CSV resource returned an HTML document.",
                "unsupported_media")
    }
  }
  invisible(raw)
}

.rf_cache_metadata_path <- function(path) {
  paste0(path, ".metadata.json")
}

.rf_is_safe_https_url <- function(url) {
  # Conservative fast path for the ordinary host/path form used by the
  # bundled workflows. It deliberately excludes ports, userinfo, Unicode,
  # and other edge spellings; those continue through httr2's parser below.
  # The URI-tail character class is intentionally narrower than RFC 3986 so
  # this branch can only accept strings whose syntax is unambiguous here.
  simple <- is.character(url) && length(url) == 1L && !is.na(url) &&
    grepl(
      paste0(
        "^https://[A-Za-z0-9](?:[A-Za-z0-9.-]*[A-Za-z0-9])?",
        "(?:[/?#](?:[A-Za-z0-9._~:/?#\\[\\]@!$&'()*+,;=-]|",
        "%[A-Fa-f0-9]{2})*)?$"
      ),
      url,
      perl = TRUE
    )
  if (isTRUE(simple)) return(TRUE)
  parsed <- tryCatch(
    httr2::url_parse(url),
    error = function(error) NULL
  )
  has_authority <- is.character(url) && length(url) == 1L &&
    !is.na(url) &&
    grepl("^https://[^/?#]+(?:[/?#]|$)", url, perl = TRUE)
  authority <- if (isTRUE(has_authority)) {
    sub("^https://([^/?#]+).*", "\\1", url, perl = TRUE)
  } else {
    ""
  }
  isTRUE(has_authority && nzchar(authority) &&
    !grepl("@", authority, fixed = TRUE) &&
    !is.null(parsed) &&
    identical(tolower(parsed$scheme %||% ""), "https") &&
    nzchar(parsed$hostname %||% "") &&
    !nzchar(parsed$username %||% "") &&
    !nzchar(parsed$password %||% ""))
}

.rf_validate_https_url <- function(
    url,
    label = "Source URL",
    subclass = "transport",
    message = NULL) {
  if (!.rf_is_safe_https_url(url)) {
    if (is.null(message)) {
      message <- paste0(
        label,
        " must be a valid, credential-free HTTPS URL."
      )
    }
    .rf_abort(
      message,
      subclass
    )
  }
  invisible(url)
}

.rf_write_cache_metadata <- function(path, fetched) {
  required <- c(
    "sha256", "cache_namespace_sha256", "source_url", "final_url",
    "retrieved_at", "content_type", "status", "response_bytes",
    "announced_bytes"
  )
  if (!file.exists(path) ||
      !is.list(fetched) ||
      length(setdiff(required, names(fetched))) ||
      !is.numeric(fetched$status) ||
      length(fetched$status) != 1L ||
      is.na(fetched$status)) {
    return(invisible(FALSE))
  }
  announced_bytes <- suppressWarnings(
    as.numeric(fetched$announced_bytes)
  )
  if (length(announced_bytes) != 1L ||
      !is.finite(announced_bytes)) {
    announced_bytes <- -1
  }
  metadata <- list(
    format_version = 1L,
    sha256 = as.character(fetched$sha256),
    cache_namespace_sha256 =
      as.character(fetched$cache_namespace_sha256),
    source_url = as.character(fetched$source_url),
    final_url = as.character(fetched$final_url),
    retrieved_at = as.character(fetched$retrieved_at),
    content_type = as.character(fetched$content_type),
    status = as.integer(fetched$status),
    response_bytes = as.numeric(fetched$response_bytes),
    announced_bytes = announced_bytes
  )
  metadata_path <- .rf_cache_metadata_path(path)
  tmp <- tempfile(
    "readfiscalau-metadata-",
    tmpdir = dirname(metadata_path)
  )
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  json <- jsonlite::toJSON(
    metadata,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  writeLines(
    json,
    con = tmp,
    useBytes = TRUE
  )
  if (!file.rename(tmp, metadata_path)) {
    # Keep the last complete metadata record rather than creating a window in
    # which concurrent readers see verified bytes without metadata.
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

.rf_read_cached_fetch <- function(
    url,
    cache_dir,
    max_bytes,
    expected_media_type,
    expected_sha256,
    cache_namespace,
    materialize_raw = TRUE) {
  if (is.null(expected_sha256) ||
      is.null(cache_namespace) ||
      !dir.exists(cache_dir)) {
    return(NULL)
  }
  namespace_sha256 <- digest::digest(
    cache_namespace,
    algo = "sha256",
    serialize = FALSE
  )
  target_dir <- file.path(
    cache_dir,
    "releases",
    namespace_sha256
  )
  if (!dir.exists(target_dir)) {
    return(NULL)
  }
  candidates <- list.files(
    target_dir,
    pattern = paste0(
      "^",
      expected_sha256,
      "\\.[a-z0-9]+$"
    ),
    full.names = TRUE
  )
  if (length(candidates) != 1L) {
    return(NULL)
  }
  path <- candidates[[1L]]
  metadata_path <- .rf_cache_metadata_path(path)
  if (!file.exists(metadata_path)) {
    return(NULL)
  }
  size <- suppressWarnings(as.numeric(file.info(path)$size))
  if (length(size) != 1L ||
      !is.finite(size) ||
      size < 0 ||
      size > max_bytes) {
    return(NULL)
  }
  actual_sha256 <- tryCatch(
    digest::digest(
      file = path,
      algo = "sha256",
      serialize = FALSE
    ),
    error = function(error) ""
  )
  if (!identical(actual_sha256, expected_sha256)) {
    return(NULL)
  }
  metadata <- tryCatch(
    jsonlite::fromJSON(
      metadata_path,
      simplifyVector = TRUE
    ),
    error = function(error) NULL
  )
  required <- c(
    "format_version", "sha256", "cache_namespace_sha256",
    "source_url", "final_url", "retrieved_at", "content_type",
    "status", "response_bytes", "announced_bytes"
  )
  scalar_character <- function(value, nonempty = FALSE) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      (!nonempty || nzchar(value))
  }
  scalar_numeric <- function(value) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value)
  }
  valid_metadata <- is.list(metadata) &&
    !length(setdiff(required, names(metadata))) &&
    identical(as.integer(metadata$format_version), 1L) &&
    scalar_character(metadata$sha256, nonempty = TRUE) &&
    identical(metadata$sha256, expected_sha256) &&
    scalar_character(
      metadata$cache_namespace_sha256,
      nonempty = TRUE
    ) &&
    identical(
      metadata$cache_namespace_sha256,
      namespace_sha256
    ) &&
    scalar_character(metadata$source_url, nonempty = TRUE) &&
    identical(metadata$source_url, url) &&
    scalar_character(metadata$final_url, nonempty = TRUE) &&
    .rf_is_safe_https_url(metadata$final_url) &&
    scalar_character(metadata$retrieved_at, nonempty = TRUE) &&
    scalar_character(metadata$content_type) &&
    scalar_numeric(metadata$status) &&
    metadata$status >= 200L &&
    metadata$status < 300L &&
    scalar_numeric(metadata$response_bytes) &&
    identical(as.numeric(metadata$response_bytes), size)
  if (!isTRUE(valid_metadata)) {
    return(NULL)
  }
  bytes_to_read <- if (isTRUE(materialize_raw)) {
    size
  } else {
    min(size, 512)
  }
  connection <- file(path, open = "rb")
  body <- tryCatch(
    readBin(connection, what = "raw", n = bytes_to_read),
    finally = close(connection)
  )
  if (length(body) != bytes_to_read) {
    return(NULL)
  }
  if (!is.null(expected_media_type)) {
    .rf_validate_payload(body, expected_media_type)
  }
  Sys.setFileTime(
    c(path, metadata_path),
    Sys.time()
  )
  announced_bytes <- suppressWarnings(
    as.numeric(metadata$announced_bytes)
  )
  if (length(announced_bytes) != 1L ||
      !is.finite(announced_bytes) ||
      announced_bytes < 0) {
    announced_bytes <- NA_real_
  }
  list(
    path = normalizePath(path, mustWork = TRUE),
    sha256 = expected_sha256,
    cache_namespace_sha256 = namespace_sha256,
    raw = if (isTRUE(materialize_raw)) body else raw(),
    source_url = url,
    final_url = as.character(metadata$final_url),
    retrieved_at = as.character(metadata$retrieved_at),
    content_type = as.character(metadata$content_type),
    status = as.integer(metadata$status),
    response_bytes = size,
    announced_bytes = announced_bytes,
    cache_hit = TRUE
  )
}

#' Return the default readfiscalau cache directory
#'
#' The path is obtained from [tools::R_user_dir()] with `which = "cache"`.
#' It is a recommendation for the bounded source cache, not a promise that the
#' directory already exists.  Readers create only the selected cache path and
#' its release namespace as needed; this accessor itself has no filesystem
#' side effects and does not inspect the network.
#'
#' @returns A length-one character path under the platform's R user-cache
#'   location for package `readfiscalau`.
#' @seealso [readfiscalau_cache_prune()], [read_au_fiscal_release()]
#' @family cache
#' @examples
#' cache <- readfiscalau_default_cache()
#' stopifnot(is.character(cache), length(cache) == 1L, nzchar(cache))
#' @export
readfiscalau_default_cache <- function() {
  tools::R_user_dir("readfiscalau", which = "cache")
}

.rf_write_atomic <- function(
    raw,
    cache_dir,
    extension = "bin",
    cache_namespace = NULL) {
  namespace_sha256 <- ""
  target_dir <- cache_dir
  if (!is.null(cache_namespace)) {
    .rf_assert_scalar_string(cache_namespace, "cache_namespace")
    namespace_sha256 <- digest::digest(
      cache_namespace,
      algo = "sha256",
      serialize = FALSE
    )
    target_dir <- file.path(cache_dir, "releases", namespace_sha256)
  }
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  hash <- .rf_sha256(raw)
  extension <- gsub("[^a-z0-9]", "", tolower(extension))
  if (!nzchar(extension)) extension <- "bin"
  target <- file.path(target_dir, paste0(hash, ".", extension))
  if (file.exists(target)) {
    existing_hash <- tryCatch(
      digest::digest(file = target, algo = "sha256", serialize = FALSE),
      error = function(e) NA_character_
    )
    if (identical(existing_hash, hash)) {
      return(list(
        path = normalizePath(target, mustWork = TRUE),
        sha256 = hash,
        cache_namespace_sha256 = namespace_sha256
      ))
    }
  }

  tmp <- tempfile("readfiscalau-", tmpdir = target_dir)
  stale <- tempfile("readfiscalau-corrupt-", tmpdir = target_dir)
  on.exit({
    if (file.exists(tmp)) unlink(tmp)
    if (file.exists(stale)) unlink(stale)
  }, add = TRUE)
  con <- file(tmp, open = "wb")
  tryCatch(writeBin(raw, con), finally = close(con))
  tmp_hash <- digest::digest(file = tmp, algo = "sha256", serialize = FALSE)
  if (!identical(tmp_hash, hash)) {
    .rf_abort("Temporary cache file failed SHA-256 verification.",
              "cache_corrupt", path = tmp)
  }

  if (file.exists(target)) {
    if (!file.rename(target, stale)) {
      competing_hash <- tryCatch(
        digest::digest(file = target, algo = "sha256", serialize = FALSE),
        error = function(e) NA_character_
      )
      if (identical(competing_hash, hash)) {
        return(list(
          path = normalizePath(target, mustWork = TRUE),
          sha256 = hash,
          cache_namespace_sha256 = namespace_sha256
        ))
      }
      .rf_abort(
        "A corrupt hash-addressed cache target could not be quarantined.",
        "cache_corrupt", path = target
      )
    }
  }

  if (!file.rename(tmp, target)) {
    competing_hash <- tryCatch(
      digest::digest(file = target, algo = "sha256", serialize = FALSE),
      error = function(e) NA_character_
    )
    if (!identical(competing_hash, hash)) {
      .rf_abort("Could not atomically publish the cached source file.",
                "cache_write", path = target)
    }
  }
  published_hash <- digest::digest(
    file = target, algo = "sha256", serialize = FALSE
  )
  if (!identical(published_hash, hash)) {
    .rf_abort("Published cache target failed SHA-256 verification.",
              "cache_corrupt", path = target)
  }
  if (file.exists(stale)) unlink(stale)
  list(
    path = normalizePath(target, mustWork = TRUE),
    sha256 = hash,
    cache_namespace_sha256 = namespace_sha256
  )
}

.rf_extension <- function(url, content_type = "") {
  path <- sub("[?#].*$", "", url)
  ext <- tools::file_ext(path)
  if (nzchar(ext)) return(ext)
  if (grepl("spreadsheet|excel", content_type, ignore.case = TRUE)) return("xlsx")
  if (grepl("csv", content_type, ignore.case = TRUE)) return("csv")
  if (grepl("html", content_type, ignore.case = TRUE)) return("html")
  if (grepl("json", content_type, ignore.case = TRUE)) return("json")
  if (grepl("zip", content_type, ignore.case = TRUE)) return("zip")
  "bin"
}

.rf_fetch <- function(
    url,
    cache_dir = readfiscalau_default_cache(),
    max_bytes = 50 * 1024^2,
    timeout_seconds = 30,
    max_attempts = 3L,
    cache_max_bytes = 500 * 1024^2,
    cache_max_files = 128L,
    cache_max_age_seconds = 90 * 24 * 60 * 60,
    expected_media_type = NULL,
    expected_sha256 = NULL,
    cache_namespace = NULL,
    materialize_raw = TRUE,
    fetcher = NULL) {
  .rf_assert_scalar_string(url, "url")
  .rf_validate_https_url(
    url,
    message = "Only HTTPS source URLs are accepted."
  )
  if (!is.numeric(max_bytes) || length(max_bytes) != 1L ||
      is.na(max_bytes) || !is.finite(max_bytes) || max_bytes <= 0) {
    .rf_abort("`max_bytes` must be positive and finite.", "invalid_argument")
  }
  if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1L ||
      is.na(timeout_seconds) || !is.finite(timeout_seconds) ||
      timeout_seconds <= 0) {
    .rf_abort("`timeout_seconds` must be positive and finite.",
              "invalid_argument")
  }
  if (!is.numeric(max_attempts) || length(max_attempts) != 1L ||
      is.na(max_attempts) || max_attempts < 1 ||
      max_attempts != as.integer(max_attempts)) {
    .rf_abort("`max_attempts` must be a positive integer.",
              "invalid_argument")
  }
  if (!is.numeric(cache_max_files) || length(cache_max_files) != 1L ||
      is.na(cache_max_files) || cache_max_files < 1 ||
      cache_max_files != as.integer(cache_max_files)) {
    .rf_abort("`cache_max_files` must be a positive integer.",
              "invalid_argument")
  }
  if (!is.numeric(cache_max_bytes) || length(cache_max_bytes) != 1L ||
      is.na(cache_max_bytes) || cache_max_bytes < 0) {
    .rf_abort("`cache_max_bytes` must be non-negative.",
              "invalid_argument")
  }
  if (!is.numeric(cache_max_age_seconds) ||
      length(cache_max_age_seconds) != 1L ||
      is.na(cache_max_age_seconds) || cache_max_age_seconds < 0) {
    .rf_abort("`cache_max_age_seconds` must be non-negative.",
              "invalid_argument")
  }
  if (!is.null(expected_media_type)) {
    .rf_assert_scalar_string(expected_media_type, "expected_media_type")
  }
  if (!is.null(expected_sha256) &&
      (!is.character(expected_sha256) || length(expected_sha256) != 1L ||
       is.na(expected_sha256) ||
       !grepl("^[a-f0-9]{64}$", expected_sha256))) {
    .rf_abort("`expected_sha256` must be a lowercase SHA-256 hash.",
              "invalid_argument")
  }
  if (!is.logical(materialize_raw) ||
      length(materialize_raw) != 1L ||
      is.na(materialize_raw)) {
    .rf_abort("`materialize_raw` must be TRUE or FALSE.", "invalid_argument")
  }

  cached_fetch <- tryCatch(
    .rf_read_cached_fetch(
      url = url,
      cache_dir = cache_dir,
      max_bytes = max_bytes,
      expected_media_type = expected_media_type,
      expected_sha256 = expected_sha256,
      cache_namespace = cache_namespace,
      materialize_raw = materialize_raw
    ),
    error = function(error) NULL
  )
  if (!is.null(cached_fetch)) {
    return(cached_fetch)
  }

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
      request, max_tries = as.integer(max_attempts),
      retry_on_failure = TRUE
    )
    request <- httr2::req_error(request, is_error = function(response) FALSE)
    download_path <- tempfile("readfiscalau-download-")
    on.exit(if (file.exists(download_path)) unlink(download_path), add = TRUE)
    response <- tryCatch(
      httr2::req_perform(request, path = download_path),
      error = function(e) .rf_abort(
        paste("Transport request failed:", conditionMessage(e)),
        "transport", url = url
      )
    )
    status <- httr2::resp_status(response)
    downloaded_bytes <- file.info(download_path)$size
    if (is.na(downloaded_bytes) || downloaded_bytes > max_bytes) {
      .rf_abort("Source exceeded the configured byte limit.", "too_large",
                size = downloaded_bytes, max_bytes = max_bytes)
    }
    con <- file(download_path, open = "rb")
    body <- tryCatch(
      readBin(con, what = "raw", n = downloaded_bytes),
      finally = close(con)
    )
    content_type <- httr2::resp_header(response, "content-type") %||% ""
    content_length <- httr2::resp_header(response, "content-length") %||% ""
    content_range <- httr2::resp_header(response, "content-range") %||% ""
    link <- httr2::resp_header(response, "link") %||% ""
    final_url <- httr2::resp_url(response)
    retrieved_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  } else {
    response <- fetcher(url)
    required <- c("raw", "status", "content_type", "final_url", "retrieved_at")
    if (!is.list(response) || any(!required %in% names(response))) {
      .rf_abort(
        "Injected fetchers must return raw, status, content_type, final_url, and retrieved_at.",
        "transport"
      )
    }
    body <- response$raw
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
    .rf_abort("Transport response status was not a valid HTTP status.",
              "transport")
  }
  if (!is.character(content_type) || length(content_type) != 1L ||
      is.na(content_type) ||
      !is.character(final_url) || length(final_url) != 1L ||
      is.na(final_url) || !nzchar(final_url) ||
      !is.character(retrieved_at) || length(retrieved_at) != 1L ||
      is.na(retrieved_at) || !nzchar(retrieved_at)) {
    .rf_abort("Transport response metadata was malformed.", "transport")
  }
  .rf_validate_https_url(final_url, "Final URL")
  if (!is.raw(body)) {
    .rf_abort("Transport response body was not raw bytes.", "transport")
  }
  if (status < 200L || status >= 300L) {
    .rf_abort(sprintf("Source returned HTTP %s.", status), "http",
              status = status, url = url)
  }
  has_next_link <- is.character(link) && length(link) == 1L &&
    !is.na(link) && nzchar(link) &&
    grepl("rel[[:space:]]*=[[:space:]]*[\"']?next[\"']?",
          link, ignore.case = TRUE)
  if (
    identical(as.integer(status), 206L) ||
      (is.character(content_range) && length(content_range) == 1L &&
        !is.na(content_range) && nzchar(trimws(content_range))) ||
      has_next_link
  ) {
    .rf_abort(
      "Source returned a partial or paginated response.",
      "truncated",
      status = status,
      url = url
    )
  }
  if (length(body) > max_bytes) {
    .rf_abort("Source exceeded the configured byte limit.", "too_large",
              size = length(body), max_bytes = max_bytes)
  }
  announced_length <- suppressWarnings(as.numeric(content_length))
  if (
    length(announced_length) == 1L &&
      is.finite(announced_length) &&
      announced_length >= 0 &&
      announced_length != length(body)
  ) {
    .rf_abort(
      "Source body length disagrees with the HTTP Content-Length.",
      "truncated",
      announced_bytes = announced_length,
      actual_bytes = length(body),
      url = url
    )
  }
  if (!is.null(expected_media_type)) {
    .rf_validate_payload(body, expected_media_type)
  }
  body_hash <- .rf_sha256(body)
  if (!is.null(expected_sha256) && !identical(body_hash, expected_sha256)) {
    .rf_abort(
      "Source bytes do not match the pinned release SHA-256.",
      "source_hash", expected_sha256 = expected_sha256,
      actual_sha256 = body_hash
    )
  }
  cached <- .rf_write_atomic(
    body,
    cache_dir,
    .rf_extension(final_url, content_type),
    cache_namespace = cache_namespace
  )
  readfiscalau_cache_prune(
    cache_dir, max_bytes = cache_max_bytes, max_files = cache_max_files,
    max_age_seconds = cache_max_age_seconds, preserve = cached$path
  )
  response_bytes <- length(body)
  fetched <- c(cached, list(
    raw = if (isTRUE(materialize_raw)) body else raw(),
    source_url = url, final_url = final_url,
    retrieved_at = as.character(retrieved_at), content_type = content_type,
    status = as.integer(status), response_bytes = response_bytes,
    announced_bytes = if (is.finite(announced_length)) {
      announced_length
    } else {
      NA_real_
    },
    cache_hit = FALSE
  ))
  .rf_write_cache_metadata(
    fetched$path,
    fetched
  )
  fetched
}

#' Prune the bounded source cache
#'
#' Removes the oldest hash-addressed source files until the cache is within
#' `max_bytes`, `max_files` and `max_age_seconds`.  Only files whose basename
#' matches the package's 64-hex digest convention are candidates; metadata
#' sidecars are removed with their source.  The file named by `preserve` is
#' never removed.  This is the package's only routine that intentionally
#' deletes files, and it operates only below the explicit `cache_dir`.
#'
#' @param cache_dir Cache directory to inspect recursively.  It is not created
#'   when absent.
#' @param max_bytes Maximum aggregate bytes for hash-addressed source files;
#'   non-negative numeric, default 500 MiB.
#' @param max_files Maximum number of hash-addressed source files; non-negative
#'   integer, default 128.
#' @param max_age_seconds Maximum source-file age in seconds; non-negative
#'   numeric, default 90 days.  `Inf` disables age expiry.
#' @param preserve Optional path to retain even when it exceeds a bound.
#' @returns Invisibly, the numeric remaining byte count after pruning.
#' @seealso [readfiscalau_default_cache()], [read_au_fiscal_release()]
#' @family cache
#' @examples
#' cache <- tempfile("readfiscalau-prune-")
#' dir.create(cache)
#' raw <- charToRaw("small deterministic cache fixture")
#' path <- file.path(
#'   cache,
#'   paste0(digest::digest(raw, algo = "sha256", serialize = FALSE), ".csv")
#' )
#' writeBin(raw, path)
#' readfiscalau_cache_prune(
#'   cache_dir = cache, max_bytes = 0, max_files = 0,
#'   max_age_seconds = Inf, preserve = path
#' )
#' stopifnot(file.exists(path))
#' readfiscalau_cache_prune(
#'   cache_dir = cache, max_bytes = 0, max_files = 0,
#'   max_age_seconds = Inf
#' )
#' stopifnot(!file.exists(path))
#' @export
readfiscalau_cache_prune <- function(
    cache_dir = readfiscalau_default_cache(),
    max_bytes = 500 * 1024^2,
    max_files = 128L,
    max_age_seconds = 90 * 24 * 60 * 60,
    preserve = character()) {
  if (!is.numeric(max_bytes) || length(max_bytes) != 1L ||
      is.na(max_bytes) || max_bytes < 0) {
    .rf_abort("`max_bytes` must be non-negative.", "invalid_argument")
  }
  if (!is.numeric(max_files) || length(max_files) != 1L ||
      is.na(max_files) || max_files < 0 ||
      max_files != as.integer(max_files)) {
    .rf_abort("`max_files` must be a non-negative integer.",
              "invalid_argument")
  }
  if (!is.numeric(max_age_seconds) || length(max_age_seconds) != 1L ||
      is.na(max_age_seconds) || max_age_seconds < 0) {
    .rf_abort("`max_age_seconds` must be non-negative.",
              "invalid_argument")
  }
  if (!dir.exists(cache_dir)) return(invisible(0))
  files <- list.files(
    cache_dir, pattern = "^[a-f0-9]{64}\\.[a-z0-9]+$",
    full.names = TRUE, recursive = TRUE
  )
  if (!length(files)) return(invisible(0))
  info <- file.info(files)
  total <- sum(info$size)
  preserve <- normalizePath(preserve, mustWork = FALSE)
  old <- is.finite(max_age_seconds) &
    as.numeric(difftime(Sys.time(), info$mtime, units = "secs")) >
      max_age_seconds
  for (path in files[old]) {
    if (normalizePath(path, mustWork = FALSE) %in% preserve) next
    size <- file.info(path)$size
    if (unlink(path) == 0L) {
      unlink(.rf_cache_metadata_path(path))
      total <- total - size
    }
  }
  files <- files[file.exists(files)]
  if (!length(files)) return(invisible(0))
  info <- file.info(files)
  total <- sum(info$size)
  removable <- files[order(info$mtime)]
  remaining_files <- length(files)
  for (path in removable) {
    if (total <= max_bytes && remaining_files <= max_files) break
    if (normalizePath(path, mustWork = FALSE) %in% preserve) next
    size <- file.info(path)$size
    if (unlink(path) == 0L) {
      unlink(.rf_cache_metadata_path(path))
      total <- total - size
      remaining_files <- remaining_files - 1L
    }
  }
  invisible(total)
}
