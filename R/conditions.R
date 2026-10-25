.rf_abort <- function(message, subclass, ...) {
  cond <- structure(
    c(list(message = message, call = NULL), list(...)),
    class = c(paste0("readfiscalau_", subclass), "readfiscalau_error",
              "error", "condition")
  )
  stop(cond)
}

.rf_assert_scalar_string <- function(x, name, allow_na = FALSE) {
  ok <- is.character(x) && length(x) == 1L &&
    (allow_na || (!is.na(x) && nzchar(x)))
  if (!ok) {
    .rf_abort(sprintf("`%s` must be one non-empty string.", name),
              "invalid_argument", argument = name)
  }
  invisible(x)
}

.rf_normalize_fiscal_year <- function(x) {
  x <- gsub("\u2013", "-", trimws(as.character(x)), fixed = TRUE)
  x <- sub("^([0-9]{4})-([0-9]{4})$", "\\1-\\2", x)
  long <- grepl("^[0-9]{4}-[0-9]{4}$", x)
  x[long] <- paste0(substr(x[long], 1L, 4L), "-", substr(x[long], 8L, 9L))
  if (any(!grepl("^[0-9]{4}-[0-9]{2}$", x))) {
    .rf_abort("Fiscal years must use YYYY-YY or YYYY-YYYY.",
              "schema", field = "fiscal_year")
  }
  x
}

.rf_period_month <- function(x) {
  x <- as.Date(x)
  if (is.na(x)) return(NA_character_)
  c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )[as.integer(format(x, "%m"))]
}

.rf_slug <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

.rf_parse_numeric <- function(x, context) {
  text <- trimws(as.character(x))
  missing <- is.na(x) | !nzchar(text)
  parenthesised <- grepl("^\\(.*\\)$", text)
  core <- text
  core[parenthesised] <- sub("^\\((.*)\\)$", "\\1", core[parenthesised])
  valid <- missing | grepl(
    "^-?(?:[0-9]+(?:\\.[0-9]+)?|[0-9]{1,3}(?:,[0-9]{3})+(?:\\.[0-9]+)?)$",
    core, perl = TRUE
  )
  valid[parenthesised & grepl("^-", core)] <- FALSE
  if (any(!valid)) {
    .rf_abort(sprintf("%s contains a non-numeric observation.", context),
              "schema", values = unique(text[!valid]))
  }
  value <- suppressWarnings(as.numeric(gsub(",", "", core, fixed = TRUE)))
  value[parenthesised & !missing] <- -value[parenthesised & !missing]
  value[missing] <- NA_real_
  value
}

.rf_bind_rows <- function(xs) {
  xs <- Filter(Negate(is.null), xs)
  if (!length(xs)) return(tibble::tibble())
  fields <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    missing <- setdiff(fields, names(x))
    for (field in missing) x[[field]] <- NA
    x[fields]
  })
  tibble::as_tibble(do.call(rbind, lapply(xs, as.data.frame,
    stringsAsFactors = FALSE)))
}

.rf_assert_unique_observations <- function(x) {
  key_fields <- c(
    "table_id", "series_id", "period_end", "actual_forecast", "profile_type"
  )
  missing <- setdiff(key_fields, names(x))
  if (length(missing)) {
    .rf_abort("Parsed observations lack their natural-key fields.", "schema",
              fields = missing)
  }
  key <- do.call(
    paste,
    c(lapply(x[key_fields], as.character), list(sep = "\r"))
  )
  if (anyDuplicated(key)) {
    .rf_abort(
      "Parsed source contains duplicate observation keys.",
      "duplicate_observation",
      rows = which(duplicated(key) | duplicated(key, fromLast = TRUE))
    )
  }
  x
}
