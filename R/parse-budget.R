.rf_release_row <- function(release, expected_type = NULL) {
  if (is.character(release) && length(release) == 1L) {
    manifest <- list_au_fiscal_releases()
    release <- manifest[manifest$release_id == release, , drop = FALSE]
  }
  release <- .rf_validate_manifest(release)
  if (nrow(release) != 1L) {
    .rf_abort("Exactly one release must be selected.", "invalid_argument")
  }
  if (!is.null(expected_type) && !release$release_type %in% expected_type) {
    .rf_abort(
      sprintf("Release type `%s` is not valid for this reader.",
              release$release_type),
      "release_type"
    )
  }
  release
}

.rf_chart_header <- function(x) {
  x <- trimws(x)
  match <- regexec(
    "^(20[0-9]{2}-[0-9]{2})(?: \\((est|estimate|projected)\\))? \\(\\$(m|b|million|billion)\\)$",
    x, ignore.case = TRUE
  )
  parts <- regmatches(x, match)[[1]]
  if (!length(parts)) {
    .rf_abort(sprintf("Unrecognised chart-data column `%s`.", x), "schema")
  }
  list(
    fiscal_year = .rf_normalize_fiscal_year(parts[2]),
    actual_forecast = if (nzchar(parts[3])) "forecast" else "actual",
    unit = if (tolower(parts[4]) %in% c("b", "billion")) "AUD billion" else
      "AUD million",
    unit_multiplier = if (tolower(parts[4]) %in% c("b", "billion")) 1e9 else 1e6
  )
}

.rf_chart_period <- function(x) {
  x <- trimws(x)
  match <- regexec(
    "^((?:19|20)[0-9]{2}-[0-9]{2})(?: \\((est|estimate|projected)\\))?$",
    x, ignore.case = TRUE
  )
  parts <- regmatches(x, match)[[1]]
  if (!length(parts)) {
    .rf_abort(sprintf("Unrecognised chart-data period `%s`.", x), "schema")
  }
  list(
    fiscal_year = .rf_normalize_fiscal_year(parts[2]),
    actual_forecast = if (nzchar(parts[3])) "forecast" else "actual"
  )
}

.rf_parse_chart_period_rows <- function(table, release, table_id) {
  periods <- lapply(table[[1L]], .rf_chart_period)
  fiscal_years <- vapply(periods, `[[`, character(1), "fiscal_year")
  statuses <- vapply(periods, `[[`, character(1), "actual_forecast")
  if (anyDuplicated(fiscal_years)) {
    .rf_abort(
      "Chart CSV repeats a fiscal period under multiple statuses.",
      "schema",
      fiscal_years = fiscal_years[duplicated(fiscal_years) |
        duplicated(fiscal_years, fromLast = TRUE)]
    )
  }
  period_ends <- as.Date(paste0(
    as.integer(substr(fiscal_years, 1L, 4L)) + 1L,
    "-06-30"
  ))
  if (is.unsorted(period_ends, strictly = TRUE)) {
    .rf_abort("Chart CSV fiscal periods are not strictly increasing.", "schema")
  }
  future_actual <- statuses == "actual" & period_ends > release$release_date
  if (any(future_actual)) {
    .rf_abort(
      "Chart CSV labels an unfinished future fiscal period as actual.",
      "schema",
      fiscal_years = fiscal_years[future_actual]
    )
  }

  labels <- trimws(names(table)[-1L])
  if (anyNA(labels) || any(!nzchar(labels))) {
    .rf_abort("Chart CSV series labels must be complete.", "schema")
  }
  percent <- grepl("\\(%\\)$", labels)
  if (!all(percent)) {
    .rf_abort(
      "Period-row chart tables must declare every series as a percentage.",
      "schema"
    )
  }
  series <- trimws(sub("\\s*\\(%\\)$", "", labels))
  series_id <- .rf_slug(series)
  if (anyDuplicated(series_id)) {
    .rf_abort(
      "Chart CSV series labels collide after normalisation.",
      "duplicate_observation"
    )
  }

  rows <- vector("list", length(series))
  for (j in seq_along(series)) {
    rows[[j]] <- tibble::tibble(
      table_id = table_id %||% tools::file_path_sans_ext(
        basename(release$resource_url)
      ),
      series_id = rep(series_id[[j]], length(periods)),
      series = rep(series[[j]], length(periods)),
      fiscal_year = fiscal_years,
      period_end = period_ends,
      value = .rf_parse_numeric(table[[j + 1L]], "Chart CSV"),
      unit = rep("% of GDP", length(periods)),
      unit_multiplier = rep(0.01, length(periods)),
      accounting_basis = rep(release$accounting_basis, length(periods)),
      consolidation = rep(release$consolidation, length(periods)),
      actual_forecast = statuses,
      profile_type = ifelse(statuses == "forecast",
                            release$release_type, "actual")
    )
  }
  .rf_assert_unique_observations(.rf_bind_rows(rows))
}

.rf_chart_accounting_basis <- function(name, fallback) {
  text <- tolower(name)
  cash <- grepl("\\(cash\\)|\\bcash\\b", text)
  accrual <- grepl("\\(accrual\\)|\\baccrual\\b", text)
  if (cash && accrual) {
    .rf_abort("Chart table name has conflicting accounting-basis labels.",
              "schema", table = name)
  }
  if (cash) return("cash")
  if (accrual) return("accrual")
  fallback
}

.rf_chart_member_has_gov_scope <- function(name) {
  key <- tolower(tools::file_path_sans_ext(basename(name)))
  grepl("australian government", key, fixed = TRUE) ||
    key %in% tolower(paste0(
      "Table ", c(2L, 4L), " - ",
      c(
        "Major categories of (cash) receipts as a proportion of gross domestic product",
        "Major categories of (accrual) revenue as a proportion of gross domestic product"
      )
    ))
}

.rf_parse_chart_csv <- function(raw, release, table_id = NULL) {
  text <- rawToChar(raw)
  table <- tryCatch(
    utils::read.csv(
      text = text, check.names = FALSE, stringsAsFactors = FALSE,
      na.strings = c("", "na", "n/a", "-", "..")
    ),
    error = function(e) .rf_abort(
      paste("Could not parse chart CSV:", conditionMessage(e)), "schema"
    )
  )
  if (ncol(table) < 2L || nrow(table) < 1L) {
    .rf_abort("Chart CSV must have a series column and at least one period.",
              "schema")
  }
  first_values <- trimws(as.character(table[[1L]]))
  period_rows <- length(first_values) > 0L && all(grepl(
    "^(?:19|20)[0-9]{2}-[0-9]{2}(?: \\((?:est|estimate|projected)\\))?$",
    first_values,
    ignore.case = TRUE
  ))
  if (period_rows) {
    return(.rf_parse_chart_period_rows(table, release, table_id))
  }
  names(table)[1] <- "series"
  if (anyNA(table$series) || any(!nzchar(trimws(table$series)))) {
    .rf_abort("Chart CSV series labels must be complete.", "schema")
  }
  series_id <- .rf_slug(table$series)
  if (anyDuplicated(series_id)) {
    .rf_abort("Chart CSV series labels collide after normalisation.",
              "duplicate_observation")
  }
  headers <- lapply(names(table)[-1], .rf_chart_header)
  fiscal_years <- vapply(headers, `[[`, character(1), "fiscal_year")
  if (anyDuplicated(fiscal_years)) {
    .rf_abort(
      "Chart CSV repeats a fiscal period under multiple statuses.",
      "schema",
      fiscal_years = fiscal_years[duplicated(fiscal_years) |
        duplicated(fiscal_years, fromLast = TRUE)]
    )
  }
  period_ends <- as.Date(paste0(
    as.integer(substr(fiscal_years, 1L, 4L)) + 1L,
    "-06-30"
  ))
  if (is.unsorted(period_ends, strictly = TRUE)) {
    .rf_abort(
      "Chart CSV fiscal periods are not strictly increasing.",
      "schema"
    )
  }
  units <- vapply(headers, `[[`, character(1), "unit")
  if (length(unique(units)) != 1L) {
    .rf_abort("Chart CSV changes unit within one table.", "schema")
  }
  statuses <- vapply(headers, `[[`, character(1), "actual_forecast")
  future_actual <- statuses == "actual" & period_ends > release$release_date
  if (any(future_actual)) {
    .rf_abort(
      "Chart CSV labels an unfinished future fiscal period as actual.",
      "schema",
      fiscal_years = fiscal_years[future_actual]
    )
  }
  rows <- vector("list", length(headers))
  for (j in seq_along(headers)) {
    value <- .rf_parse_numeric(table[[j + 1L]], "Chart CSV")
    h <- headers[[j]]
    rows[[j]] <- tibble::tibble(
      table_id = table_id %||% tools::file_path_sans_ext(
        basename(release$resource_url)
      ),
      series_id = series_id,
      series = trimws(table$series),
      fiscal_year = h$fiscal_year,
      period_end = period_ends[[j]],
      value = value,
      unit = h$unit,
      unit_multiplier = h$unit_multiplier,
      accounting_basis = release$accounting_basis,
      consolidation = release$consolidation,
      actual_forecast = h$actual_forecast,
      profile_type = ifelse(h$actual_forecast == "forecast",
                            release$release_type, "actual")
    )
  }
  .rf_assert_unique_observations(.rf_bind_rows(rows))
}

.rf_parse_chart_zip <- function(path, release, table_id = NULL,
                                max_member_bytes = 20 * 1024^2) {
  listing <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) .rf_abort("Source is not a readable ZIP archive.",
                                  "schema")
  )
  csv <- listing[grepl("\\.csv$", listing$Name, ignore.case = TRUE), ,
                 drop = FALSE]
  if (!nrow(csv)) {
    .rf_abort("Chart-data ZIP contains no CSV members.", "schema")
  }
  unsafe <- startsWith(csv$Name, "/") |
    grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", csv$Name)
  if (any(unsafe)) {
    .rf_abort("Chart-data ZIP contains an unsafe member path.", "schema")
  }
  if (is.null(table_id)) {
    if (nrow(csv) != 1L) {
      .rf_abort(
        "Chart-data ZIP has multiple CSV tables; select one with `table_id`.",
        "table_required", tables = csv$Name
      )
    }
    selected <- csv[1, , drop = FALSE]
  } else {
    .rf_assert_scalar_string(table_id, "table_id")
    key <- tolower(tools::file_path_sans_ext(basename(csv$Name)))
    wanted <- tolower(tools::file_path_sans_ext(basename(table_id)))
    hit <- key == wanted | startsWith(key, paste0(wanted, " -")) |
      grepl(wanted, key, fixed = TRUE)
    if (sum(hit) != 1L) {
      .rf_abort(
        sprintf("`table_id` matched %s ZIP members; expected exactly one.",
                sum(hit)),
        "table_required", tables = csv$Name
      )
    }
    selected <- csv[hit, , drop = FALSE]
  }
  if (selected$Length > max_member_bytes) {
    .rf_abort("Selected chart-data member exceeds the byte limit.",
              "too_large")
  }
  extract_dir <- tempfile("readfiscalau-zip-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  utils::unzip(path, files = selected$Name, exdir = extract_dir,
               junkpaths = FALSE)
  member_path <- file.path(extract_dir, selected$Name)
  size <- file.info(member_path)$size
  if (is.na(size) || size > max_member_bytes) {
    .rf_abort("Extracted chart-data member exceeds the byte limit.",
              "too_large")
  }
  if (!identical(as.numeric(size), as.numeric(selected$Length))) {
    .rf_abort(
      "Extracted chart-data member length disagrees with the ZIP directory.",
      "truncated"
    )
  }
  con <- file(member_path, open = "rb")
  member_raw <- readBin(con, what = "raw", n = size)
  close(con)
  data <- .rf_parse_chart_csv(
    member_raw, release,
    table_id = tools::file_path_sans_ext(basename(selected$Name))
  )
  data$accounting_basis <- .rf_chart_accounting_basis(
    selected$Name, release$accounting_basis
  )
  if (
    identical(release$consolidation, "general_government_sector") &&
      !.rf_chart_member_has_gov_scope(selected$Name)
  ) {
    .rf_abort(
      "Selected Budget table name does not identify Australian Government scope.",
      "schema",
      table = selected$Name
    )
  }
  attr(data, "readfiscalau_member") <- list(
    name = selected$Name,
    sha256 = .rf_sha256(member_raw),
    bytes = length(member_raw)
  )
  data
}

.rf_treasury_vintage_observations <- function(release_type) {
  if (!release_type %in% c("budget", "myefo")) {
    .rf_abort("Unsupported Treasury vintage panel.", "invalid_argument")
  }
  publication <- if (identical(release_type, "budget")) {
    "Budget"
  } else {
    "MYEFO"
  }
  filename <- paste0(release_type, "-vintage-observations.csv")
  cache_kind <- paste0(release_type, "_vintage_observations")
  path <- system.file(
    "extdata",
    filename,
    package = "readfiscalau"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", filename)
  }
  if (!file.exists(path)) {
    .rf_abort(
      sprintf(
        "The curated %s-vintage observation panel is missing.",
        publication
      ),
      "schema"
    )
  }
  cached <- .rf_internal_file_cache_get(cache_kind, path)
  if (!is.null(cached)) {
    return(cached)
  }
  panel <- utils::read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  if (!"observation_source_url" %in% names(panel)) {
    panel$observation_source_url <- panel$source_url
  }
  if (!"observation_source_sha256" %in% names(panel)) {
    panel$observation_source_sha256 <- panel$source_sha256
  }
  if (!"table_family" %in% names(panel)) {
    panel$table_family <- "cash_receipts"
  }
  if (!"table_label" %in% names(panel)) {
    panel$table_label <- "Cash receipts"
  }
  if (!"source_table" %in% names(panel)) {
    panel$source_table <- "Australian Government general government (cash) receipts"
  }
  required <- c(
    "release_id", "source_url", "source_sha256",
    "observation_source_url", "observation_source_sha256",
    "table_id", "table_family", "table_label", "source_table",
    "series_id", "series", "fiscal_year", "period_end", "value",
    "unit", "unit_multiplier", "accounting_basis", "consolidation",
    "actual_forecast", "profile_type"
  )
  missing <- setdiff(required, names(panel))
  if (length(missing)) {
    .rf_abort(
      paste(
        sprintf("The curated %s-vintage panel is missing:", publication),
        paste(missing, collapse = ", ")
      ),
      "schema",
      missing_fields = missing
    )
  }
  panel <- panel[required]
  panel$fiscal_year <- .rf_normalize_fiscal_year(panel$fiscal_year)
  panel$period_end <- as.Date(panel$period_end)
  panel$value <- suppressWarnings(as.numeric(panel$value))
  panel$unit_multiplier <- suppressWarnings(
    as.numeric(panel$unit_multiplier)
  )
  key <- paste(
    panel$release_id,
    panel$table_id,
    panel$series_id,
    panel$fiscal_year,
    panel$actual_forecast,
    sep = "\r"
  )
  if (!nrow(panel) ||
      anyDuplicated(key) ||
      anyNA(panel$period_end) ||
      anyNA(panel$value) ||
      anyNA(panel$unit_multiplier) ||
      any(!grepl("^[a-f0-9]{64}$", panel$source_sha256)) ||
      any(!grepl(
        "^[a-f0-9]{64}$",
        panel$observation_source_sha256
      )) ||
      any(!nzchar(panel$observation_source_url)) ||
      any(!nzchar(panel$table_family)) ||
      any(!nzchar(panel$table_label)) ||
      any(!nzchar(panel$source_table)) ||
      any(!panel$accounting_basis %in% c(
        "cash", "accrual", "not_applicable"
      )) ||
      any(!panel$consolidation %in% c(
        "general_government_sector", "australian_economy"
      )) ||
      any(!panel$unit %in% c(
        "AUD million", "% of GDP", "Per cent",
        "Per cent change", "Percentage points", "Persons"
      )) ||
      any(!panel$actual_forecast %in% c("actual", "forecast")) ||
      any(panel$profile_type != ifelse(
        panel$actual_forecast == "actual",
        "actual",
        ifelse(
          panel$table_family == "budget_comparator_aggregates",
          "budget",
          release_type
        )
      ))) {
    .rf_abort(
      sprintf(
        "The curated %s-vintage observation panel failed its contract.",
        publication
      ),
      "schema"
    )
  }
  attr(panel, "readfiscalau_panel_path") <- path
  .rf_internal_file_cache_set(cache_kind, path, panel)
  panel
}

.rf_budget_vintage_observations <- function() {
  .rf_treasury_vintage_observations("budget")
}

.rf_myefo_vintage_observations <- function() {
  .rf_treasury_vintage_observations("myefo")
}

.rf_parse_treasury_pdf_snapshot <- function(
    release,
    release_type,
    table_id = NULL) {
  publication <- if (identical(release_type, "budget")) {
    "Budget"
  } else {
    "MYEFO"
  }
  table_id <- table_id %||% "cash_receipts"
  .rf_assert_scalar_string(table_id, "table_id")
  family_aliases <- c(
    "table 1" = "cash_receipts",
    "1" = "cash_receipts",
    "cash receipts" = "cash_receipts",
    "cash_receipts" = "cash_receipts",
    "major economic parameters" = "major_economic_parameters",
    "major_economic_parameters" = "major_economic_parameters",
    "budget aggregates" = "budget_aggregates",
    "budget_aggregates" = "budget_aggregates",
    "domestic economy - detailed forecasts" =
      "domestic_economy_forecasts",
    "domestic economy \u2014 detailed forecasts" =
      "domestic_economy_forecasts",
    "domestic_economy_forecasts" = "domestic_economy_forecasts",
    "cash interest" = "cash_interest",
    "cash_interest" = "cash_interest",
    "budget comparator aggregates" =
      "budget_comparator_aggregates",
    "budget_comparator_aggregates" =
      "budget_comparator_aggregates"
  )
  table_key <- tolower(trimws(table_id))
  table_family <- unname(family_aliases[table_key])
  if (length(table_family) != 1L || is.na(table_family)) {
    panel <- .rf_treasury_vintage_observations(release_type)
    tables <- unique(panel$table_family)
    .rf_abort(
      sprintf(
        "Historical %s observations do not include table family `%s`.",
        publication,
        table_id
      ),
      "table_required",
      tables = tables
    )
  }
  panel <- .rf_treasury_vintage_observations(release_type)
  if (!table_family %in% panel$table_family) {
    .rf_abort(
      sprintf(
        "Historical %s observations do not include table family `%s`.",
        publication,
        table_family
      ),
      "table_required",
      tables = unique(panel$table_family)
    )
  }
  path <- attr(panel, "readfiscalau_panel_path")
  selected <- panel[
    panel$release_id == release$release_id &
      panel$table_family == table_family,
    ,
    drop = FALSE
  ]
  if (!nrow(selected)) {
    .rf_abort(
      sprintf(
        "The selected %s release has no curated PDF observation panel.",
        publication
      ),
      "schema",
      release_id = release$release_id
    )
  }
  if (length(unique(selected$source_url)) != 1L ||
      !identical(unique(selected$source_url), release$resource_url) ||
      length(unique(selected$source_sha256)) != 1L ||
      !identical(
        unique(selected$source_sha256),
        release$expected_sha256
      ) ||
      length(unique(selected$table_family)) != 1L ||
      !identical(unique(selected$table_family), table_family)) {
    .rf_abort(
      sprintf(
        "Curated %s observations conflict with the pinned source.",
        publication
      ),
      "source_hash",
      release_id = release$release_id
    )
  }
  future_actual <- selected$actual_forecast == "actual" &
    selected$period_end > release$release_date
  if (any(future_actual)) {
    .rf_abort(
      sprintf(
        "Curated %s observations label an unfinished period as actual.",
        publication
      ),
      "schema",
      fiscal_years = selected$fiscal_year[future_actual]
    )
  }
  data <- tibble::as_tibble(selected[c(
    "table_id", "table_family", "table_label", "source_table",
    "series_id", "series", "fiscal_year", "period_end",
    "value", "unit", "unit_multiplier", "accounting_basis",
    "consolidation", "actual_forecast", "profile_type"
  )])
  data <- .rf_assert_unique_observations(data)
  attr(data, "readfiscalau_member") <- list(
    name = paste0(release_type, "-vintage-observations.csv"),
    sha256 = digest::digest(
      file = path,
      algo = "sha256",
      serialize = FALSE
    ),
    bytes = as.numeric(file.info(path)$size),
    observation_sources = unique(selected[c(
      "observation_source_url",
      "observation_source_sha256"
    )]),
    derivation = paste(
      "Maintainer-normalised observations from official PBO structured",
      "workbooks where available and exact Treasury release tables for",
      "the remaining historical table families."
    )
  )
  data
}

.rf_parse_budget_pdf_snapshot <- function(release, table_id = NULL) {
  .rf_parse_treasury_pdf_snapshot(release, "budget", table_id)
}

.rf_parse_myefo_pdf_snapshot <- function(release, table_id = NULL) {
  .rf_parse_treasury_pdf_snapshot(release, "myefo", table_id)
}
