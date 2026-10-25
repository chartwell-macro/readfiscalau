.rf_contract_revision <-
  "readfiscalau-2026-10-13-treasury-actual-history-audit"
.rf_parser_revision <-
  "readfiscalau-parser-2026-10-13-treasury-history-repair"

.rf_internal_file_cache <- new.env(parent = emptyenv())
.rf_runtime_metadata_cache <- new.env(parent = emptyenv())

.rf_internal_file_cache_key <- function(kind, path) {
  path <- normalizePath(path, mustWork = TRUE)
  info <- file.info(path)
  paste(
    kind,
    path,
    as.numeric(info$size),
    as.numeric(info$mtime),
    sep = "::"
  )
}

.rf_internal_file_cache_get <- function(kind, path) {
  key <- .rf_internal_file_cache_key(kind, path)
  if (!exists(
    key,
    envir = .rf_internal_file_cache,
    inherits = FALSE
  )) {
    return(NULL)
  }
  get(key, envir = .rf_internal_file_cache, inherits = FALSE)
}

.rf_internal_file_cache_set <- function(kind, path, value) {
  key <- .rf_internal_file_cache_key(kind, path)
  path <- normalizePath(path, mustWork = TRUE)
  prefix <- paste0(kind, "::", path, "::")
  stale <- ls(
    envir = .rf_internal_file_cache,
    all.names = TRUE
  )
  stale <- stale[
    startsWith(stale, prefix) & stale != key
  ]
  if (length(stale)) {
    rm(
      list = stale,
      envir = .rf_internal_file_cache
    )
  }
  assign(key, value, envir = .rf_internal_file_cache)
  invisible(value)
}

.rf_clear_internal_file_cache <- function() {
  keys <- ls(
    envir = .rf_internal_file_cache,
    all.names = TRUE
  )
  if (length(keys)) {
    rm(
      list = keys,
      envir = .rf_internal_file_cache
    )
  }
  invisible(NULL)
}

.rf_schema_contracts <- data.frame(
  schema_family = c(
    "treasury_chart_csv_v1",
    "treasury_chart_csv_v2",
    "treasury_chart_zip_csv_v2",
    "treasury_budget_pdf_snapshot_v1",
    "treasury_myefo_pdf_snapshot_v1",
    "treasury_fbo_history_docx_v1",
    "pbo_historical_fiscal_data_xlsx_v1",
    "finance_mfs_pre_2008_v1",
    "finance_mfs_post_2008_v2"
  ),
  parser_version = rep("0.1.0", 9L),
  expected_unit = c(
    NA, NA, NA, "AUD million", NA, "AUD million",
    NA, "AUD million", NA
  ),
  stringsAsFactors = FALSE
)

.rf_terms_contracts <- list(
  australian_treasury = list(
    terms_url = "https://budget.gov.au/legal-notices.htm#h1",
    license = "Creative Commons Attribution 4.0 International",
    attribution = "\u00a9 Commonwealth of Australia"
  ),
  department_finance_data = list(
    terms_url = "https://creativecommons.org/licenses/by/3.0/au/",
    license = "Creative Commons Attribution 3.0 Australia",
    attribution = paste(
      "Attribute the Australian Government Department of Finance",
      "and link the source"
    )
  ),
  parliamentary_budget_office = list(
    terms_url = "https://www.pbo.gov.au/copyright-information",
    license = paste(
      "Creative Commons Attribution-NonCommercial-NoDerivatives",
      "4.0 International"
    ),
    attribution = "The Parliamentary Budget Office - pbo.gov.au"
  ),
  department_finance_website = list(
    terms_url = "https://www.finance.gov.au/about-us/copyright-notice",
    license = "Creative Commons Attribution 4.0 International",
    attribution = "\u00a9 Commonwealth of Australia, Department of Finance"
  )
)

.rf_schema_contract <- function(schema_family) {
  hit <- .rf_schema_contracts[
    .rf_schema_contracts$schema_family == schema_family,
    ,
    drop = FALSE
  ]
  if (nrow(hit) != 1L) {
    .rf_abort(
      sprintf("Unsupported schema family `%s`.", schema_family),
      "schema"
    )
  }
  hit
}

.rf_terms_contract <- function(source_id, resource_url) {
  if (identical(source_id, "australian_treasury")) {
    return(.rf_terms_contracts$australian_treasury)
  }
  if (identical(source_id, "department_finance")) {
    if (grepl("^https://data\\.gov\\.au/", resource_url)) {
      return(.rf_terms_contracts$department_finance_data)
    }
    return(.rf_terms_contracts$department_finance_website)
  }
  if (identical(source_id, "parliamentary_budget_office")) {
    return(.rf_terms_contracts$parliamentary_budget_office)
  }
  .rf_abort("Release source has no terms contract.", "schema")
}

.rf_release_cache_namespace <- function(release) {
  fields <- c(
    "release_id", "source_id", "release_type", "release_status",
    "release_date", "period_end", "resource_id", "resource_role",
    "resource_url", "resource_updated_at", "expected_sha256",
    "media_type", "schema_family", "parser_version"
  )
  values <- vapply(fields, function(field) {
    value <- release[[field]]
    if (length(value) != 1L || is.na(value)) "" else as.character(value)
  }, character(1))
  paste(
    c(
      paste0("contract_revision=", .rf_contract_revision),
      paste0("parser_revision=", .rf_parser_revision),
      paste0(fields, "=", values)
    ),
    collapse = "\n"
  )
}

.rf_runtime_dependency_versions <- function() {
  if (exists(
    "dependency_versions",
    envir = .rf_runtime_metadata_cache,
    inherits = FALSE
  )) {
    return(get(
      "dependency_versions",
      envir = .rf_runtime_metadata_cache,
      inherits = FALSE
    ))
  }
  packages <- c("digest", "httr2", "jsonlite", "readxl", "tibble", "xml2")
  versions <- stats::setNames(vapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(package))
  }, character(1)), packages)
  assign(
    "dependency_versions",
    versions,
    envir = .rf_runtime_metadata_cache
  )
  versions
}
