example_path <- function(name) {
  installed <- system.file("extdata", "examples", name,
                          package = "readfiscalau")
  if (nzchar(installed)) return(installed)
  source_tree <- file.path("inst", "extdata", "examples", name)
  if (file.exists(source_tree)) return(source_tree)
  stop("Example fixture is not available: ", name, call. = FALSE)
}

example_raw <- function(name) {
  path <- example_path(name)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = file.info(path)$size)
}

example_budget_release <- function() {
  data.frame(
    release_id = "example-budget-2026-27",
    source_id = "australian_treasury",
    release_type = "budget",
    release_status = "final",
    release_date = "2026-05-12",
    period_end = "2027-06-30",
    fiscal_year = "2026-27",
    frequency = "release",
    accounting_basis = "mixed",
    consolidation = "general_government_sector",
    actual_forecast = "mixed",
    revision_semantics = "Small packaged chart-data-shaped example fixture.",
    landing_url = "https://budget.gov.au/content/downloads.htm",
    resource_id = "example-budget-chart-data",
    resource_role = "example_chart_csv",
    resource_url = "https://budget.gov.au/content/download/chart-data-final.csv",
    resource_updated_at = "2026-05-12T07:36:13Z",
    expected_sha256 = NA_character_,
    media_type = "text/csv",
    schema_family = "treasury_chart_csv_v2",
    parser_version = "0.1.0",
    terms_url = "https://budget.gov.au/legal-notices.htm#h1",
    license = "Creative Commons Attribution 4.0 International",
    attribution = "© Commonwealth of Australia",
    stringsAsFactors = FALSE
  )
}

example_fixture_read <- function() {
  read_au_budget(
    example_budget_release(),
    raw = example_raw("budget-current.csv"),
    cache_dir = tempfile("readfiscalau-example-cache-")
  )
}
