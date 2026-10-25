# Rebuild the deterministic, official-shaped Budget chart-data ZIP fixture.
#
# The member names follow the official Treasury archive. The contents are tiny
# committed replay grids, not a redistribution of the full official archive.
script_arg <- commandArgs(trailingOnly = FALSE)
script_arg <- script_arg[grep("^--file=", script_arg)][1L]
script_path <- sub("^--file=", "", script_arg)
fixture_dir <- dirname(normalizePath(script_path, mustWork = TRUE))

staging <- tempfile("readfiscalau-budget-zip-")
dir.create(staging)
on.exit(unlink(staging, recursive = TRUE), add = TRUE)

members <- c(
  "Table 1 - Australian Government (cash) receipts.csv",
  "Table 3 - Australian Government (accrual) revenue.csv"
)
sources <- c("budget-current.csv", "budget-historical.csv")
copied <- file.copy(
  file.path(fixture_dir, sources),
  file.path(staging, members)
)
if (!all(copied)) {
  stop("Could not stage the Budget ZIP fixture members.")
}
Sys.setFileTime(
  file.path(staging, members),
  as.POSIXct("2026-07-27 00:00:00", tz = "UTC")
)

output <- file.path(fixture_dir, "budget-chart-archive.zip")
if (file.exists(output)) file.remove(output)
old <- setwd(staging)
on.exit(setwd(old), add = TRUE)
status <- utils::zip(output, sort(members), flags = "-X -q")
setwd(old)
if (!identical(status, 0L) || !file.exists(output)) {
  stop("Could not create deterministic Budget ZIP fixture.")
}
