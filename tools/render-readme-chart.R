if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload to render the documentation chart.", call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Install ggplot2 to render the documentation chart.", call. = FALSE)
}

pkgload::load_all(".", quiet = TRUE)

chart_env <- new.env(parent = globalenv())
chart_plot <- NULL
for (expression in parse("vignettes/_chart-example.R")) {
  chart_plot <- eval(expression, envir = chart_env)
}

chart_provenance <- attr(
  chart_env$treasury_budget,
  "readfiscalau_provenance"
)
chart_rows <- chart_env$treasury_budget[
  chart_env$treasury_budget$series == "Total receipts",
  ,
  drop = FALSE
]

stopifnot(
  inherits(chart_plot, "ggplot"),
  identical(
    chart_provenance$raw_content_sha256,
    "2db5571dea5dba82cd90679f3db3796bb4ca308a862d89c6e53cdf2f4fce9597"
  ),
  nrow(chart_env$chart_data) == 23L,
  sum(chart_rows$actual_forecast == "actual") == 18L,
  sum(chart_rows$actual_forecast == "forecast") == 5L
)

output_path <- commandArgs(trailingOnly = TRUE)
if (length(output_path) == 0L) {
  output_path <- "man/figures/treasury-total-receipts.png"
}
stopifnot(length(output_path) == 1L)

ggplot2::ggsave(
  filename = output_path,
  plot = chart_plot,
  width = 7.8,
  height = 5.8,
  units = "in",
  dpi = 160,
  bg = "white"
)
