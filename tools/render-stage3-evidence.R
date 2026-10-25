#!/usr/bin/env Rscript

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("ggplot2 is required to render the stage-3 evidence charts.",
       call. = FALSE)
}

script_arg <- commandArgs(trailingOnly = FALSE)
script_arg <- script_arg[grep("^--file=", script_arg)][1L]
script_path <- sub("^--file=", "", script_arg)
package_dir <- dirname(dirname(normalizePath(script_path, mustWork = TRUE)))
evidence_dir <- file.path(package_dir, "inst", "evidence")
evidence <- utils::read.csv(
  file.path(evidence_dir, "value-crosscheck.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

theme_readfiscalau <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = "#f5f8f4",
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "#f5f8f4",
        colour = NA
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 22,
        margin = ggplot2::margin(b = 12)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        margin = ggplot2::margin(b = 16)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 10,
        lineheight = 1.05,
        margin = ggplot2::margin(t = 14)
      ),
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(t = 24, r = 54, b = 28, l = 46),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = "#c8d4e8",
        linewidth = 0.55
      ),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(r = 12)
      ),
      axis.text = ggplot2::element_text(colour = "#111827"),
      legend.position = "bottom",
      legend.justification = "left",
      legend.title = ggplot2::element_blank()
    )
}

budget <- evidence[evidence$chart == "budget_receipts", , drop = FALSE]
budget$period <- factor(budget$period, levels = budget$period)
budget$value_billions <- budget$value / 1000
budget_caption <- paste0(
  "Based on Commonwealth of Australia data • Budget 2026–27 chart data, ",
  "Table 1 • Raw SHA-256 ",
  unique(budget$raw_sha256)
)
budget_plot <- ggplot2::ggplot(
  budget,
  ggplot2::aes(x = .data$period, y = .data$value_billions, group = 1L)
) +
  ggplot2::geom_line(colour = "#4b5563", linewidth = 1.1) +
  ggplot2::geom_point(
    ggplot2::aes(fill = .data$actual_forecast),
    shape = 21,
    size = 4.2,
    colour = "#111827",
    stroke = 0.6
  ) +
  ggplot2::scale_fill_manual(
    values = c(actual = "#2f6fb0", forecast = "#d18b2c"),
    labels = c(actual = "Actual", forecast = "Estimate")
  ) +
  ggplot2::scale_y_continuous(
    labels = function(value) paste0("$", format(
      value,
      trim = TRUE,
      big.mark = ",",
      nsmall = 0
    ), "b"),
    expand = ggplot2::expansion(mult = c(0.08, 0.12))
  ) +
  ggplot2::labs(
    title = "Australian Government cash receipts",
    subtitle = "Budget 2026–27: actual outturns and published estimates",
    y = "AUD billion",
    caption = budget_caption
  ) +
  theme_readfiscalau()

finance <- evidence[evidence$chart == "finance_may", , drop = FALSE]
finance$series <- factor(
  finance$series,
  levels = c("Underlying cash balance", "Receipts(b)", "Payments(c)")
)
finance$value_label <- paste0(
  ifelse(finance$value < 0, "\u2212$", "$"),
  format(abs(finance$value), trim = TRUE, nsmall = 1, digits = 4),
  "b"
)
finance_caption <- paste0(
  "Australian Government Department of Finance • May 2026 actual YTD ",
  "aggregates • Raw SHA-256 ",
  unique(finance$raw_sha256)
)
finance_plot <- ggplot2::ggplot(
  finance,
  ggplot2::aes(x = .data$series, y = .data$value, fill = .data$series)
) +
  ggplot2::geom_col(width = 0.64, show.legend = FALSE) +
  ggplot2::geom_text(
    ggplot2::aes(label = .data$value_label),
    vjust = ifelse(finance$value < 0, 1.25, -0.55),
    size = 4.2,
    fontface = "bold"
  ) +
  ggplot2::geom_hline(yintercept = 0, colour = "#111827", linewidth = 0.55) +
  ggplot2::scale_fill_manual(
    values = c(
      "Underlying cash balance" = "#d18b2c",
      "Receipts(b)" = "#2f6fb0",
      "Payments(c)" = "#6b8e5b"
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = function(value) paste0("$", format(
      value,
      trim = TRUE,
      big.mark = ",",
      nsmall = 0
    ), "b"),
    expand = ggplot2::expansion(mult = c(0.08, 0.12))
  ) +
  ggplot2::scale_x_discrete(
    labels = c(
      "Underlying cash balance" = "Underlying cash\nbalance",
      "Receipts(b)" = "Receipts",
      "Payments(c)" = "Payments"
    )
  ) +
  ggplot2::labs(
    title = "Commonwealth monthly financial aggregates",
    subtitle = "May 2026 actual year-to-date values",
    y = "AUD billion",
    caption = finance_caption
  ) +
  theme_readfiscalau() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 0,
      hjust = 0.5,
      margin = ggplot2::margin(t = 8)
    )
  )

outputs <- c(
  budget = file.path(evidence_dir, "budget-receipts.png"),
  finance = file.path(evidence_dir, "finance-may-aggregates.png")
)
ggplot2::ggsave(
  outputs[["budget"]],
  budget_plot,
  width = 12,
  height = 7,
  dpi = 144,
  bg = "#f5f8f4"
)
ggplot2::ggsave(
  outputs[["finance"]],
  finance_plot,
  width = 12,
  height = 7,
  dpi = 144,
  bg = "#f5f8f4"
)

for (output in outputs) {
  message(
    basename(output),
    " bytes=", file.info(output)$size,
    " sha256=", digest::digest(
      file = output,
      algo = "sha256",
      serialize = FALSE
    )
  )
}
message("readfiscalau stage-3 evidence charts rendered.")
