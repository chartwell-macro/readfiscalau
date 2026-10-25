test_that("Finance period month names remain English outside an English locale", {
  original_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", original_locale), add = TRUE)

  candidates <- c(
    "de_DE.UTF-8", "de_DE", "German_Germany.1252",
    "fr_FR.UTF-8", "fr_FR", "French_France.1252",
    "es_ES.UTF-8", "es_ES", "Spanish_Spain.1252",
    "it_IT.UTF-8", "it_IT", "Italian_Italy.1252",
    "nl_NL.UTF-8", "nl_NL", "Dutch_Netherlands.1252",
    "pt_BR.UTF-8", "pt_BR", "Portuguese_Brazil.1252",
    "ru_RU.UTF-8", "ru_RU", "Russian_Russia.1251",
    "zh_CN.UTF-8", "zh_CN", "Chinese_China.936"
  )
  selected <- NULL
  for (candidate in candidates) {
    changed <- tryCatch(
      Sys.setlocale("LC_TIME", candidate),
      warning = function(w) NA_character_,
      error = function(e) NA_character_
    )
    if (!is.na(changed) && nzchar(changed) &&
        !identical(format(as.Date("2026-05-31"), "%B"), "May")) {
      selected <- changed
      break
    }
  }
  dates <- as.Date(sprintf("2026-%02d-28", 1:12))
  expect_identical(
    vapply(dates, readfiscalau:::.rf_period_month, character(1)),
    c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    )
  )

  if (is.null(selected)) {
    skip("No installed non-English LC_TIME locale.")
  }

  expect_identical(
    readfiscalau:::.rf_period_month(as.Date("2026-05-31")),
    "May"
  )
})
