test_that("source note references never become financial values", {
  table <- read_au_budget_myefo_table('treasury-budget-2020-21-final', 'formal_financial_statements')
  row <- table[table$series_id == 'suppliers-payables' & table$actual_forecast == 'actual', ]
  expect_equal(row$value, 7396)
  expect_identical(row$period_end, as.Date('2020-06-30'))
})

test_that("same printed labels retain their published scope", {
  table <- read_au_budget_myefo_table('treasury-budget-2022-23-october-final', 'formal_financial_statements')
  ggs <- table[table$consolidation == 'general_government_sector' & table$actual_forecast == 'actual', ]
  expect_equal(ggs$value[ggs$series_id == 'borrowing-financing-receipts'], 202252)
  expect_equal(ggs$value[ggs$series_id == 'borrowing-financing-payments'], -123358)
  expect_equal(ggs$value[ggs$series_id == 'actuarial-revaluations-included-in-operating-result'], 7031)
  expect_equal(ggs$value[ggs$series_id == 'actuarial-revaluations-included-in-equity'], -19774)
  functions <- read_au_budget_myefo_table('treasury-budget-2022-23-october-final', 'expenses_by_function_subfunction')
  functions <- functions[functions$actual_forecast == 'actual', ]
  scopes <- c('education-general-administration','health-general-administration',
    'social-security-and-welfare-general-administration','agriculture-forestry-and-fishing-general-administration')
  expect_equal(functions$value[match(scopes,functions$series_id)],c(333,4648,4716,1121))
})

test_that("explicit estimates and India fiscal dates preserve publication roles", {
  table <- read_au_budget_myefo_table('treasury-budget-2025-26-final', 'international_gdp_forecasts')
  india <- table[table$series_id == 'india-fiscal-year-ending-march', ]
  expect_equal(india$value,6.5)
  expect_identical(india$period_end,as.Date('2025-03-31'))
  expect_identical(india$actual_forecast,'actual')
  x <- .rf_budget_myefo_history_store()$observations
  cgt <- x[x$series_id=='capital-gains-tax' & x$fiscal_year=='2008-09' & x$release_id=='treasury-budget-2010-11-final', ]
  expect_true(nrow(cgt)>0L)
  expect_true(all(cgt$actual_forecast=='forecast'))
  world <- x[x$series_id%in%c('world','major-trading-partners') & x$calendar_year%in%c(2019L,2020L), ]
  expect_false(any(world$actual_forecast=='actual'))
})

test_that("single-column FBO values and historical units remain native", {
  s <- .rf_treasury_actual_supplement()$observations
  row <- s[s$release_id=='treasury-fbo-2014-15-final' & s$series_id=='passenger-motor-vehicles', ]
  expect_equal(row$value,732)
  expect_identical(row$period_end,as.Date('2015-06-30'))
  expect_identical(row$unit,'AUD million')
  rows <- s[s$release_id=='treasury-fbo-2013-14-final' & s$consolidation=='public_financial_corporations_sector' & s$accounting_basis=='cash', ]
  expect_equal(rows$value[rows$series_id=='interest-receipts'],1894)
  expect_equal(rows$value[rows$series_id=='interest-paid'],-1137)
  history <- list_au_budget_myefo_actual_histories(series='net-debt',unit='AUD million',consolidation='general_government_sector')
  expect_true(all(history$first_period==as.Date('1971-06-30')))
})

test_that('budget-summary debt and receipts use their published accounting measures', {
  s <- .rf_budget_myefo_history_store()$observations
  summary <- s[s$family_id=='ggs_budget_aggregates',]
  expect_true(all(summary$accounting_basis[summary$series_id=='net-debt']=='accrual'))
  expect_true(all(summary$accounting_basis[summary$series_id%in%c('tax-receipts','non-tax-receipts')]=='cash'))
  fbo <- .rf_treasury_actual_supplement()$observations
  expect_true(all(fbo$accounting_basis[fbo$series_id=='net-debt']=='accrual'))
})
