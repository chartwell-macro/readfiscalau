#!/usr/bin/env Rscript
# Offline maintainer build. Source bytes, publication month, table and column
# identity are retained; only explicitly labelled Outcome columns are admitted.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=3L) stop('Usage: build-treasury-actual-supplement.R SOURCES_JSON TEXT_DIR OUTPUT_RDS')
expressions <- parse('tools/build-budget-myefo-history.R',keep.source=FALSE)
main <- which(vapply(expressions,function(e)is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('options')),logical(1)))[1]
builder <- new.env(parent=globalenv())
for(i in seq_len(main-1L))eval(expressions[[i]],builder)
sources <- jsonlite::fromJSON(args[1])
numeric_tokens <- builder$token_info
builder$token_info <- function(line) {
  info <- numeric_tokens(line)
  hit <- gregexpr('(?<!\\S)-(?!\\S)',line,perl=TRUE)[[1L]]
  if(hit[1L]!=-1L) {
    hit <- hit[!vapply(hit,function(at)any(info$start<=at & info$end>=at),logical(1))]
    if(length(hit)) info <- rbind(info,data.frame(text='-',start=hit,end=hit))
  }
  info[order(info$start),,drop=FALSE]
}
fbo_columns <- function(block) {
  layout <- vapply(block,builder$decode_legacy_font_line,character(1))
  layout <- gsub('2XWFRPH', 'Outcome', layout, fixed=TRUE)
  layout <- gsub('[\u2010\u2011\u2012\u2013\u2014\u2212]','-',layout)
  marks <- function(line,pattern) {
    h <- gregexpr(pattern,line,perl=TRUE,ignore.case=TRUE)[[1]]
    if(h[1]==-1L)return(data.frame())
    data.frame(label=regmatches(line,list(h))[[1]],end=h+attr(h,'match.length')-1L)
  }
  scope <- seq_len(min(16L,length(layout)))
  unit_marks <- lapply(layout[scope],marks,pattern='\\$m\\b|\\$b\\b')
  counts <- vapply(unit_marks,nrow,integer(1))
  single_unit <- which(vapply(layout[scope],function(x)grepl('^\\s*\\$m\\s*$',x,perl=TRUE),logical(1)))
  if(max(counts)<2L && !length(single_unit)) {
    # Early summary tables declare units on rows, with four dated columns.
    hdr <- builder$header_columns(block)
    if(length(hdr$ends)<2L)return(NULL)
    data_rows <- which(vapply(layout,function(line) {
      info <- builder$token_info(line)
      if(nrow(info)<2L)return(FALSE)
      prefix <- trimws(substr(line,1L,info$start[1L]-1L))
      nzchar(prefix)&&grepl('[[:alpha:]]',prefix)&&!grepl('^Table|^Note|^Source',prefix)
    },logical(1)))
    unit_at <- if(length(data_rows))data_rows[1L]-1L else hdr$header_at
    units <- data.frame(label=rep('$b',length(hdr$ends)),end=hdr$ends)
  } else {
    unit_at <- if(max(counts)<2L)single_unit[1L] else which.max(counts); units <- unit_marks[[unit_at]]
  }
  # Endpoints identify observation columns; note references remain label text.
  ends <- units$end; count <- length(ends)
  header <- seq_len(unit_at)
  years <- lapply(layout[header],marks,pattern='(?:19|20)[0-9]{2}-(?:(?:19|20)[0-9]{2}|[0-9]{2})(?![0-9])')
  year_counts <- vapply(years,nrow,integer(1))
  if(max(year_counts)<1L)return(NULL)
  year_marks <- do.call(rbind, years[year_counts>0L])
  outcomes <- do.call(rbind,lapply(layout[header],marks,pattern='\\b(?:Outcome(?:s)?|Actual(?:s)?)\\b'))
  if(is.null(outcomes)||!nrow(outcomes))return(NULL)
  actual_columns <- unique(vapply(outcomes$end,function(e)which.min(abs(ends-e)),integer(1)))
  labels <- rep('Non-time comparison',count)
  for(i in actual_columns) labels[i] <- year_marks$label[which.min(abs(year_marks$end-ends[i]))]
  list(header_at=unit_at,labels=labels,ends=ends,profiles=rep(NA_character_,count),
       units=ifelse(grepl('b',units$label),'AUD billion','AUD million'),
       is_actual=seq_len(count)%in%actual_columns)
}
families <- builder$family_spec
builder$family_spec <- families[families$family_id %in% c('formal_financial_statements','expenses_by_function_subfunction','detailed_cash_receipts','ggs_budget_aggregates','net_debt_components','cash_interest','accrual_interest'),]
builder$family_spec$pattern[builder$family_spec$family_id == 'formal_financial_statements'] <- paste0(
  '^Table [^:]+: (?:(?:Australian Government|Commonwealth) )?',
  '(?:general government|public non-financial corporations|total non-financial public|public financial corporations)',
  '(?: sector)? (?:operating statement|balance sheet|cash flow statement)')
builder$family_spec$pattern[builder$family_spec$family_id == 'ggs_budget_aggregates'] <-
  '^Table [^:]+: (?:(?:Australian Government|Commonwealth) )?general government(?: sector)? budget aggregates'
builder$family_spec$pattern[builder$family_spec$family_id == 'detailed_cash_receipts'] <-
  '^Table [^:]+: (?:(?:Australian Government|Commonwealth) )?general government(?: sector)? (?:\\(cash\\) receipts|receipts \\(cash basis\\))'
original_parser <- builder$parse_table_part
builder$parse_table_part <- function(block,table_row,release) {
  columns <- fbo_columns(block)
  if(is.null(columns))return(NULL)
  parser_block <- block
  overlaid <- release$budget_vintage=='2013-14' &&
    table_row$family_id=='formal_financial_statements' &&
    grepl('^Table 21:',table_row$source_table)
  if(overlaid) {
    # Table 21 has two overlapping PDF text layers. Its 33 printed Outcome
    # rows were checked visually against PDF page 41. Repair labels in source
    # order, leaving every numeric token and the retained raw table untouched.
    labels <- c('Receipts from sales of goods and services','Grants and subsidies received',
      'GST input credit receipts','Interest receipts','Other receipts','Total operating receipts',
      'Payments to employees','Payment for goods and services','Interest paid',
      'GST payments to taxation authority','Other payments','Total operating payments',
      'Net cash flows from operating activities','Sales of non-financial assets',
      'Purchases of non-financial assets','Net cash flows from investments in non-financial assets',
      'Net cash flows from investments in financial assets for policy purposes',
      'Increase in investments','Net cash flows from investments in financial assets for liquidity purposes',
      'Advances received (net)','Borrowing (net)','Deposits received (net)','Other financing (net)',
      'Distributions paid (net)','Net cash flows from financing activities',
      'Net increase/(decrease) in cash held','Cash at the beginning of the year','Cash at the end of the year',
      'Net cash from operating activities and investments in non-financial assets',
      'Distributions paid','Equals surplus(+)/deficit(-)','Finance leases and similar arrangements',
      'GFS cash surplus(+)/deficit(-)')
    expected <- c(6675,8970,19,1894,78,17637,-696,-10730,-1137,-1,5032,-7532,10105,
      5,-803,-798,0,-40097,-40097,0,27366,0,3961,-603,30725,-65,1095,1030,9307,-603,8704,0,8704)
    tokens <- lapply(block,builder$token_info)
    data_rows <- which(vapply(seq_along(tokens),function(i) {
      i>columns$header_at && nrow(tokens[[i]])==1L &&
        tokens[[i]]$end[[1L]]>=columns$ends[[1L]]-10L
    },logical(1)))
    observed <- vapply(tokens[data_rows],function(info)builder$parse_number(info$text[[1L]]),numeric(1))
    if(length(data_rows)!=length(labels)||!identical(observed,expected)) stop('FBO 2013-14 Table 21 does not match the visually reviewed source rows')
    data_lines <- character(length(data_rows))
    for(j in seq_along(data_rows)) {
      info <- tokens[[data_rows[[j]]]]
      data_lines[[j]] <- paste0(labels[[j]],
        strrep(' ',info$start[[1L]]-nchar(labels[[j]])-1L),info$text[[1L]])
    }
    parser_block <- c(block[seq_len(columns$header_at)],data_lines)
  }
  rows <- original_parser(parser_block,table_row,release,columns=columns)
  if(is.null(rows))return(NULL)
  if(overlaid) rows$raw_line <- block[data_rows[rows$row_index]]
  rows <- rows[which(rows$series_eligible & rows$actual_forecast=='actual'),,drop=FALSE]
  # The harmonised AASB 1049 statements begin in 2008-09. Earlier GFS-only
  # statements are not interchangeable with the admitted current framework.
  if(as.integer(substr(release$budget_vintage,1,4))<2008L) {
    rows <- rows[rows$family_id!='formal_financial_statements',,drop=FALSE]
  }
  if(!nrow(rows))return(NULL)
  rows$source_kind <- rep('pdf_table_cell',nrow(rows))
  rows$source_container_sha256 <- rep(release$pdf_sha256,nrow(rows))
  rows$source_archive_members <- ''
  rows$source_was_assembled <- FALSE
  rows$publication_month <- release$publication_month
  rows
}
out <- list(); tables <- list(); ledger <- list()
for(i in seq_len(nrow(sources))) {
  fy <- sources$fy[i]; year <- as.integer(substr(fy,1,4))+1L
  text <- readLines(file.path(args[2],paste0('fbo-',fy,'.txt')),warn=FALSE)
  # Use the published month. No day is fabricated when the source supplies only a month.
  decoded <- builder$normalize_line(text[seq_len(min(50L,length(text)))])
  month <- regmatches(paste(decoded,collapse=' '),regexpr(paste0('September ',year),paste(decoded,collapse=' '),ignore.case=TRUE))
  publication_month <- if(length(month)&&nzchar(month))sprintf('%d-09',year) else NA_character_
  release <- data.frame(release_id=paste0('treasury-fbo-',fy,'-final'),publication='fbo',budget_vintage=fy,
    release_date=as.Date(NA),publication_month=publication_month,source_url=sources$url[i],pdf_sha256=sources$sha256[i],stringsAsFactors=FALSE)
  source_file <- file.path(args[2],paste0('fbo-',fy,'.txt'))
  target_file <- file.path(args[2],paste0('fbo-',fy,'.txt'))
  extracted <- builder$extract_release(release,args[2])
  out[[i]] <- extracted$cells
  tables[[i]] <- extracted$tables
  ledger[[i]] <- cbind(release,byte_size=sources$bytes[i])
  message('Reviewed FBO ',fy,': ',if(is.null(extracted$cells))0L else nrow(extracted$cells),' actual cells')
}
# The latest FBO publishes these historical quantities directly, in native
# units. This is a column parser, not a calculation from statement components.
# In particular, cash net interest payments are distinct from accrual expense.
latest <- which(sources$fy == '2024-25')
stopifnot(length(latest) == 1L)
raw <- readLines(file.path(args[2], 'fbo-2024-25.txt'), warn=FALSE, encoding='UTF-8')
page <- cumsum(nchar(raw) - nchar(gsub('\\f', '', raw))) + 1L
hits <- grep('^Table B\\.4: Australian Government general government sector net debt and net', raw)
stopifnot(length(hits) == 2L)
historical_cells <- list(); historical_tables <- list(); cursor <- 0L
for(part in seq_along(hits)) {
  start <- hits[part]
  next_page <- which(seq_along(raw) > start & grepl('\\f', raw))
  end <- if(length(next_page)) next_page[1L]-1L else length(raw)
  block <- raw[start:end]
  header <- paste(block[seq_len(min(5L,length(block)))], collapse=' ')
  stopifnot(grepl('Net debt', header), grepl('Net interest payments', header),
            length(gregexpr('Per cent of GDP', header, fixed=TRUE)[[1L]]) == 2L)
  row_at <- grep('^(?:19|20)[0-9]{2}-[0-9]{2}\\s', block, perl=TRUE)
  for(column in seq_len(4L)) {
    family <- if(column<=2L) 'net_debt_components' else 'cash_interest'
    template <- tables[[latest]][1L,,drop=FALSE]
    template$family_id <- family
    template$family_label <- if(column<=2L) 'Net debt components' else 'Cash interest'
    template$table_instance_id <- paste0(template$release_id,'--',family,'--b4')
    template$source_table <- 'Table B.4: Australian Government general government sector net debt and net interest payments'
    template$source_page <- page[start]; template$table_part <- part
    template$raw_text <- paste(block,collapse='\n')
    if(column%%2L==1L) historical_tables[[length(historical_tables)+1L]] <- template
    for(row in seq_along(row_at)) {
      line <- block[row_at[row]]
      pattern <- paste0('^((?:19|20)[0-9]{2}-[0-9]{2})',
                        paste(rep('\\s+(-?[0-9,]+(?:\\.[0-9]+)?)',4L),collapse=''), '\\s*$')
      tokens <- regmatches(line,regexec(pattern,line,perl=TRUE))[[1L]]
      if(length(tokens)!=6L) stop('Unexpected FBO B.4 historical row: ',line)
      cell <- out[[latest]][1L,,drop=FALSE]
      for(field in intersect(names(template),names(cell))) cell[[field]] <- template[[field]]
      cell$row_index <- row; cell$row_depth <- 0L; cell$column_index <- column
      cell$series <- cell$row_label <- cell$row_path <- if(column<=2L) 'Net debt' else 'Net interest payments'
      cell$series_id <- builder$canonical_series_id(cell$series)
      cell$column_label <- if(column%%2L==1L) '$m' else 'Per cent of GDP'
      cell$value_text <- tokens[column+2L]; cell$value <- builder$parse_number(cell$value_text)
      cell$unit <- if(column%%2L==1L) 'AUD million' else '% of GDP'
      cell$unit_multiplier <- if(column%%2L==1L) 1e6 else 0.01
      cell$accounting_basis <- if(column<=2L) 'accrual' else 'cash'
      cell$consolidation <- 'general_government_sector'
      cell$fiscal_year <- tokens[2L]; cell$calendar_year <- NA_integer_
      cell$period_end <- builder$period_info(tokens[2L],as.Date(NA))$period_end
      cell$period_type <- 'fiscal_year'; cell$series_eligible <- TRUE
      cell$actual_forecast <- cell$profile_type <- 'actual'; cell$raw_line <- line
      cursor <- cursor+1L; historical_cells[[cursor]] <- cell
    }
  }
}
historical <- do.call(rbind,historical_cells)
stopifnot(nrow(historical)==220L, min(historical$period_end)==as.Date('1971-06-30'),
          max(historical$period_end)==as.Date('2025-06-30'))
out[[length(out)+1L]] <- historical
tables[[length(tables)+1L]] <- do.call(rbind,historical_tables)
observations <- do.call(rbind,out)
observations$is_preferred <- TRUE
if(!nrow(observations)||any(!observations$series_eligible)||any(observations$actual_forecast!='actual'))stop('Invalid actual supplement')
saveRDS(list(schema='treasury_actual_supplement_v1',sources=do.call(rbind,ledger),tables=do.call(rbind,tables),observations=observations),args[3],version=3,compress='xz')
