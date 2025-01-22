library(data.table)
library(magrittr)
library(openxlsx2)


TimeStamp <-
  # Sys.time() %>% 
  # format("%Y-%m-%d %H:%M:%S") %>% 
  # gsub(':','.',.,fixed=TRUE)
  OUTPUT_FOLDER %>%
  sub('Social Scoreboard output ',"",.,fixed=TRUE)

makePairs <- function(x)
  Map(c, x[-length(x)], x[-1])

message('\nImporting past runs and merging...')

PastRuns <-
  list.files(pattern='^Scoreboard run .+\\.Rds$') %>% 
  `if`(length(.)>0,
       sort(.) %>% rev %>% 
         lapply(\(x) {cat(x,sep='\n'); readRDS(x)}) %>% 
         Reduce(x=.,
                f=\(dt1,dt2) {
                  cat('merging...\n')
                  merge(dt1, dt2, all=TRUE,
                        by=c('INDIC_NUM','time','geo'))
                }),
       .)

message('\nSaving the current run...')

`%not equal%` <- function(x,y)
  ifelse(is.na(x) | is.na(y),
         is.na(x) & !is.na(y) | !is.na(x) & is.na(y),
         x != y)

compareColumns <- function(dt, prefix)
  Reduce(init=dt,
         x=colnames(dt) %>% grep(prefix,.,value=TRUE) %>% makePairs,
         f=function(dt, col_name_pair)
           dt[, paste('Change in',col_name_pair[1],'compared to',col_name_pair[2]) :=
                get(col_name_pair[1]) %not equal% get(col_name_pair[2])])

CurrentRun <-
  SCOREBOARD_GRAND_TABLE %>% 
  copy %>% 
  .[, time := as.integer(time)] %>% 
  .[time > 2000L] %>% 
  .[, .(INDIC_NUM,time,geo,value_,flags_)] %>% 
  setorder(INDIC_NUM,time,geo) %>% 
  setnames(c('value_','flags_'),
           c('value','flags') %>% paste(TimeStamp)) %T>% 
  saveRDS(paste0('Scoreboard run ',TimeStamp,'.Rds'))



if (is.data.table(PastRuns)) {
  
  message('Merging current run with past runs\n',
          'and saving the comparison...\n')
  
  dta <-
    CurrentRun %>% 
    merge(PastRuns, all=TRUE,
          by=c('INDIC_NUM','time','geo')) %>% 
    merge(SCOREBOARD_NAMES_DESCRIPTIONS[,.(INDIC_NUM,name,type)],
          by='INDIC_NUM', all=TRUE) %>% 
    compareColumns('^value') %>% 
    compareColumns('^flags') %>% 
    setcolorder(c('INDIC_NUM','name','type','time','geo')) %>% 
    setorder(INDIC_NUM,time,geo) %>% 
    setnames(\(x) sapply(x, . %>% strwrap(width=5) %>% paste(collapse='\n')))
  
  message('Formatting and saving the Excel file...')
  
  write_xlsx(x=dta, zoom=85, sheet=TimeStamp, col_widths=12,
             first_active_row=2, first_active_col=6,
             na.strings="") %>% 
    wb_set_row_heights(rows=1, heights=150) %>% 
    wb_add_filter(rows=1, cols=1:ncol(dta)) %>% 
    wb_add_cell_style(dims=paste0('A1:',int2col(ncol(dta)),'1'),
                      wrap_text=TRUE) %>% 
    wb_set_col_widths(cols=2, widths=55) %>% 
    wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard runs compared.xlsx'))
}

