library(data.table)
library(magrittr)
library(openxlsx2)

TimeStamp <-
  OUTPUT_FOLDER %>% 
  sub('Social Scoreboard output ',"",.,fixed=TRUE)

PastRuns <-
  list.files(pattern='^Scoreboard run .+\\.Rds$') %>% 
  `if`(length(.)>0,
       sort(.) %>% rev %>% 
         lapply(readRDS) %>% 
         Reduce(x=.,
                f=\(dt1,dt2) merge(dt1,dt2, all=TRUE,
                                   by=c('INDIC_NUM','time','geo'))),
       .)

CurrentRun <-
  SCOREBOARD_GRAND_TABLE %>% 
  copy %>% 
  .[, high_is_good := NULL] %>% 
  merge(SCOREBOARD_NAMES_DESCRIPTIONS[,.(INDIC_NUM,name)],
        by='INDIC_NUM') %>% 
  setcolorder(c('INDIC_NUM','name','time','geo','value_','flags_')) %>% 
  setorder(INDIC_NUM,name,time,geo) %>% 
  setnames(c('value_','flags_'),
           c('value','flags') %>% paste(TimeStamp)) %T>% 
  saveRDS(paste0('Scoreboard run ',TimeStamp,'.Rds'))

if (is.data.table(PastRuns))
  CurrentRun %>% 
  merge(PastRuns, all=TRUE,
        by=c('INDIC_NUM','time','geo')) %>% 
  setcolorder(c('INDIC_NUM','name','time','geo')) %>% 
  setorder(INDIC_NUM,name,time,geo) %>% 
  write_xlsx(paste0(OUTPUT_FOLDER,'/Social Scoreboard runs compared.xlsx'),
             zoom=85, sheet=TimeStamp, widths='auto',
             first_active_row=1, first_active_col=5,
             with_filter = rep.int(TRUE,4))
