library(openxlsx2)
library(kit)

colourNameToInt <- function(colour_char_vec)
  colour_char_vec %>% 
  nswitch('green', 6L,
          'white', 5L,
          'red', 1L,
          'orange', 2L,
          'darkgreen', 7L,
          'yellow', 3L,
          'blue', 4L,
          NA_character_, 0L)

# Determine "cycle year" based on a May–April cycle
CycleYear <- local({
  date <- Sys.Date()
  # Extract numeric year and month from the date
  yr <- as.integer(format(date, "%Y"))
  mo <- as.integer(format(date, "%m"))
  # If month is April (4) or later, add 1 to the year
  if (mo >= 4) yr + 1  else yr
}) %>% as.character

source_excel_template <-
  wb_load('Social Scoreboard file4 TEMPLATE.xlsx')

template_indic_nums <-
  source_excel_template %>% 
  wb_to_df(dims='K:K', col_names=FALSE) %>% 
  as.data.table() %>% 
  setnames(new='INDIC_NUM') %>% 
  .[INDIC_NUM %>% isNotNA] %>% 
  .[, INDIC_NUM_order := .I]

ExcelRowsAndCols <-
  data.table(row =
               c(3,24,46,67,89,110) %>% 
               sapply(rep.int, 5) %>% 
               as.integer(),
             col =
               c(2,6,10,14,18) %>% 
               rep.int(6)) %>% 
  .[seq_along(EU_Members_geo_codes)] %>% 
  split(seq_len(nrow(.)))


HistoricalFile5 <-
  list.files(path='O:/European Semester – Coordination/02 Horizontal issues/02 Analytical tools/Social Scoreboard',
             pattern="file ?5.*\\.xlsx$",
             full.names=TRUE) %>% 
  grep('^(?!.*\\/~).*', ., perl=TRUE, value=TRUE) %T>% # ignoring those with /~ (hidden temp files)
  {stopifnot('None or more than 1 historical File 5 found!' = length(.)==1)} %>% 
  wb_load()

oldestYearColName <- function(dt)
  colnames(dt) %>% 
  {suppressWarnings(as.integer(.))} %>% 
  min(na.rm=TRUE) %>% 
  as.character()

HistoricalFile5Contents <-
  lapply(ExcelRowsAndCols,
         function(dt)
           wb_to_df(file=HistoricalFile5,
                    rows=dt$row %>% seq(from=.,to=.+nrow(template_indic_nums)),
                    cols=dt$col %>% seq(from=.,to=.+2),
                    col_names=TRUE) %>% 
           as.data.table() %>% 
           .[, INDIC_NUM := template_indic_nums$INDIC_NUM] %>% 
           .[, geo := wb_to_df(file=HistoricalFile5,
                               rows=dt$row-1,
                               cols=dt$col,
                               col_names=FALSE) %>% 
               as.character()] %>% 
           {`if`(CycleYear %in% colnames(.),
                 .[, (CycleYear) := NULL],
                 .[, oldestYearColName(.) := NULL])})


Reduce(init=HistoricalFile5,
       x=seq_len(length(HistoricalFile5Contents)),
       f=function(wb,n)
       HistoricalFile5Contents[[n]] %T>% 
         {cat(unique(.$geo),"")} %>% 
         merge(SCOREBOARD_SCORES %>% 
                 .[, .SD[time==round(mean(time))], by=INDIC_NUM] %>% # pick the dominating year across countries
                 .[,.(INDIC_NUM,geo,colour_group)],
               by=c('INDIC_NUM','geo'), all.x=TRUE) %>% 
         .[, (CycleYear) := colour_group %>% colourNameToInt] %>% 
         setorder(INDIC_NUM) %>% 
         .[, c('INDIC_NUM','geo','colour_group') := NULL] %>% 
         setcolorder(sort(colnames(.))) %>% 
         wb_add_data(wb=wb, sheet=1,
                     start_col=ExcelRowsAndCols[[n]]$col,
                     start_row=ExcelRowsAndCols[[n]]$row)
) %>% 
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 5.xlsx'))
