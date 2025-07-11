library(openxlsx2)
library(kit)

# Determine "cycle year" based on a May–April cycle
CycleYear <- local({
  date <- Sys.Date()
  # Extract numeric year and month from the date
  yr <- as.integer(format(date, "%Y"))
  mo <- as.integer(format(date, "%m"))
  # If month is April (4) or later, add 1 to the year
  if (mo >= 4) yr + 1  else yr
}) %>% as.character

timeNstepsSmaller <- function(time., n.) {
  stopifnot(length(n.)==1,
            is.numeric(n.),
            n.==round(n.),
            n.>0)
  Reduce(init=time.,
         x=1:n.,
         f=function(t,x) t[t<max(t)]) %>% 
    max()
}



irregular_indics <-
  SCOREBOARD_NAMES_DESCRIPTIONS %>% 
  .[!(is_regular_annual_timeseries), INDIC_NUM]

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

scoresForTminus <- function(N) {
  message('Calculating lags for N=',N,'...')
  SCOREBOARD_LAGS_DIFFS. <-
    SCOREBOARD_GRAND_TABLE %>%
    .[, .(INDIC_NUM,geo,time,high_is_good,change_in_percent,value_,flags_)] %T>% 
    {if (nrow(.)!=nrow(unique(.[,.(INDIC_NUM,geo,time)]))) {
      View(.[duplicated(.[,.(INDIC_NUM,geo,time)])])
      stop('\n`INDIC_NUM`, `geo`, `time` do not uniquely identify the rows in `SCOREBOARD_GRAND_TABLE`!\n',
           'The offending rows are shown in data viewer.')
    }} %>%
    .[, time := as.integer(time)] %>% 
    .[, value_ := as.numeric(value_)] %>% 
    .[isNotNA(value_)] %>% 
    .[geo %in% c(EU_Members_geo_codes,EU_geo_code,EA_geo_code)] %>% 
    .[, sufficiently_many_countries :=
        value_[geo %in% EU_Members_geo_codes] %>%
        {length(.)>=MIN_NUMBER_OF_COUNTRIES}
      , by=.(INDIC_NUM,time)] %>%
    .[, latest_year_overall :=
        suppressWarnings(max(time[sufficiently_many_countries])) %>% # suppressed warning if time[sufficiently_many_countries] is empty i.e. -> max = -Inf
        ifelse(is.infinite(.), NA_integer_, .)
      , by=INDIC_NUM] %>%
    .[, latest_year_individual :=
        time %>% 
        .[isNotNA(.) & isNotNA(value_)] %>% 
        max()
      , by=.(INDIC_NUM,geo)] %>% 
    .[time <= latest_year_individual] %>% 
    .[, prevailing_latest_year := latest_year_overall
      - ifelse(INDIC_NUM %not in% irregular_indics, N, 0L)   # here N is used ❗❗❗ ❗❗❗ ❗❗❗
      , by=INDIC_NUM] %>%
    # .[, prevailing_latest_year := time[time<=latest_year_overall] %>% 
    #     timeNstepsSmaller(t,N)   # here N is used ❗❗❗ ❗❗❗ ❗❗❗
    #   , by=INDIC_NUM] %>% 
    # ❗❗❗ Irregular exception ❗❗❗:
    .[, prevailing_latest_year := prevailing_latest_year %>% 
        ifelse(N==2 & INDIC_NUM=='10040_ex4', # Share of individuals who have basic or above basic overall digital skills
               max(time[time<. & INDIC_NUM=='10040_ex4']),
               .)]  %>% 
    .[, prevailing_latest_year :=  
        ifelse(INDIC_NUM=='10610_ex61' &  # GDHI
                 prevailing_latest_year <= as.integer(CycleYear) - 4L,
               prevailing_latest_year + 1,
               prevailing_latest_year)] %>% 
    # .[time <= prevailing_latest_year] %>% 
    .[prevailing_latest_year %>% isNotNA(.)] %>% 
    setorder(INDIC_NUM,geo,time) %>% 
    .[, previous_year := time[time<prevailing_latest_year] %>% max(na.rm=TRUE)
      , by=.(INDIC_NUM)] %>% 
    .[, previous_year_2 := time[time<previous_year] %>% max(na.rm=TRUE)
      , by=.(INDIC_NUM)] %>% 
    .[, latest_value := value_[time==prevailing_latest_year] 
      , by=.(INDIC_NUM,geo)] %>% 
    .[, previous_value := value_[time==previous_year]
      , by=.(INDIC_NUM,geo)] %>%
    .[, previous_value_2 := value_[time==previous_year_2]
      , by=.(INDIC_NUM,geo)] %>%
    .[, change := 
        ifelse(!change_in_percent, latest_value - previous_value,
               100*(latest_value/previous_value - 1))] %>% 
    .[, Diff_EU := latest_value - mean(latest_value[geo %in% EU_Members_geo_codes],
                                       na.rm=TRUE)
      , by=.(INDIC_NUM)] %>% 
    .[, Diff_MSEU := change - mean(change[geo %in% EU_Members_geo_codes],
                                   na.rm=TRUE)
      , by=.(INDIC_NUM)]
  message('Calculating scores for N=',N,'...')
  SCOREBOARD_SCORES. <-
    SCOREBOARD_LAGS_DIFFS. %>% 
    .[time==prevailing_latest_year] %>% 
    # .[, .SD[!(time < round(mean(time))-1)], by=INDIC_NUM] %>% # drop too old years for some countries
    melt(id.vars=c('INDIC_NUM','geo','time','high_is_good','flags_'),
         measure.vars=c('latest_value','change'),
         variable.name="variable", value.name="value",
         na.rm=TRUE) %>% 
    .[, reference := mean(value[geo %in% EU_Members_geo_codes], na.rm=TRUE),
      , by=.(INDIC_NUM, variable)] %>% 
    .[, std := sd(value[geo %in% EU_Members_geo_codes], na.rm=TRUE),
      , by=.(INDIC_NUM, variable)] %>% 
    .[, score := -1.019049* # rescaling to make it compatible with the Python results
        ifelse(high_is_good,1,-1)*
        (value - reference)/std] %>% 
    .[, t1 := reference - std] %>% 
    .[, t2 := reference - std/2] %>% 
    .[, t3 := reference + std/2] %>% 
    .[, t4 := reference + std] %>% 
    dcast(INDIC_NUM + geo + time + flags_ + high_is_good ~ variable,
          value.var=c('value','score','reference','std','t1','t2','t3','t4'),
          fun.aggregate=identity,
          fill=NA) %>% 
    # ❗❗❗ Irregular exception ❗❗❗: Ignore changes, only use old levels
    `if`(N==2,
         .[, score_change := score_change %>% 
             ifelse(INDIC_NUM=='10040_ex4',0L,.)] %>% # Share of individuals who have basic or above basic overall digital skills
           .[, value_change := value_change %>% 
               ifelse(INDIC_NUM=='10040_ex4',0,.)],
         .) %>% 
    .[, colour_group :=
        kit::nif(
          # from Python's getException():
          score_latest_value %>% inRange(-0.5,0.5) & score_change>=1 &
            value_change>=0 & high_is_good, 'white',
          score_latest_value %>% inRange(-0.5,0.5) & score_change>=1 &
            value_change<0 & !high_is_good, 'white',
          score_latest_value %>% inRange(-1,-0.5) & score_change>=1 &
            value_change>=0 & high_is_good, 'green',
          score_latest_value %>% inRange(-1,-0.5) & score_change>=1 &
            value_change<0 & !high_is_good, 'green',
          score_latest_value<=-1 & score_change>=1 & 
            value_change>=0 & high_is_good, 'darkgreen',
          score_latest_value<=-1 & score_change>=1 & 
            value_change<0 & !high_is_good, 'darkgreen',
          # from Python's getColorGroup():
          score_latest_value>1 & score_change>-1, 'red',
          score_latest_value %>% inRange(0.5,1) & score_change>-1 |
            score_latest_value %>% inRange(-0.5,0.5) & score_change>1, 'orange',
          score_latest_value> 0.5 & score_change<=-1, 'yellow',
          score_latest_value<=-0.5 & score_change>1, 'blue',
          score_latest_value %>% inRange(-1,-0.5) & score_change<=1 |
            score_latest_value %>% inRange(-0.5,0.5) & score_change<=-1, 'green',
          score_latest_value<=-1 & score_change<=1, 'darkgreen',
          score_latest_value %>% inRange(-0.5,0.5) & score_change %>% inRange(-1,1), 'white'
        )] %>% 
    # ❗❗❗ Irregular exception ❗❗❗: ignore indicator 
    `if`(N==2,
         .[, colour_group := colour_group %>% 
             ifelse(INDIC_NUM=='10000_ex0',NA_character_,.)], # Adult participation in learning 
         .) %>% 
    setnames('score_latest_value','score1_L') %>% 
    setnames('score_change','score2_D') %>% 
    setorder(INDIC_NUM,geo,time)
  SCOREBOARD_SCORES.
}

fillInIrregularIndics <- function(SCOREBOARD_SCORES_dt) 
  SCOREBOARD_SCORES_dt %>% 
  .[INDIC_NUM %not in% irregular_indics] %>% 
  rbind(SCOREBOARD_SCORES[INDIC_NUM %in% irregular_indics])

SCOREBOARD_SCORES_list <-
  list('2'=scoresForTminus(2),
       '1'=scoresForTminus(1) %>% fillInIrregularIndics,
       '0'=SCOREBOARD_SCORES)

SCOREBOARD_SCORES_list_copy <-
  SCOREBOARD_SCORES_list %>% 
  lapply(copy)

File5db <-
  SCOREBOARD_SCORES_list_copy %>% 
  names %>% 
  lapply(\(x)
         SCOREBOARD_SCORES_list_copy[[x]] %>% 
           .[, `JER Year` :=
               as.integer(CycleYear) - as.integer(x)]) %>% 
  rbindlist() %>% 
  .[geo %in% EU_Members_geo_codes] %>% 
  merge(SCOREBOARD_NAMES_DESCRIPTIONS[type=='H',.(INDIC_NUM,name)],
        by='INDIC_NUM') %>% 
  setnames('name','Indicator') %>% 
  setnames('time','Data Year') %>% 
  .[, colour_num := colour_group %>% colourNameToInt] %>% 
  dcast(`JER Year` + INDIC_NUM + Indicator + `Data Year` ~ geo,
        value.var='colour_num', fun.aggregate=identity, fill=0L) %>% 
  .[, INDIC_NUM := NULL] %>% 
  setcolorder(c('JER Year','Indicator','Data Year',EU_Members_geo_codes)) %T>% 
  write_xlsx(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 5 database.xlsx'),
             sheet='Input Data', zoom=85)

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
  # list.files(path='O:/European Semester – Coordination/02 Horizontal issues/02 Analytical tools/Social Scoreboard',
  #            pattern="file ?5.*\\.xlsx$",
  #            full.names=TRUE) %>% 
  # grep('^(?!.*\\/~).*', ., perl=TRUE, value=TRUE) %T>% # ignoring those with /~ (hidden temp files)
  # {stopifnot('None or more than 1 historical File 5 found!' = length(.)==1)} %>% 
  'Social Scoreboard file5 TEMPLATE.xlsx' %>% 
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
       x=2:0,
       f=function(wb.,N) {
         new_col_name <-
           as.character(as.integer(CycleYear)-N)
         message('\nAdding column ',new_col_name,'...')
         Reduce(init=wb.,
                x=seq_len(length(HistoricalFile5Contents)),
                f=function(wb,n)
                  HistoricalFile5Contents[[n]] %T>% 
                  {cat(unique(.$geo),"")} %>% 
                  .[,.(INDIC_NUM, geo)] %>% 
                  merge(SCOREBOARD_SCORES_list[[as.character(N)]] %>% 
                          .[, .SD[time==round(mean(time))], by=INDIC_NUM] %>% # pick the dominating year across countries
                          .[,.(INDIC_NUM,geo,colour_group)],
                        by=c('INDIC_NUM','geo'), all.x=TRUE) %>% 
                  .[, (new_col_name) := colour_group %>% colourNameToInt] %>% 
                  setorder(INDIC_NUM) %>% 
                  .[, c('INDIC_NUM','geo','colour_group') := NULL] %>% 
                  setcolorder(sort(colnames(.))) %>% 
                  wb_add_data(wb=wb, sheet=1,
                              x=.,
                              start_col=ExcelRowsAndCols[[n]]$col - N + 2,
                              start_row=ExcelRowsAndCols[[n]]$row)
         )}
) %>% 
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 5.xlsx'))
