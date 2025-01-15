library(data.table)
library(magrittr)
library(eurodata)
library(collapse)
library(stringr)
library(openxlsx2)


# Functions and constants -------------------------------------------------

rename_with_mod_time <- function(file_path, time_format="%Y-%m-%d %H.%M.%S") {
  modification_time <- 
    file.info(file_path)$mtime
  time_str <- 
    format(modification_time, time_format)
  new_file_path <-
    sub("(.*)(\\.[a-zA-Z0-9]+)$", paste0("\\1 ",time_str,"\\2"), file_path)
  success <- 
    file.rename(file_path, new_file_path)
  if (success) new_file_path else
    stop("Failed to rename the file.")
}

modifyRows <- function(dt, rowfilter, expr)
  eval(bquote({
    . <- dt[(.(substitute(rowfilter)))] # to make it compatible with %>%
    dt[(.(substitute(rowfilter)))] <- 
      dt[(.(substitute(rowfilter)))][,.(substitute(expr))]
    dt
  }))

sanitiseFilename <- function(string)
  string %>%
  # Replace reserved characters with an underscore
  gsub('[<>:"/\\|?*]', '_', .) %>%
  # Remove trailing spaces or periods
  gsub('[ .]$', '', .) %>%
  # Append an underscore if it matches reserved names
  ifelse(. %in% c('CON', 'PRN', 'AUX', 'NUL', 'COM1', 'COM2', 'COM3', 'COM4', 
                  'COM5', 'COM6', 'COM7', 'COM8', 'COM9', 'LPT1', 'LPT2', 'LPT3', 
                  'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9'),
         paste0(., '_'),.) %>%
  # Ensure filename doesn't exceed max length
  substr(1, 255)

isNotNA <- Negate(is.na)

EU_Members_geo_names <-
  importLabels('geo') %>% 
  as.data.table() %>% 
  .[, geo_labels := geo_labels %>%
      sub('-','\u2013',.,fixed=TRUE) %>% 
      sub('\u00e2\u20AC\u201C','\u2013',.,fixed=TRUE)] %>%
  .[, lapply(.,as.character)] %>% 
  .[geo %in% c(EU_Members_geo_codes,EU_geo_code,EA_geo_code)]

OUTPUT_FOLDER <-
  Sys.time() %>% 
  format("%Y-%m-%d %H:%M:%S") %>% 
  paste('Social Scoreboard output',.) %>% 
  gsub(':','.',.,fixed=TRUE)

reportProblem <- function(message.) 
  function(x) { # x = error or warning
    message('Something went wrong:\n',x)
    stop(message.,call.=FALSE)
  }

createFolder <- function(folder_name) {
  message('\nCreating a new output directory/folder:\n',
          paste0(getwd(),'/',folder_name))
  reportProblem('Folder not created!') %>% 
    tryCatch(dir.create(folder_name),
             error = .,
             warning = .)
}

toJSON. <- function(x, filename)
  x %>% 
  serialize(NULL) %>% 
  memCompress() %>% 
  serializeJSON() %>%
  cat(file=filename)

volatility <- function(x,y) {
  lowess. <- lowess(x,y,1)
  vol <- 1 - cor(y,lowess.$y)
  y_deviations <- abs(y - lowess.$y)
  outl <- y_deviations > 1.5*median(y_deviations)
  # plot(x,y)
  # lines(lowess.)
  list(vol, outl)
}

checkFlags <- function(dt, flag)
  dt[, mget(grep('flags_',colnames(dt),value=TRUE))] %>% 
  Reduce(x = .,
         init = logical(nrow(dt)),
         f = function(i,x) i | grepl(flag,x))

MIN_NUMBER_OF_COUNTRIES <- 18L

CURRENT_YEAR <-
  Sys.Date() %>% 
  substr(1,4) %>% 
  as.integer()

inRange <- function(x,bottom,top)
  bottom < x & x <= top

QUALITY_CHECKS_FUNCTIONS <-
  list(
    \(dt) dt[, paste0('Latest year considered (for which the number of available Member States ≥ ',
                      MIN_NUMBER_OF_COUNTRIES,')') := max(time)
             , by=INDIC_NUM],
    \(dt) dt[, `Old data` := max(time) < CURRENT_YEAR - 3
             , by=.(INDIC_NUM,geo)],
    \(dt) dt[, `Very old data` := max(time) < CURRENT_YEAR - 5
             , by=.(INDIC_NUM,geo)],
    \(dt) dt[, `One time point only` := length(unique(time))==1
             , by=.(INDIC_NUM,geo)],
    \(dt) dt[, `Two time points only` := length(unique(time))==2
             , by=.(INDIC_NUM,geo)],
    \(dt) dt[, `Three time points only` := length(unique(time))==3
             , by=.(INDIC_NUM,geo)],
    \(dt) dt[, `5 or more countries missing across all years` := 
               length(unique(geo)) <= length(EU_Members_geo_codes)-5
             , by=INDIC_NUM],
    \(dt) dt[, `All large countries missing across all years` :=
               length(large_EU_Members_geo_codes %without% geo) == length(large_EU_Members_geo_codes)
             , by=INDIC_NUM],
    \(dt) dt[, `One or more large countries missing across all years` :=
               length(large_EU_Members_geo_codes %without% geo) > 0
             , by=INDIC_NUM],
    \(dt) dt[, `5 or more countries missing in the latest year considered` := 
               length( geo[time==max(time)] ) <= length(EU_Members_geo_codes)-5
             , by=INDIC_NUM],
    \(dt) dt[, `All large countries missing in the latest year considered` :=
               length(large_EU_Members_geo_codes %without% geo[time==max(time)]) == length(large_EU_Members_geo_codes)
             , by=INDIC_NUM],
    \(dt) dt[, `One or more large countries missing in the latest year considered` :=
               length(large_EU_Members_geo_codes %without% geo[time==max(time)]) > 0
             , by=INDIC_NUM],
    \(dt) dt[, `No EU aggregate` := EU_geo_code %not in% geo
             , by=INDIC_NUM],
    \(dt) dt[, `No EU aggregate for the last time point` := EU_geo_code %not in% geo[time==max(time)]
             , by=INDIC_NUM],
    \(dt) dt[, c('Volatility of time series (the higher, the more volatile)','Is an outlier') :=
               volatility(time, value_)
             , by=.(INDIC_NUM,geo)],
    \(dt) dt[, `Break in time series (flag 'b')` := checkFlags(dt,'b')],
    \(dt) dt[, `Unreliable (flag 'u')` := checkFlags(dt,'u')],
    \(dt) dt[, `Not significant (flag 'n')` := checkFlags(dt,'n')]
  )

qualityChecksTable <- function(SCOREBOARD_GRAND_TABLE)
  SCOREBOARD_GRAND_TABLE %>% 
  copy() %>% 
  .[, time := as.integer(time)] %>% 
  .[!is.na(value_)] %>% 
  .[geo %in% c(EU_geo_code,EU_Members_geo_codes)] %>% 
  .[CURRENT_YEAR - 10 <= time] %>% 
  .[length(value_)>=MIN_NUMBER_OF_COUNTRIES,
    by=.(INDIC_NUM,time)] %>% 
  Reduce(init = .,
         f = function(dt,x) x(dt),
         x = QUALITY_CHECKS_FUNCTIONS
  ) %>% 
  .[, c('INDIC_NUM','geo','time','value_',
        colnames(.) %without% colnames(SCOREBOARD_GRAND_TABLE))
    , with=FALSE]


# Actions -----------------------------------------------------------------

if (exists('DEVMODE') && DEVMODE) { # development mode -- restoring pre-calculated SCOREBOARD_INDICATORS from disk
  message('\n\U1F537 DEVMODE=TRUE -- restoring SCOREBOARD_INDICATORS from SCOREBOARD_INDICATORS.Rds')
  SCOREBOARD_INDICATORS <-
    readRDS('SCOREBOARD_INDICATORS.Rds')
} else {
  if (file.exists('SCOREBOARD_INDICATORS.Rds')) {
    if (!exists('SCOREBOARD_INDICATORS'))
      stop('\n\U274C Object `SCOREBOARD_INDICATORS` not found!\n',
           'Maybe you forgot to set DEVMODE=TRUE ?')
    message('\nRenaming/archiving the exisitng/old\nSCOREBOARD_INDICATORS.Rds -> ',
            appendLF=FALSE)
    message(rename_with_mod_time('SCOREBOARD_INDICATORS.Rds'))
  }
  message('\nSaving new SCOREBOARD_INDICATORS.Rds...')
  SCOREBOARD_INDICATORS %>%
    saveRDS('SCOREBOARD_INDICATORS.Rds')
}

# POP_WEIGHTS <-
#   rbind(
#     retry(memoised_importData(
#       'lfsa_pganws',
#       with_filters(wstatus='POP',unit='THS_PER',
#                    citizen=unique(IndicatorsWithPopulationWeigths$citizen),
#                    geo=c(EU_Members_geo_codes,EU_geo_code,EA_geo_code)))) %>% 
#       as.data.table() %>% 
#       .[, c('wstatus','unit','freq','flags_') := NULL],
#     retry(memoised_importData(
#       'lfsa_pgaed',
#       with_filters(unit='THS_PER',
#                    isced11=unique(IndicatorsWithPopulationWeigths$isced11),
#                    geo=c(EU_Members_geo_codes,EU_geo_code,EA_geo_code)))) %>% 
#       as.data.table() %>% 
#       .[, c('unit','freq','flags_') := NULL],
#     fill=TRUE
#   ) %>% 
#   .[, lapply(.,. %>% `if`(is.factor(.),as.character(.),.))] %>% 
#   .[, time := TIME_PERIOD %>% as.integer()] %>% # year
#   .[, TIME_PERIOD := NULL] %>% 
#   .[, isced11 := isced11 %>% ifelse(is.na(.),'TOTAL',.)] %>% # filling in isced11 for lfsa_pganws
#   .[, citizen := citizen %>% ifelse(is.na(.),'TOTAL',.)] %>% # filling in citizen for lfsa_pgaed
#   .[, is_total := 
#       sex=="T" & age=="Y20-64" & isced11=='TOTAL' & citizen=='TOTAL'] %>% 
#   .[, total := ifelse(is_total, value_, NA_real_)] %>%
#   .[, total := mean(total, na.rm=TRUE), by=.(geo,time)] %>% 
#   .[, popweight := value_/total] %>% 
#   .[, c('value_','is_total','total') := NULL] %>%
#   merge(IndicatorsWithPopulationWeigths, 
#         by=colnames(IndicatorsWithPopulationWeigths) %without% 'INDIC_NUM') %>% 
#   .[, .(INDIC_NUM,geo,time,popweight)]

SCOREBOARD_NAMES_DESCRIPTIONS <-
  SCOREBOARD_INDICATORS %>% 
  names() %>% 
  lapply(\(x) data.table(INDIC_NUM = x,
                         name = SCOREBOARD_INDICATORS[[x]]$name, 
                         chapter = SCOREBOARD_INDICATORS[[x]]$chapter, 
                         group = SCOREBOARD_INDICATORS[[x]]$group,
                         type = SCOREBOARD_INDICATORS[[x]]$type,
                         url = SCOREBOARD_INDICATORS[[x]]$url,
                         high_is_good = SCOREBOARD_INDICATORS[[x]]$high_is_good,
                         reference_in_scores = SCOREBOARD_INDICATORS[[x]]$reference_in_scores)) %>% 
  rbindlist()

SCOREBOARD_INDIC_NUM__reference_name <- 
  SCOREBOARD_NAMES_DESCRIPTIONS %>% 
  .[,.(INDIC_NUM, reference_in_scores)] %>% 
  .[, reference_name := toupper(reference_in_scores)] %>% 
  .[, reference_in_scores := NULL]

message('\nPreparing SCOREBOARD_GRAND_TABLE...')
SCOREBOARD_GRAND_TABLE <-
  SCOREBOARD_INDICATORS %>% 
  names() %>%
  lapply(function(x)
    SCOREBOARD_INDICATORS[[x]]$value %>% 
      .[, INDIC_NUM:=x] %>% 
      .[, high_is_good := SCOREBOARD_INDICATORS[[x]]$high_is_good] %>% 
      setcolorder(c('INDIC_NUM','high_is_good'))
  ) %>% rbindlist(fill=TRUE) %>% 
  .[, grep('^(INDIC_NUM|high_is_good|geo|time|value_|flags_.*|.)$',
           colnames(.),value=TRUE),
    with=FALSE] %>% 
  setcolorder(c('INDIC_NUM','geo','time','value_','flags_',
                grep('^.$',colnames(.),value=TRUE),
                grep('^flags_.$',colnames(.),value=TRUE)))

message('Calculating lags...')
SCOREBOARD_LAGS_DIFFS <-
  SCOREBOARD_GRAND_TABLE %>%
  .[, .(INDIC_NUM,geo,time,high_is_good,value_,flags_)] %T>% 
  {if (nrow(.)!=nrow(unique(.[,.(INDIC_NUM,geo,time)]))) {
    View(.[duplicated(.[,.(INDIC_NUM,geo,time)])])
    stop('\n`INDIC_NUM`, `geo`, `time` do not uniquely identify the rows in `SCOREBOARD_GRAND_TABLE`!\n',
         'The offending rows are shown in data viewer.')
  }} %>%
  .[, time := as.integer(time)] %>% 
  .[, value_ := as.numeric(value_)] %>% 
  .[isNotNA(value_)] %>% 
  .[geo %in% c(EU_Members_geo_codes,EU_geo_code,EA_geo_code)] %>% 
  # .[, sufficiently_many_countries :=
  #     value_[geo %in% EU_Members_geo_codes] %>% 
  #     {length(.)>=MIN_NUMBER_OF_COUNTRIES}
  #   , by=.(INDIC_NUM,time)] %>% 
  # .[, latest_year_overall :=
  #     suppressWarnings(max(time[sufficiently_many_countries])) %>% # suppressed warning if time[sufficiently_many_countries] is empty i.e. -> max = -Inf
  #     ifelse(is.infinite(.), NA_integer_, .)
  #   , by=INDIC_NUM] %>% 
  .[, latest_year_individual :=
      time %>% 
      .[isNotNA(.)] %>% 
      max()
    , by=.(INDIC_NUM,geo)] %>% 
  .[, previous_year :=
      time %>% 
      .[isNotNA(.) & .<latest_year_individual] %>% 
      max()
    , by=.(INDIC_NUM,geo)] %>% 
  .[, previous_year_2 :=
      time %>% 
      .[isNotNA(.) & .<previous_year] %>% 
      max()
    , by=.(INDIC_NUM,geo)] %>% 
  .[, latest_value := value_[time==latest_year_individual]
    , by=.(INDIC_NUM,geo)] %>% 
  .[, previous_value := value_[time==previous_year]
    , by=.(INDIC_NUM,geo)] %>%
  .[, previous_value_2 := value_[time==previous_year_2]
    , by=.(INDIC_NUM,geo)] %>%
  .[, change := latest_value - previous_value] %>% 
  .[, Diff_EU := latest_value - mean(latest_value[geo %in% EU_Members_geo_codes],
                                     na.rm=TRUE)
    , by=.(INDIC_NUM)] %>% 
  .[, Diff_MSEU := change - mean(change[geo %in% EU_Members_geo_codes],
                                 na.rm=TRUE)
    , by=.(INDIC_NUM)]

# Diff_EU = deviations of latest level from avg(latest level)
# Diff_MSEU = deviations of timediff from avg(timediff)


message('Calculating scores...')
SCOREBOARD_SCORES <-
  SCOREBOARD_LAGS_DIFFS %>%
  .[time==latest_year_individual] %>% 
  .[, .SD[!(time < round(mean(time))-1)], by=INDIC_NUM] %>% # drop too old years for some countries
  melt(id.vars=c('INDIC_NUM','geo','time','high_is_good','flags_'),
       measure.vars=c('latest_value','change'),
       variable.name="variable", value.name="value",
       na.rm=TRUE) %>%
  # .[!(INDIC_NUM %in% SCOREBOARD_NAMES_DESCRIPTIONS[!(calculate_score_change), INDIC_NUM] & 
  #       variable=='change')] %>%
  # .[!(INDIC_NUM %in% SCOREBOARD_NAMES_DESCRIPTIONS[!(calculate_score_change_with_break_in_series), INDIC_NUM] & 
  #       variable=='change' &
  #       grepl('b',flags_))] %>% 
  # merge(INDIC_NUM__reference_name, by='INDIC_NUM') %>% 
  # .[, reference_name := 
  #     reference_name %>% 
  #     ifelse(.==EU_geo_code & length(value[geo==EU_geo_code & time==max(time)])==0, # EU wanted but not available for the latest year
  #            names(LIST_OF_REFERENCE_POINT_FUNCTIONS)[2], # fall-back option (currently 'SIMPLE AVERAGE')
  #            .)
  #   , by=.(INDIC_NUM, variable)] %>% 
  # .[, reference :=
  #     LIST_OF_REFERENCE_POINT_FUNCTIONS[[unique(reference_name)]](value, geo)
  #   , by=.(INDIC_NUM, variable)] %>% 
  # .[, reference_time :=
  #     LIST_OF_REFERENCE_POINT_FUNCTIONS[[unique(reference_name)]](time, geo, is_time=TRUE) %>% 
  #     as.character()
  #   , by=.(INDIC_NUM, variable)] %>% 
  .[, reference := mean(value[geo %in% EU_Members_geo_codes]),
    , by=.(INDIC_NUM, variable)] %>% 
  .[, std := sd(value[geo %in% EU_Members_geo_codes]),
    , by=.(INDIC_NUM, variable)] %>% 
  .[, score := -1.019049* # rescaling to make it compatible with the Python results
      ifelse(high_is_good,1,-1)*
      (value - reference)/std] %>% 
  .[, t1 := reference - std] %>% 
  .[, t2 := reference - std/2] %>% 
  .[, t3 := reference + std/2] %>% 
  .[, t4 := reference + std] %>% 
  # .[, is_popweighted := FALSE] %>% 
  # rbind(.[INDIC_NUM %in% IndicatorsWithPopulationWeigths$INDIC_NUM] %>%  # duplicate the selected indicators to create the population-weighted versions
  #         .[, is_popweighted := TRUE] %>% 
  #         merge(POP_WEIGHTS, by=c('INDIC_NUM','geo','time')) %>% 
  #         .[, score := score * popweight] %>% 
  #         .[, popweight := NULL]) %>%
  dcast(INDIC_NUM + geo + time + flags_ + high_is_good ~ variable,
        value.var=c('value','score','reference','std','t1','t2','t3','t4'),
        fun.aggregate=identity,
        fill=NA) %>% 
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
  # .[, comment := time %>% 
  #     {paste0(.,' for latest_value; ',.,' minus ',.-1,' for change',
  #             ifelse(grepl('b',flags_),
  #                    ' (but there is a break in time series!)',"")
  #     )}] %>% 
  setnames('score_latest_value','score1_L') %>% 
  setnames('score_change','score2_D') %>% 
  setorder(INDIC_NUM,geo,time) # %>% 
# parseINDIC_NUM() %>% 
# merge(QuantAssessmentDescriptions,
#       all.x=TRUE, # needed to keep those indics for which change is not calculated
#       by=c('score_category_latest_value','score_category_change')) %>% 
# merge(EU_Members_geo_names,
#       by='geo') %>% 
# merge(SCOREBOARD_NAMES_DESCRIPTIONS,
#       by='INDIC_NUM') %>% 
# .[, Description := paste0(name,', ',unit) %>% 
#     ifelse(is_popweighted, paste0(.,', population-weighted'), .)] %>% 
# .[, INDIC_NUM := ifelse(is_popweighted,paste0(INDIC_NUM,'_popweighted_score'),
#                       INDIC_NUM)]

createFolder(OUTPUT_FOLDER)

# message('\nGenerating `Quality Checks.xlsx`...')
# QCT <- qualityChecksTable(SCOREBOARD_GRAND_TABLE)
# wb_workbook() %>% 
#   wb_add_worksheet("Scoreboard quality checks", zoom=75) %>%
#   wb_add_data(x=QCT) %>% 
#   wb_add_font(dims=paste0('A1:',int2col(ncol(QCT)),'1'), bold="bold") %>% 
#   wb_add_cell_style(dims=paste0('A1:',int2col(ncol(QCT)),'1'), wrap_text=TRUE) %>% 
#   wb_set_col_widths(cols=1:ncol(QCT), widths=12) %>%
#   wb_set_row_heights(rows=1, heights=107) %>% 
#   wb_freeze_pane(first_row=TRUE) %>% 
#   wb_add_filter(rows=1, cols=1:ncol(QCT)) %>% 
#   wb_save(paste0(OUTPUT_FOLDER,'/Quality Checks.xlsx'))

# message('\nPreparing the data.Rds file for the Shiny/Shinylive app...')
# if (!dir.exists('../JAF2R_shinylive')) createFolder('../JAF2R_shinylive')
# list(SCOREBOARD_INDICATORS=SCOREBOARD_INDICATORS,
#      SCOREBOARD_GRAND_TABLE_reduced = SCOREBOARD_GRAND_TABLE %>% 
#        .[isNotNA(.$value_) & 
#            .$geo %in% c(EU_Members_geo_codes,EU_geo_code,EA_geo_code)
#          , c('INDIC_NUM','geo','time','value_','high_is_good',
#              grep('flags_',colnames(.),value=TRUE)), with=FALSE] %>% 
#        .[, all_flags := # collapse all flags_ columns into one column
#            do.call(paste0,c(mget(grep('flags_',colnames(.),value=TRUE)))) %>% 
#            gsub('NA',"",.,fixed=TRUE) %>% gsub(':',"",.,fixed=TRUE)] %>% 
#        .[, grep('^flags_.*$',colnames(.),value=TRUE) := NULL] %>% 
#        .[, time := as.integer(time)] %>% 
#        .[, value_change := 
#            if (length(value_)==1) NA_real_ else collapse::D(value_, t=time) 
#          , by=.(INDIC_NUM,geo)],
#      JAF_SCORES=JAF_SCORES,
#      SCOREBOARD_NAMES_DESCRIPTIONS=SCOREBOARD_NAMES_DESCRIPTIONS,
#      EU_Members_geo_names=EU_Members_geo_names,
#      EU_geo_code=EU_geo_code,
#      EA_geo_code=EA_geo_code) %T>% 
#   saveRDS('../JAF2R_shinylive/data/data.Rds') %T>% 
#   {toJSON(.) %>% cat(file='DATA.json')} # for the JAF PowerBI dashboard
