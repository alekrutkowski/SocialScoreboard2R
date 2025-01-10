library(openxlsx2)
library(kit)

scoreLevelToLabel <- function(score_level_value)
  -score_level_value %>% 
  {nif(.< -1.0,'5 Very low',
       .< -0.5,'4 Low',
       .<  0.5,'3 On average',
       .<  1.0,'2 High',
       .<  Inf,'1 Very High')}

scoreChangeToLabel <- function(score_change_value)
  -score_change_value %>% 
  {nif(.< -1.0,'5 Much lower than average',
       .< -0.5,'4 Lower than average',
       .<  0.5,'3 On average',
       .<  1.0,'2 Higher than average',
       .<  Inf,'1 Much higher than average')}

colourToNumberedColour <- function(colour_char_vec)
  colour_char_vec %>% 
  nswitch('green', '03 green',
          'white', '06 white',
          'red', '05 red',
          'orange', '04 orange',
          'darkgreen', '02 darkgreen',
          'yellow', '07 yellow',
          'blue', '01 blue')

wb_add_data__in_multiple_locations <- function(wb, sheet, dta., dims.)
  Reduce(init=wb,
         x=dims.,
         f=function(wb,x)
           wb_add_data(wb, sheet=sheet, x=dta., dims=x, col_names=FALSE))

# data for the colorful table
dta_list <-
  SCOREBOARD_SCORES %>% 
  merge(SCOREBOARD_NAMES_DESCRIPTIONS[,.(INDIC_NUM,type,name)],
        by='INDIC_NUM') %>% 
  .[type=='H'] %>% 
  .[!is.na(colour_group)] %>% 
  .[geo %not in% c(EU_geo_code,EA_geo_code)] %>% 
  .[, score_level_label := score1_L %>% scoreLevelToLabel] %>% 
  .[, score_change_label := score2_D %>% scoreChangeToLabel] %>% 
  .[, .(geos = unique(geo) %>% paste(collapse=', '))
    , by=.(INDIC_NUM, name, time, score_level_label, score_change_label)] %>% 
  dcast(INDIC_NUM + time + score_level_label ~ score_change_label,
        fill="", fun.aggregate=identity, value.var='geos') %>% 
  .[, .SD[time==max(time)], by=INDIC_NUM] %>% 
  .[, time := NULL] %>% 
  setorder(INDIC_NUM, score_level_label) %>% 
  split(by='INDIC_NUM', keep.by=FALSE)

years <-
  SCOREBOARD_SCORES %>% 
  merge(SCOREBOARD_NAMES_DESCRIPTIONS[,.(INDIC_NUM,type,name)],
        by='INDIC_NUM') %>% 
  .[type=='H'] %>% 
  .[!is.na(colour_group)] %>% 
  .[geo %not in% c(EU_geo_code,EA_geo_code)] %>% 
  .[, .SD[time==max(time)], by=INDIC_NUM] %>% 
  split(by='INDIC_NUM', keep.by=FALSE)

# data for the chart
dta_list_2 <-
  SCOREBOARD_SCORES %>% 
  merge(SCOREBOARD_NAMES_DESCRIPTIONS[,.(INDIC_NUM,type,name)],
        by='INDIC_NUM') %>% 
  .[type=='H'] %>% 
  .[!is.na(colour_group)] %>% 
  .[, numbered_colour_group := colour_group %>% colourToNumberedColour] %>% 
  .[geo %not in% c(EU_geo_code,EA_geo_code)] %>% 
  .[, .SD[time==max(time)], by=INDIC_NUM] %>% 
  .[, time := NULL] %>% 
  .[, blank := ""] %>% 
  .[, geo2 := geo] %>% 
  .[, value_latest_value2 := value_latest_value] %>% 
  {merge(.[,.(INDIC_NUM, geo, value_latest_value, value_change, blank, geo2, value_latest_value2)],
         dcast(., INDIC_NUM + geo ~ numbered_colour_group,
               fill=NA_real_, fun.aggregate=identity, value.var='value_change'),
         by=c('INDIC_NUM','geo'))} %>% 
  setorder(INDIC_NUM, geo) %>% 
  split(by='INDIC_NUM', keep.by=FALSE)

source_excel_template <-
  wb_load('Social Scoreboard file3 TEMPLATE.xlsx')

template_worksheet_names <-
  wb_get_sheet_names(source_excel_template)

if (length(template_worksheet_names)<length(dta_list %>% names))
  stop('Too few worksheets in "Social Scoreboard file3 TEMPLATE.xlsx":\n',
       'There are ',length(template_worksheet_names),
       ', there should be ',length(dta_list %>% names),'.\n',
       'Add ',length(dta_list %>% names)-length(template_worksheet_names),' worksheets (with any names).')

Reduce(init=source_excel_template,
       x=seq_along(template_worksheet_names),
       f=function(wb,x) {
         if (dta_list %>% names %>% .[x] %>% is.na) # too many worksheets compared to the INDIC_NUMs
           wb_remove_worksheet(wb,x) else {
             indic_num <-
               dta_list %>% names %>% .[x]
             cat('Indic',indic_num,'-> sheet',template_worksheet_names[x],'\n')
             indic_name <-
               SCOREBOARD_NAMES_DESCRIPTIONS[INDIC_NUM==indic_num, name]
             dta <-
               dta_list_2[[indic_num]]
             min_level <-
               dta[, min(value_latest_value, na.rm=TRUE)]
             max_level <-
               dta[, max(value_latest_value, na.rm=TRUE)]
             min_change <-
               dta[, min(value_change, na.rm=TRUE)]
             max_change <-
               dta[, max(value_change, na.rm=TRUE)]
             avg_level <-
               dta[, mean(value_latest_value, na.rm=TRUE)]
             avg_change <-
               dta[, mean(value_change, na.rm=TRUE)]
             wb %>% 
               # wb_set_sheet_names(old=x, new=indic_num) %>% 
               wb_add_data(sheet=x, x=indic_name, start_row=5, start_col=11) %>% 
               wb_add_data(sheet=x, x=years[[indic_num]][, time] %>% unique, start_row=6, start_col=11) %>% 
               wb_add_data(sheet=x, x=paste(indic_name,'- change'), start_row=9, start_col=11) %>% 
               wb_add_data(sheet=x, x=dta_list[[indic_num]] %>% 
                             merge(data.table(score_level_label=c('5 Very low',
                                                                  '4 Low',
                                                                  '3 On average',
                                                                  '2 High',
                                                                  '1 Very High')),
                                   by='score_level_label', all.y=TRUE) %>% # to include missing countries
                             setorder(score_level_label) %>% 
                             .[, score_level_label := NULL],
                           start_row=10, start_col=3, col_names=FALSE,
                           na.strings="") %>% 
               wb_add_data(sheet=x, x=dta_list_2[[indic_num]] %>% 
                             merge(data.table(geo=EU_Members_geo_codes),
                                   by='geo', all.y=TRUE) %>% # to include missing countries
                             setorder(geo),
                           start_row=14, start_col=10, col_names=FALSE) %>% 
               wb_add_data__in_multiple_locations(sheet=x, min_level, c('K43','K48')) %>% 
               wb_add_data__in_multiple_locations(sheet=x, max_level, c('K44','K49')) %>% 
               wb_add_data__in_multiple_locations(sheet=x, avg_level, c('K45','K51','K52')) %>% 
               wb_add_data__in_multiple_locations(sheet=x, min_change, c('L43','L51')) %>% 
               wb_add_data__in_multiple_locations(sheet=x, max_change, c('L44','L52')) %>% 
               wb_add_data__in_multiple_locations(sheet=x, avg_change, c('L45','L48','L49'))
           }
       }
) %>% 
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 3.xlsx'))
