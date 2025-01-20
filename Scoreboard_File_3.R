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
       'there are ',length(template_worksheet_names),
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



# Significances -----------------------------------------------------------

message('\nAdding significance info.')

SPPM_file <-
  paste0("//net1.cec.eu.int/EMPL/F/F4/04 Data and tools/Reports/SPPM Dashboard/config",
         "/SPPM Dashboard/Variance_significativity_2008_2019_2022_2023_SPPM.xlsx")
EPM_file <-
  paste0("//net1.cec.eu.int/EMPL/F/F4/04 Data and tools/Reports/EPM Dashboard/config/",
         "Variance_annual_changes_2022_2023_results - PROVISIONAL.xlsx")
# SPPM = Excel worksheet name in `SPPM_file`
# EPM = Excel worksheet name in `EPM_file`
SignificanceDataLocations <- "
| INDIC_NUM    | SPPM            | EPM                   |
|--------------|-----------------|-----------------------|
| 10000_ex0    |                 |                       |
| 10010_ex1    |                 | 03_Early leavers      |
| 10040_ex4    |                 |                       |
| 10070_ex7    |                 |                       |
| 10170_ex17   |                 | 17_Gender gap         |
| 10200_ex20   | S20S80          |                       |
| 10220_ex22   |                 | 05_Empl_20_64         |
| 10310_ex31   |                 | 11_Unempl rate 15-74  |
| 10500_ex50   |                 | 21_Long_term          |
| 10610_ex61   |                 |                       |
| 10660_ex66   | AROPE_NEW       |                       |
| 10830_ex83   | AROPE_NEW_0_17  |                       |
| 10990_ex99   |                 |                       |
| 11060_ex106  |                 |                       |
| 11090_ex109  | Housing_cost    |                       |
| 11130_ex113  |                 |                       |
| 11140_ex114  |                 |                       |
" %>% readMarkDownTable %>%
  # .[SPPM!="" | EPM!=""] %>% 
  melt(id.vars='INDIC_NUM',
       variable.name="source_file", value.name="worksheet",
       variable.factor=FALSE) %>% 
  .[worksheet!=""] %>% 
  split(by='INDIC_NUM')

importFromSPPM <- function(sheet)
  read_xlsx(SPPM_file, sheet, start_row=4, cols=c(1,7),
            col_names=FALSE) %>% 
  as.data.table %>% 
  setnames(c('geo','significant')) %>% 
  .[, significant := significant=='Y'] %>% 
  .[geo %in% EU_Members_geo_codes]

importFromEPM <- function(sheet)
  read_xlsx(EPM_file, sheet, start_row=3, cols=c(2,6),
            col_names=FALSE) %>% 
  as.data.table %>% 
  setnames(c('geo','significant')) %>% 
  .[, significant := significant=='Yes'] %>% 
  .[, geo := geo %>% ifelse(.=='FR_metrop','FR',.)] %>% 
  .[geo %in% EU_Members_geo_codes]

Significances <-
  SignificanceDataLocations %>% 
  lapply(\(x) switch(x$source_file, EPM=importFromEPM, SPPM=importFromSPPM,
                     stop('Unknown data significance source file.')) %>% 
           {.(x$sheet_or_IndicatorID)})

File3 <-
  paste0(OUTPUT_FOLDER,'/Social Scoreboard file 3.xlsx') %>% 
  wb_load()

INDIC_NUM_codes <-
  names(dta_list)

Reduce(init=File3,
       x=seq_along(dta_list),
       f=function(wb,sheet_num) {
         geos <-
           wb_to_df(wb, sheet=sheet_num, cols=14,
                    rows=14:(14+length(EU_Members_geo_codes)-1),
                    col_names=FALSE) %>% 
           as.data.table() %>% 
           setnames(new='geo') %>% 
           .[, geo_order := .I]
         INDIC_NUM <-
           names(dta_list)[sheet_num] %T>% cat(" ")
         if (INDIC_NUM %in% names(Significances))
           Significances %>% 
           .[[INDIC_NUM]] %>% 
           merge(geos, all.y=TRUE, by='geo') %>% 
           .[, geo := geo %>%
               ifelse(significant,paste0(.,'*'),.)] %>% 
           setorder(geo_order) %>% 
           .[,.(geo)] %>% 
           wb_add_data(wb, sheet_num, x=.,
                       start_row=14, start_col=14,
                       col_names=FALSE) else {
                         cat('<-skipped'," "); wb
                       }
         
       }
) %>% 
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 3.xlsx'))
