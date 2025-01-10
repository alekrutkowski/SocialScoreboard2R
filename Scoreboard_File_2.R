library(openxlsx2)
library(kit)

dta <-
  SCOREBOARD_SCORES %>% 
  merge(SCOREBOARD_NAMES_DESCRIPTIONS[,.(INDIC_NUM,type,name)],
        by='INDIC_NUM') %>% 
  .[type=='H'] %>% 
  .[!is.na(colour_group)] %>% 
  .[geo %not in% c(EU_geo_code,EA_geo_code)] %>% 
  .[, colour_group_num := nswitch(
    colour_group,
    'darkgreen', 1,
    'green', 2,
    'blue',3,
    'white',4,
    'yellow',5,
    'orange',6,
    'red',7
  )] %>% 
  .[, .(geos = unique(geo) %>% paste(collapse=', '))
    , by=.(INDIC_NUM,name,time,colour_group_num)] %>% 
  dcast(INDIC_NUM + name + time ~ colour_group_num,
        fill="", fun.aggregate=identity, value.var='geos') %>% 
  .[, .SD[time==max(time)], by=INDIC_NUM] %>% 
  .[, INDIC_NUM := NULL]

wb_load('Social Scoreboard file2 TEMPLATE.xlsx') %>%
  wb_add_data(x=dta, start_row=2, start_col=2, col_names=FALSE) %>% 
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 2.xlsx'))
  