library(openxlsx2)

colourNameToIntModified <- function(colour_char_vec)
  colour_char_vec %>% 
  nswitch('green', 2L,
          'white', 0L,
          'red', -3L,
          'orange', -2L,
          'darkgreen', 3L,
          'yellow', -1L,
          'blue', 1L,
          NA_character_, NA_integer_)

extractDate <- function(s) 
  s %>%
  regexpr("\\d{4}-\\d{2}-\\d{2}", .) %>%
  regmatches(s, .) %>%
  as.Date() %>%
  format("%d %b %Y") %>% 
  sub("^0", "", .)

source_excel_template <-
  list.files(pattern='SSBcc') %>% 
  `if`(length(.)!=1,
       stop('No or multiple `SSBcc...` Excel files found!',call.=FALSE)
       ,.) %>% 
  wb_load()

column_country_order <-
  source_excel_template %>% 
  wb_to_df(sheet=1, dims='C1:AC1', col_names=FALSE) %>% 
  as.character()

init_data <-
  SCOREBOARD_SCORES[INDIC_NUM %in% 
                      SCOREBOARD_NAMES_DESCRIPTIONS[type=='H',INDIC_NUM]]

finishData <- function(dt, col_name)
  dt %>% 
  dcast(INDIC_NUM + time ~ geo, value.var=col_name,
        fun.aggregate=identity, fill=NA_integer_) %>% 
  setorder(INDIC_NUM) %>% 
  .[,mget(c('time',column_country_order))]

`data for SSB categories` <-
  init_data %>% 
  .[, colour_int := colour_group %>% colourNameToIntModified] %>% 
  finishData('colour_int')

`data for SSB levels` <-
  init_data %>% 
  finishData('value_latest_value')

`data for SSB changes` <-
  init_data %>% 
  finishData('value_change')

empty_data <-
  matrix("",
         ncol=ncol(`data for SSB categories`),
         nrow=nrow(`data for SSB categories`))

source_excel_template %>% 
  Reduce(init=.,
         x=c("SSB categories", "SSB levels", "SSB changes"),
         f=function(wb,x)
           wb_add_data(wb,x,dims='A1',
                       x=paste('Based on data extracted on',
                               OUTPUT_FOLDER %>% extractDate))
  ) %>% 
  Reduce(init=.,
         x=c("SSB categories WAS", "SSB levels WAS", "SSB changes WAS"),
         f=function(wb,x)
           wb_add_data(wb,x, dims='A1', x="")
  ) %>% 
  wb_add_data(sheet='SSB categories', start_col=2,start_row=2, col_names=FALSE,
              na.strings="",
              x=`data for SSB categories`) %>% 
  wb_add_data(sheet='SSB levels', start_col=2,start_row=2, col_names=FALSE,
              na.strings="",
              x=`data for SSB levels`) %>% 
  wb_add_data(sheet='SSB changes', start_col=2,start_row=2, col_names=FALSE,
              na.strings="",
              x=`data for SSB changes`) %>% 
  # empty the data area with previous-edition data:
  wb_add_data(sheet='SSB categories WAS', start_col=2,start_row=2, col_names=FALSE,
              x=empty_data) %>% 
  wb_add_data(sheet='SSB levels WAS', start_col=2,start_row=2, col_names=FALSE,
              x=empty_data) %>% 
  wb_add_data(sheet='SSB changes WAS', start_col=2,start_row=2, col_names=FALSE,
              x=empty_data) %>% 
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 6.xlsx'))




