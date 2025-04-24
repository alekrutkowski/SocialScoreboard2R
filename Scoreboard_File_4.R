library(openxlsx2)
library(kit)

colourNameToHex <- function(colour_char_vec)
  colour_char_vec %>% 
  nswitch('green', '#92d050',
          'white', '#ffffff',
          'red', '#ff0000',
          'orange', '#ffc000',
          'darkgreen', '#00b050',
          'yellow', '#ffff00',
          'blue', '#00b0f0',
          NA_character_, '#d9d9d9')

colourNameToHexInk <- function(colour_char_vec)
  colour_char_vec %>% 
  nswitch('green', '#000000',
          'white', '#000000',
          'red', '#ffffff',
          'orange', '#000000',
          'darkgreen', '#ffffff',
          'yellow', '#000000',
          'blue', '#000000',
          NA_character_, '#000000')

replaceStringInCell <- function(wb, sheet, dims, old_string, new_string) 
  wb_to_df(wb, sheet=sheet, dims=dims, col_names=FALSE) %>% 
  .[[1]] %>% paste(collapse='\n') %>% 
  sub(old_string,
      new_string %>% ifelse(is.na(.),old_string,.),
      ., fixed=TRUE) %>% 
  wb_add_data(wb=wb, sheet=sheet, dims=dims, x=.)


source_excel_template <-
  wb_load('Social Scoreboard file4 TEMPLATE.xlsx')

template_indic_nums <-
  source_excel_template %>% 
  wb_to_df(dims='K:K', col_names=FALSE) %>% 
  as.data.table() %>% 
  setnames(new='INDIC_NUM') %>% 
  .[INDIC_NUM %>% isNotNA] %>% 
  .[, INDIC_NUM_order := .I]

if (!all(template_indic_nums$INDIC_NUM %in% unique(SCOREBOARD_SCORES$INDIC_NUM)))
  stop('"Social Scoreboard file4 TEMPLATE.xlsx", column K, contains INDIC_NUM codes\n',
       'not available in SCOREBOARD_SCORES:\n',
       template_indic_nums$INDIC_NUM %without% unique(SCOREBOARD_SCORES$INDIC_NUM) %>%
         paste(collapse='\n'))

dta_list <-
  SCOREBOARD_SCORES %>% 
  # to keep missing country-indic combinations
  merge(expand.grid(INDIC_NUM=template_indic_nums$INDIC_NUM,
                    geo=c(EU_geo_code, EU_Members_geo_codes)) %>% 
          as.data.table %>% merge(template_indic_nums) 
        , by=c('geo','INDIC_NUM'), all.y=TRUE) %>%  
  .[, .SD[time==max(time) || is.na(time)], by=.(geo,INDIC_NUM)] %>%
  setorder(geo, INDIC_NUM, INDIC_NUM_order) %>% 
  split(by='geo', keep.by=FALSE)


Reduce(init=source_excel_template,
       x=c(EU_geo_code, EU_Members_geo_codes),
       f=function(wb,x) {
         cat(x,"")
         wb %>% 
           wb_clone_worksheet(old='XX', new=x) %>% 
           replaceStringInCell(sheet=x, dims='A1', old_string='XXXXXX',
                               new_string=EU_Members_geo_names[geo==x, geo_labels]) %>% 
           replaceStringInCell(sheet=x, dims=paste0('A',nrow(template_indic_nums)+3),
                               old_string='DD MMMMM YYYY',
                               new_string=format(Sys.Date(),"%e %B %Y") %>% trimws) %>% 
           wb_add_data(sheet=x,
                       x=dta_list[[x]]$value_latest_value,
                       start_row=2, start_col=9,
                       na.strings="") %>%
           wb_add_numfmt(sheet=x, dims=paste0('I2:I',nrow(template_indic_nums)+1), numfmt="0.0") %>% 
           wb_add_numfmt(sheet=x, 
                         dims=template_indic_nums %>% 
                           .[INDIC_NUM=='10200_ex20',INDIC_NUM_order+1] %>% 
                           paste0('I',.),
                         numfmt="0.00") %>% 
           wb_add_data(sheet=x, start_col=11, start_row=2,
                       x=rep.int("",nrow(template_indic_nums))) %>% 
           Reduce(init=.,
                  x=seq_len(nrow(template_indic_nums)),
                  f=function(wb,x2)
                    wb %>% 
                    replaceStringInCell(sheet=x, dims=paste0('C',x2+1),
                                        old_string='YYYY',
                                        new_string=dta_list[[x]] %>% 
                                          .[INDIC_NUM==template_indic_nums[x2,INDIC_NUM], time]) %>% 
                    wb_add_fill(sheet=x, dims=paste0('C',x2+1,':I',x2+1),
                                color=wb_color(hex=dta_list[[x]] %>% 
                                                 .[INDIC_NUM==template_indic_nums[x2,INDIC_NUM], colour_group] %>% 
                                                 as.character %>% colourNameToHex)) %>% 
                    wb_add_font(sheet=x, dims=paste0('C',x2+1,':I',x2+1),
                                color=wb_color(hex=dta_list[[x]] %>% 
                                                 .[INDIC_NUM==template_indic_nums[x2,INDIC_NUM], colour_group] %>% 
                                                 as.character %>% colourNameToHexInk),
                                name='Calibri',size='10',bold=TRUE) 
           )
       }
) %>% 
  wb_remove_worksheet(sheet='XX') %>% 
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 4.xlsx'))
