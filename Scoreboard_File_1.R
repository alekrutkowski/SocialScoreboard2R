library(openxlsx2)
library(kit)

rows_template <-
  c(EU_geo_code,EA_geo_code,
    "EUnw","EAnw","BE","BG","CZ","DK","DE","EE","IE","EL",
    "ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO",
    "SI","SK","FI","SE") %>% 
  data.table(.order=seq_along(.), geo=.)

worksheets_dt_list <- '
| worksheet                       | type    |
|---------------------------------|---------|
| Headline                        | H       |
| Flags                           | H       |
| Headline_flags                  | H       |
| Differences                     | H       |
| Scores                          | H       |
| Components                      | C       |
| Components_flags                | C       |
| Components breakdowns           | CB      |
| Components breakdowns_flags     | CB      |
| Breakdowns                      | B       |
| Breakdowns_flags                | B       |
| Supplementary                   | S       |
| Supplementary_flags             | S       |
| Supplementary breakdowns        | SB      |
| Supplementary breakdowns_flags  | SB      |
| Comparison                      | H       |
| Cut_offs                        | H       |
' %>% 
  readMarkDownTable() %>% 
  # Differences -- Change	Diff_EU	Diff_MSEU, all in the latest year
  # Scores -- score1_L	score2_D
  # Comparison -- latest yr, latest minus 10, latest minus 15
  # Cut_offs -- Indicator name  t1	t2	t3	t4
  split(by='worksheet')

nonweightedAverages <- function(dt)
  dt %>% 
  rowbind(.[, .(geo='EUnw',
                value_=mean(value_[geo %in% EU_Members_geo_codes],
                            na.rm=TRUE) %>% round(1))
            , by=eval(ifelse('time' %in% colnames(.),'time','variable'))]
          , fill=TRUE) %>%
  rowbind(.[, .(geo='EAnw',
                value_=mean(value_[geo %in% EA_Members_geo_codes],
                            na.rm=TRUE) %>% round(1))
            , by=eval(ifelse('time' %in% colnames(.),'time','variable'))]
          , fill=TRUE)

reshapeAndSort <- function(dt)
  dt %>% 
  .[,value_ := if (is.numeric(value_)) round(value_,1) else value_] %>% 
  dcast(paste('geo ~',
              ifelse('time' %in% colnames(.),'time','variable')) %>% 
          as.formula(), 
        value.var='value_', fun.aggregate=identity, fill=NA) %>% 
  merge(rows_template, by='geo', all.y=TRUE) %>% 
  setorder(.order) %>% 
  .[,c('.order','geo') := NULL] %>% 
  `if`(ncol(.)>3,
       .[, (ncol(.)-2):ncol(.), with=FALSE],
       .)


Reduce(
  init=wb_workbook(),
  x=worksheets_dt_list,
  f=function(wb,ws_dt) {
    ws_name <- 
      ws_dt$worksheet
    indic_type <-
      ws_dt$type
    message('\nPreparing worksheet ',ws_name,'...')
    rows <-
      switch(ws_name,
             'Headline' = c("Indicator","bookmarks","year",rows_template$geo),
             'Differences' = c("Indicator","diff",rows_template$geo),
             'Scores' = c("Indicator","score_type",rows_template$geo),
             c("Indicator","year",rows_template$geo)
      )
    wb %>% 
      wb_add_worksheet(ws_name, zoom=80) %>% 
      {`if`(ws_name=='Cut_offs', {
        dta <-
          SCOREBOARD_SCORES %>% 
          merge(SCOREBOARD_NAMES_DESCRIPTIONS[,.(INDIC_NUM,type,name)],
                by='INDIC_NUM') %>% 
          .[type=='H'] %>% 
          .[.[, .I[time==max(time)] #
              , by=INDIC_NUM]$V1] %>% 
          setorder(INDIC_NUM) %>% 
          .[,c('INDIC_NUM','type') := NULL] %>% 
          .[,.(name,
               reference_latest_value,std_latest_value,t1_latest_value,t2_latest_value,t3_latest_value,t4_latest_value,
               reference_change,std_change,t1_change,t2_change,t3_change,t4_change)] %>% 
          .[!duplicated(.)] # percentiles/cuttofs are repeated across country
        wb_add_data(., x=dta, start_col=1, start_row=1) %>%
          wb_freeze_pane(first_active_row=2,
                         first_active_col=2) %>% 
          wb_set_col_widths(cols=1,
                            widths=35) %>% 
          wb_set_col_widths(cols=2:9,
                            widths=10) %>% 
          list(wb=.)
      }
      ,
      {Reduce(init=list(wb=.,col_num=2),
              x=SCOREBOARD_NAMES_DESCRIPTIONS[type==indic_type,sort(unique(INDIC_NUM))],
              f=function(.list,indic_num) {
                cat(indic_num,"")
                wb <-
                  .list$wb
                dta <-
                  if (ws_name %not in% c('Differences','Scores','Comparison','Cut_offs'))
                    SCOREBOARD_LAGS_DIFFS %>% 
                  .[INDIC_NUM==indic_num & 
                      (time==latest_year_individual | time==previous_year | time==previous_year_2)] %>%
                  nonweightedAverages() %>%
                  `if`(ws_name=='Flags', .[, value_ := flags_], .) %>% 
                  `if`(grepl('_flags',ws_name), .[, value_ := paste(value_,flags_)], .) %>% 
                  reshapeAndSort() else
                    # Differences
                    if (ws_name=='Differences')
                      SCOREBOARD_LAGS_DIFFS %>%
                  .[INDIC_NUM==indic_num & time==latest_year_individual] %>%
                  melt(id.vars='geo', measure.vars=c("change","Diff_EU","Diff_MSEU"),
                       value.name='value_') %>% 
                  nonweightedAverages() %>%
                  reshapeAndSort() else
                    # Scores
                    if (ws_name=='Scores')
                      SCOREBOARD_SCORES %>% 
                  .[INDIC_NUM==indic_num] %>%
                  melt(id.vars='geo', measure.vars=c("score1_L","score2_D"),
                       value.name='value_')  %>% 
                  nonweightedAverages()   %>%
                  reshapeAndSort()  else
                    # Comparison
                    if (ws_name=='Comparison')
                      SCOREBOARD_LAGS_DIFFS %>%
                  .[INDIC_NUM==indic_num &
                      (time==latest_year_individual | time==latest_year_individual-10 | time==latest_year_individual-15)] %>%
                  .[, num_of_geos := length(geo), by=.(INDIC_NUM,time)] %>% 
                  .[!num_of_geos<10] %>% 
                  nonweightedAverages() %>% 
                  reshapeAndSort()
                num_of_cols <-
                  ncol(dta)
                dta_start_row <-
                  which(rows=='year') %>%
                  `if`(length(.)==0,which(rows=='diff'),.) %>% 
                  `if`(length(.)==0,which(rows=='score_type'),.) %>% 
                  `if`(length(.)==0,1,.) 
                wb %>% 
                  wb_add_data(x=rows, start_col=1, start_row=1) %>% 
                  wb_add_data(x=SCOREBOARD_NAMES_DESCRIPTIONS[INDIC_NUM==indic_num, name],
                              dims=paste0(int2col(.list$col_num),'1:')) %>% 
                  wb_merge_cells(dims=paste0(int2col(.list$col_num),'1:',
                                             int2col(.list$col_num + num_of_cols - 1),'1')) %>% 
                  wb_add_cell_style(dims=paste0(int2col(.list$col_num),'1'),
                                    wrap_text=TRUE) %>% 
                  wb_set_row_heights(rows=1, heights=75) %>% 
                  wb_add_data(x=dta,
                              start_row=dta_start_row, 
                              start_col=.list$col_num) %>%
                  wb_freeze_pane(first_active_row=dta_start_row+1,
                                 first_active_col=2) %>% 
                  wb_set_col_widths(cols=.list$col_num:(.list$col_num + num_of_cols),
                                    widths=30/num_of_cols) %>% 
                  `if`(ws_name=='Headline',
                       wb_add_data(., x=SCOREBOARD_NAMES_DESCRIPTIONS[INDIC_NUM==indic_num, url],
                                   dims=paste0(int2col(.list$col_num),'2:')) %>% 
                         wb_merge_cells(dims=paste0(int2col(.list$col_num),'2:',
                                                    int2col(.list$col_num + num_of_cols - 1),'2')),
                       .) %>% 
                  list(wb =., col_num=.list$col_num + num_of_cols)
              })})} %>% 
      .$wb
  }) %>%
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 1.xlsx'))