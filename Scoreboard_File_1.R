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
| Differences_flags               | H       |
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
| Cut_offs II                     | H       |
' %>% 
  readMarkDownTable() %>% 
  # Differences -- Change	Diff_EU	Diff_MSEU, all in the latest year
  # Scores -- score1_L	score2_D
  # Comparison -- latest yr, latest minus 10, latest minus 15
  # Cut_offs -- Indicator name  t1	t2	t3	t4
  split(by='worksheet')

asNumber <- function(x)
  if (is.numeric(x)) x else
    gsub("[^0-9.-]", "", x) %>% 
  as.numeric()

nonweightedAverage <- function(dt, new_code, country_codes)
  dt %>% 
  rowbind(.[, .(geo=new_code,
                value_=mean(value_[geo %in% country_codes] %>% asNumber,
                            na.rm=TRUE) %>% round(2) %>% 
                  ifelse(is.nan(.),NA_real_,.)) # for mean of numeric(0)
            , by=eval(ifelse('time' %in% colnames(.),'time','variable'))]
          , fill=TRUE) 


nonweightedAverages <- function(dt)
  dt %>% 
  nonweightedAverage('EUnw', EU_Members_geo_codes) %>% 
  nonweightedAverage('EAnw', EA_Members_geo_codes) 


reshapeAndSort <- function(dt, indic_num, num_of_rounding_digits=1)
  dt %>% copy %>% 
  .[,value_ := if (!is.numeric(value_)) value_ else 
    `if`(indic_num=="10200_ex20",round(value_,2),round(value_,num_of_rounding_digits))] %>% 
  dcast(paste('geo ~',
              ifelse('time' %in% colnames(.),'time','variable')) %>% 
          as.formula(), 
        value.var='value_', fun.aggregate=identity, fill=NA) %>% 
  merge(rows_template, by='geo', all.y=TRUE) %>% 
  setorder(.order) %>% 
  .[,c('.order','geo') := NULL] %>% 
  `if`(ncol(.)>3,
       .[, (ncol(.)-2):ncol(.), with=FALSE],
       .) %>% 
  Filter(\(x) !all(is.na(x)),.) %>% 
  .[, lapply(.SD, \(x)
             `if`(is.character(x),
                  ifelse(is.na(x),"",
                         gsub(' NA',"",x,fixed=TRUE)),
                  x))]

between <- function(low,high,unit)
  paste0('between ',low,unit,' and ',high,unit)

Reduce(
  init=wb_workbook(),
  x=worksheets_dt_list, # list(data.table(worksheet='Differences_flags',type='H')),  # 
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
             'Differences_flags' = c("Indicator","diff",rows_template$geo),
             'Scores' = c("Indicator","score_type",rows_template$geo),
             c("Indicator","year",rows_template$geo)
      )
    wb %>% 
      wb_add_worksheet(ws_name, zoom=80) %>% 
      {`if`(ws_name %in% c('Cut_offs','Cut_offs II'), {
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
               reference_change,std_change,t1_change,t2_change,t3_change,t4_change, high_is_good)] %>% 
          na.omit() %>%
          .[!duplicated(.)] %>% # percentiles/cutoffs are repeated across country
          `if`(ws_name=='Cut_offs II',
               setnames(.,\(x) x %>% 
                          sub('_latest_value',' 1_levels',.,fixed=TRUE) %>% 
                          sub('_change',' 2_changes',.,fixed=TRUE)) %>% 
                 .[, id := .I] %>% 
                 melt(id.vars=c('id','name','high_is_good')) %>%  
                 .[, c('var.','level or change') := tstrsplit(variable,split=' ')] %>% 
                 .[, value := round(value,1)] %>% 
                 dcast(id + name + `level or change` + high_is_good ~ var.,
                       value.var='value',
                       fun.aggregate=identity) %>% 
                 setorder(id) %>% 
                 .[,.(name, `level or change`, high_is_good,
                      reference, std, t1, t2, t3, t4)] %>% 
                 .[, unit := nswitch(`level or change`,
                                     '1_levels', '%',
                                     '2_changes', ' pps',
                                     default="")] %>% 
                 .[, unit := nswitch(name,
                                     'Income quintile ratio (S80/S20)', "",
                                     'Gross disposable household income (GDHI) per capita growth (index, 2008=100)', "",
                                     default=unit)] %>% 
                 .[, `Very low` := paste0('less than ',t1,unit)] %>% 
                 .[, `Low` := between(t1,t2,unit)] %>% 
                 .[, `On average` := between(t2,t3,unit)] %>% 
                 .[, `High` := between(t3,t4,unit)] %>% 
                 .[, `Very high` := paste0('more than ',t4,unit)],
               .)
        wb_add_data(., x=dta, start_col=1, start_row=1, na.strings="") %>%
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
                  if (ws_name %not in% c('Differences','Differences_flags','Scores','Comparison','Cut_offs','Cut_offs II'))
                    SCOREBOARD_LAGS_DIFFS %>% 
                  .[INDIC_NUM==indic_num & 
                      (time==prevailing_latest_year | time==previous_year | time==previous_year_2)] %>%
                  nonweightedAverages() %>%
                  `if`(ws_name=='Flags', .[, value_ := flags_], .) %>% 
                  `if`(grepl('_flags',ws_name), .[, value_ := 
                                                    paste(round(value_,2) %>% ifelse(is.na(.),"",.),
                                                          flags_)], .) %>%  
                  reshapeAndSort(indic_num) else
                    # Differences
                    if (ws_name=='Differences')
                      SCOREBOARD_LAGS_DIFFS %>% 
                  .[INDIC_NUM==indic_num & time==prevailing_latest_year] %>%
                  melt(id.vars='geo', measure.vars=c("change","Diff_EU","Diff_MSEU"),
                       value.name='value_') %>% 
                  nonweightedAverages() %>%
                  reshapeAndSort(indic_num, num_of_rounding_digits=2) else
                    # Differences_flags
                    if (ws_name=='Differences_flags')
                      SCOREBOARD_LAGS_DIFFS %>% 
                  .[INDIC_NUM==indic_num] %>% 
                  .[, Diff_EU := paste(Diff_EU %>% formatC(digits=2,format="f") %>% ifelse(.==" NA","",.),
                                       flags_)] %>% 
                  .[, Diff_MSEU := Diff_MSEU %>% formatC(digits=2,format="f") %>% ifelse(.==" NA","",.)] %>% 
                  setorder(INDIC_NUM,geo,time) %>% 
                  .[, shflags_ := shift(flags_), by=.(INDIC_NUM,geo)] %>% 
                  .[, change := paste(change %>% formatC(digits=2,format="f") %>% ifelse(.==" NA","",.),
                                      flags_ %>% ifelse(is.na(.),"",.),shflags_ %>% ifelse(is.na(.),"",.)) %>% 
                      gsub('  '," ",.,fixed=TRUE)] %>% 
                  .[time==prevailing_latest_year] %>%
                  melt(id.vars='geo', measure.vars=c("change","Diff_EU","Diff_MSEU"),
                       value.name='value_') %>% 
                  nonweightedAverages() %>%
                  reshapeAndSort(indic_num, num_of_rounding_digits=2) else
                    # Scores
                    if (ws_name=='Scores')
                      SCOREBOARD_SCORES %>% 
                  .[INDIC_NUM==indic_num] %>%
                  melt(id.vars='geo', measure.vars=c("score1_L","score2_D"),
                       value.name='value_')  %>% 
                  nonweightedAverages()   %>%
                  reshapeAndSort(indic_num, num_of_rounding_digits=2)  else
                    # Comparison
                    if (ws_name=='Comparison')
                      SCOREBOARD_LAGS_DIFFS %>%
                  .[INDIC_NUM==indic_num &  (time==2008 | time==2013 | time==prevailing_latest_year)] %>% 
                      # (time==prevailing_latest_year | time==prevailing_latest_year-10 | time==prevailing_latest_year-15)] %>%
                  # .[, num_of_geos := length(geo[isNotNA(value_)]), by=.(INDIC_NUM,time)] %>% 
                  # .[!num_of_geos<10] %>% 
                  nonweightedAverages() %>% 
                  reshapeAndSort(indic_num)
                num_of_cols <-
                  ncol(dta)
                dta_start_row <-
                  which(rows=='year') %>%
                  `if`(length(.)==0,which(rows=='diff'),.) %>% 
                  `if`(length(.)==0,which(rows=='score_type'),.) %>% 
                  `if`(length(.)==0,1,.) 
                wb %>% 
                  wb_add_data(x=rows, start_col=1, start_row=1, na.strings="") %>% 
                  wb_add_data(x=SCOREBOARD_NAMES_DESCRIPTIONS[INDIC_NUM==indic_num, name],
                              na.strings="",
                              dims=paste0(int2col(.list$col_num),'1:')) %>% 
                  wb_merge_cells(dims=paste0(int2col(.list$col_num),'1:',
                                             int2col(.list$col_num + num_of_cols - 1),'1')) %>% 
                  wb_add_cell_style(dims=paste0(int2col(.list$col_num),'1'),
                                    wrap_text=TRUE) %>% 
                  wb_set_row_heights(rows=1, heights=75) %>% 
                  wb_add_data(x=dta,
                              start_row=dta_start_row, 
                              start_col=.list$col_num,
                              na.strings="") %>%
                  wb_freeze_pane(first_active_row=dta_start_row+1,
                                 first_active_col=2) %>% 
                  wb_set_col_widths(cols=.list$col_num:(.list$col_num + num_of_cols),
                                    widths=30/num_of_cols) %>% 
                  `if`(ws_name=='Headline',
                       wb_add_data(., x=SCOREBOARD_NAMES_DESCRIPTIONS[INDIC_NUM==indic_num, url],
                                   dims=paste0(int2col(.list$col_num),'2:'),
                                   na.strings="") %>% 
                         wb_merge_cells(dims=paste0(int2col(.list$col_num),'2:',
                                                    int2col(.list$col_num + num_of_cols - 1),'2')),
                       .) %>% 
                  list(wb =., col_num=.list$col_num + num_of_cols)
              })})} %>% 
      .$wb
  }) %>%
  wb_save(paste0(OUTPUT_FOLDER,'/Social Scoreboard file 1.xlsx'))