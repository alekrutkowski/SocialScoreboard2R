library(magrittr)
library(tibble)
library(data.table)
library(memoise)
library(eurodata)
library(collapse)
library(OECD)
library(countrycode)
library(rvest)
library(kit)
library(xml2)


# Constants and aliases ---------------------------------------------------

EU_Members_geo_codes <-
  c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR",
    "HR","IT","CY","LV","LT","LU","HU","MT","NL","AT",
    "PL","PT","RO","SI","SK","FI","SE") %T>% 
  {stopifnot(length(.)==27, all(nchar(.)==2),
             all(. %in% eurodata::importLabels('geo')$geo))}

EA_Members_geo_codes <-
  c("BE","DE","EE","IE","EL","ES","FR","HR","IT","CY",
    "LV","LT","LU","MT","NL","AT","PT","SI","SK","FI") %T>% 
  {stopifnot(length(.)==20, all(nchar(.)==2),
             all(. %in% eurodata::importLabels('geo')$geo))}

large_EU_Members_geo_codes <-
  c('DE','FR','IT','ES','PL','RO','NL') %T>% 
  {stopifnot(all(. %in% EU_Members_geo_codes))}

EU_geo_code <-
  'EU27_2020'

EA_geo_code <-
  'EA20'

`%without%` <- setdiff

delimiter <-
  rep.int('-',80) %>% paste(collapse="") %>% paste0('\n',.,'\n')

`inside<-` <- function(.list,  indicator_number, value) {
  stopifnot(is.string.scalar(indicator_number))
  .list[[indicator_number]] <-
    calculate(indicator_number, value)
  .list
}

variables <- list

`%not in%` <- Negate(`%in%`)

memoised_importData <-
  memoise::memoise(eurodata::importData)

memoised_importDataList <-
  memoise::memoise(eurodata::importDataList)


# Functions ---------------------------------------------------------------

specification <- function(...)
  substitute(list(...))

isError <- function(x)
  inherits(x,'simpleError')

# For short-lived network problems
retry <- function(expr, timeout=6, interval=2) {
  t0 <- Sys.time()
  repeat {
    result <-
      withCallingHandlers(
        tryCatch(expr,
                 error = function(e) e),
        warning=function(w)
          if (grepl("restarting interrupted promise evaluation", w$message))
            invokeRestart("muffleWarning")
      )
    if (!isError(result) || Sys.time()-t0 > timeout) break
    Sys.sleep(interval)
    message('Re-trying...')
  }
  if (isError(result)) stop(result) else result
}

is.string.scalar <- function(x)
  is.character(x) && length(x)==1

is.logical.scalar <- function(x)
  is.logical(x) && length(x)==1

LIST_OF_REFERENCE_POINT_FUNCTIONS <- 
  list()
LIST_OF_REFERENCE_POINT_FUNCTIONS[[EU_geo_code]] <-
  function(val,geo,is_time=FALSE) val[geo==EU_geo_code]
LIST_OF_REFERENCE_POINT_FUNCTIONS[['SIMPLE AVERAGE']] <-
  function(val,geo,is_time=FALSE) 
    val[geo %in% EU_Members_geo_codes] %>% 
  {`if`(is_time,
        paste(sort(unique(.)),collapse=', '),
        mean(.))}

calculate <- memoise::memoise(
  function(indicator_number, unevaluated_specification_list) {
    if (!is.null(SCOREBOARD_INDICATORS[[indicator_number]]))
      stop('\nIndicator `',indicator_number,'` is already defined!')
    message(delimiter,'Calculating ',indicator_number)
    retry(do.call(function(name,
                           chapter="",
                           group="",
                           type,
                           url="",
                           high_is_good,
                           reference_in_scores='SIMPLE AVERAGE',
                           value) {
      stopifnot(
        is.string.scalar(name),
        is.string.scalar(chapter),
        is.string.scalar(group),
        is.string.scalar(type),
        type %in% c("H","B","S","SB","C","CB"),
        is.string.scalar(url),
        is.logical.scalar(high_is_good),
        is.string.scalar(reference_in_scores),
        toupper(reference_in_scores) %in% toupper(names(LIST_OF_REFERENCE_POINT_FUNCTIONS)),
        is.data.frame(value),
        nrow(value)>0,
        '\nIt is required that the data.table has all the\nidentifier columns (`geo`, `time`) and the `value_`.' =
          c('geo','time','value_') %in% colnames(value),
        '\nIt is required that columns `geo` and `time` uniquely\nidentify the rows in the data.table (there are no duplicates).' =
          nrow(value[,.(geo,time)] %>% .[duplicated(.)])==0,
        '\nIt is required that the column `value_` is numeric.' =
          is.numeric(value$value_)
      )
      list(name=name,
           chapter=chapter,
           group=group,
           type=type,
           url=url,
           high_is_good=high_is_good,
           reference_in_scores=reference_in_scores,
           value = value %>% 
             .[, grep('^(geo|time|value_|flags_.*|.)$',
                      colnames(value),value=TRUE),
               with=FALSE] %>% 
             setorder(geo, time))
    }, eval(unevaluated_specification_list)))
  }
)

with_filters <- function(...) {
  filters <-
    list(...)
  if (!identical(filters,list(NA))) {
    if (any(names(filters)==""))
      stop('There is an unnamed element inside with_filters()!')
    if (length(names(filters))!=length(unique(names(filters))))
      stop('There are repeated names of elements inside with_filters()!')
    filters
  }
}

anyProblems <- function(dt) {
  current_year <-
    Sys.Date() %>% 
    substr(1,4) %>% 
    as.integer()
  dt_nonmiss <-
    dt[!is.na(value_)]
  max_time <-
    max(dt_nonmiss$time)
  not_available_geos <-
    dt_nonmiss %>% 
    .$geo %>% 
    unique() %>% 
    setdiff(EU_Members_geo_codes,.)
  dt_nonmiss_last_t <-
    dt_nonmiss[time==max_time]
  not_available_geos_last_time_point <-
    dt_nonmiss_last_t %>% 
    .$geo %>% 
    unique() %>% 
    setdiff(EU_Members_geo_codes,.)
  if_add <- function(list, cond, entry)
    if (cond) c(list, entry) else list
  no_problems <- function(list.)
    (list. %>%
       sapply(is.logical) %>% 
       all()) &&
    (list. %>% 
       unlist() %>% 
       {!.} %>% 
       all())
  to_report <- function(list.)
    list. %>% 
    Filter(\(x) is.logical(x) && x || !is.logical(x),.)
  list(`Old data` = max_time < (current_year - 3),
       `Very old data` = max_time < (current_year - 5),
       `One time point only` = length(unique(dt_nonmiss$time))==1,
       `Many countries missing` = length(not_available_geos)>=10,
       `All large countries missing` = all(large_EU_Members_geo_codes %in% not_available_geos),
       `One or more large countries missing` = any(large_EU_Members_geo_codes %in% not_available_geos),
       `All large countries missing at the last time point` = all(large_EU_Members_geo_codes %in% not_available_geos_last_time_point),
       `One or more large countries missing at the last time point` = any(large_EU_Members_geo_codes %in% not_available_geos_last_time_point),
       `No EU aggregate` = EU_geo_code %not in% unique(dt_nonmiss$geo),
       `No EU aggregate for the last time point` = EU_geo_code %not in% unique(dt_nonmiss_last_t$geo)
       # More checks to be added e.g. if high jumps in values or variance
  ) %>%
    if_add(.$`Old data`,
           list(`Latest time point` = max_time)) %>% 
    if_add(.$`Many countries missing`,
           list(`Not available countries` = not_available_geos)) %>% 
    if_add(.$`One or more large countries missing`,
           list(`Large countries missing` = intersect(large_EU_Members_geo_codes, not_available_geos))) %>% 
    if_add(.$`One or more large countries missing at the last time point`,
           list(`Large countries missing at the last time point` = intersect(large_EU_Members_geo_codes, not_available_geos_last_time_point))) %>% 
    `if`(no_problems(.),.,
         {message('Some problems detected:'); str(to_report(.)); .})
}


fromEurostatDataset <- function(EurostatDatasetCode, with_filters, time_period=0L) {
  cmd_line <-
    paste0('\nfromEurostatDataset("',EurostatDatasetCode,'", ',
           deparse(with_filters),
           ifelse(time_period!=0L,
                  paste0(', time_period = ',time_period,')\n'),
                  ")\n"))
  `if`(tolower(EurostatDatasetCode) %not in% c(memoised_importDataList()$Code,
                                               # The datasets below may be for some reason absent in
                                               # https://ec.europa.eu/eurostat/api/dissemination/catalogue/toc/txt?lang=EN
                                               'lfsa_ergaed','lfsa_ergan','lfsa_urgaed',
                                               'lfsa_argaed','edat_lfse_20'), 
       paste0(cmd_line,
              '"',EurostatDatasetCode,
              '" not found in the list of Eurostat datasets or tables!\n',
              'check https://ec.europa.eu/eurostat/api/dissemination/catalogue/toc/txt?lang=EN') %>% 
         {warning(.); message(.)})
  tryCatch(memoised_importData(EurostatDatasetCode,
                               c(with_filters)),
           error = function(e) e) %>% 
    `if`(isError(.) && grepl('HTTP error 400',.),
         stop('\nIn',
              cmd_line,
              '\n`with_filters()` contains invalid filter value(s) (=...) or filter name(s) (...=)\n',
              estatDatasetDimNames(EurostatDatasetCode) %>% 
                suggestedWords(names(with_filters) %without% .,
                               .) %>% 
                paste0('\nSee also\n',
                       'https://ec.europa.eu/eurostat/databrowser/view/',
                       EurostatDatasetCode
                       ,'/default/table?lang=en'),
              call.=FALSE),
         .) %>% 
    `if`(nrow(.)==0,
         stop(cmd_line,
              "returned empty data.frame!\n",call.=FALSE),
         .) %>%
    as.data.table() %>% 
    .[geo %in% c(EU_Members_geo_codes,EU_geo_code,EA_geo_code)] %>% # not in filters because sometimes some geo not available, that would result in HTTP error 400
    .[, sapply(colnames(.), # 
               \(colname) colname %in% c('geo','TIME_PERIOD','value_','flags_') ||
                 length(unique(.[[colname]]))!=1), # drop columns with identifiers = single category
      with=FALSE] %>%
    setnames('TIME_PERIOD','time') %>% 
    .[, time := sub('-',"",time,fixed=TRUE)] %>% 
    .[, lapply(., \(col) `if`(is.factor(col),as.character(col),col))] %>% 
    `if`(time_period!=0L,
         .[, value_ := collapse::L(value_, n=-time_period, t=time),
           by = eval(setdiff(colnames(.),
                             c('time','value_','flags_')))],
         .)
}


fromOECDdataset <- function(OECDdatasetURL, with_filters) {
  OECDdatasetURL %>% 
    fread() %>% 
    setnames(c('TIME_PERIOD','OBS_VALUE','REF_AREA','OBS_STATUS'),
             c('time','value_','country','flags_'),
             skip_absent=TRUE) %>% 
    .[, geo := countrycode(country,
                           origin='iso3c',
                           destination='eurostat')] %>% 
    .[geo %in% EU_Members_geo_codes] %>% 
    .[, value_ := as.numeric(value_)] %>% 
    .[, country := NULL]
}


fromLMPdataset <- function(LMPdatasetCode, with_filters) {
  stopifnot(is.character(LMPdatasetCode),
            length(LMPdatasetCode)==1,
            is.list(with_filters),
            length(with_filters)>0)
  if (any(names(with_filters)=="")) 
    stop("All elements of `filters` must be named.")
  web_dataset_code <-
    LMPdatasetCode %>% 
    ifelse(.=='lmp_expenditure','LMP_IND_EXP',.) %>% 
    toupper()
  url <-
    paste0('https://webgate.ec.europa.eu/empl/redisstat/api/dissemination/sdmx/2.1/data/',
           web_dataset_code,'/',
           LMPurlFilters(web_dataset_code, with_filters),
           '/?format=TSV&i')
  RawData <-
    url %>% 
    data.table::fread(sep="\t", colClasses="character", 
                      header=TRUE)
  FirstColName <-
    colnames(RawData)[1]
  IdNames <-
    FirstColName %>% 
    strsplit(",|\\\\") %>% 
    .[[1]]
  ColIdName <-
    IdNames %>%
    tail(1)
  RowIdNames <-
    IdNames %>%
    head(-1)
  RawData %>% 
    as.data.table() %>% 
    .[, (RowIdNames) := tstrsplit(get(FirstColName), split=",")] %>% 
    .[, (FirstColName) := NULL] %>% 
    melt(id.vars=RowIdNames, 
         variable.name=ColIdName, value.name="value_") %>% 
    .[, flags_ := gsub("[0-9\\.-]", "", value_)] %>% 
    .[, value_ := gsub("[^0-9\\.-]", "", value_) %>% as.numeric()] %>% 
    Filter(\(col) length(unique(col))!=1, .) %>% 
    setnames('TIME_PERIOD','time') %>% 
    .[, time := as.integer(as.character(time))] %>% # converting from factor
    setnames(colnames(.),tolower(colnames(.)))
}


LMPurlStructure <- function(ds_code) 
  ds_code %>%
  toupper() %>%
  paste0("https://webgate.ec.europa.eu/empl/redisstat/api/dissemination/sdmx/2.1/dataflow/EMPL/",
         .,
         "/1.0?references=descendants&detail=referencepartial&format=sdmx_2.1_generic&compressed=false") %>%
  xml2::read_xml() %>%
  xml2::as_list() %>% {
    .$Structure$Structures$DataStructures$DataStructure$
      DataStructureComponents$DimensionList
  } %>% sapply(\(x) attr(x$ConceptIdentity$Ref, "id"))


LMPurlFilters <- function(ds_code, filters_list) {
  filters <-
    filters_list %>% 
    `names<-`(toupper(names(.)))
  ds_code %>% 
    LMPurlStructure() %>% 
    toupper() %>% 
    .[.!='TIME_PERIOD'] %T>% {
      not_present_dims <- setdiff(names(filters),.)
      `if`(length(not_present_dims) > 0,
           warning("`filters` contains dimension(s) not present in the dataset, ignored:\n", 
                   paste(not_present_dims, collapse=", "), 
                   call.=FALSE, immediate.=TRUE))
    } %>% filters[.] %>% lapply(paste, collapse = "+") %>% 
    {do.call(paste, c(., sep="."))}
}


fromBenefitsAndWages <- function(table_code, with_filters) {
  url_dataset <-
    table_code %>% 
    switch(.,
           "nrr_ub" = 'NRR/NRRUB',
           "earn_nt_lowwtrp" = 'TR',
           "tax_ben_traps" = 'TR',
           .)
  url_filters <-
    with_filters$indicator %>% 
    gsub('.','/',.,fixed=TRUE) %>% 
    sub('^S/',"1EC/",.) %>% 
    `if`(table_code=='nrr_ub',
         sub('^(.+)/(.+)/(.+)$',"\\1/\\3/\\2",.),
         .) %>% 
    `if`(table_code %in% c('earn_nt_lowwtrp','tax_ben_traps'),
         sub('LW/(.+)/.+/(.+/.+)','LW/\\2/\\1',.) %>% 
           sub('IT/(.+)/(.+)/.+/.+','IT/\\2/\\1',.),
         .)
  url_pfix <-
    'https://europa.eu/economy_finance/db_indicators/tab/wq/details_all_webquery.php?url='
  url_geos <-
    EU_Members_geo_codes %>% 
    c(EU_geo_code,.) %>% 
    sub('EL','GR',.,fixed=TRUE) %>% 
    sub(EU_geo_code,'EUBr',.,fixed=TRUE) %>% 
    paste(collapse=',')
  url_years <-
    2000:as.integer(format(Sys.Date(),"%Y")) %>% 
    paste(collapse=',')
  url <-
    paste0(url_pfix,url_dataset,'/',url_filters,'/filter/',url_geos,'/',url_years)
  url %T>% message('Opening:\n',.) %>%
    getTaxBenTable() %>% 
    `if`(table_code=='nrr_ub',
         Filter(\(colvals) !all(colvals==""), .) %>% 
           .[, c(1,2,length(.)), with=FALSE] %>% 
           setnames(colnames(.),
                    c('Country','time','value_')),
         .) %>%
    `if`(table_code %in% c('earn_nt_lowwtrp','tax_ben_traps'),
         setnames(., seq_along(.),
                  as.character(.[1])) %>%
           .[,.(Country,Year,METR)] %>% 
           setnames(c('Year','METR'),
                    c('time','value_')),
         .) %>% 
    .[, time := as.integer(time)] %>% 
    .[, value_ := as.numeric(value_)] %>% 
    .[!is.na(value_)] %>% 
    .[, geo := countrycode(Country,
                           origin='country.name',
                           destination='eurostat')] %>% 
    .[, geo := ifelse(grepl('European Union',Country),EU_geo_code,geo)] %>% 
    .[, Country := NULL]
}


getTaxBenTable <- function(url)
  url %>% 
  read_html() %>% 
  html_node(xpath='/html/body/table') %>% 
  html_table(convert=FALSE) %>% # wrong colnames = title "European Commission	Economic and Financial Affairs	Tax and Benefits"
  as.data.table() %>% 
  # setnames(seq_along(.),
  #          as.character(.[1])) %>% 
  .[-c(nrow(.))] # last row includes date e.g. "Last update :	20-03-2023"


memoised_fread <- memoise(fread)


fromLFSspecialFile <- function(jaf_lfs_code, with_filters) {
  name_of_raw_file_from_estat <-
    jaf_lfs_code %>% 
    switch(.,
           'lfse_jobtenure'="IESS_PA2_S5_v2_Y.csv",
           'lfse_nacegap'="IESS_17_PA7_1_C6_N1_N2_AA.csv",
           'lfse_iscogap'="IESS_16_PA7_1_C5_AA.csv",
           'lfse_erfgap2064'="IESS_15_PA7_1_C4_2064_FTE_AA.csv",
           'lfse_er_child'="IESS_11_PA7_2_S1_Y.csv",
           'lfse_inactpt_lackcare'="IESS_10_PA5_C3_mod_Y.csv",
           'lfse_overtime'="IESS_PA2_C3_AA.csv",
           .
    )
  memoised_fread(name_of_raw_file_from_estat) %>%
    copy() %>% 
    setnames(colnames(.),
             colnames(.) %>% tolower()) %>% 
    .[, c('quarter','flag','flag_break') := NULL] %>% 
    .[, country := ifelse(country=='EUR20','EA20',country)] %>% 
    setnames(c('country','year','value'),
             c('geo','time','value_')) %>% 
    .[, time := as.integer(time)] %>% 
    .[, value_ := as.numeric(value_)] %>% 
    Reduce(\(dt,x) dt[dt[[tolower(x)]] %in% with_filters[[x]]],
           x=names(with_filters),
           init=.)
}


fromDESI <- function(with_filters) {
  'https://digital-decade-desi.digital-strategy.ec.europa.eu/api/v1/chart-groups/desi-2022/facts/' %>% 
    fread() %>% 
    # Reduce(\(dt,x) dt[dt[[x]] %in% with_filters[[x]]],
    #        x=names(with_filters),
    #        init=.) %>% 
    .[indicator=="desi" & breakdown=="desi_conn" & unit=="pc_desi"] %>% 
    .[,.(period,country,value,flags)] %>% 
    setnames(c('period','country','value','flags'),
             c('time','geo','value_','flags_'))
}

desi <- fromDESI

OECD_Pisa2 <- function(with_filters)
  'OECD_Pisa2.csv' %>% 
  fread %>% 
  setnames('value_n','value_') %>% 
  setnames('flag','flags_') %>% 
  setnames('year','time') %>%
  .[, file := NULL]



getParticipation_in_education_and_training <- memoise(function() {
  ### Translated from Python ###
  # Define the file path
  file_path <- "Participation in education and training (excluding guided on the job training) 2016-2022.xlsx"
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop(paste0(
      "The file is not present in the specified directory:\n", file_path,
      "\nYou can download it from:\n",
      "https://circabc.europa.eu/ui/group/d14c857a-601d-438a-b878-4b4cebd0e10f/library/c5a8b987-1e37-44d7-a20e-2c50d6101d27/details",
      "\nor get from Elodie CAYOTTE (ESTAT) <Elodie.CAYOTTE@ec.europa.eu> or Sabine GAGEL (ESTAT) <Sabine.GAGEL@ec.europa.eu>"
    ))
  }
  years <-
    file_path %>% 
    wb_load() %>% 
    wb_get_sheet_names() %>% 
    gsub("[^0-9]","",.) %>% 
    unique %>% 
    .[.!=""] %>% 
    sort
  # List of tuples with (sheet_name_prefix, usecols, new_column_names)
  sheets_and_columns <- list(
    list('TIME', c(1,12:15), years %>% 
           expand.grid(c('value_ ','flags_ '),.) %>% do.call(paste0,.) %>% 
           c('Country',.)),
    list("SEX", c(1, 6, 7), c("Country", "MEN", "WOMEN")),
    list("AGE", c(1, 8, 9, 10, 11), c("Country", "Y25-34", "Y35-44", "Y45-54", "Y55-64")),
    list("ISCED", c(1, 7, 8, 9), c("Country", "ISCED_0-2", "ISCED_3-4", "ISCED_5-8"))
  )
  # Initialize an empty list to store data tables
  datatables <- list()
  # Loop through each sheet and year to process the data
  for (sheet_info in sheets_and_columns) {
    sheet_name_prefix <- sheet_info[[1]]
    usecols <- sheet_info[[2]]
    colnames <- sheet_info[[3]]
    for (year in years) {
      # Read the relevant rows and columns from the Excel file
      dt <- read_xlsx(file_path, 
                      sheet = sheet_name_prefix %>% 
                        `if`(.=='TIME',.,paste0(.," - ",year)),
                      rows=6:33, cols=usecols, col_names=FALSE) %>% 
        setDT() %>% 
        # .[6:33, ..usecols] %>% 
        setnames(colnames) %>% 
        `if`(sheet_name_prefix!='TIME',
             .[, time := as.integer(year)] %>% 
               .[, Country := Country %>% 
                   gsub('(¹)',"",.,fixed=TRUE) %>% 
                   gsub('(²)',"",.,fixed=TRUE) %>% 
                   gsub(' ',"",.,fixed=TRUE)] %>% 
               melt(id.vars = c("Country", "time"), 
                    variable.name = "group", 
                    value.name = "value_"),
             melt(., id.vars='Country') %>% 
               .[, c('var.','time') := tstrsplit(variable,split=' ')] %>% 
               dcast(Country + time ~ var., value.var='value',
                     fun.aggregate=identity) %>% 
               .[, time := as.integer(time)] %>% 
               .[, group := 'TOTAL'])
      # Append the data.table to the list
      datatables <- append(datatables, list(dt))
    }
  }
  dts <- rbindlist(datatables, fill=TRUE)
  # Define the Eurostat country codes
  Eurostat_country_codes <- c(
    "EU-27" = "EU27_2020",
    "Belgium" = "BE",
    "Bulgaria" = "BG",
    "Czechia" = "CZ",
    "Denmark" = "DK",
    "Germany" = "DE",
    "Estonia" = "EE",
    "Ireland" = "IE",
    "Greece" = "EL",
    "Spain" = "ES",
    "France" = "FR",
    "Croatia" = "HR",
    "Italy" = "IT",
    "Cyprus" = "CY",
    "Latvia" = "LV",
    "Lithuania" = "LT",
    "Luxembourg" = "LU",
    "Hungary" = "HU",
    "Malta" = "MT",
    "Netherlands" = "NL",
    "Austria" = "AT",
    "Poland" = "PL",
    "Portugal" = "PT",
    "Romania" = "RO",
    "Slovenia" = "SI",
    "Slovakia" = "SK",
    "Finland" = "FI",
    "Sweden" = "SE"
  ) %>% 
    data.table(Country=names(.), geo=.)
  # Replace the country names in the "Country" column with their Eurostat codes
  dts %>% 
    merge(Eurostat_country_codes, by='Country') %>% 
    .[, Country := NULL] %>% 
    .[, value_ := as.numeric(value_)] %>% 
    .[, .(time, geo, group, value_)]
})

Participation_in_education_and_training <- function(with_filters) {
  getParticipation_in_education_and_training() %>% 
    .[ group==with_filters[[1]] ]
}



readMarkDownTable <- function(markdown_string)
  markdown_string %>% 
  fread(sep="|", header=TRUE, encoding='UTF-8') %>% 
  .[-1] %>% 
  .[, sapply(.,\(col) !is.logical(col)), with=FALSE]

AMECO_Eurostat_country_codes <- '
| CNTRY  | geo        | Country         |
|--------|------------|-----------------|
| EU27   | EU27_2020  | European Union  |
| EA20   | EA20       | Euro area       |
| BEL    | BE         | Belgium         |
| BGR    | BG         | Bulgaria        |
| CZE    | CZ         | Czechia         |
| DNK    | DK         | Denmark         |
| DEU    | DE         | Germany         |
| EST    | EE         | Estonia         |
| IRL    | IE         | Ireland         |
| GRC    | EL         | Greece          |
| ESP    | ES         | Spain           |
| FRA    | FR         | France          |
| HRV    | HR         | Croatia         |
| ITA    | IT         | Italy           |
| CYP    | CY         | Cyprus          |
| LVA    | LV         | Latvia          |
| LTU    | LT         | Lithuania       |
| LUX    | LU         | Luxembourg      |
| HUN    | HU         | Hungary         |
| MLT    | MT         | Malta           |
| NLD    | NL         | Netherlands     |
| AUT    | AT         | Austria         |
| POL    | PL         | Poland          |
| PRT    | PT         | Portugal        |
| ROM    | RO         | Romania         |
| SVN    | SI         | Slovenia        |
| SVK    | SK         | Slovakia        |
| FIN    | FI         | Finland         |
| SWE    | SE         | Sweden          |
' %>% readMarkDownTable

intColnames <- function(dt)
  suppressWarnings(colnames(dt) %>% .[!is.na(as.integer(.))])

getAMECO <- function(ameco_variable_code)
  ## Explanations from:
  ## https://economy-finance.ec.europa.eu/document/download/39ecd989-4f52-4f43-b91b-eb9dada2718d_en?filename=ameco_online_data_files_numerical_coding.pdf
  ## In the `switch` function below:
  ## TRN: Transformations over time (first numerical code)
  ## 3 = Index numbers (and moving arithmetic mean for time periods)
  ## AGG: Aggregation mode (second numerical code)
  ## 1 = Weighted mean of t/t-1 national ratios, weights current prices in ECU/EUR
  ## UNIT: Unit codes (third numerical code)
  ## 0 Original units (e.g. national currency, persons, etc.)
  ## REF: Codes for relative performance (fourth numerical code)
  ## For all other variables simply including a value for the reporting country, the code is 0.
  ameco_variable_code %>% 
  switch('QLCD'='3.1.0.0.',
         stop('The code ',.," doesn't have a defined four digit (X.X.X.X.) prefix code\n",
              'in the `switch` function inside `getAMECO` function!',call.=FALSE)) %>% 
  paste0('https://webgate.ec.europa.eu/fastop/wq/ameco/online?fullVariable=',.,
         ameco_variable_code,'&countries=',
         'AUT,BEL,BGR,CYP,CZE,DEU,DNK,EA20,ESP,EST,EU27,FIN,FRA,GRC,HRV,HUN,IRL,ITA,LTU,LUX,LVA,MLT,',
         'NLD,POL,PRT,ROM,SVK,SVN,SWE&years=',
         seq.int(2000, Sys.Date() %>% substr(1,4) %>% as.integer()) %>% 
           paste(collapse=',')) %>% 
  read_html() %>% 
  html_node(xpath='/html/body/table') %>% 
  html_table(convert=FALSE) %>% # wrong colnames = title "European Commission	Economic and Financial Affairs	Tax and Benefits"
  as.data.table() %>% 
  .[, c('Country',intColnames(.)), with=FALSE] %>%
  melt(id.vars = 'Country',
       measure.vars = intColnames(.),
       variable.name='time',
       value.name='value_') %>%
  merge(AMECO_Eurostat_country_codes, by='Country') %>%
  .[,value_ := as.numeric(value_)] %>%
  .[,time := as.integer(as.character(time))] %>%
  .[,.(geo,time,value_)]

memo_getAMECO <- memoise::memoise(getAMECO)

fromAMECO <- function(ameco_variable_code, time_period=0L)
  toupper(ameco_variable_code) %>% 
  memo_getAMECO() %>% 
  copy() %>% 
  .[,time := time - time_period]

fromSpecialCalculation <- function(indicator, with_filters=NULL)
  tryCatch(get(indicator),
           error = function(e) {
             stop('Function `',indicator,'` is not implemented!', call.=FALSE)
           })(with_filters)


vacancy_rate <- function(with_filters=NULL) {
  f <- eurodata::importData
  dt1 <-
    f('jvs_q_nace2',
      list(indic_em='JOBRATE', s_adj='NSA',
           nace_r2='B-S', sizeclas='TOTAL',
           geo = c(EU_Members_geo_codes,EU_geo_code,EA_geo_code) %without% c('IT','DK','FR','MT') ))
  dt2 <-
    f('jvs_q_nace2', 
      list(indic_em='JOBRATE', s_adj='NSA',
           nace_r2='B-N', sizeclas='GE10', geo='IT'))
  dt3 <-
    f('jvs_q_nace2', 
      list(indic_em='JOBRATE', s_adj='NSA',
           nace_r2='B-N', sizeclas='TOTAL', geo='DK'))
  dt4 <-
    f('jvs_q_nace2', 
      list(indic_em='JOBRATE', s_adj='NSA',
           nace_r2='B-S', sizeclas='GE10', geo=c('FR','MT')))
  list(dt1, dt2, dt3, dt4) %>% 
    lapply(as.data.table) %>% 
    rbindlist() %>% 
    setnames('TIME_PERIOD','time') %>% 
    .[, time := as.integer(substr(time, 1, 4))] %>%  # quarters to years
    .[, .(value_ = mean(value_, na.rm=TRUE)),
      by=.(geo,time)] %>% # across quarters
    merge(data.table(time = seq.int(min(.$time, na.rm=TRUE),max(.$time, na.rm=TRUE)) ),
          by='time', all.y=TRUE) %>% # fill in potentially missing years for the correct setting of the rolling mean window
    setorder(geo,time) %>% 
    .[, value_ := frollmean(value_, 3, algo='exact'),
      by=geo] %>% 
    .[!is.na(value_)]
}

estatDatasetDimNames <- function(EurostatDatasetCode)
  EurostatDatasetCode %>% 
  toupper(.) %>% 
  paste0(eurodata:::EurostatBaseUrl,'datastructure/estat/',.) %>% 
  xml2::read_xml() %>% 
  xml2::as_list() %>% 
  {.$Structure$
      Structures$
      DataStructures$
      DataStructure$
      DataStructureComponents$
      DimensionList} %>% 
  sapply(function(x) attr(x$ConceptIdentity$Ref,'id')) %without% 
  'TIME_PERIOD'

normalizedAdist <- function(x,y)
  c(TRUE,FALSE) %>% 
  lapply(function(p)
    utils::adist(x, y, ignore.case=TRUE, partial=p) %>% 
      {10*./median(.)} %>% 
      round()) %>% 
  do.call(`+`,.)

suggestedWords <- function(wrong_words, correct_words)
  normalizedAdist(wrong_words, correct_words) %>% 
  set_colnames(correct_words) %>% 
  as.data.table() %>% 
  .[, wrong_words := paste0('`',wrong_words,'`')] %>% 
  melt(id.vars='wrong_words',
       variable.name='correct_words') %>% 
  .[, min_val := min(value), by=wrong_words] %>% 
  .[value==min_val] %>% 
  .[,.(wrong_words,correct_words)] %>% 
  .[, .(correct_words = paste0('`',correct_words,'`',collapse=' or ')),
    , by=wrong_words] %>% 
  setnames(c('wrong_words','correct_words'),
           c('Wrong name(s)','  Suggested name(s)')) %>% 
  {capture.output(print(., nrows=nrow(.), row.names=FALSE))} %>% 
  paste(collapse='\n')
