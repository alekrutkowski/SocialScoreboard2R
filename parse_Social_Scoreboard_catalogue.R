library(data.table)
library(magrittr)

# setwd(r"{C:\Users\rutkoal\OneDrive - European Commission\Social_Scoreboard_in_R\FRESH NEW APPROACH}")

# parseCond <- function(cond_string)
#   cond_string %>% 
#   gsub("\\s*(.*)\\s*=\\s*(.*)\\s*",
#        '\\1="\\2"',.)

Cat <-
  fread('catalogue - JER Scoreboard.csv') %>% 
  .[, Order := Order %>% 
      {paste0(((.+1000)*10) %>% sprintf("%05d", .),'_ex',.)}]

Cat_melt <-
  Cat %>% 
  melt(id.vars=c("Order","table"),
       measure.vars=colnames(.) %>% grep('^cond',.,value=TRUE),
       variable.name="cond_num",
       value.name="cond") %>% 
  .[cond!="" | table %in% c("OECD_Pisa2","desi",
                            "tesem060","tepsr_lm210","sdg_05_20","tepsr_wc310",
                            "tespm140","tepsr_sp210","tepsr_sp310")] %>% 
  .[, cond := gsub("\\s*(.*)\\s*=\\s*(.*)\\s*",
                   '\\1="\\2"',cond)] %>% 
  .[, .(cond = paste(cond,collapse=', '))
    , by="Order"]

Cat %>% 
  merge(Cat_melt, by="Order") %>% 
  setorder(Order) %>% {
    paste0(
      '
inside(SCOREBOARD_INDICATORS, indicator_number = "',.$Order,'") = 
specification(
name = "',.$Indicator,'",
chapter = "',.$Chapter,'",
group = "',.$Group,'",
type = "',.$type,'",
high_is_good = ',.$sense=="pos",',
value = ',ifelse(toupper(.$standard)=="Y","fromEurostatDataset","fromSpecialCalculation"),
      '("',.$table,'", 
    with_filters(',.$cond,'))
)
      '
    )
  } %>% 
  gsub('with_filters(, , , , )','with_filters(NA)',.,fixed=TRUE) %>% 
  cat(file='Scoreboard_indicators__definitions.R')
