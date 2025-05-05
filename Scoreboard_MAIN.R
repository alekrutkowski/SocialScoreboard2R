# setwd('C:/Users/rutkoal/OneDrive - European Commission/Social_Scoreboard_in_R/FRESH NEW APPROACH')

runScript <- function(file_name) {
  message('\n\n====== Starting ',file_name,' ======\n')
  source(file_name)
  message('\n\U2705 Finished ',file_name,'\n')
}

init <- function() {
  runScript("Scoreboard_functions.R")
  SCOREBOARD_INDICATORS <<- list()
}
if (interactive() && exists("SCOREBOARD_INDICATORS"))
  repeat {
    answer <- toupper(readline("Re-calculate everything? (Y/N): "))
    if (answer=="Y") {
      init()
      break
    } else if (answer=="N") break
  } else init()

# Order matters:
# preCheckInidcators('Scoreboard_indicators__definitions.R')
runScript('Scoreboard_indicators__definitions.R')
runScript('Scoreboard_output.R')
runScript('Scoreboard_runs_compared.R')
runScript('Scoreboard_File_1.R')
runScript('Scoreboard_File_2.R')
runScript('Scoreboard_File_3.R')
runScript('Scoreboard_File_4.R')
runScript('Scoreboard_File_5.R')
runScript('Scoreboard_File_6.R')

message('\n\n\U2705 All Scoreboard scripts finished.')
