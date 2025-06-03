# Inventory of files

All the "input" files (scripts and template files) are located in one folder, no
subfolders are used for storing these "input" files. All the files are available and
[version-controlled](https://en.wikipedia.org/wiki/Version_control) at
https://github.com/alekrutkowski/SocialScoreboard2R.

### Scripts

The scripts, named like `Scoreboard_File_N.R` where `N` is an integer, correspond 
to the numbered output files.

```
- Scoreboard_MAIN.R
- Scoreboard_functions.R
- Scoreboard_indicators__definitions.R
- Scoreboard_output.R
- Scoreboard_runs_compared.R
- Scoreboard_File_1.R
- Scoreboard_File_2.R
- Scoreboard_File_3.R
- Scoreboard_File_4.R
- Scoreboard_File_5.R
- Scoreboard_File_6.R
```

### Template files

To be possibly adjusted based on F1's requests.

```
- Social Scoreboard file2 TEMPLATE.xlsx
- Social Scoreboard file3 TEMPLATE.xlsx
- Social Scoreboard file4 TEMPLATE.xlsx
- Social Scoreboard file5 TEMPLATE.xlsx
- SSBcc_JER25_ 4 Nov 2024.xlsx  <-- for file 6
```

### Data files

For one of the indicators, an Excel file needs to be downloaded from:
https://circabc.europa.eu/ui/group/d14c857a-601d-438a-b878-4b4cebd0e10f/library/c5a8b987-1e37-44d7-a20e-2c50d6101d27/details

# Running the scripts

It is recommended to use RStudio IDE to run the files.

In the code below replace XXXX with your user name and YYYY with your password
used in the web browser for web proxy authentication and replace the path in
the 7th code line:

```r
# In the Commission:
Sys.setenv(http_proxy =
  "http://XXXX:YYYY@proxy-t2-bx.welcome.ec.europa.eu:8012")
Sys.setenv(https_proxy =
  "http://XXXX:YYYY@proxy-t2-bx.welcome.ec.europa.eu:8012")
# Replace the path below with the actual path, use / instead of \
setwd("/path/to/the/folder/with/Scoreboard/scripts")
source("Scoreboard_MAIN.R") # This script will run all the needed sub-scripts
```

# System requirements

The following packages need to be installed:

```r
needed_pkgs <-
  c("collapse","countrycode","data.table","eurodata","kit","magrittr",
    "memoise","OECD","openxlsx2","rvest","stringr","tibble","utils","xml2")
installed_pkgs <-
  row.names(installed.packages())
for (n in needed_pkgs)
  if (!n %in% installed_pkgs)
      install.packages(n)
```
