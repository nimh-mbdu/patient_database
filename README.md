# Scripts to update the MBDU databases
---
### Author: Georgia O'Callaghan
---
### Location of repository on the server: Database/Database_Scripts_Github/
---
## Description of scripts: 
1. Master code needed to set up environmental variables & directories: 
	Update_databases_master_script.R
2. Code to merge IRTA trackers to make a master patient list: 
	IRTA_Merge_Code.R
3. Code to create clinical, behavioural and treatment databases using information from the master IRTA tracker, CTDB and SDQ: 
	Database_code.R
---
### Created in RStudio: "R version 3.6.1 (2019-07-05)"
---
Required R libraries (aka packages): 
readxl
writexl
tidyr
dplyr 
summarytools 
rmarkdown 
eeptools 
openxlsx 
data.table
reshape2
stringr
lubridate
ggplot2
rlang
purrr
tidyverse
knitr
shiny