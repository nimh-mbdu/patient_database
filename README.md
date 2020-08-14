# Scripts to update the MBDU databases
---
### Author: Georgia O'Callaghan
---
### Location of repository on the server: Database/Database_Scripts_Github/
---
## Description of scripts: 
1. Master script needed to set up environmental variables & directories + to run other scripts from: 
	Update_databases_master_script.R
2. Scripts to merge patient trackers into a master patient list: 
	IRTA_Merge_Code.R
	Other_functions/Schedule_script_functions.R 
3. Script to create clinical, behavioural and treatment databases using information from the master patient tracker, CTDB and SDQ: 
	Database_code.R
4. Script to create DAWBA database & generate lists for deleting old data from server: 
	DAWBA_database_and_deletions.R
5. Scripts to generate weekly numbers for clinical meeting: 
	Reports/Research_meeting_numbers.Rmd
	Other_functions/Referrals_summary.R
	Other_functions/check_referrals.R
6. Script to generate eval and screening summaries for clinical meeting: 
	Reports/Clinician_sheet.Rmd
7. Scripts to generate CBT reports: 
	CBT_scripts/Produce_CBT_final_report.Rmd
	CBT_scripts/Produce_CBT_final_report_provider.Rmd
	CBT_scripts/Produce_CBT_progress_report.Rmd
8. Scripts to generate inpatient summary: 
	Reports/Inpatient_summary.Rmd
	Reports/Inpatient_end_of_treatment_graphs.R
9. Script to generate clinician supervision summary: 
	Reports/All_treatment_summary.Rmd
10. Script to generate numbers for annual continuing review (CR) for IRB:
	Reports/Continuing_review.Rmd
10. COVID19 related research database generation: 
	Reports/crisis_dataset_description.Rmd
	Other_functions/Crisis_distribution.R
11. Other: 
	Other_functions/followup_counts.R


---
## Manual: 

All procedures for running these scripts are described in the latest version of the 'database_and_report_scripts_manual' contained in the  repository on the server. This has not been uploaded to GitHub as it contains account login information for our online data collection platforms. 

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
flextable
shiny
kableExtra
chron
ggpubr