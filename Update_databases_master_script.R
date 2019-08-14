#############Master script###################

rm(list = ls()) # command to clear all variables from R environment

# things to check and may need to modify before running -------------------

# dates
todays_date_formatted <- as.Date("2019-08-13")

# what device are you running this script on? 
computer = 'mac' # set this to either 'mac' or 'pc' or 'other' (Georgia = W:/ as I have string mounted differently)

# task related 
max_tasks = 5 # maximum total number of tasks done in a single week 
max_MID = 13 # maximum times a single person has done the MID
max_MMI = 1 # maximum times a single person has done the MMI

# database related - update with names of latest pulls (without file extension)
latest_ctdb_pull = "SDAN_and_BSD_MBDU.08.12.2019" # change to name of excel file you want to use 
latest_dawba_pull = "DAWBA.92200052_08122019" # will be a cvs file 
latest_sdq_pull = "Total.2019-08-12T13_47_52" # will be a text file 

# check list of current IRTAs is correct
current_IRTAs_full <- c("Kenzie Jackson", "Katy Chang", "Christine Wei", "Stuart Kirwan", "Lisa Gorham", "Kate Haynes", "Chris Camp")
current_IRTAs_init <- c("KJ", "KC", "CW", "SK", "LG", "KH", "CC")

# If CBT report, enter participant's initials below & into the 
Participant <- c("ANEN") # enter the initials of the participant you want to produce a summary report for 
Clinician <- c("Kathryn") # enter either "Kathryn", "Ken", "Argyris", "Chana", "Jeasmine"
report_type <- c("progress") # enter either "progress" (if still in treatment) or "final" (if you want final summary of their treatment)

# modules to run ----------------------------------------------------------

# enter number below that you want to run - reference the list below 
modules2run <- c(8)

# description of modules: 
# 0 = none
# 1 = "update master IRTA tracker and tasks database" = W:/Georgia/Analysis_Georgia/Database/IRTA tracker merge/IRTA_Merge_Code_Georgia_06102019.R    ***UPDATE!!!
# 2 = "update master database" = W:/Georgia/Analysis_Georgia/Database/Database code_06192019.R       ***UPDATE!!!
# 3 = "update DAWBA database & deletion list" = W:/Georgia/Analysis_Georgia/Database/DAWBA_database_and_deletions_06192019.R     ***UPDATE!!!
# 4 = "produce weekly numbers" 
#         updates the dataset for this = W:/Georgia/Analysis_Georgia/Database/IRTA tracker merge/creating weekly meeting sheet/rough work extracted from IRTA master tracker code_04182019.R
#         updates the webpage for this = W:/Georgia/Analysis_Georgia/Database/IRTA tracker merge/creating weekly meeting sheet/attempt1.Rmd
# 5 = all of the above
# 6 = modules 1 & 2
# 7 = update master database & create CBT progress report - remember to change input the initials of the participant above & specify whether you want a progress or final report 
# 8 = create CBT report only (master database already updated)

# directories -------------------------------------------------------------

if (computer=="pc") {
  string = 'W:/'
  sdan1 = 'Y:/'
} else if (computer=="mac") {
  string = '/Volumes/string-mbd/'
  sdan1 = '/Volumes/sdan1/'
} else { # if using a PC and your drives aren't mounted as specified above, enter what letter your drives are mounted under here... 
  string = 'W:/'
  sdan1 = 'Y:/'
}

# main folders needed
georgia = paste0(string, "Georgia/Analysis_Georgia/Database/") # temp useful directory while scripts are still under development 
database_location = paste0(string, "Database/Master Psychometric Database/") # tasks database also located here 
IRTA_tracker_location = paste0(string, "Database/Master Participant Tracker/")
weekly_numbers_location = paste0(georgia, "IRTA tracker merge/creating weekly meeting sheet/") # to change with server restructuring 
referrals_location = paste0(string, "RA Instruction Manuals/") # to change with server restructuring 
graphs_location = paste0(database_location, "graphs/")

# other data: 
imputed_mfqs = paste0(database_location, 'other_data_never_delete/IMPUTED_MFQ_NEVER_DELETE.csv')
data_23495 = paste0(database_location, 'other_data_never_delete/4711-5358-6649-5157_23495_pull_03222019_01-M-0192.xlsx') # this is the data we pulled from SDQ for the participant who signed into 0192 as an adult
data_23544 = paste0(database_location, 'other_data_never_delete/8768-8233-7459-5808_23544_pull_05222019_02-M-0021.xlsx') 
data_22279 = paste0(database_location, 'other_data_never_delete/2738-0093-0639-3598_22279_pull_07242019_02-M-0186.xlsx') 
data_old_dx_checklist = paste0(database_location, 'other_data_never_delete/ksads_dx_checklist.2019-07-26T14_03_12.txt')
data_old_mdd_form = paste0(database_location, 'other_data_never_delete/mdd.2019-07-26T14_03_38.txt')

# location of backups
backup_location = paste0(IRTA_tracker_location, "IRTA_Master_Backups/") # IRTA tracker backup location 
folder_backup = paste0(database_location, "Backup/") # Database backup location 

# data pulls 
dawba_pull = paste0(database_location, "DAWBA_pull/")
ctdb_pull = paste0(database_location, "CTDB_pull/")
sdq_pull = paste0(database_location, "SDQ_pull/")

# related to the CBT database
CBT_location = paste0(string, "Database/Master Psychometric Database/CBT/") 
CBT_backup = paste0(CBT_location, "Backup/") 
saving_reports = paste0(CBT_location, "Reports/")

# task related, e.g. tracker locations 
MID_tracker_location = paste0(sdan1, "Data/MID1/")
MMI_tracker_location = paste0(sdan1, "Data/MMI/")
# to add once these are in use: 
# ReACT_tracker_location = paste0(string, "ReACT/")
# MEG_tracker_location = paste0(string, "MEG/")

# packages ----------------------------------------------------------------

suppressPackageStartupMessages(library(readxl))
# suppressPackageStartupMessages(library(rJava))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr)) 
suppressPackageStartupMessages(library(summarytools)) 
suppressPackageStartupMessages(library(rmarkdown)) 
suppressPackageStartupMessages(library(eeptools)) 
suppressPackageStartupMessages(library(openxlsx)) 
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(knitr))

# update master IRTA tracker and tasks database ---------------------------

if (modules2run==1 | modules2run==5 | modules2run==6) {
  
  suppressWarnings(source(paste0(georgia, 'IRTA tracker merge/IRTA_Merge_Code_Georgia_08082019.R')))

} else {
  
  print("master IRTA tracker and tasks database not updated - NA")
  
}

# update master database --------------------------------------------------

if (modules2run==2 | modules2run==5 | modules2run==6 | modules2run==7) {

  suppressWarnings(source(paste0(georgia, 'Database code_08122019.R')))

} else {

  print("master database not updated - NA")

}

# update DAWBA database & deletion list -----------------------------------

if (modules2run==3 | modules2run==5) {
  
  suppressWarnings(source(paste0(georgia, 'DAWBA_database_and_deletions_06192019.R')))

} else {
  
  print("DAWBA database & deletion list not updated - NA")
  
}

# produce weekly numbers --------------------------------------------------

if (modules2run==4 | modules2run==5) {

  suppressWarnings(source(paste0(weekly_numbers_location, 'rough work extracted from IRTA master tracker code_04182019.R')))
  render(paste0(weekly_numbers_location, "attempt1_04182019.Rmd"))

} else {

  print("weekly numbers not produced - NA")

}

# Produce CBT report ------------------------------------------------------

# to do: create script for making weekly individual CBT spreadsheets for Kathryn with measures she requested & insert code below to produce this too. 

if (modules2run==7 | modules2run==8) {
  
  out_file <- paste0(saving_reports, Participant)
  
  if (file.exists(out_file)){
    print("file exists")
  } else {
    print("doesn't exist, creating directory")
    dir.create(file.path(saving_reports, Participant))
  }
  
  if (report_type=="progress") {
    render(paste0(CBT_location, "Produce_CBT_progress_report_08092019.Rmd"), output_format = "word_document", 
           output_file = paste0(Participant, "_", todays_date_formatted), output_dir = out_file)
  } else {
    render(paste0(CBT_location, "Produce_CBT_final_report_08092019.Rmd"), output_format = "word_document", 
           output_file = paste0(Participant, "_final_", todays_date_formatted), output_dir = out_file)
    render(paste0(CBT_location, "Produce_CBT_final_report_provider_08092019.Rmd"), output_format = "word_document", 
           output_file = paste0(Participant, "_final_provider_", todays_date_formatted), output_dir = out_file)
    }

} else {

  print("No CBT report generated - NA")

}

# end ---------------------------------------------------------------------


