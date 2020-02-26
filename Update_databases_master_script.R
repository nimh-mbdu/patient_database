#############Master script###################

rm(list = ls()) # command to clear all variables from R environment

# directories -------------------------------------------------------------

# what device are you running this script on? 
computer = 'pc' # set this to either 'mac' or 'pc' or 'other' (Georgia = W:/ as I have string mounted differently)

if (computer=="pc") {
  string = 'W:/'
  sdan1 = 'Y:/'
} else if (computer=="mac") {
  string = '/Volumes/string-mbd/'
  sdan1 = '/Volumes/sdan1/'
} else { # if using a PC and your drives aren't mounted as specified above, enter what letter your drives are mounted under here... 
  string = 'Z:/'
  sdan1 = 'Y:/'
}

# main folders needed
scripts = paste0(string, "Database/Database_Scripts_Github/") # temp useful directory while scripts are still under development 
database_location = paste0(string, "Database/Master Psychometric Database/") # tasks database also located here 
IRTA_tracker_location = paste0(string, "Database/Master Participant Tracker/")
weekly_numbers_location = paste0(string, "Minutes and conversation archives/Weekly Meeting Sheet/")
clinician_sheet_location = paste0(string, "Minutes and conversation archives/Thursday Clinical Meetings/Clinician Sheet/")
referrals_location = paste0(string, "RA Instruction Manuals/") # to change with server restructuring 
graphs_location = paste0(database_location, "graphs/")

# other data: 
imputed_mfqs = paste0(database_location, 'other_data_never_delete/IMPUTED_MFQ_NEVER_DELETE.csv')
data_23495 = paste0(database_location, 'other_data_never_delete/4711-5358-6649-5157_23495_pull_03222019_01-M-0192.xlsx') # this is the data we pulled from SDQ for the participant who signed into 0192 as an adult
data_23544 = paste0(database_location, 'other_data_never_delete/8768-8233-7459-5808_23544_pull_05222019_02-M-0021.xlsx') 
data_22279 = paste0(database_location, 'other_data_never_delete/2738-0093-0639-3598_22279_pull_07242019_02-M-0186.xlsx')
data_hyphen_issue = paste0(database_location, 'other_data_never_delete/data_plusids_without_hyphens.xlsx')
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
CBT_location = paste0(database_location, "CBT/") 
CBT_backup = paste0(CBT_location, "Backup/") 
saving_reports = paste0(CBT_location, "Reports/")

# related to the CBT database
inpatient_location = paste0(database_location, "Inpatient/") 
inpatient_backup = paste0(inpatient_location, "Backup/") 
inpatient_summary_location = paste0(inpatient_location, "Reports/")

# task related, e.g. tracker locations 
MID_tracker_location = paste0(sdan1, "Data/MID1/")
MMI_tracker_location = paste0(sdan1, "Data/MMI/")
MEG_tracker_location = paste0(string, "Tasks/MEG/")

# packages ----------------------------------------------------------------

suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl)) 
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
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(chron))

# things to check and may need to modify before running -------------------

to_change <- read_excel(paste0(scripts, "to_change_before_running_master_script.xlsx"))

# date
todays_date_formatted <- c(to_change$todays_date_formatted)
todays_date_formatted <- as.Date(todays_date_formatted)
last_week_date_formatted <- todays_date_formatted - as.difftime(7, unit="days")
# todays_date_formatted <- as.Date("2019-11-15") # if you want to manually set the date (outside of the excel sheet)

# task related 
max_tasks <- c(to_change$max_tasks)
max_MID <- c(to_change$max_MID)
max_MMI <- c(to_change$max_MMI)
max_MEG <- c(to_change$max_MEG)

# database related - update with names of latest pulls (without file extension)
latest_ctdb_pull <- c(to_change$latest_ctdb_pull)
latest_dawba_pull <- c(to_change$latest_dawba_pull)
latest_sdq_pull <- c(to_change$latest_sdq_pull)

# check list of current IRTAs is correct
current_IRTAs_full <- c("Kenzie Jackson", "Katy Chang", "Christine Wei", "Stuart Kirwan", "Lisa Gorham", "Kate Haynes", "Chris Camp")
current_IRTAs_init <- c("KJ", "KC", "CW", "SK", "LG", "KH", "CC")

# functions ---------------------------------------------------------------

count_na <- function(x) sum(is.na(x))

FitFlextableToPage <- function(ft, pgwidth = 10){
  ft_out <- ft %>% autofit(., add_h = 0.3)
  ft_out <- width(ft_out, width = dim(ft_out)$widths*10.5/(flextable_dim(ft_out)$widths))
  return(ft_out)
}

# modules to run ----------------------------------------------------------

modules2run <- c(to_change$modules2run)
# to overwrite the above, uncomment the below and enter number that you want to run isntead - reference the list below
# modules2run <- c(1)

# description of modules: 
# 0 = none
# 1 = "update master IRTA tracker and tasks database" = string-mbd/Database/Database_Scripts_Github/IRTA_Merge_Code.R
# 2 = "update master database" = string-mbd/Database/Database_Scripts_Github/Database_code.R
# 3 = "update DAWBA database & deletion list" = string-mbd/Database/Database_Scripts_Github/DAWBA_database_and_deletions.R
# 4 = "produce weekly numbers" = string-mbd/Database/Database_Scripts_Github/Research_meeting_numbers.R
# 5 = all of the above
# 6 = modules 1 & 2
# 7 = update master database & create CBT progress report - remember to change input the initials of the participant above & specify whether you want a progress or final report 
# 8 = create CBT report only (master database already updated)
# 9 = modules 1 & 4
# 10 = generate clinician meeting sheet
# 11 = modules 1 & 10
# 12 = create inpatient summary sheet 

# update master IRTA tracker and tasks database ---------------------------

if (modules2run==1 | modules2run==5 | modules2run==6 | modules2run==9 | modules2run==11 | modules2run==13) {
  
  suppressWarnings(source(paste0(scripts, 'IRTA_Merge_Code.R')))
  
  # Note - check that the files have saved with a new date - this won't happen if someone else has the file open.
  # If any of the files have not exported properly, uncomment out and run the relevant line below, which will save it under a new name: 
  
  # master_IRTA_latest %>% write_xlsx(paste0(IRTA_tracker_location,"MASTER_IRTA_DATABASE_updated.xlsx"))
  # task_reshape_master_QC %>% write_xlsx(paste0(IRTA_tracker_location,"TASKS_DATABASE_QC_updated.xlsx"))
  
  # master_IRTA_screens_latest %>% write_xlsx(paste0(IRTA_tracker_location,"REFERRAL_AND_SCREENING_DATABASE_updated.xlsx"))
  # master_IRTA_oldest_screens_latest %>% write_xlsx(paste0(IRTA_tracker_location,"OLD_REFERRALS_DATABASE_updated.xlsx"))
  
} else {
  
  print("master IRTA tracker and tasks database not updated - NA")
  
}

# update master database --------------------------------------------------

if (modules2run==2 | modules2run==5 | modules2run==6 | modules2run==7 | modules2run==13) {

  suppressWarnings(source(paste0(scripts, 'Database_code.R')))
  
  # Note - check that the files have saved with a new date - this won't happen if someone else has the file open.
  # Similar to the above, if any of the database files have not exported properly, uncomment out and run the relevant line below, which will save it under a new name: 
  
  # Psychometrics_treatment %>% write_xlsx(paste0(database_location, "MASTER_DATABASE_CLINICAL_updated.xlsx"))
  # Psychometrics_behav %>% write_xlsx(paste0(database_location, "MASTER_DATABASE_BEHAVIOURAL_updated.xlsx"))
  
  # CBT_report %>% write_xlsx(paste0(database_location, "CBT/MASTER_DATABASE_CBT_updated.xlsx"))
  # MATCH_tracker %>% write_xlsx(paste0(database_location, "Inpatient/MASTER_DATABASE_Inpatient_updated.xlsx"))

} else {

  print("master database not updated - NA")

}

# update DAWBA database & deletion list -----------------------------------

if (modules2run==3 | modules2run==5) {

  suppressWarnings(source(paste0(scripts, 'DAWBA_database_and_deletions.R')))

} else {

  print("DAWBA database & deletion list not updated - NA")

}

# produce weekly numbers --------------------------------------------------

if (modules2run==4 | modules2run==5 | modules2run==9) {
  
  suppressPackageStartupMessages(library(kableExtra))
  render(paste0(scripts, 'Reports/Research_meeting_numbers.Rmd'), 
         output_format = "html_document",
         # output_format = "word_document", 
         output_file = paste0("Weekly_Numbers_", todays_date_formatted), output_dir = weekly_numbers_location)
  detach(package:kableExtra)
  
} else {

  print("weekly numbers not produced - NA")

}

# Produce CBT report ------------------------------------------------------

# to do: create script for making weekly individual CBT spreadsheets for Kathryn with measures she requested & insert code below to produce this too. 

if (modules2run==7 | modules2run==8) {
  
  # make sure this file has been updated with all the patients you want to create reports for: 
  cbt_participants <- read_excel(paste0(scripts, "CBT_scripts/cbt_reports_to_produce.xlsx")) 
  
  for(a in seq_len(nrow(cbt_participants))) {
  iter9 <- as.numeric(a)
    # iter9 = 1
    
    Participant <- as.character(cbt_participants[iter9, 1])
    Clinician <- as.character(cbt_participants[iter9, 2])
    report_type <- as.character(cbt_participants[iter9, 3])
    out_file <- paste0(saving_reports, Participant)
    
    if (file.exists(out_file)){
      print("file exists")
    } else {
      print("doesn't exist, creating directory")
      dir.create(file.path(saving_reports, Participant))
    }
    
    if (report_type=="progress") {
      print(string)
      render(paste0(scripts, "CBT_scripts/Produce_CBT_progress_report.Rmd"), output_format = "word_document", 
             output_file = paste0(Participant, "_", todays_date_formatted), output_dir = out_file)
    } else {
      render(paste0(scripts, "CBT_scripts/Produce_CBT_final_report.Rmd"), output_format = "word_document", 
             output_file = paste0(Participant, "_final_", todays_date_formatted), output_dir = out_file)
      render(paste0(scripts, "CBT_scripts/Produce_CBT_final_report_provider.Rmd"), output_format = "word_document", 
             output_file = paste0(Participant, "_final_provider_", todays_date_formatted), output_dir = out_file)
    }
    
    # creating individual patient BA tracker:
    CBT_report %>% filter(Initials==Participant) %>% 
      select(FIRST_NAME:DOB, Age_at_visit:Clinical_Visit_Type, c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed,
             s_mfq1w_tot, p_mfq1w_tot, p_mfq1w_parent, s_ari1w_tot, p_ari1w_tot, p_ari1w_parent, s_scared_tot, p_scared_tot, p_scared_parent,
             s_shaps_tot, s_lsas_tot, c_cadam_tot, s_vadis_tot, c_cgi_severity, c_cgi_global, c_cdrs_tot, matches("s_fua_"),
             matches("p_fua_"), s_ba_sess_mood_diff, s_ba_sess_difficulty_diff, s_ba_sess_enjoy_diff,
             s_ba_sess_anxiety_diff, s_ba_sess_satisfaction_diff, s_after_ba_sess_come_again, 
             # matches("c_medsclin_"),
             matches("s_baexpout_act_1_")) %>% 
      write_xlsx(paste0(out_file, "/", Participant, "_BA_TRACKER.xlsx"))
      
}

rm(data, start, end, weeks_treat, provider, gender, datebirth, age, dx, pronoun, Participant, CBT_report, date_variabes, numeric_variables, 
   report_type, out_file, iter9, cbt_participants, a)
rm(list=ls(pattern="f_name"))
rm(list=ls(pattern="l_name"))
rm(list=ls(pattern="linician"))

} else {

  print("No CBT report generated - NA")

}

# produce clinician meeting sheet -----------------------------------------

if (modules2run==10 | modules2run==11) {
  
  suppressPackageStartupMessages(library(flextable))
  render(paste0(scripts, 'Reports/Clinician_sheet.Rmd'),
         # output_format = "html_document",
         output_format = "word_document",
         output_file = paste0("Clinician_sheet_", todays_date_formatted), output_dir = clinician_sheet_location)
  detach(package:flextable)
  
} else {
  
  print("clinician meeting sheet not produced - NA")
  
}

# produce inpatient summaries ---------------------------------------------

if (modules2run==12 | modules2run==13) {
  
  suppressPackageStartupMessages(library(flextable))
  render(paste0(scripts, 'Reports/Inpatient_summary.Rmd'), output_format = "word_document",
         output_file = paste0("Inpatient_summary_", todays_date_formatted), output_dir = inpatient_summary_location)
  detach(package:flextable)
  
} else {
  
  print("Inpatient summary not produced - NA")
  
}

# end ---------------------------------------------------------------------

rm(to_change)
