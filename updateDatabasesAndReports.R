updateDatabasesAndReports=function(module){

msg = " Need to pass the number of the module you would like to run \n
  Some common modules are:
    1: update master IRTA tracker & produce QC reports, 
    2: update master database, 
    6: runs 1 and 2
  Reports:
    4: Weekly numbers, 9: runs 1 and 4
    8: CBT report, 7: runs 1 and 8
    10: Clinician sheet, 11: runs 1 and 10
    12: Inpatient report, 13: runs 1, 2, and 12
    14: Supervision sheet, 15: runs 1,2, and 14
    for full list see 'to_change_before_running_master_script.xlsx' "

  modules2run=module


if(modules2run > 15 | modules2run < 0){
  stop(msg)
}

start_time <- Sys.time()

computer = Sys.getenv("R_PLATFORM") #no need to set this anymore
username = Sys.getenv("USER") #no need to set this anymore
isdocker = Sys.getenv("IS_DOCKER")

if (isdocker!="") { #running inside of a container
  string="/string-mbd/"
  sdan1="/sdan1/"
} else if (Sys.info()[[1]]=="Windows"){ #R_platform returns empty string for windows! Check via Sys.info()
  string = 'W:/string-mbd/'
  sdan1 = 'Y:/sdan1/'
} else {
  if (length(grep("apple",computer)) > 0) {
    string = '/Volumes/string-mbd/'
    sdan1 = '/Volumes/SDAN1/'
  } else if (length(grep("linux",computer))>0 & length(grep("jsbach",Sys.getenv("HOSTNAME")) > 0)){#"jsbach") 
    string = paste0('/home/', username, '/cifs/jsbach/string-mbd/')
    sdan1 = paste0('/home/', username, '/cifs/jsbach/sdan1/')
  }else { 
    warning("Can't determine computer type. Manually set string and sdan1 servers!")
  }
}
# if using a PC and your drives aren't mounted as specified above, enter what letter your drives are mounted under here... 
#string = 'W:/'
#sdan1 = 'Y:/'

todays_date_formatted <- Sys.Date()
print(paste("Running module:", modules2run))
print(paste("Today's date:", todays_date_formatted))
print(paste("OS:", computer))
print(paste("Username:", username))
print(paste("String server:", string))
print(paste("SDAN server:", sdan1))

# main folders needed
scripts = paste0(string, "Database/Database_Scripts_Github/") # temp useful directory while scripts are still under development 
database_location = paste0(string, "Database/Master Psychometric Database/") # tasks database also located here 
IRTA_tracker_location = paste0(string, "Database/Master Participant Tracker/")
weekly_numbers_location = paste0(string, "Minutes and conversation archives/Weekly Meeting Sheet/")
clinician_sheet_location = paste0(string, "Patient Information/Clinician Sheet/")
referrals_location = paste0(string, "RA Instruction Manuals/") # to change with server restructuring 
graphs_location = paste0(database_location, "graphs/")
clinician_supervision_location = paste0(string, "Psychotherapy/Supervision/")
otherfunctions=paste0(scripts, "Other_functions", .Platform$file.sep)

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

# related to the Inpatient database
inpatient_location = paste0(database_location, "Inpatient/") 
inpatient_backup = paste0(inpatient_location, "Backup/") 
inpatient_summary_location = paste0(inpatient_location, "Reports/")

# task related, e.g. tracker locations 
MID_tracker_location = paste0(sdan1, "Data/MID1/")
MMI_tracker_location = paste0(sdan1, "Data/MMI/")
MEG_tracker_location = paste0(string, "Tasks/MEG/")
MMI_recovery_file_location = paste0(string, "Tasks/MMI_recovery/")
supreme_file_location = paste0(string, "Tasks/supreme/data/")

# packages ----------------------------------------------------------------

# source script to install packages if missing here 
#source(paste0(otherfunctions,"installpackages.R"))
# packages <- c("readxl", "writexl", "tidyr", "dplyr", "summarytools", "rmarkdown", "eeptools", 
#               "openxlsx", "data.table", "reshape2", "stringr","lubridate","ggplot2","rlang", 
#               "purrr", "tidyverse","shiny","knitr","ggpubr","chron","kableExtra", "ggthemes", "ggrepel", "flextable")
# 
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))  
# }

# load packages 
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
# date
last_week_date_formatted <- todays_date_formatted - as.difftime(7, unit="days")

# task related 
#change it later so we can pass if need to
max_tasks <- 5
max_MID <- 13
max_MMI <- 1
max_MEG <- 4

# crisis recruitment related - these numbers obtained by running the following script: 
# 'Database/Master Psychometric Database/COVID19/identifying_num_agreed_participate.R'
child_agreed <- 179 #was 177, changed to 179 for new rounds as of September 23, 2020
parent_agreed <- 139 #this remained the same as of September 23, 2020

# database related - update with names of latest pulls (without file extension)
latest_ctdb_pull <- "SDAN_and_BSD_MBDU.08.21.2020" #this is the latest we have and won't change
latest_dawba_pull <- "DAWBA.92200052_09082020" #this will change every 3-4 months
#need to download the sdq file everyday, get the latest from directory
sdqfiles=list.files(path = paste0(sdq_pull), pattern = "^Total", all.files = FALSE,full.names = FALSE, recursive = FALSE,
                    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) #lists in alphabetical order if full.names=TRUE, then can get the last file
latest_sdq_pull=gsub(".txt","",sdqfiles[length(sdqfiles)])
print(paste("SDQ file:",latest_sdq_pull))

# check list of current IRTAs is correct
current_IRTAs_full <- c("Payton Fors", "Lily Eisner","Karen Qi", "Jeremy Taigman", "Lisa Gorham", "Chris Camp")
current_IRTAs_init <- c("PF", "LE", "KQ", "JT", "LG", "CC")

# functions ---------------------------------------------------------------

count_na <- function(x) sum(is.na(x))

FitFlextableToPage <- function(ft, pgwidth = 10){
  ft_out <- ft %>% autofit(., add_h = 0.3)
  ft_out <- width(ft_out, width = dim(ft_out)$widths*10.5/(flextable_dim(ft_out)$widths))
  return(ft_out)
}

# update master IRTA tracker and tasks database ---------------------------

if (modules2run==1 | modules2run==5 | modules2run==6 | modules2run==9 | modules2run==11 | modules2run==13 | modules2run==15) {

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

if (modules2run==2 | modules2run==5 | modules2run==6 | modules2run==7 | modules2run==13 | modules2run==15) {

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
    # iter9 = 6
    
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
             output_file = paste0(Participant, "_final", todays_date_formatted), output_dir = out_file)
      render(paste0(scripts, "CBT_scripts/Produce_CBT_final_report_provider.Rmd"), output_format = "word_document", 
             output_file = paste0(Participant, "_final_provider", todays_date_formatted), output_dir = out_file)
    }
    
    # creating individual patient BA tracker:
    CBT_report %>% filter(Initials==Participant) %>% 
      select(FIRST_NAME:DOB, Age_at_visit:Clinical_Visit_Type, c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed,
             s_mfq1w_tot, p_mfq1w_tot, p_mfq1w_parent, s_ari1w_tot, p_ari1w_tot, p_ari1w_parent, s_scared_tot, p_scared_tot, p_scared_parent,
             s_shaps_tot, s_lsas_tot, c_cadam_tot, s_vadis_tot, c_cgi_severity, c_cgi_global, c_cdrs_tot, c_ygtss_severity_tot, c_ygtss_tot, 
             matches("s_srsors_"), matches("s_fua_"), matches("p_fua_")
        # , s_ba_sess_mood_diff, s_ba_sess_difficulty_diff, s_ba_sess_enjoy_diff, s_ba_sess_anxiety_diff, s_ba_sess_satisfaction_diff, 
        # s_after_ba_sess_come_again, matches("c_medsclin_"), matches("s_baexpout_act_1_")
               ) %>% 
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

# produce clinician supervision sheet: all treatment ----------------------

if (modules2run==14 | modules2run==15) {
  
  suppressPackageStartupMessages(library(flextable))
  render(paste0(scripts, 'Reports/All_treatment_summary.Rmd'), output_format = "word_document",
         output_file = paste0("Clinician_supervision_", todays_date_formatted), output_dir = clinician_supervision_location)
  detach(package:flextable)
  
} else {
  
  print("Treatment summary not produced - NA")

}

# end ---------------------------------------------------------------------

end_time <- Sys.time()
timeIttakes=end_time - start_time
print(paste("Time it took to run module", modules2run, "is", timeIttakes))
}