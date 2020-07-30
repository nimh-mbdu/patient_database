
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(tibble)
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(eeptools))
suppressPackageStartupMessages(library(summarytools))
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
suppressPackageStartupMessages(library(kableExtra))

computer = "pc"

if (computer=="pc") {
  string = 'W:/'
  sdan1 = 'Y:/'
} else if (computer=="mac") {
  string = '/Volumes/string-mbd/'
  sdan1 = '/Volumes/sdan1/'
}

database_location = paste0(string, "Database/Master Psychometric Database/") # tasks database also located here
IRTA_tracker_location = paste0(string, "Database/Master Participant Tracker/")
weekly_numbers_location = paste0(string, "Minutes and conversation archives/Weekly Meeting Sheet/")
CBT_location = paste0(string, "Database/Master Psychometric Database/CBT/")
scripts = paste0(string, "Database/Database_Scripts_Github/") # temp useful directory while scripts are still under development


to_change <- read_excel(paste0(scripts, "to_change_before_running_master_script.xlsx"))
max_tasks <- c(to_change$max_tasks)
# todays_date_formatted <- c(to_change$todays_date_formatted)
# todays_date_formatted <- as.Date(todays_date_formatted)
todays_date_formatted <- as.Date("2020-07-29")
last_week_date_formatted <- todays_date_formatted - as.difftime(7, unit="days")


todays_date <- todays_date_formatted %>% format(., "%B %d %Y")
last_week_date <- last_week_date_formatted %>% format(., "%B %d %Y")
two_weeks_date_formatted <- todays_date_formatted - as.difftime(14, unit="days")
Psychometrics_date <- substring("Total.2020-06-29T12_37_44", 7, 16)



master_database_file <- list.files(path = paste0(database_location), pattern = "^MASTER_DATABASE_CLINICAL", all.files = FALSE,
                                   full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
master_database_file_time <- file.mtime(paste0(database_location, "/", master_database_file)) %>% as.Date()
master_database_combined <- tibble(File=c(master_database_file), Date=c(master_database_file_time)) %>% 
  arrange(desc(Date)) %>% slice(1)
Psychometrics_treatment <- read_excel(paste0(database_location, master_database_combined[1]))
family_hist <- Psychometrics_treatment %>% select(Initials, IRTA_tracker, c_ksadsdx_primary_dx, c_family_interview_date, 
                                                  c_family_interview_interviewer) %>% filter(!is.na(c_family_interview_interviewer)) %>% group_by(Initials) %>% 
  arrange(Initials, c_family_interview_date) %>% slice(n()) %>% ungroup()
crisis <- Psychometrics_treatment %>% select(Initials, IRTA_tracker, Clinical_Visit_Date, c_ksadsdx_primary_dx, PLUSID, c_ksadsdx_dx_detailed,
                                             s_crisis_base_date, s_crisis_fu_date, s_crisis_3m_tot, s_crisis_base_tot, s_crisis_fu_tot, 
                                             p_crisis_base_date, p_crisis_fu_date, p_crisis_3m_tot, p_crisis_base_tot, p_crisis_fu_tot, 
                                             s_mfq1w_tot, p_mfq1w_tot, s_mfq_tot, p_mfq_tot, s_crisis_base_1_exposed, s_crisis_base_2_self_diagnosis, 
                                             s_crisis_fu_1_exposed, s_crisis_fu_2_self_diagnosis) %>% 
  filter(!is.na(s_crisis_base_tot) | !is.na(s_crisis_fu_tot) | !is.na(s_mfq1w_tot)  | !is.na(s_mfq_tot) | 
           !is.na(p_crisis_base_tot) | !is.na(p_crisis_fu_tot) | !is.na(p_mfq1w_tot)  | !is.na(p_mfq_tot))
rm(master_database_file, master_database_file_time, master_database_combined)

need_remind <- Psychometrics_treatment %>% select(Initials, IRTA_tracker, Clinical_Visit_Date, c_ksadsdx_primary_dx, PLUSID, c_ksadsdx_dx_detailed,
                                                  s_crisis_base_date, s_crisis_fu_date, s_crisis_3m_tot, s_crisis_base_tot, s_crisis_fu_tot, 
                                                  p_crisis_base_date, p_crisis_fu_date, p_crisis_3m_tot, p_crisis_base_tot, p_crisis_fu_tot, 
                                                  s_mfq1w_tot, p_mfq1w_tot, s_mfq_tot, p_mfq_tot, s_crisis_base_1_exposed, s_crisis_base_2_self_diagnosis, 
                                                  s_crisis_fu_1_exposed, s_crisis_fu_2_self_diagnosis) %>% slice(n()) %>% 
  filter(is.na(s_crisis_fu_tot) | is.na(s_mfq1w_tot)  | is.na(s_mfq_tot) | 
           is.na(p_crisis_fu_tot) | is.na(p_mfq1w_tot)  | is.na(p_mfq_tot))

crisis_cc <- crisis %>% filter(IRTA_tracker == "CC") %>% filter(!is.na(p_crisis_base_date) | !is.na(p_crisis_fu_date) | !is.na(s_crisis_base_date) | !is.na(s_crisis_fu_date)) %>%
  group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date)

master_IRTA <- read_excel(paste0(IRTA_tracker_location, "MASTER_IRTA_DATABASE.xlsx"))
emails <- master_IRTA %>% select(Initials, Parent_Email,Child_Email,DOB) %>% filter(!is.na(Parent_Email)) %>% group_by(Initials) %>% slice(1)
crisis_emails_cc <- left_join(crisis_cc, emails, by="Initials")

parent_crisis <- crisis_emails_cc %>% filter(!is.na(p_crisis_base_date) | !is.na(p_crisis_fu_date)) %>% group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date) %>% slice(n())
child_crisis <- crisis_emails_cc %>% filter(!(Initials %in% parent_crisis$Initials)) %>% group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date) %>% slice(n())

parent_remind <- left_join(parent_crisis, need_remind, by="Initials")
child_remind <- left_join(child_crisis, need_remind, by="Initials")


write.xlsx(parent_crisis, paste0(string,"RA Instruction Manuals/Chris Camp/parent_crisis_CC.xlsx"), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(child_crisis, paste0(string,"RA Instruction Manuals/Chris Camp/child_crisis_CC.xlsx"), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
