
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
today = "2020-07-28"
irta = "CC"

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
todays_date_formatted <- as.Date(today)
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
crisis <- Psychometrics_treatment %>% select(Initials, IRTA_tracker, Clinical_Visit_Date, c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed,
                                             s_crisis_base_date, s_crisis_fu_date, s_crisis_3m_tot, s_crisis_base_tot, s_crisis_fu_tot, 
                                             p_crisis_base_date, p_crisis_fu_date, p_crisis_3m_tot, p_crisis_base_tot, p_crisis_fu_tot, 
                                             s_mfq1w_tot, p_mfq1w_tot, s_mfq_tot, p_mfq_tot, s_crisis_base_1_exposed, s_crisis_base_2_self_diagnosis, 
                                             s_crisis_fu_1_exposed, s_crisis_fu_2_self_diagnosis) %>% 
  filter(!is.na(s_crisis_base_tot) | !is.na(s_crisis_fu_tot) | !is.na(s_mfq1w_tot)  | !is.na(s_mfq_tot) | 
           !is.na(p_crisis_base_tot) | !is.na(p_crisis_fu_tot) | !is.na(p_mfq1w_tot)  | !is.na(p_mfq_tot))
rm(master_database_file, master_database_file_time, master_database_combined)


baseline <- Psychometrics_treatment %>% 
  select(Initials, IRTA_tracker, Participant_Type2, Protocol, Clinical_Visit_Date, Clinical_Visit_Type, c_ksadsdx_visit_type, c_ksadsdx_date) %>% 
  filter(!is.na(c_ksadsdx_visit_type) & c_ksadsdx_visit_type != "Inpatient Treatment" & c_ksadsdx_visit_type != "Outpatient Treatment")
baseline$Clinical_Visit_Date <- as.Date(baseline$Clinical_Visit_Date)
baseline$c_ksadsdx_date <- as.Date(baseline$c_ksadsdx_date)
baseline$c_ksadsdx_TDiff <- as.numeric(difftime(baseline$Clinical_Visit_Date, baseline$c_ksadsdx_date, tz="", units = "days"))
baseline <- baseline %>% mutate(measurement_TDiff_abs=abs(c_ksadsdx_TDiff)) %>% 
  group_by(Initials, c_ksadsdx_visit_type) %>% arrange(Initials, c_ksadsdx_visit_type, measurement_TDiff_abs) %>% 
  slice(1) %>% ungroup() %>% select(-measurement_TDiff_abs) %>% arrange(Initials, Clinical_Visit_Date)

#### excluding removed participants
latest_eligibility <- Psychometrics_treatment %>% select(Initials, Eligible, IRTA_tracker, Clinical_Visit_Type, Clinical_Visit_Date, Protocol) %>% 
  group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date) %>% slice(n()) %>% ungroup() %>% mutate(Eligible = as.numeric(Eligible))
# missing_diag <- merge.default(baseline, latest_eligibility, all = TRUE) %>% group_by(Initials) %>% fill(IRTA_tracker:Eligible, .direction = "up") %>% 
#   fill(IRTA_tracker:Eligible, .direction = "down") %>% slice(1) %>% ungroup() %>% filter(is.na(c_ksadsdx_date))
# missing_diag %>% write_xlsx(paste0(IRTA_tracker_location, "QCing/Missing_diagnosis.xlsx"))
baseline <- latest_eligibility %>% select(Initials, Eligible) %>% left_join(baseline, .)
latest_eligibility <- latest_eligibility %>% mutate(remove1 = ifelse((Eligible > 3 & Eligible < 11), 1, 0))
latest_eligibility <- latest_eligibility %>% mutate(remove2 = ifelse((IRTA_tracker == "REMOVED"), 1, 0))
latest_eligibility <- latest_eligibility %>% mutate(remove3 = ifelse((!str_detect(Protocol, "0037") | str_detect(Protocol, "SCREENING")), 1, 0))
latest_eligibility[,7:9] <- lapply(latest_eligibility[7:9], replace_na, 0)
latest_eligibility <- latest_eligibility %>% mutate(remove=(remove1 + remove2 + remove3)) %>% filter(remove>0)
removed_initials <- latest_eligibility$Initials
baseline$exclude <- (baseline$Initials %in% removed_initials) %>% as.character()

#### incorporating scanner information 
# master_IRTA_latest <- read_excel(paste0(IRTA_tracker_location, irta_master_combined[1]))
# scanner <- master_IRTA_latest %>% filter(!is.na(Scanner)) %>% group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date) %>% slice(n()) %>% ungroup() %>% select(Initials, Scanner)
# baseline <- left_join(baseline, scanner)

#### completed annual FU visit 
FU_completed <- baseline %>% group_by(Initials) %>% filter(n()>1) %>% ungroup() %>% 
  select(-c_ksadsdx_TDiff, -exclude, -Protocol, -Clinical_Visit_Type) %>% filter(c_ksadsdx_visit_type!="baseline")
MDD_one_year <- FU_completed %>% filter(c_ksadsdx_visit_type=="12 month FU") %>% filter(Participant_Type2=="MDD") %>% nrow() %>% as.numeric()
MDD_two_year <- FU_completed %>% filter(c_ksadsdx_visit_type=="24 month FU") %>% filter(Participant_Type2=="MDD") %>% nrow() %>% as.numeric()
HV_one_year <- FU_completed %>% filter(c_ksadsdx_visit_type=="12 month FU") %>% filter(Participant_Type2=="HV") %>% nrow() %>% as.numeric()
HV_two_year <- FU_completed %>% filter(c_ksadsdx_visit_type=="24 month FU") %>% filter(Participant_Type2=="HV") %>% nrow() %>% as.numeric()

FU_completed_summary <- tibble(Type=c("12 month", "12 month", "24 month", "24 month"), Diagnosis=c("MDD", "HV", "MDD", "HV"), 
                               Freq=c(MDD_one_year, HV_one_year, MDD_two_year, HV_two_year))

#### eligible for annual FU - not yet completed 
baseline_keep <- baseline %>% filter(exclude=="FALSE") %>% group_by(Initials) %>% slice(n()) %>% ungroup() %>% 
  select(-c_ksadsdx_TDiff, -exclude, -Protocol, -Clinical_Visit_Type)

baseline_keep <- baseline_keep %>% mutate(expt_FU_date = (c_ksadsdx_date + 365))
baseline_keep$expt_FU_date_TDiff <- as.numeric(difftime(baseline_keep$expt_FU_date, todays_date_formatted, tz="", units = "days"))

#### Georgia's divisions
baseline_keep <- baseline_keep %>% filter(IRTA_tracker == irta) %>% mutate(status = 
                                            ifelse((expt_FU_date_TDiff < -30), 1, # more than one month overdue 
                                                   ifelse((expt_FU_date_TDiff >= -30 & expt_FU_date_TDiff < 0), 2, # less than one month overdue
                                                          ifelse((expt_FU_date_TDiff >= 0 & expt_FU_date_TDiff < 31), 3, # due within the next month
                                                                 ifelse((expt_FU_date_TDiff >= 31 & expt_FU_date_TDiff < 191), 4, # due in >1 month but <6 months
                                                                        ifelse((expt_FU_date_TDiff >= 191 & expt_FU_date_TDiff < 275), 5, # due in >6 months but <9 months
                                                                               ifelse((expt_FU_date_TDiff >= 275), 6, #due in >9 months
                                                                                      0)))))))



table(baseline_keep$status)

baseline_keep <- baseline_keep %>% mutate(status = 
                                            ifelse((expt_FU_date_TDiff < -30), 1, # more than one month overdue 
                                                   ifelse((expt_FU_date_TDiff >= -30 & expt_FU_date_TDiff < 0), 2, # less than one month overdue
                                                          ifelse((expt_FU_date_TDiff >= 0 & expt_FU_date_TDiff < 31), 3, # due within the next month
                                                                 ifelse((expt_FU_date_TDiff >= 31 & expt_FU_date_TDiff < 62), 4, # due in >1 month but <2 months
                                                                        ifelse((expt_FU_date_TDiff >= 62 & expt_FU_date_TDiff < 275), 5, # due in >2 months but <9 months
                                                                               ifelse((expt_FU_date_TDiff >= 275), 6, #due in >9 months
                                                                                      0)))))))

#Output tables

A_more_one_month_overdue <- baseline_keep %>% filter(status == 1)
A_less_one_month_overdue <- baseline_keep %>% filter(status == 2)
A_next_month <- baseline_keep %>% filter(status  == 3)
A_next_two_months <- baseline_keep %>% filter(status == 4)
A_next_eight_months <- baseline_keep %>% filter(status == 5)
A_more_than_nine_months <- baseline_keep %>% filter(status == 6)
to_graph <- baseline_keep %>% filter(expt_FU_date_TDiff > 0 & expt_FU_date_TDiff < 93)



table(next_month$Clinical_Visit_Date)


ggplot(data = to_graph, aes(Clinical_Visit_Date)) +
  geom_bar() +
  labs(x = "Date",
       y = "Followups",
       title = "Followups Due")
