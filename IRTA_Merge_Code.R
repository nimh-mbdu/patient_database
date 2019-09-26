######################################################################################
######Doing a fresh merge 

# to do: ------------------------------------------------------------------

# finish cleaning up the removed referrals
# load & merge all ruled out referrals and merge, including those in the individual 'Unsuccessful_Screens' tabs of the IRTA trackers 
# clean up empty rows in referral tracker 
# edit names to remove at end of script - update 

############ script begins here:  ----------------------------------------------------

# loading all IRTA trackers -----------------------------------------------

for(i in seq_along(current_IRTAs_full)) {
  iter <- as.numeric(i)
  # iter=7
  
  IRTA_full <- current_IRTAs_full[iter]
  IRTA_init <- current_IRTAs_init[iter]
  
  ##### checking IRTA column names 
  # temp_columns <- colnames(temp_active_data)
  # assign(paste0(current_IRTAs_init[iter], "_col"), temp_columns)
  # temp_columns %>% write.csv(file = paste0(IRTA_tracker_location,current_IRTAs_init[iter], "_col.csv"), na = " ", row.names = FALSE)
  
  temp_active_data <- read_excel(paste0(referrals_location, IRTA_full, "/", IRTA_init, "_Patient_List.xlsx"), sheet = "Active_Participants") %>% 
    mutate_all(as.character) %>% mutate(IRTA_tracker=IRTA_init)
  assign(paste0(current_IRTAs_init[iter], "_active_data"), temp_active_data) 
  
}

temp_active_data <- read_excel(paste0(IRTA_tracker_location, "/other_data_never_delete/REMOVED_Patient_List.xlsx"), sheet = "Active_Participants") %>% mutate_all(as.character) %>% mutate(IRTA_tracker="REMOVED")

# merge & tidy up

irta_sets <- ls(pattern="_active_data")
irta_sets <- mget(irta_sets)
master_IRTA_template <- reduce(irta_sets, full_join) %>% mutate(Participant_Type2 = NA)

# loading all current screens ---------------------------------------------

for(u in seq_along(current_IRTAs_full)) {
  iter6 <- as.numeric(u)
  # iter6=1
  
  IRTA_full <- current_IRTAs_full[iter6]
  IRTA_init <- current_IRTAs_init[iter6]
  
  temp_current_screens <- read_excel(paste0(referrals_location, IRTA_full, "/", IRTA_init, "_Patient_List.xlsx"), sheet = "Current_Screens") %>% 
    mutate_all(as.character) %>% mutate(IRTA_tracker=IRTA_init)
  assign(paste0(current_IRTAs_init[iter6], "_current_screens"), temp_current_screens) 
  
}

# temp_current_screens <- read_excel(paste0(IRTA_tracker_location, "/other_data_never_delete/REMOVED_Patient_List.xlsx"), sheet = "Current_Screens") %>% mutate_all(as.character) %>% mutate(IRTA_tracker="REMOVED")
rm(temp_current_screens)

# merge & tidy up
irta_screen_sets <- ls(pattern="_current_screens")
irta_screen_sets <- mget(irta_screen_sets)
master_IRTA_screens_template <- reduce(irta_screen_sets, full_join) %>% mutate(Participant_Type2 = NA, Clinical_Visit_Number = NA, Clinical_Visit_Code = NA)

# loading all old and unsuccessful screens ---------------------------------------------

for(o in seq_along(current_IRTAs_full)) {
  iter4 <- as.numeric(o)
  # iter4=1
  
  IRTA_full <- current_IRTAs_full[iter4]
  IRTA_init <- current_IRTAs_init[iter4]
  
  temp_old_screens <- read_excel(paste0(referrals_location, IRTA_full, "/", IRTA_init, "_Patient_List.xlsx"), sheet = "Unsuccessful_Screens") %>% 
    mutate_all(as.character) %>% mutate(IRTA_tracker=IRTA_init)
  assign(paste0(current_IRTAs_init[iter4], "_old_screens"), temp_old_screens) 
  
}

temp_old_screens <- read_excel(paste0(IRTA_tracker_location, "/other_data_never_delete/REMOVED_Patient_List.xlsx"), sheet = "Unsuccessful_Screens") %>% mutate_all(as.character) %>% mutate(IRTA_tracker="REMOVED")

# merge & tidy up
irta_old_screen_sets <- ls(pattern="_old_screens")
irta_old_screen_sets <- mget(irta_old_screen_sets)
master_IRTA_old_screens_template <- reduce(irta_old_screen_sets, full_join) %>% mutate(Participant_Type2 = NA)

# reordering and creating date variables ----------------------------------

# reordering colunns based on list in following document 
irta_tracker_columns <- read_excel(paste0(IRTA_tracker_location, "other_data_never_delete/irta_tracker_columns.xlsx")) %>% filter(include!="Overall_date")
master_IRTA_template <- master_IRTA_template %>% select(irta_tracker_columns$include)
master_IRTA_screens_template <- master_IRTA_screens_template %>% select(irta_tracker_columns$include)
master_IRTA_old_screens_template <- master_IRTA_old_screens_template %>% select(irta_tracker_columns$include)

# converting date variables to date format
date_variabes <- c("DOB", "Screening_Start_Date", "Referral_Date", "Consent_Date", "Clinical_Visit_Date", "Clinicals_date")
for(i in seq_len(max_tasks)) { date_variabes <- c(date_variabes, paste0("Task", i, "_Date"))}
master_IRTA_template[date_variabes] <- lapply(master_IRTA_template[date_variabes], as.Date, "%Y-%m-%d")
master_IRTA_screens_template[date_variabes] <- lapply(master_IRTA_screens_template[date_variabes], as.Date, "%Y-%m-%d")
master_IRTA_old_screens_template[date_variabes] <- lapply(master_IRTA_old_screens_template[date_variabes], as.Date, "%Y-%m-%d")

# creating an 'overall date' column, prioritizing clinic visit date for the main IRTA trackers, where this is missing, inserting the instead the task1 date
master_IRTA_template$Overall_date <- coalesce(master_IRTA_template$Clinical_Visit_Date, master_IRTA_template$Task1_Date)
# filling in demographic information for each participant & removing exact duplicates - master IRTA tracker
master_IRTA_reordered <- master_IRTA_template %>% filter(!is.na(Current)) %>% group_by(Initials) %>% arrange(Initials, Overall_date) %>% 
  fill(FIRST_NAME:Handedness, Parent_CTSS_username:Metal, .direction = "down") %>% 
  fill(FIRST_NAME:Handedness, Parent_CTSS_username:Metal, .direction = "up") %>% 
  ungroup() %>% distinct(., .keep_all = TRUE)
master_IRTA_reordered <- master_IRTA_reordered %>% group_by(Initials) %>% arrange(Initials, Overall_date) %>% 
  fill(Current:Treatment_Notes, Clinicals, Clinicals_date, .direction = "down") %>% 
  ungroup() %>% distinct(., .keep_all = TRUE)

# creating an 'overall date' column, prioritizing referral date from the screening tabs of the IRTA trackers, where this is missing, inserting the screening start date instead
master_IRTA_screens_template$Overall_date <- coalesce(master_IRTA_screens_template$Referral_Date, master_IRTA_screens_template$Screening_Start_Date) 
master_IRTA_old_screens_template$Overall_date <- coalesce(master_IRTA_old_screens_template$Referral_Date, master_IRTA_old_screens_template$Screening_Start_Date) 
# arranging by referral date
master_IRTA_screens_reordered <- master_IRTA_screens_template %>% arrange(Initials, Overall_date)
master_IRTA_old_screens_reordered <- master_IRTA_old_screens_template %>% arrange(Overall_date)

# calculating age at visit - IRTA tracker 
age_dummy <- master_IRTA_reordered %>% filter(!is.na(DOB)) %>% filter(!is.na(Overall_date)) %>% 
  select(FIRST_NAME, LAST_NAME, Initials, SDAN, DOB, Overall_date) %>% distinct(., .keep_all = TRUE)
age_dummy$Age_at_visit2 <- age_calc(dob = age_dummy$DOB, enddate = age_dummy$Overall_date, units = "years", precise = TRUE) %>% round(., digits = 2) 
# merging age variable back into master dataset 
master_IRTA_reordered$Age_at_visit <- as.numeric(master_IRTA_reordered$Age_at_visit)
master_IRTA_reordered <- left_join(master_IRTA_reordered, age_dummy, all=TRUE)
master_IRTA_reordered$Age_at_visit <- coalesce(master_IRTA_reordered$Age_at_visit, master_IRTA_reordered$Age_at_visit2)
master_IRTA_reordered <- master_IRTA_reordered %>% select(-Age_at_visit2)

# calculating age at visit - current referrals tracker 
age_dummy_screens <- master_IRTA_screens_reordered %>% filter(!is.na(DOB)) %>% filter(!is.na(Overall_date)) %>% 
  select(FIRST_NAME, LAST_NAME, Initials, SDAN, DOB, Overall_date) %>% distinct(., .keep_all = TRUE)
age_dummy_screens$Age_at_visit2 <- age_calc(dob = age_dummy_screens$DOB, enddate = age_dummy_screens$Overall_date, units = "years", precise = TRUE) %>% round(., digits = 2)
# merging age variable back into master dataset 
master_IRTA_screens_reordered$Age_at_visit <- as.numeric(master_IRTA_screens_reordered$Age_at_visit)
master_IRTA_screens_reordered <- left_join(master_IRTA_screens_reordered, age_dummy_screens, all=TRUE)
master_IRTA_screens_reordered$Age_at_visit <- coalesce(master_IRTA_screens_reordered$Age_at_visit, master_IRTA_screens_reordered$Age_at_visit2)
master_IRTA_screens_reordered <- master_IRTA_screens_reordered %>% select(-Age_at_visit2)

# calculating age at visit - old referrals tracker 
age_dummy_old_screens <- master_IRTA_old_screens_reordered %>% filter(!is.na(DOB)) %>% filter(!is.na(Overall_date)) %>% 
  select(FIRST_NAME, LAST_NAME, Initials, SDAN, DOB, Overall_date) %>% distinct(., .keep_all = TRUE)
age_dummy_old_screens$Age_at_visit2 <- age_calc(dob = age_dummy_old_screens$DOB, enddate = age_dummy_old_screens$Overall_date, units = "years", precise = TRUE) %>% round(., digits = 2)
# merging age variable back into master dataset 
master_IRTA_old_screens_reordered$Age_at_visit <- as.numeric(master_IRTA_old_screens_reordered$Age_at_visit)
master_IRTA_old_screens_reordered <- left_join(master_IRTA_old_screens_reordered, age_dummy_old_screens, all=TRUE)
master_IRTA_old_screens_reordered$Age_at_visit <- coalesce(master_IRTA_old_screens_reordered$Age_at_visit, master_IRTA_old_screens_reordered$Age_at_visit2)
master_IRTA_old_screens_reordered <- master_IRTA_old_screens_reordered %>% select(-Age_at_visit2)

# creating participant type 2 variable

# active participants
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'HV')] <- 'HV'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'MDD')] <- 'MDD'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'DMDD')] <- 'DMDD'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'ANX')] <- 'Anxious'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'xious')] <- 'Anxious'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'ADHD')] <- 'ADHD'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'UNSURE')] <- 'UNSURE'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'Unsure')] <- 'UNSURE'
master_IRTA_reordered$SDAN[master_IRTA_reordered$SDAN=='999'] <- NA
# current referrals
master_IRTA_screens_reordered$Participant_Type2[str_detect(master_IRTA_screens_reordered$Participant_Type, 'HV')] <- 'HV'
master_IRTA_screens_reordered$Participant_Type2[str_detect(master_IRTA_screens_reordered$Participant_Type, 'MDD')] <- 'MDD'
master_IRTA_screens_reordered$Participant_Type2[str_detect(master_IRTA_screens_reordered$Participant_Type, 'DMDD')] <- 'DMDD'
master_IRTA_screens_reordered$Participant_Type2[str_detect(master_IRTA_screens_reordered$Participant_Type, 'ANX')] <- 'Anxious'
master_IRTA_screens_reordered$Participant_Type2[str_detect(master_IRTA_screens_reordered$Participant_Type, 'xious')] <- 'Anxious'
master_IRTA_screens_reordered$Participant_Type2[str_detect(master_IRTA_screens_reordered$Participant_Type, 'ADHD')] <- 'ADHD'
master_IRTA_screens_reordered$Participant_Type2[str_detect(master_IRTA_screens_reordered$Participant_Type, 'UNSURE')] <- 'UNSURE'
master_IRTA_screens_reordered$Participant_Type2[str_detect(master_IRTA_screens_reordered$Participant_Type, 'Unsure')] <- 'UNSURE'
master_IRTA_screens_reordered$SDAN[master_IRTA_screens_reordered$SDAN=='999'] <- NA
# old referrals 
master_IRTA_old_screens_reordered$Participant_Type2[str_detect(master_IRTA_old_screens_reordered$Participant_Type, 'HV')] <- 'HV'
master_IRTA_old_screens_reordered$Participant_Type2[str_detect(master_IRTA_old_screens_reordered$Participant_Type, 'MDD')] <- 'MDD'
master_IRTA_old_screens_reordered$Participant_Type2[str_detect(master_IRTA_old_screens_reordered$Participant_Type, 'DMDD')] <- 'DMDD'
master_IRTA_old_screens_reordered$Participant_Type2[str_detect(master_IRTA_old_screens_reordered$Participant_Type, 'ANX')] <- 'Anxious'
master_IRTA_old_screens_reordered$Participant_Type2[str_detect(master_IRTA_old_screens_reordered$Participant_Type, 'xious')] <- 'Anxious'
master_IRTA_old_screens_reordered$Participant_Type2[str_detect(master_IRTA_old_screens_reordered$Participant_Type, 'ADHD')] <- 'ADHD'
master_IRTA_old_screens_reordered$Participant_Type2[str_detect(master_IRTA_old_screens_reordered$Participant_Type, 'UNSURE')] <- 'UNSURE'
master_IRTA_old_screens_reordered$Participant_Type2[str_detect(master_IRTA_old_screens_reordered$Participant_Type, 'Unsure')] <- 'UNSURE'
master_IRTA_old_screens_reordered$SDAN[master_IRTA_old_screens_reordered$SDAN=='999'] <- NA

# splitting clinicial visit type into code & numeric columns 
split1 <- colsplit(master_IRTA_reordered$Clinical_Visit_Type, "", names = c("Clinical_Visit_Code2", "Clinical_Visit_Number2"))
master_IRTA_reordered <- cbind(master_IRTA_reordered, split1)
master_IRTA_reordered$Clinical_Visit_Code <- coalesce(master_IRTA_reordered$Clinical_Visit_Code, master_IRTA_reordered$Clinical_Visit_Code2)
master_IRTA_reordered$Clinical_Visit_Number <- coalesce(master_IRTA_reordered$Clinical_Visit_Number, master_IRTA_reordered$Clinical_Visit_Number2)
master_IRTA_reordered <- master_IRTA_reordered %>% select(-Clinical_Visit_Code2, -Clinical_Visit_Number2)

# another reorder & sort 

irta_tracker_columns <- read_excel(paste0(IRTA_tracker_location, "other_data_never_delete/irta_tracker_columns.xlsx"))
master_IRTA_latest <- master_IRTA_reordered %>% select(irta_tracker_columns$include) %>% arrange(Initials, Clinical_Visit_Date)
master_IRTA_screens_latest <- master_IRTA_screens_reordered %>% select(irta_tracker_columns$include) %>% arrange(Initials, Referral_Date)
master_IRTA_old_screens_latest <- master_IRTA_old_screens_reordered %>% select(irta_tracker_columns$include) %>% arrange(Referral_Date)

####################Chris's here..... 

suppressWarnings(source(paste0(scripts,"Schedule_script_functions.R")))
master_IRTA_latest$Next_FU_date <- as.Date(NA,origin = "1899-12-30")
master_IRTA_latest$Next_FU_notes <- NA
for (row in c(1:nrow(master_IRTA_latest))) {
  if (!is.na(master_IRTA_latest[row,"IRTA_tracker"]) & master_IRTA_latest[row, "IRTA_tracker"] != "REMOVED") {
    print_dates(row,master_IRTA_latest)
    print_notes(row,master_IRTA_latest)
    assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)

  }
  #print("completed")

}

master_IRTA_latest$Clinical_Visit_Date <- as.Date(master_IRTA_latest$Clinical_Visit_Date, origin = "1899-12-30")
master_IRTA_latest$Task1_Date <- as.Date(master_IRTA_latest$Task1_Date, origin = "1899-12-30")
master_IRTA_latest$Next_FU_date <- as.Date(master_IRTA_latest$Next_FU_date, origin = "1899-12-30")

######################################################################################
#######Creating tasks database

task_reshape_master = data.frame(matrix(ncol = 18, nrow = 0))
x <- c("FIRST_NAME", "LAST_NAME", "Initials", "SDAN", "DAWBA_ID", "PLUSID",
       "SEX", "DOB", "Handedness", "Participant_Type2", 
       "Age_at_visit", "Overall_date", "Protocol", "Scanner", 
       "Task_Name", "Task_Number", "Task_Date", "Task_Visit_Type")
colnames(task_reshape_master) <- x

for(i in seq_len(max_tasks)) {
  iter <- as.numeric(i)
  # iter=1 # for manually running without the loop

  task_reshape <- master_IRTA_latest %>% 
    select(FIRST_NAME, LAST_NAME, Initials, SDAN, DAWBA_ID, PLUSID, 
           SEX, DOB, Handedness, Participant_Type2, 
           Age_at_visit, Overall_date, Protocol, Scanner,
           paste0("Task", iter, "_Name"), paste0("Task", iter, "_Number"), 
           paste0("Task", iter, "_Date"), paste0("Task", iter, "_Visit_Type")) 
  
  names(task_reshape)[names(task_reshape) == paste0("Task", iter, "_Name")] <- "Task_Name"
  names(task_reshape)[names(task_reshape) == paste0("Task", iter, "_Number")] <- "Task_Number"
  names(task_reshape)[names(task_reshape) == paste0("Task", iter, "_Date")] <- "Task_Date"
  names(task_reshape)[names(task_reshape) == paste0("Task", iter, "_Visit_Type")] <- "Task_Visit_Type"
 
  task_reshape <- task_reshape %>% filter(!is.na(Task_Name)) %>% distinct(., .keep_all = TRUE)
  # assign(paste0("task", iter), task_reshape) # uncomment this row if you would like to KEEP all individual files from loop to QC them

  task_reshape_master <- merge.default(task_reshape_master, task_reshape, all=TRUE)
}

eligibility_variables <- master_IRTA_latest %>% 
  select(Initials, SDAN, IRTA_tracker, Overall_date, Eligible, Clinical_Visit_Date, 
         Clinical_Visit_Type, Clinical_Visit_Code, Clinical_Visit_Number, Scheduling_status, 
         Participant_Type, Participant_Type2) %>%
  arrange(Initials, Overall_date, desc(Clinical_Visit_Type)) %>%
  distinct(., Initials, SDAN, Overall_date, .keep_all = TRUE)

# check <- eligibility_variables %>% filter(is.na(Clinical_Visit_Type)) %>% filter(Scheduling_status=='3')

task_reshape_master <- left_join(task_reshape_master, eligibility_variables, all=TRUE)
task_reshape_master <- task_reshape_master %>% 
  select(FIRST_NAME, LAST_NAME, Initials, SDAN, DAWBA_ID, PLUSID, IRTA_tracker,
         SEX, DOB, Handedness, Participant_Type, Participant_Type2, Age_at_visit, Overall_date, 
         Eligible, Clinical_Visit_Date, Clinical_Visit_Type, Clinical_Visit_Code, Clinical_Visit_Number,
         Scheduling_status, Protocol, Scanner, 
         Task_Name, Task_Number, Task_Date, Task_Visit_Type) %>% 
  arrange(Initials, Overall_date) %>% 
  filter(!is.na(Task_Name)) %>% 
  distinct(., .keep_all = TRUE) 
# %>% select(-FIRST_NAME, -LAST_NAME)

######################################################################################
#######Adding MRI QC information

##### MMI

MMI_task_QC = data.frame(matrix(ncol = 5, nrow = 0))
x <- c("SDAN", "Task_Name", "Task_Date", "QC_tracker_tab_no", "Include")
colnames(MMI_task_QC) <- x

for(i in seq_len(max_MMI)) {
  # iter=1
  iter <- as.numeric(i)
  float <- read_excel(paste0(MMI_tracker_location, "MMI Participant Tracker_new - USE ME.xlsx"), sheet = "MMI_fMRI", col_names = TRUE, skip = 1, col_types = NULL) %>%
    mutate(QC_tracker_tab_no = iter) %>% mutate(Task_Name = "MMI_3blocks_scan") %>% 
    rename(Task_Date = Date) %>%
    select(SDAN, Task_Name, Task_Date, QC_tracker_tab_no, Include) %>% 
    filter(!is.na(SDAN))
  MMI_task_QC <- merge.default(MMI_task_QC, float, all=TRUE)
}
MMI_task_QC <- MMI_task_QC %>% arrange(SDAN, QC_tracker_tab_no)
MMI_task_QC$Task_Date <- as.Date(MMI_task_QC$Task_Date)
MMI_task_QC$SDAN <- as.character(MMI_task_QC$SDAN)

##### checking for merge conflicts/missing MMIs: 
MMI_missing <- merge.default(task_reshape_master, MMI_task_QC, all=TRUE) %>%
  filter(Task_Name=="MMI_3blocks_scan") %>% 
  select(Initials, SDAN, DAWBA_ID, PLUSID, IRTA_tracker, 
         SEX, DOB, Handedness, Age_at_visit, Overall_date,
         Eligible, Scheduling_status, Participant_Type,
         Participant_Type2, Protocol,
         Clinical_Visit_Type, Scanner,
         Task_Name, Task_Number, Task_Date, Task_Visit_Type, QC_tracker_tab_no, Include) %>% 
  group_by(SDAN) %>% 
  fill(Initials, IRTA_tracker, .direction = "down") %>% 
  fill(Initials, IRTA_tracker, .direction = "up") %>% 
  ungroup() %>% 
  filter(is.na(Task_Number) | is.na(Include)) %>% 
  mutate(Todays_Date = todays_date_formatted)

MMI_missing$Days_since_scan <- as.numeric(difftime(MMI_missing$Task_Date, MMI_missing$Todays_Date, tz="", units = "days"))
of_interest <- c('IRTA_tracker', 'Eligible', 'Scheduling_status', 'Protocol', 'Scanner', 'Task_Number', 'QC_tracker_tab_no', 'Include', 'Days_since_scan')
MMI_missing[of_interest] <- lapply(MMI_missing[of_interest], replace_na, '666')
numeric <- c('Eligible', 'Scheduling_status', 'Task_Number', 'QC_tracker_tab_no', 'Include', 'Days_since_scan')
MMI_missing[numeric] <- lapply(MMI_missing[numeric], as.numeric)
MMI_missing <- MMI_missing %>% filter(Eligible<4 | Eligible>7) %>% 
  filter(Task_Number != '777' & Task_Number != '999') %>% 
  filter(Days_since_scan<1 | Days_since_scan==666) %>% 
  filter(Scheduling_status != 1) %>% 
  arrange(SDAN, Task_Date)
MMI_missing[of_interest] <- lapply(MMI_missing[of_interest], na_if, '666')

MMI_missing %>% write_xlsx(paste0(IRTA_tracker_location,"MMI_check.xlsx")) # if file empty, everything is perfect 

##### MID

MID_task_QC = data.frame(matrix(ncol = 5, nrow = 0))
x <- c("SDAN", "Task_Name", "Task_Date", "QC_tracker_tab_no", "Include")
colnames(MID_task_QC) <- x

for(i in seq_len(max_MID)) {
  # iter=1
  iter <- as.numeric(i)
  float <- read_excel(paste0(MID_tracker_location, "MID Tasks.xlsx"), sheet = paste0("QC_", iter), col_names = TRUE, skip = 1, col_types = NULL) %>%
    mutate(QC_tracker_tab_no = iter) %>% mutate(Task_Name = "MID_scan") %>% 
    rename(Task_Date = Date) %>% 
    # rename(Participant_Type_QC = Participant_Type) %>%
    select(SDAN, Task_Name, Task_Date, QC_tracker_tab_no, Include) %>% 
    filter(!is.na(SDAN))
  MID_task_QC <- merge.default(MID_task_QC, float, all=TRUE)
} 
# MID_task_QC <- merge.default(MID_task_QC, floatDx, all=TRUE)
MID_task_QC <- MID_task_QC %>% arrange(SDAN, QC_tracker_tab_no)
MID_task_QC$Task_Date <- as.Date(MID_task_QC$Task_Date)
MID_task_QC$SDAN <- as.character(MID_task_QC$SDAN)

##### checking for merge conflicts/missing information:
MID_temp <- merge.default(task_reshape_master, MID_task_QC, all=TRUE) %>%
  filter(Task_Name=="MID_scan") %>% 
  select(Initials, SDAN, DAWBA_ID, PLUSID, IRTA_tracker, 
         SEX, DOB, Handedness, Age_at_visit, Overall_date,
         Eligible, Scheduling_status, Participant_Type,
         Participant_Type2, Protocol,
         Clinical_Visit_Type, Scanner,
         Task_Name, Task_Number, Task_Date, Task_Visit_Type, QC_tracker_tab_no, Include) %>% 
  group_by(SDAN) %>% 
  fill(Initials, IRTA_tracker, .direction = "down") %>% 
  fill(Initials, IRTA_tracker, .direction = "up") %>% 
  ungroup() %>% 
  mutate(Todays_Date = todays_date_formatted)

MID_temp$Days_since_scan <- as.numeric(difftime(MID_temp$Task_Date, MID_temp$Todays_Date, tz="", units = "days"))
of_interest <- c('IRTA_tracker', 'Eligible', 'Scheduling_status', 'Protocol', 'Scanner', 'Task_Number', 'QC_tracker_tab_no', 'Include', 'Days_since_scan')
MID_temp[of_interest] <- lapply(MID_temp[of_interest], replace_na, '666')

missing_date <- MID_temp %>% filter(Task_Number != '777' & Task_Number != '999') %>% 
  filter(Scheduling_status !='0' & Scheduling_status !='1') %>% 
  filter(is.na(Task_Date)) %>% 
  filter(Task_Number != '666')
missing_number <- MID_temp %>% 
  filter(Days_since_scan<0) %>% 
  filter(Task_Number == '666')
missing_qc <- MID_temp %>% filter(Task_Number != '777' & Task_Number != '999') %>% 
  filter(Days_since_scan<0) %>% 
  filter(Include=='666')

MID_missing <- merge.default(missing_date, missing_number, all=TRUE) %>% 
  merge.default(., missing_qc, all=TRUE)
MID_missing[of_interest] <- lapply(MID_missing[of_interest], na_if, '666')

MID_missing %>% write_xlsx(paste0(IRTA_tracker_location,"MID_check.xlsx")) # if file empty, everything is perfect 

#####Merge of QC information with main task tracker, drops inconsistencies, hence why accuracy is so important 

task_QC <- rbind(MID_task_QC, MMI_task_QC)
task_reshape_master_QC <- left_join(task_reshape_master, task_QC, all=TRUE)

######################################################################################
#######Saving Master IRTA sheet in typical format

master_IRTA_latest %>% write_xlsx(paste0(IRTA_tracker_location,"MASTER_IRTA_DATABASE.xlsx")) # will not save if someone else has this dataset open 
master_IRTA_latest %>% write_xlsx(paste0(backup_location,"MASTER_IRTA_DATABASE","_",todays_date_formatted,".xlsx"))
# also an option to add a password to a saved excel, e.g. = password = "string"

######################################################################################
#######Saving Tasks Dataset

task_reshape_master_QC %>% write_xlsx(paste0(IRTA_tracker_location,"TASKS_DATABASE_QC.xlsx"))
task_reshape_master_QC %>% write_xlsx(paste0(backup_location,"TASKS_DATABASE_QC","_",todays_date_formatted,".xlsx"))

######################################################################################
#######Saving Screens Dataset

master_IRTA_screens_latest %>% write_xlsx(paste0(IRTA_tracker_location,"REFERRAL_AND_SCREENING_DATABASE.xlsx"))
master_IRTA_screens_latest %>% write_xlsx(paste0(backup_location,"REFERRAL_AND_SCREENING_DATABASE","_",todays_date_formatted,".xlsx"))

######################################################################################
#######Saving Old Screens Dataset

master_IRTA_old_screens_latest %>% write_xlsx(paste0(IRTA_tracker_location,"OLD_REFERRALS_DATABASE.xlsx"))
master_IRTA_old_screens_latest %>% write_xlsx(paste0(backup_location,"OLD_REFERRALS_DATABASE","_",todays_date_formatted,".xlsx"))

#####Removing unnecessary variables

rm(list=ls(pattern="_active_data"))
rm(list=ls(pattern="_current_screens"))
rm(list=ls(pattern="_old_screens"))
rm(list=ls(pattern="iter"))
rm(list=ls(pattern="age_"))
rm(list=ls(pattern="_sets"))
rm(list=ls(pattern="missing"))
rm(list=ls(pattern="_reordered"))
rm(list=ls(pattern="_template"))
rm(IRTA_full, IRTA_init, irta_tracker_columns, date_variabes, split1, i, eligibility_variables, x, row, u, numeric, of_interest, o)
rm(MID_task_QC, MMI_task_QC, float, task_reshape, task_reshape_master, task_QC, MID_temp)
rm(get_last_scan, get_last_visit, has_scan, is_last_v, print_dates, print_notes)
