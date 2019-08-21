######################################################################################
######Doing a fresh merge 

# to do: ------------------------------------------------------------------

# script begins here:  ----------------------------------------------------

master_IRTA_template <- read_excel(paste0(IRTA_tracker_location, "MASTER_IRTA_DATABASE_blank.xlsx")) #96 variables 

for(i in seq_along(current_IRTAs_full)) {
  iter <- as.numeric(i)
  # iter=7
  
  IRTA_full <- current_IRTAs_full[iter]
  IRTA_init <- current_IRTAs_init[iter]
  
  ##### checking IRTA column names 
  # temp_columns <- colnames(temp)
  # assign(paste0(current_IRTAs_init[iter], "_col"), temp_columns)
  # temp_columns %>% write.csv(file = paste0(IRTA_tracker_location,current_IRTAs_init[iter], "_col.csv"), na = " ", row.names = FALSE)
  
  temp <- read_excel(paste0(referrals_location, IRTA_full, "/", IRTA_init, "_Patient_List.xlsx")) %>% mutate_all(as.character) %>% mutate(IRTA_tracker=IRTA_init)
  master_IRTA_template <- merge.default(master_IRTA_template, temp, all=TRUE) %>% mutate_all(as.character)
  # assign(paste0(current_IRTAs_init[iter], "_data"), temp) # uncomment this row out if you would like to KEEP all individual IRTA trackers as well as merge them - useful for QC
  
}

temp <- read_excel(paste0(IRTA_tracker_location, "/Other/REMOVED_Patient_List.xlsx")) %>% mutate_all(as.character) %>% mutate(IRTA_tracker="REMOVED")
master_IRTA_template <- merge.default(master_IRTA_template, temp, all=TRUE) %>% mutate_all(as.character)

######################################################################################
######Tidying up

master_IRTA_template <- master_IRTA_template %>% 
                        select(FIRST_NAME, LAST_NAME, Initials, SDAN, MRN, DAWBA_ID, PLUSID, IRTA_tracker,
                              SEX, DOB, Handedness, Current, 
                              Eligible, Eligibility_notes, Scheduling_status, Scheduling_status_notes, 
                              Participant_Type, Participant_Type2, Treatment_Notes, DAWBA_completed, Screening_Start_Date, DAWBA_MFQ, 
                              Consent_Date, Protocol, Data_sharing, 
                              Clinical_Visit_Type, Clinical_Visit_Date, NIMH_Clinician, Visit_Cancelled, 
                              Parent_CTSS_username, Parent_CTSS_password, Child_CTSS_username, Child_CTSS_password, 
                              parent_SDQplus_login, child_SDQplus_login, Child_Phone_Number, Child_Email, 
                              Primary_clinician,	Referral_Source,	Important_Info,
                              Marital_Status,	Sibling_Init,	Sibling_Type,
                              Parent_Name, FIRST_NAME_P1,	LAST_NAME_P1, Parent_Contact_Number,	Parent_Email, Street_address,	City,	State,	Zip,
                              Second_Parent_Name,	FIRST_NAME_P2,	LAST_NAME_P2, Second_Parent_Contact_Number,	Second_Parent_Email,	Second_Parent_Address, Parent_Consented,
                              Scanner,	Glasses,	Glasses_Prescription,	Metal,	Clinicals,	Clinicals_date,
                              Task1_Name:paste0("Task", max_tasks, "_Visit_Type")) #should have 82 variables after this (if max no. of tasks = 5)
# way of exporting all column names to check for consistency: 
# temp_columns <- colnames(master_IRTA_template)
# temp_columns %>% write.csv(file = paste0(IRTA_tracker_location, "col.csv"), na = " ", row.names = FALSE)

# converting date variables to date format
# if problem with dates in one or more individual trackers don't run these 3 lines - 
# if you still want to create the dataset running this with improperly formatted dates will result in the removal of the dates from those columns. 
# not running this line keeps them in character format (string)
date_variabes <- c("DOB", "Screening_Start_Date", "Consent_Date", "Clinical_Visit_Date", "Clinicals_date")
for(i in seq_len(max_tasks)) { date_variabes <- c(date_variabes, paste0("Task", i, "_Date"))}
master_IRTA_template[date_variabes] <- lapply(master_IRTA_template[date_variabes], as.Date, "%Y-%m-%d")

# creating an 'overall date' column, prioritizing task1 date, where this is missing, inserting the clinic visit date instead, if missing, try other task dates 
master_IRTA_template$Overall_date <- coalesce(master_IRTA_template$Task1_Date, master_IRTA_template$Clinical_Visit_Date) 
for(i in seq_len(max_tasks-1)) {
  i <- as.numeric(i)
  j=i+1
  # temp1 <- paste0("Task", i, "_Date")
  temp2 <- paste0("Task", j, "_Date")
  master_IRTA_template$Overall_date <- coalesce(master_IRTA_template$Overall_date, master_IRTA_template$temp2)
  }

# filling in demographic information for each participant & removing exact duplicates 
master_IRTA_reordered <- master_IRTA_template %>% filter(!is.na(Current)) %>% arrange(LAST_NAME, FIRST_NAME, Overall_date)
master_IRTA_reordered <- master_IRTA_reordered %>% group_by(LAST_NAME, FIRST_NAME) %>% 
  fill(Initials:Handedness, Participant_Type, DAWBA_completed:DAWBA_MFQ, Parent_CTSS_username:Metal, .direction = "down") %>% 
  fill(Initials:Handedness, Participant_Type, DAWBA_completed:DAWBA_MFQ, Parent_CTSS_username:Metal, .direction = "up") %>% 
  ungroup() %>% distinct(., .keep_all = TRUE)
master_IRTA_reordered <- master_IRTA_reordered %>% group_by(LAST_NAME, FIRST_NAME) %>% arrange(LAST_NAME, FIRST_NAME, Clinical_Visit_Date) %>% 
  fill(Eligible:Treatment_Notes, Consent_Date, Protocol, Data_sharing, .direction = "down") %>% 
  ungroup() %>% distinct(., .keep_all = TRUE)

# calculating age at visit
age_dummy <- master_IRTA_reordered %>% filter(!is.na(DOB)) %>% filter(!is.na(Overall_date)) %>% 
  select(FIRST_NAME, LAST_NAME, Initials, SDAN, DOB, Overall_date) %>% distinct(., .keep_all = TRUE)
age_dummy$Age_at_visit <- age_calc(dob = age_dummy$DOB, enddate = age_dummy$Overall_date, units = "years", precise = TRUE) %>% 
  round(., digits = 2) 
# alternative line to the above to run if you weren't able to convert date variables to date format earlier 
# age_dummy$Age_at_visit <- age_calc(dob = as.Date(age_dummy$DOB), enddate = as.Date(age_dummy$Overall_date), units = "years", precise = TRUE) %>%
#   round(., digits = 2)
# merging age variable back into master dataset 
master_IRTA_reordered <- left_join(master_IRTA_reordered, age_dummy, all=TRUE)

master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'HV')] <- 'HV'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'MDD')] <- 'MDD'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'DMDD')] <- 'DMDD'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'ANX')] <- 'Anxious'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'xious')] <- 'Anxious'
master_IRTA_reordered$Participant_Type2[str_detect(master_IRTA_reordered$Participant_Type, 'ADHD')] <- 'ADHD'

master_IRTA_reordered$SDAN[master_IRTA_reordered$SDAN=='999'] <- NA

split1 <- colsplit(master_IRTA_reordered$Clinical_Visit_Type, "", names = c("Clinical_Visit_Code", "Clinical_Visit_Number"))
master_IRTA_reordered <- cbind(master_IRTA_reordered, split1) 

# another reorder & sort 

master_IRTA_latest <- master_IRTA_reordered %>% 
  select(FIRST_NAME, LAST_NAME, Initials, SDAN, MRN, DAWBA_ID, PLUSID, IRTA_tracker, SEX, DOB, Handedness, Age_at_visit, Overall_date, Current, 
            Eligible, Eligibility_notes, Scheduling_status, Scheduling_status_notes, 
            Participant_Type, Participant_Type2, Treatment_Notes, DAWBA_completed, Screening_Start_Date, DAWBA_MFQ, Consent_Date, Protocol, Data_sharing, 
            Clinical_Visit_Date, Clinical_Visit_Type, Clinical_Visit_Code, Clinical_Visit_Number, NIMH_Clinician, Visit_Cancelled, 
            Parent_CTSS_username, Parent_CTSS_password, Child_CTSS_username, Child_CTSS_password, 
            parent_SDQplus_login, child_SDQplus_login, Child_Phone_Number, Child_Email, 
            Primary_clinician,	Referral_Source,	Important_Info,
            Marital_Status,	Sibling_Init,	Sibling_Type,
            Parent_Name, FIRST_NAME_P1,	LAST_NAME_P1,	Parent_Contact_Number,	Parent_Email, Street_address,	City,	State,	Zip,
            Second_Parent_Name,	FIRST_NAME_P2,	LAST_NAME_P2, Second_Parent_Contact_Number,	Second_Parent_Email,	Second_Parent_Address, Parent_Consented,
            Scanner,	Glasses,	Glasses_Prescription,	Metal,	Clinicals,	Clinicals_date,
            Task1_Name:paste0("Task", max_tasks, "_Visit_Type")) %>% # should have 86 variables
  arrange(LAST_NAME, FIRST_NAME, Clinical_Visit_Date)

####################Chris's here..... 

suppressWarnings(source(paste0(scripts,"Schedule_script_functions.R")))
for (row in c(1:nrow(master_IRTA_latest))) {
  #print(paste0("Row: ",row))
  
  if (!is.na(master_IRTA_latest[row,"IRTA_tracker"]) & master_IRTA_latest[row, "IRTA_tracker"] != "REMOVED") {
    print_dates(row,master_IRTA_latest)
    
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
  arrange(LAST_NAME, FIRST_NAME, Overall_date) %>% 
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

#####Removing unnecessary variables

rm(temp, IRTA_full, IRTA_init)
rm(age_dummy, date_variabes, j, temp2, split1)
rm(task_reshape, i, iter, eligibility_variables, x)
rm(MID_task_QC, MMI_task_QC, float, MID_missing, MID_temp, task_reshape_master, task_QC, MMI_missing, missing_date, missing_number, missing_qc)
rm(master_IRTA_reordered, master_IRTA_template)
# rm(task1, task2, task3, task4, task5)
# rm("KJ_data", "KC_data", "CW_data", "SK_data", "LG_data", "CC_data", "KH_data") # run this to remove all individual trackers, if created with 'assign' above

