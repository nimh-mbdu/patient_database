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
master_IRTA_template <- reduce(irta_sets, full_join) %>% mutate(Participant_Type2 = NA) %>% mutate(Age_at_visit = NA)

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
master_IRTA_screens_template <- reduce(irta_screen_sets, full_join) %>% mutate(Participant_Type2 = NA, Clinical_Visit_Number = NA, Clinical_Visit_Code = NA) %>% mutate(Age_at_visit = NA)

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
master_IRTA_old_screens_template <- reduce(irta_old_screen_sets, full_join) %>% mutate(Participant_Type2 = NA) %>% mutate(Age_at_visit = NA)

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
  fill(FIRST_NAME, LAST_NAME, SDAN:Handedness, Parent_CTSS_username:Metal, .direction = "down") %>% 
  fill(FIRST_NAME, LAST_NAME, SDAN:Handedness, Parent_CTSS_username:Metal, .direction = "up") %>% 
  ungroup() %>% distinct(., .keep_all = TRUE)
master_IRTA_reordered <- master_IRTA_reordered %>% group_by(Initials) %>% arrange(Initials, Overall_date) %>% 
  fill(Consent_Date, Protocol, Data_sharing, Clinicals, Clinicals_date, .direction = "down") %>% 
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
master_IRTA_oldest_screens_latest <- master_IRTA_old_screens_reordered %>% select(irta_tracker_columns$include) %>% arrange(Referral_Date)

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

task_reshape_master = data.frame(matrix(ncol = 15, nrow = 0))
x <- c("FIRST_NAME", "LAST_NAME", "Initials", "SDAN", "IRTA_tracker", "Participant_Type2", 
       "Eligible", "Scheduling_status", "Overall_date", "Protocol", "Scanner", 
       "Task_Name", "Task_Number", "Task_Date", "Task_Visit_Type")
colnames(task_reshape_master) <- x

for(i in seq_len(max_tasks)) {
  iter <- as.numeric(i)
  # iter=2 # for manually running without the loop

  task_reshape <- master_IRTA_latest %>% 
    select(FIRST_NAME, LAST_NAME, Initials, SDAN, IRTA_tracker, Participant_Type2, 
           Eligible, Scheduling_status, Overall_date, Protocol, Scanner,
           paste0("Task", iter, "_Name"), paste0("Task", iter, "_Number"), 
           paste0("Task", iter, "_Date"), paste0("Task", iter, "_Visit_Type")) 
  
  if(iter==1) {
    dummy <- task_reshape %>% filter(is.na(Task1_Name)) %>%
      filter(is.na(Task1_Date) & is.na(Task1_Number) & is.na(Task1_Visit_Type)) %>% filter(!is.na(Overall_date))
    dummy$Task1_Name <- replace_na(dummy$Task1_Name, "No_Task")
    dummy$Task1_Date <- dummy$Overall_date
    dummy2 <- task_reshape %>% filter(!is.na(Task1_Name) | !is.na(Task1_Date) | !is.na(Task1_Number) | !is.na(Task1_Visit_Type))
    task_reshape <- merge.default(dummy2, dummy, all=TRUE)
    rm(dummy, dummy2)
  }
  
  names(task_reshape)[names(task_reshape) == paste0("Task", iter, "_Name")] <- "Task_Name"
  names(task_reshape)[names(task_reshape) == paste0("Task", iter, "_Number")] <- "Task_Number"
  names(task_reshape)[names(task_reshape) == paste0("Task", iter, "_Date")] <- "Task_Date"
  names(task_reshape)[names(task_reshape) == paste0("Task", iter, "_Visit_Type")] <- "Task_Visit_Type"
  
  task_reshape <- task_reshape %>% 
    filter(!is.na(Task_Name) | !is.na(Task_Date) & !is.na(Task_Number) & !is.na(Task_Visit_Type)) %>% 
    distinct(., .keep_all = TRUE)
  # assign(paste0("task", iter), task_reshape) # uncomment this row if you would like to KEEP all individual files from loop to QC them

  task_reshape_master <- merge.default(task_reshape_master, task_reshape, all=TRUE)
}

##### Adding other info to tasks dataset 

eligibility_variables <- master_IRTA_latest %>% 
  select(Initials, SDAN, DAWBA_ID, PLUSID, IRTA_tracker, Overall_date, Eligible, 
         SEX, DOB, Handedness, Participant_Type, Participant_Type2, Age_at_visit, Clinical_Visit_Date, 
         Clinical_Visit_Type, Clinical_Visit_Code, Clinical_Visit_Number, Scheduling_status) %>%
  arrange(Initials, Overall_date, desc(Clinical_Visit_Type)) %>%
  distinct(., Initials, SDAN, Overall_date, .keep_all = TRUE)

task_reshape_master <- left_join(task_reshape_master, eligibility_variables, all=TRUE)
task_reshape_master <- task_reshape_master %>% 
  select(FIRST_NAME, LAST_NAME, Initials, SDAN, DAWBA_ID, PLUSID, IRTA_tracker,
         SEX, DOB, Handedness, Participant_Type, Participant_Type2, Age_at_visit, Overall_date, 
         Eligible, Clinical_Visit_Date, Clinical_Visit_Type, Clinical_Visit_Code, Clinical_Visit_Number,
         Scheduling_status, Protocol, Scanner, 
         Task_Name, Task_Number, Task_Date, Task_Visit_Type) %>% 
  arrange(Initials, Overall_date) %>% 
  filter(!is.na(Task_Name)) %>% filter(!is.na(Task_Date)) %>% 
  distinct(., .keep_all = TRUE) 

task_reshape_master$Clinical_Visit_Code <- na_if(task_reshape_master$Clinical_Visit_Code, "")
task_reshape_master$Clinical_Visit_Number <- na_if(task_reshape_master$Clinical_Visit_Number, "")

##### Overall task QC

# task name check
  task_names <- read_excel(paste0(IRTA_tracker_location, "/other_data_never_delete/tasks_list.xlsx"))
  task_name_check <- task_reshape_master %>% filter(!is.na(Task_Name)) %>% select(Initials, SDAN, IRTA_tracker, Task_Name, Task_Date, Task_Number, Task_Visit_Type)
  task_name_check$correct <- (task_name_check$Task_Name %in% task_names$Task_Name) %>% as.character() 
  task_name_check <- task_name_check %>% filter(correct=="FALSE") %>% mutate(reason1 = "Task name incorrect") %>% select(-correct)
    
# uniqeness: 
  task_duplicate_date <- task_reshape_master %>% filter(Task_Name!="No_Task") %>% 
    group_by(Initials, Task_Name, Task_Date) %>% filter(n()>1) %>% ungroup() %>% mutate(reason2="Duplicate task date")
  task_duplicate_v_type <- task_reshape_master %>% filter(Task_Name!="No_Task") %>% filter(!is.na(Task_Visit_Type)) %>% 
    group_by(Initials, Task_Name, Task_Visit_Type) %>% filter(n()>1) %>% ungroup() %>% mutate(reason3="Duplicate task visit type")
  task_duplicate_number <- task_reshape_master %>% filter(Task_Name!="No_Task") %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
    group_by(Initials, Task_Name, Task_Number) %>% filter(n()>1) %>% ungroup() %>% mutate(reason4="Duplicate task number")  
  
# task information missing 
  task_number_check <- task_reshape_master %>% filter(!is.na(Task_Name) & !is.na(Task_Date)) %>% filter(Task_Name!="No_Task" & Task_Name!="Measures") %>% 
    filter(is.na(Task_Number)) %>% mutate(reason5 = "Missing task number")
  task_scanner_missing <- task_reshape_master %>% filter(str_detect(Task_Name, "_scan")) %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
    filter(is.na(Scanner)) %>% mutate(reason6="Task scanner missing") 
  
# missing other crucial information
  task_missing_handedness <- task_reshape_master %>% filter(str_detect(Task_Name, "_scan")) %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
    filter(Participant_Type2=="MDD" | Participant_Type2=="HV" | is.na(Participant_Type2)) %>% 
    group_by(Initials) %>% slice(1) %>% ungroup() %>% filter(is.na(Handedness)) %>% mutate(reason7 = "Missing handedness")
  task_missing_dx <- task_reshape_master %>% filter(!is.na(Task_Name) & Task_Name!="No_Task") %>% 
    filter(Task_Number!="777" & Task_Number!="999") %>% filter(is.na(Participant_Type2)) %>% mutate(reason8 = "Missing diagnosis")
  duplicate_initials <- task_reshape_master %>% filter(!is.na(SDAN)) %>% select(Initials, SDAN) %>% distinct(., .keep_all = TRUE) %>% 
    group_by(SDAN) %>% filter(n()>1) %>% ungroup() %>% mutate(reason9="Duplicate Initials") 
  duplicate_sdan <- task_reshape_master %>% filter(!is.na(Initials)) %>% select(Initials, SDAN) %>% distinct(., .keep_all = TRUE) %>% 
    group_by(Initials) %>% filter(n()>1) %>% ungroup() %>% mutate(reason10="Duplicate SDAN") 
  task_missing_sex <- task_reshape_master %>% group_by(Initials) %>% slice(1) %>% ungroup() %>% filter(is.na(SEX)) %>% mutate(reason11="Missing gender") 
  task_missing_dob <- task_reshape_master %>% group_by(Initials) %>% slice(1) %>% ungroup() %>% filter(is.na(DOB)) %>% mutate(reason12="Missing DOB") 
  task_missing_initials <- task_reshape_master %>% filter(is.na(Initials)) %>% mutate(reason13="Missing Initials") 
  task_missing_eligible <- task_reshape_master %>% filter(!is.na(Task_Name) & Task_Name!="No_Task") %>% filter(is.na(Eligible)) %>% 
    filter(Task_Date<todays_date_formatted) %>% mutate(reason14="Missing eligibility information") 
  task_missing_clinical_date <- task_reshape_master %>% filter(!is.na(Task_Name) & Task_Name!="No_Task") %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
    filter(!is.na(Task_Date)) %>% filter(is.na(Clinical_Visit_Date)) %>% mutate(reason15="Missing clinical visit date") 
  task_check_clinical_code <- task_reshape_master %>% filter(!is.na(Task_Name) & Task_Name!="No_Task") %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
    filter(!is.na(Task_Date)) %>% filter(is.na(Clinical_Visit_Code) | is.na(Clinical_Visit_Number)) %>% mutate(reason16="Check clinical visit code") 
  task_missing_scheduling <- task_reshape_master %>% filter(is.na(Scheduling_status)) %>% 
    filter(!is.na(Task_Date)) %>% filter(is.na(Clinical_Visit_Date)) %>% mutate(reason17="Missing scheduling status") 
  task_missing_dawbaid <- task_reshape_master %>% select(FIRST_NAME:Clinical_Visit_Type, Protocol) %>% 
    group_by(Initials) %>% slice(1) %>% ungroup() %>% filter(Participant_Type2=="MDD" | Participant_Type2=="HV") %>% 
    filter(!str_detect(Participant_Type, "Fox")) %>% distinct(., .keep_all = TRUE) %>% filter(is.na(DAWBA_ID)) %>% 
    filter(str_detect(Protocol, "0037")) %>% filter(Overall_date > as.Date("2018-05-01")) %>% mutate(reason18="Missing DAWBA ID") 
  task_missing_protocol <- task_reshape_master %>% select(FIRST_NAME:Clinical_Visit_Type, Protocol) %>% 
    group_by(Initials) %>% arrange(Overall_date) %>% slice(1) %>% ungroup() %>% filter(is.na(Protocol)) %>% 
    filter(Participant_Type2=="MDD" | Participant_Type2=="HV") %>% filter(!str_detect(Participant_Type, "Fox")) %>% 
    filter(Clinical_Visit_Date < todays_date_formatted) %>% mutate(reason19="Missing protocol number")
  
  # comparing to previous version 
  task_master_file <- list.files(path = paste0(IRTA_tracker_location, "IRTA_Master_Backups/"), pattern = "^TASKS_DATABASE_QC", all.files = FALSE,
                                   full.names = FALSE, recursive = FALSE,
                                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  task_master_file_time <- file.mtime(paste0(IRTA_tracker_location, "IRTA_Master_Backups/", task_master_file)) %>% as.Date()
  task_master_combined <- tibble(File=c(task_master_file), Date=c(task_master_file_time)) 
  task_master_combined$date_diff <- as.numeric(difftime(last_week_date_formatted, task_master_combined$Date, tz="", units = "days"))
  task_master_combined$day_of_week <- weekdays(as.Date(task_master_combined$Date))
  task_master_combined <- task_master_combined %>% filter(day_of_week=="Wednesday") %>% arrange(date_diff) %>% filter(date_diff>=0) %>% slice(1)
  prev_task_database <- read_excel(paste0(IRTA_tracker_location, "IRTA_Master_Backups/", task_master_combined[1]))  %>%
    select(Initials:Participant_Type, Eligible:Task_Visit_Type) %>% mutate(source2="old version of tracker")
  date_variabes <- c("DOB", "Task_Date")
  numeric_variables <- c("Task_Number")
  prev_task_database[date_variabes] <- lapply(prev_task_database[date_variabes], as.Date) 
  prev_task_database[numeric_variables] <- lapply(prev_task_database[numeric_variables], as.numeric) 

  historical_check <- task_reshape_master %>% select(Initials:Participant_Type, Eligible:Task_Visit_Type) %>% 
    mutate(source1="new version of tracker") %>% merge.default(., prev_task_database, all=TRUE) %>% filter(is.na(source1) | is.na(source2))
  historical_check$Info_source <- coalesce(historical_check$source1, historical_check$source2)
  historical_check <- historical_check %>% select(-source1, -source2) %>% group_by(Initials) %>% filter(n()>1) %>% ungroup() %>% 
    mutate(reason20="Information change from previous tracker merge: check deliberate vs. accidental. Origin of information") 
  
# combining the above 
  task_errors_combined <- merge.default(task_name_check, task_duplicate_date, all=TRUE) %>% merge.default(., task_duplicate_v_type, all=TRUE) %>% 
    merge.default(., task_duplicate_number, all=TRUE) %>% merge.default(., task_number_check, all=TRUE) %>% merge.default(., task_scanner_missing, all=TRUE) %>% 
    merge.default(., task_missing_handedness, all=TRUE) %>% merge.default(., task_missing_dx, all=TRUE) %>% merge.default(., duplicate_initials, all=TRUE) %>% 
    merge.default(., duplicate_sdan, all=TRUE) %>% merge.default(., task_missing_sex, all=TRUE) %>% merge.default(., task_missing_dob, all=TRUE) %>% 
    merge.default(., task_missing_initials, all=TRUE) %>% merge.default(., task_missing_eligible, all=TRUE) %>% merge.default(., task_missing_clinical_date, all=TRUE) %>% 
    merge.default(., task_check_clinical_code, all=TRUE) %>% merge.default(., task_missing_scheduling, all=TRUE) %>% merge.default(., task_missing_dawbaid, all=TRUE) %>% 
    merge.default(., task_missing_protocol, all=TRUE) %>% merge.default(., historical_check, all=TRUE) %>% select(-FIRST_NAME, -LAST_NAME)
  
  task_errors_combined$QC_task <- paste(task_errors_combined$reason1, task_errors_combined$reason2, task_errors_combined$reason3, task_errors_combined$reason4, 
                                        task_errors_combined$reason5, task_errors_combined$reason6, sep = "; ")
  task_errors_combined$QC_other <- paste(task_errors_combined$reason7, task_errors_combined$reason8, task_errors_combined$reason9, task_errors_combined$reason10, 
                                         task_errors_combined$reason11, task_errors_combined$reason12, task_errors_combined$reason13, task_errors_combined$reason14, 
                                         task_errors_combined$reason15, task_errors_combined$reason16, task_errors_combined$reason17, task_errors_combined$reason18, 
                                         task_errors_combined$reason19, sep = "; ")
  task_errors_combined$QC_historical <- paste(task_errors_combined$reason20, task_errors_combined$Info_source, sep = "; ")
  
  task_errors_combined$QC_task <- gsub("NA; ", "", task_errors_combined$QC_task, fixed=TRUE)
  task_errors_combined$QC_task <- gsub("; NA", "", task_errors_combined$QC_task, fixed=TRUE)
  task_errors_combined$QC_task <- na_if(task_errors_combined$QC_task, "NA")
  task_errors_combined$QC_other <- gsub("NA; ", "", task_errors_combined$QC_other, fixed=TRUE)
  task_errors_combined$QC_other <- gsub("; NA", "", task_errors_combined$QC_other, fixed=TRUE)
  task_errors_combined$QC_other <- na_if(task_errors_combined$QC_other, "NA")
  task_errors_combined$QC_historical <- gsub("NA; ", "", task_errors_combined$QC_historical, fixed=TRUE)
  task_errors_combined$QC_historical <- gsub("; NA", "", task_errors_combined$QC_historical, fixed=TRUE)
  task_errors_combined$QC_historical <- na_if(task_errors_combined$QC_historical, "NA")
  
  task_errors_combined <- task_errors_combined %>% select(-matches("reason"), -Info_source, -Participant_Type2, -Age_at_visit, -Overall_date) %>% arrange(Initials, Task_Date)

# check <- eligibility_variables %>% filter(is.na(Clinical_Visit_Type)) %>% filter(Scheduling_status=='3')
# check <- table(task_reshape_master$Task_Name, task_reshape_master$IRTA_tracker) %>% as.data.frame() %>% filter(Freq!='0')

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
MMI_missing <- task_reshape_master %>% 
  filter(Task_Name=="MMI_3blocks_scan") %>% 
  select(Initials, SDAN, IRTA_tracker, Eligible, Scheduling_status,
         Participant_Type2, Protocol, Clinical_Visit_Type, Scanner,
         Task_Name, Task_Number, Task_Date) %>% 
  merge.default(., MMI_task_QC, all=TRUE)

MMI_missing$Days_since_scan <- as.numeric(difftime(MMI_missing$Task_Date, todays_date_formatted, tz="", units = "days"))
of_interest <- c('IRTA_tracker', 'Eligible', 'Scheduling_status', 'Protocol', 'Scanner', 'Task_Number', 'QC_tracker_tab_no', 'Include', 'Days_since_scan')
MMI_missing[of_interest] <- lapply(MMI_missing[of_interest], replace_na, '666')
numeric <- c('Eligible', 'Scheduling_status', 'Task_Number', 'QC_tracker_tab_no', 'Include', 'Days_since_scan')
MMI_missing[numeric] <- lapply(MMI_missing[numeric], as.numeric)

MMI_missing_date <- MMI_missing %>% filter(is.na(Task_Date)) %>% filter(Task_Number != '666') %>% mutate(reason1="Missing task date")
MMI_missing_number <- MMI_missing %>% filter(Days_since_scan<0) %>% 
  filter(Task_Number == '666') %>% mutate(reason2="Missing task number")
MMI_missing_qc <- MMI_missing %>% filter(Days_since_scan<0) %>% 
  filter(Include=='666') %>% mutate(reason3="Missing from MMI tracker (check really missing vs. info mismatch)")
MMI_missing_irta <- MMI_missing %>% filter(Days_since_scan<0) %>% 
  filter(IRTA_tracker=='666') %>% mutate(reason4="Missing from IRTA tracker")
MMI_duplicate_date <- MMI_missing %>% group_by(SDAN, Task_Date) %>% filter(n()>1) %>% ungroup() %>% mutate(reason5="Duplicate date")
MMI_duplicate_number <- MMI_missing %>% group_by(SDAN, Task_Number) %>% filter(n()>1) %>% ungroup() %>% mutate(reason6="Duplicate number")

MMI_missing_combined <- merge.default(MMI_missing_date, MMI_missing_number, all=TRUE) %>% merge.default(., MMI_missing_qc, all=TRUE) %>% 
  merge.default(., MMI_missing_irta, all=TRUE) %>% merge.default(., MMI_duplicate_date, all=TRUE) %>% merge.default(., MMI_duplicate_number, all=TRUE)
MMI_missing_combined$QC_missing <- paste(MMI_missing_combined$reason1, MMI_missing_combined$reason2, MMI_missing_combined$reason3, 
                                         MMI_missing_combined$reason4, MMI_missing_combined$reason5, MMI_missing_combined$reason6, sep = "; ")
MMI_missing_combined$QC_missing <- gsub("NA; ", "", MMI_missing_combined$QC_missing, fixed=TRUE)
MMI_missing_combined$QC_missing <- gsub("; NA", "", MMI_missing_combined$QC_missing, fixed=TRUE)
MMI_missing_combined <- MMI_missing_combined %>% select(-matches("reason")) %>% arrange(Initials, Task_Date)
MMI_missing_combined[of_interest] <- lapply(MMI_missing_combined[of_interest], na_if, '666')

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
    select(SDAN, Task_Name, Task_Date, QC_tracker_tab_no, Include) %>% 
    filter(!is.na(SDAN))
  MID_task_QC <- merge.default(MID_task_QC, float, all=TRUE)
} 
MID_task_QC <- MID_task_QC %>% arrange(SDAN, QC_tracker_tab_no)
MID_task_QC$Task_Date <- as.Date(MID_task_QC$Task_Date)
MID_task_QC$SDAN <- as.character(MID_task_QC$SDAN)

##### checking for merge conflicts/missing information:

MID_check <- task_reshape_master %>% filter(Task_Name=="MID_scan") %>% 
  select(Initials, SDAN, IRTA_tracker, Eligible, Scheduling_status, Participant_Type2, Protocol, Clinical_Visit_Type, 
         Scanner, Task_Name, Task_Number, Task_Date, Task_Visit_Type)
RS_check <- task_reshape_master %>% filter(Task_Name=="Resting_state_scan") %>% 
  select(Initials, SDAN, IRTA_tracker, Eligible, Scheduling_status, Participant_Type2, Protocol, Clinical_Visit_Type, 
         Scanner, Task_Name, Task_Number, Task_Date, Task_Visit_Type) %>% rename(Resting = "Task_Name")
MID_missing <- MID_check %>% merge.default(., MID_task_QC, all=TRUE) %>% 
  merge.default(., RS_check, all=TRUE)

MID_missing$Days_since_scan <- as.numeric(difftime(MID_missing$Task_Date, todays_date_formatted, tz="", units = "days"))
of_interest <- c('IRTA_tracker', 'Eligible', 'Scheduling_status', 'Protocol', 'Scanner', 'Task_Number', 'QC_tracker_tab_no', 'Include', 'Days_since_scan')
MID_missing[of_interest] <- lapply(MID_missing[of_interest], replace_na, '666')
numeric <- c('Eligible', 'Scheduling_status', 'Task_Number', 'QC_tracker_tab_no', 'Include', 'Days_since_scan')
MID_missing[numeric] <- lapply(MID_missing[numeric], as.numeric)

MID_missing_date <- MID_missing %>% filter(is.na(Task_Date)) %>% filter(Task_Number != '666') %>% mutate(reason1="Missing task date")
MID_missing_number <- MID_missing %>% filter(Days_since_scan<0) %>% filter(Task_Number == '666') %>% mutate(reason2="Missing task number")
MID_missing_qc <- MID_missing %>% filter(Days_since_scan<0) %>% filter(Task_Number!="777" & Task_Number!="999") %>%
  filter(Include=='666' & !is.na(Task_Name)) %>% mutate(reason3="Missing from MID tracker (check really missing vs. info mismatch)")
MID_missing_irta <- MID_missing %>% filter(Days_since_scan<0) %>% 
  filter(IRTA_tracker=='666') %>% mutate(reason4="Missing from IRTA tracker")
MID_duplicate_date <- MID_missing %>% filter(!is.na(Task_Name)) %>% group_by(SDAN, Task_Date) %>% filter(n()>1) %>% ungroup() %>% mutate(reason5="Duplicate MID date")
MID_duplicate_number <- MID_missing %>% filter(!is.na(Task_Name)) %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
  group_by(SDAN, Task_Number) %>% filter(n()>1) %>% ungroup() %>% mutate(reason6="Duplicate MID number")
MID_duplicate_v_type <- MID_missing %>% filter(!is.na(Task_Name)) %>% filter(!is.na(Task_Visit_Type)) %>% 
  group_by(SDAN, Task_Visit_Type) %>% filter(n()>1) %>% ungroup() %>% mutate(reason7="Duplicate MID task visit type")
RS_duplicate_date <- MID_missing %>% filter(!is.na(Resting)) %>% group_by(SDAN, Task_Date) %>% filter(n()>1) %>% ungroup() %>% mutate(reason8="Duplicate RS date")
RS_duplicate_number <- MID_missing %>% filter(!is.na(Resting)) %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
  group_by(SDAN, Task_Number) %>% filter(n()>1) %>% ungroup() %>% mutate(reason9="Duplicate RS number")
RS_duplicate_v_type <- MID_missing %>% filter(!is.na(Resting)) %>% filter(!is.na(Task_Visit_Type)) %>% 
  group_by(SDAN, Task_Visit_Type) %>% filter(n()>1) %>% ungroup() %>% mutate(reason10="Duplicate RS task visit type")
MID_resting_discrepancy<- MID_missing %>% filter(is.na(Resting) | is.na(Task_Name)) %>% group_by(Initials) %>% filter(n()>1) %>% # mutate(count = row_number()) %>%
  ungroup() %>% mutate(reason11="MID & RS info doesn't match; check task date, visit type & number are the same for both")
MID_missing_resting <- MID_missing %>% filter(is.na(Resting) | is.na(Task_Name)) %>% group_by(Initials) %>% filter(n()<2) %>% 
  ungroup() %>% mutate(reason12="MID &/or RS missing")

MID_missing_combined <- merge.default(MID_missing_date, MID_missing_number, all=TRUE) %>% merge.default(., MID_missing_qc, all=TRUE) %>% 
  merge.default(., MID_missing_irta, all=TRUE) %>% merge.default(., MID_duplicate_date, all=TRUE) %>% merge.default(., MID_duplicate_number, all=TRUE) %>% 
  merge.default(., MID_duplicate_v_type, all=TRUE) %>% merge.default(., RS_duplicate_date, all=TRUE) %>% merge.default(., RS_duplicate_number, all=TRUE) %>% 
  merge.default(., RS_duplicate_v_type, all=TRUE) %>% merge.default(., MID_resting_discrepancy, all=TRUE) %>% merge.default(., MID_missing_resting, all=TRUE)
  
MID_missing_combined$QC_missing <- paste(MID_missing_combined$reason1, MID_missing_combined$reason2, MID_missing_combined$reason3, 
                                         MID_missing_combined$reason4, MID_missing_combined$reason5, MID_missing_combined$reason6, 
                                         MID_missing_combined$reason7, MID_missing_combined$reason8, MID_missing_combined$reason9, 
                                         MID_missing_combined$reason10, MID_missing_combined$reason11, MID_missing_combined$reason12, sep = "; ")
MID_missing_combined$QC_missing <- gsub("NA; ", "", MID_missing_combined$QC_missing, fixed=TRUE)
MID_missing_combined$QC_missing <- gsub("; NA", "", MID_missing_combined$QC_missing, fixed=TRUE)
MID_missing_combined <- MID_missing_combined %>% select(-matches("reason")) %>% arrange(Initials, Task_Date)
MID_missing_combined[of_interest] <- lapply(MID_missing_combined[of_interest], na_if, '666')

######################################################################################
#######Adding MEG QC information

##### list of MEG from QC tracker 

MEG_tasks <- c("MEG_MMI", "Booster", "RL_GNG")

for(i in seq_along(MEG_tasks)) {
  iter <- as.numeric(i)
  # iter=1
  temp_MEG_data <- read_excel(paste0(MEG_tracker_location, "MEG_Tracker.xlsx"), sheet = MEG_tasks[iter]) %>%
    mutate_all(as.character) %>% mutate(MEG_tab=MEG_tasks[iter])
  assign(paste0("task_", iter, "_meg_data"), temp_MEG_data)
}

# merge & tidy up

rm(temp_MEG_data)
meg_sets <- ls(pattern="_meg_data")
meg_sets <- mget(meg_sets)
meg_combined <- reduce(meg_sets, full_join) %>% rename(Participant_Type = "Group") %>% rename(Task_Date = "Date") %>%
  rename(Task_Time = "taskTime") %>% rename(IRTA_tracker = "IRTA Contact") %>% rename(MEG_Scan_Notes = "Scan_Notes") %>% 
  select(-Age, -MRN, -Sex, -Earnings, -Task_Time, -Recent_MPRAGE, -IRTA_tracker, -Participant_Type, -`T1 moved to MEG directory`, -`T1 Image Notes`)
meg_combined$Days_since_scan <- as.numeric(difftime(meg_combined$Task_Date, todays_date_formatted, tz="", units = "days")) %>% round(., digits=0)

meg_reshape_master = data.frame(matrix(ncol = 12, nrow = 0))
x <- c("Initials", "SDAN", "MEG_tab", "Task_Date", "Days_since_scan", "Task_Name", "Task_Number", "Include",
       "Experimenter1", "Experimenter2", "MEG_Scan_Notes", "Photo_PII_Removal")
colnames(meg_reshape_master) <- x

for(j in seq_len(max_MEG)) {
  iter <- as.numeric(j)
  # iter=3

  meg_reshape <- meg_combined %>%
      select(Initials, SDAN, MEG_tab, Task_Date, Days_since_scan, paste0("Task", iter, "_Name"), paste0("Task", iter, "_Number"), paste0("Task", iter, "_Include"),
             Experimenter1, Experimenter2, MEG_Scan_Notes, Photo_PII_Removal)

    names(meg_reshape)[names(meg_reshape) == paste0("Task", iter, "_Name")] <- "Task_Name"
    names(meg_reshape)[names(meg_reshape) == paste0("Task", iter, "_Number")] <- "Task_Number"
    names(meg_reshape)[names(meg_reshape) == paste0("Task", iter, "_Include")] <- "Include"

    meg_reshape_master <- merge.default(meg_reshape_master, meg_reshape, all=TRUE) %>% filter(!is.na(Task_Name))
}

##### list of MEG from IRTA trackers 

meg_list <- task_reshape_master %>% filter(str_detect(Task_Name, "MEG")) %>%
  select(Initials, SDAN, IRTA_tracker, Participant_Type2, Task_Name, Task_Number, Task_Date)

##### checking for merge conflicts/missing information:

MEG_task_QC <- meg_reshape_master %>% filter(str_detect(Task_Name, "MEG")) %>% merge.default(meg_list, ., all=TRUE) %>% 
  select(-Photo_PII_Removal, -Experimenter1, -Experimenter2)

of_interest <- c('IRTA_tracker', 'Task_Number', 'Include', 'Days_since_scan', 'MEG_tab')
MEG_task_QC[of_interest] <- lapply(MEG_task_QC[of_interest], replace_na, '666')
numeric <- c('Task_Number', 'Include', 'Days_since_scan')
MEG_task_QC[numeric] <- lapply(MEG_task_QC[numeric], as.numeric)

meg_missing_date <- MEG_task_QC %>% filter(is.na(Task_Date)) %>% mutate(reason1="Missing task date")
meg_missing_number <- MEG_task_QC %>% filter(Days_since_scan<0 | Days_since_scan=='666') %>% filter(Task_Number=='666') %>% mutate(reason2="Missing task number")
meg_missing_qc_tracker <- MEG_task_QC %>% filter(MEG_tab=="666") %>% mutate(reason3="Missing from MEG tracker")
meg_missing_qc <- MEG_task_QC %>% filter(Task_Number!="777" & Task_Number!="999") %>% filter(Include=="666") %>% mutate(reason4="Missing QC status information")
meg_missing_irta_tracker <- MEG_task_QC %>% filter(IRTA_tracker=="666") %>% mutate(reason5="Missing from IRTA tracker")
meg_duplicate_date <- MEG_task_QC %>% group_by(Initials, Task_Name, Task_Date) %>% filter(n()>1) %>% ungroup() %>% mutate(reason6="Duplicate MEG date (check dates but also potential task number mismatch")
meg_duplicate_number1 <- MEG_task_QC %>% filter(Task_Name!="MEG_Resting_state") %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
  group_by(Initials, Task_Name, Task_Number) %>% filter(n()>1) %>% ungroup() %>% mutate(reason7="Duplicate MEG task number (check number but also check potential date mismatch)")
meg_duplicate_number2 <- MEG_task_QC %>% filter(Task_Name=="MEG_Resting_state") %>% filter(Task_Number!="777" & Task_Number!="999") %>% 
  group_by(Initials, MEG_tab, Task_Number) %>% filter(n()>1) %>% ungroup() %>% mutate(reason7="Duplicate MEG task number (check number but also check potential date mismatch)")

MEG_missing <- merge.default(meg_missing_date, meg_missing_number, all=TRUE) %>% merge.default(., meg_missing_qc_tracker, all=TRUE) %>% 
  merge.default(., meg_missing_qc, all=TRUE) %>% merge.default(., meg_missing_irta_tracker, all=TRUE) %>% merge.default(., meg_duplicate_date, all=TRUE) %>% 
  merge.default(., meg_duplicate_number1, all=TRUE) %>% merge.default(., meg_duplicate_number2, all=TRUE)
MEG_missing$QC_missing <- paste(MEG_missing$reason1, MEG_missing$reason2, MEG_missing$reason3, MEG_missing$reason4, 
                                MEG_missing$reason5, MEG_missing$reason6, MEG_missing$reason7, sep = "; ")
MEG_missing$QC_missing <- gsub("NA; ", "", MEG_missing$QC_missing, fixed=TRUE)
MEG_missing$QC_missing <- gsub("; NA", "", MEG_missing$QC_missing, fixed=TRUE)
MEG_missing <- MEG_missing %>% select(-matches("reason")) %>% arrange(Initials, Task_Date)
MEG_missing[of_interest] <- lapply(MEG_missing[of_interest], na_if, '666')
MEG_task_QC[of_interest] <- lapply(MEG_task_QC[of_interest], na_if, '666')
MEG_task_QC <- MEG_task_QC %>% select(SDAN, Task_Name, Task_Date, Include)

##### list of MPRAGE from IRTA trackers 

mprage_list <- task_reshape_master %>% filter(str_detect(Task_Name, "_scan") | Task_Name=="MPRAGE") %>% 
  filter(Task_Number!="777" & Task_Number!="999") %>% 
  group_by(Initials) %>% arrange(Task_Date) %>% slice(n()) %>% ungroup() %>% 
  select(Initials, SDAN, IRTA_tracker, Participant_Type2, Task_Name, Task_Date) %>% 
  rename(MPRAGE="Task_Name") %>% rename(MPRAGE_Date="Task_Date")
mprage_list$MPRAGE <- gsub("MID_scan", "Other_MRI_scan", mprage_list$MPRAGE)
mprage_list$MPRAGE <- gsub("Resting_state_scan", "Other_MRI_scan", mprage_list$MPRAGE)
mprage_list$MPRAGE <- gsub("DTI_scan", "Other_MRI_scan", mprage_list$MPRAGE)
mprage_list$MPRAGE <- gsub("MMI_3blocks_scan", "Other_MRI_scan", mprage_list$MPRAGE)

##### checking MPRAGE listed

MEG_mprage <- meg_list %>% filter(Task_Name=="MEG_MMI") %>% merge.default(., mprage_list, all=TRUE) %>% 
  filter(Task_Number!="777" & Task_Number!="999")
MEG_mprage$Days_between_scans <- as.numeric(difftime(MEG_mprage$Task_Date, MEG_mprage$MPRAGE_Date, tz="", units = "days")) %>% round(., digits=0)
MEG_mprage <- MEG_mprage %>% filter(is.na(Days_between_scans) | Days_between_scans>365) %>% select(-Task_Name, -Task_Number, -MPRAGE)

######################################################################################
#####Merge of QC information with main task tracker, drops inconsistencies, hence why accuracy is so important 

task_QC <- merge.default(MID_task_QC, MMI_task_QC, all=TRUE) %>% merge.default(., MEG_task_QC, all=TRUE)
task_reshape_master_QC <- left_join(task_reshape_master, task_QC, all=TRUE)

######################################################################################
#######Export of QC information

if (weekdays(as.Date(todays_date_formatted))=="Wednesday") {
  print("Today is Wednesday -> exporting QC reports")
  
  # general reports - exported into individual IRTA trackers 
  for(j in seq_along(current_IRTAs_full)) {
    iter <- as.numeric(j)
    # iter=1
    IRTA_full <- current_IRTAs_full[iter]
    IRTA_init <- current_IRTAs_init[iter]
    task_errors_combined %>% filter(IRTA_tracker==eval(IRTA_init)) %>% write_xlsx(paste0(referrals_location, IRTA_full, "/", IRTA_init, "_task_qc_", todays_date_formatted, ".xlsx")) 
  }
  task_errors_combined %>% filter(IRTA_tracker=="REMOVED") %>% write_xlsx(paste0(IRTA_tracker_location,"QCing/REMOVED_task_qc_", todays_date_formatted, ".xlsx")) # if file empty, everything is perfect
  
  # Task specific QC reports 
  MMI_missing_combined %>% write_xlsx(paste0(IRTA_tracker_location,"QCing/MMI_check_", todays_date_formatted, ".xlsx")) # if file empty, everything is perfect 
  MID_missing_combined %>% write_xlsx(paste0(IRTA_tracker_location,"QCing/MID_check_", todays_date_formatted, ".xlsx")) # if file empty, everything is perfect 
  MEG_missing %>% write_xlsx(paste0(IRTA_tracker_location,"QCing/MEG_check_", todays_date_formatted, ".xlsx"))
  MEG_mprage %>% write_xlsx(paste0(IRTA_tracker_location,"QCing/MEG_need_mprage_", todays_date_formatted, ".xlsx"))
  
} else {
  print("QC reports not produced - only produced on Wednesdays")
}

######################################################################################
#######Saving Master IRTA sheet in typical format

master_IRTA_latest %>% write_xlsx(paste0(IRTA_tracker_location,"MASTER_IRTA_DATABASE.xlsx")) # will not save if someone else has this dataset open
master_IRTA_latest %>% write_xlsx(paste0(backup_location,"MASTER_IRTA_DATABASE","_",todays_date_formatted,".xlsx"))
# also an option to add a password to a saved excel, e.g. = password = "string"

# checking saved properly
file_save_check <- list.files(path = paste0(IRTA_tracker_location), pattern = "^MASTER_IRTA_DATABASE.xlsx", all.files = FALSE,
                               full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(IRTA_tracker_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'MASTER_IRTA_DATABASE'")
} else {
  print("Conflict: exporting as 'MASTER_IRTA_DATABASE_updated'")
  master_IRTA_latest %>% write_xlsx(paste0(IRTA_tracker_location,"MASTER_IRTA_DATABASE_updated.xlsx"))
}

######################################################################################
#######Saving Tasks Dataset

task_reshape_master_QC %>% write_xlsx(paste0(IRTA_tracker_location,"TASKS_DATABASE_QC.xlsx"))
task_reshape_master_QC %>% write_xlsx(paste0(backup_location,"TASKS_DATABASE_QC","_",todays_date_formatted,".xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(IRTA_tracker_location), pattern = "^TASKS_DATABASE_QC.xlsx", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(IRTA_tracker_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'TASKS_DATABASE_QC'")
} else {
  print("Conflict: exporting as 'TASKS_DATABASE_QC_updated'")
  task_reshape_master_QC %>% write_xlsx(paste0(IRTA_tracker_location,"TASKS_DATABASE_QC_updated.xlsx"))
}

######################################################################################
#######Saving Screens Dataset

master_IRTA_screens_latest %>% write_xlsx(paste0(IRTA_tracker_location,"REFERRAL_AND_SCREENING_DATABASE.xlsx"))
master_IRTA_screens_latest %>% write_xlsx(paste0(backup_location,"REFERRAL_AND_SCREENING_DATABASE","_",todays_date_formatted,".xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(IRTA_tracker_location), pattern = "^REFERRAL_AND_SCREENING_DATABASE.xlsx", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(IRTA_tracker_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'REFERRAL_AND_SCREENING_DATABASE'")
} else {
  print("Conflict: exporting as 'REFERRAL_AND_SCREENING_DATABASE_updated'")
  master_IRTA_screens_latest %>% write_xlsx(paste0(IRTA_tracker_location,"REFERRAL_AND_SCREENING_DATABASE_updated.xlsx"))
}

######################################################################################
#######Saving Old Screens Dataset

master_IRTA_oldest_screens_latest %>% write_xlsx(paste0(IRTA_tracker_location,"OLD_REFERRALS_DATABASE.xlsx"))
master_IRTA_oldest_screens_latest %>% write_xlsx(paste0(backup_location,"OLD_REFERRALS_DATABASE","_",todays_date_formatted,".xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(IRTA_tracker_location), pattern = "^OLD_REFERRALS_DATABASE.xlsx", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(IRTA_tracker_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'OLD_REFERRALS_DATABASE'")
} else {
  print("Conflict: exporting as 'OLD_REFERRALS_DATABASE_updated'")
  master_IRTA_oldest_screens_latest %>% write_xlsx(paste0(IRTA_tracker_location,"OLD_REFERRALS_DATABASE_updated.xlsx"))
}

#####Removing unnecessary variables

rm(list=ls(pattern="_active_data"))
rm(list=ls(pattern="_meg_data"))
rm(list=ls(pattern="_current_screens"))
rm(list=ls(pattern="_old_screens"))
rm(list=ls(pattern="iter"))
rm(list=ls(pattern="_sets"))
rm(list=ls(pattern="missing"))
rm(list=ls(pattern="_reordered"))
rm(list=ls(pattern="_template"))
rm(list=ls(pattern="duplicate"))
rm(list=ls(pattern="age_"))
rm(IRTA_full, IRTA_init, j, irta_tracker_columns, date_variabes, split1, i, eligibility_variables, x, row, u, numeric, of_interest, o)
rm(MID_task_QC, MMI_task_QC, float, task_reshape, task_reshape_master, task_QC, meg_reshape_master, meg_reshape, MEG_tasks, meg_combined,  
   MEG_task_QC, meg_list, MID_check, RS_check, task_check_clinical_code, task_name_check, task_number_check, prev_task_database,
   task_errors_combined, task_names, MID_resting_discrepancy, task_master_file, task_master_file_time, task_master_combined, file_save_check, 
   file_save_check_time, file_save_check_combined, historical_check)
rm(get_last_scan, get_last_visit, has_scan, is_last_v, print_dates, print_notes)
