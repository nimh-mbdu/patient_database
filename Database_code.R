  
# to do: ------------------------------------------------------------------
  
# QC: export column names -------------------------------------------------
  
  # If you want to export the column names of any of the below, for QC or troubleshooting reasons, use the following & replace dataframe name: 
  # temp_columns <- colnames(SDQ_Data_Download_raw)
  # temp_columns %>% write.csv(file = paste0(sdq_pull, "dummy_", todays_date_formatted, ".csv"), na = " ", row.names = FALSE)
  
# Loading files -----------------------------------------------------------
  
  #******Master task tracker
  
  if (exists("task_reshape_master_QC")==FALSE) {
    
        task_master_file <- list.files(path = paste0(IRTA_tracker_location), pattern = "^TASKS_DATABASE_QC", all.files = FALSE,
                                       full.names = FALSE, recursive = FALSE,
                                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
        task_master_file_time <- file.mtime(paste0(IRTA_tracker_location, "/", task_master_file)) %>% as.Date()
        task_master_combined <- tibble(File=c(task_master_file), Date=c(task_master_file_time)) %>% arrange(desc(Date)) %>% slice(1)
        task_reshape_master_QC <- read_excel(paste0(IRTA_tracker_location, task_master_combined[1]))
        date_variabes <- c("DOB", "Overall_date", "Task_Date")
        numeric_variables <- c("Task_Number")
        task_reshape_master_QC[date_variabes] <- lapply(task_reshape_master_QC[date_variabes], as.Date) 
        task_reshape_master_QC[numeric_variables] <- lapply(task_reshape_master_QC[numeric_variables], as.numeric) 
        rm(date_variabes, task_master_file, task_master_file_time, task_master_combined)
  
  } else {
    print("master IRTA tracker + QC info already imported")
  }
  
#******Master IRTA tracker
  
  if (exists("master_IRTA_latest")==FALSE) {
    
        irta_master_file <- list.files(path = paste0(IRTA_tracker_location), pattern = "^MASTER_IRTA_DATABASE", all.files = FALSE,
                                       full.names = FALSE, recursive = FALSE,
                                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
        irta_master_file_time <- file.mtime(paste0(IRTA_tracker_location, "/", irta_master_file)) %>% as.Date()
        irta_master_combined <- tibble(File=c(irta_master_file), Date=c(irta_master_file_time)) %>% arrange(desc(Date)) %>% slice(1)
        master_IRTA_latest <- read_excel(paste0(IRTA_tracker_location, irta_master_combined[1]))
        date_variabes <- c("DOB", "Screening_Start_Date", "Referral_Date", "Consent_Date", "Clinical_Visit_Date", "Clinicals_date", "Overall_date")
        for(i in seq_len(max_tasks)) { date_variabes <- c(date_variabes, paste0("Task", i, "_Date"))}
        master_IRTA_latest[date_variabes] <- lapply(master_IRTA_latest[date_variabes], as.Date)
        rm(i, date_variabes, irta_master_file, irta_master_file_time, irta_master_combined)
    
  } else {
    print("master task tracker already imported")
  }
  
  #****** SDQ+ = where we collect our own psychometric data 

  SDQ_Data_Download_raw <- read.delim(paste0(sdq_pull, latest_sdq_pull, ".txt"),  quote="",  
                                    encoding="UTF-8", row.names = NULL, header = TRUE, stringsAsFactors = FALSE)
  
  #****** adding special cases

  SDQ_Data_Download_raw <- SDQ_Data_Download_raw %>% filter(PlusIID != "4711-5358-6649-5157")
  SDQ_Data_Download_raw <- SDQ_Data_Download_raw %>% filter(PlusIID != "8768-8233-7459-5808")
  SDQ_Data_Download_raw <- SDQ_Data_Download_raw %>% filter(PlusIID != "2738-0093-0639-3598")
  imported_data_23495 <- read_excel(data_23495) %>% mutate_all(as.character)
  imported_data_23544 <- read_excel(data_23544) %>% mutate_all(as.character)
  imported_data_22279 <- read_excel(data_22279) %>% mutate_all(as.character)
  imported_hyphen_issue <- read_excel(data_hyphen_issue) %>% mutate_all(as.character)
  
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, imported_data_23495, all=TRUE)
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, imported_data_23544, all=TRUE)
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, imported_data_22279, all=TRUE)
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, imported_hyphen_issue, all=TRUE)
  
  #****** old dx checklist & mdd form (from before these were combined on sdq & the mdd form removed)
  
  old_ksads_checklist <- read.delim(paste0(data_old_dx_checklist),  quote="", encoding="UTF-8", row.names = NULL, header = TRUE, stringsAsFactors = FALSE)
  old_mdd_form <- read.delim(paste0(data_old_mdd_form),  quote="", encoding="UTF-8", row.names = NULL, header = TRUE, stringsAsFactors = FALSE)
  old_dx_temp <- merge.default(old_ksads_checklist, old_mdd_form, all=TRUE)
  
  old_dx_temp$yfu_clin_name <- coalesce(old_dx_temp$yfu_clin_name, old_dx_temp$ksads_dx_clin_name)
  old_dx_temp$yfu1_today_date <- coalesce(old_dx_temp$yfu1_today_date, old_dx_temp$ksads_dx_today_date)
  old_dx_temp$yfu2_visit_type <- coalesce(old_dx_temp$yfu2_visit_type, old_dx_temp$ksads_dx_visit_type)
  
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, old_dx_temp, all=TRUE)
  
  # fix date conversion below, check format & specify 
  
  SDQ_Data_Download_raw$dateadded <- gsub("0019", "2019", SDQ_Data_Download_raw$dateadded, fixed=TRUE)
  SDQ_Data_Download_raw$dateadded <- gsub("0018", "2018", SDQ_Data_Download_raw$dateadded, fixed=TRUE)
  
  SDQ_Data_Download_raw$Overall_date <- coalesce(as.Date(SDQ_Data_Download_raw$modedate, "%Y-%m-%d"), as.Date(SDQ_Data_Download_raw$dateadded, "%Y-%m-%d")) %>%
    coalesce(., as.Date(SDQ_Data_Download_raw$maxdate, "%Y-%m-%d")) %>% 
    coalesce(., as.Date(SDQ_Data_Download_raw$mindate, "%Y-%m-%d")) 
  
  # changing column names

  sdq_columns <- read_excel(paste0(database_location, "other_data_never_delete/sdq_column_names_and_descriptions.xlsx"))
  setnames(SDQ_Data_Download_raw, old=c(sdq_columns$old_name), new=c(sdq_columns$new_name), skip_absent=TRUE)
  SDQ_Data_Download <- SDQ_Data_Download_raw %>% select(sdq_columns$new_name) %>% arrange(PLUSID, Overall_date) %>% select(-SID)

  #****** CTDB = where data is stored for those not in 0037
  
  CTDB_Data_Download <- read_excel(paste0(ctdb_pull, latest_ctdb_pull, ".xlsx"), sheet = 'Data_and_Scores', col_types = "text")
  
  # changing column names
  
  ctdb_columns <- read_excel(paste0(database_location, "other_data_never_delete/ctdb_column_names_and_descriptions.xlsx"))
  setnames(CTDB_Data_Download, old=c(ctdb_columns$old_name), new=c(ctdb_columns$new_name), skip_absent=TRUE)
  CTDB_Data_Download$Overall_date <- as.Date(CTDB_Data_Download$Overall_date, tz="", "%Y-%m-%d")
  
  #****** Imputed data
  
  imported_imputed_mfqs <- read.csv(imputed_mfqs) 
  imported_imputed_mfqs$s_mfq_date <- as.Date(imported_imputed_mfqs$s_mfq_date, tz="", "%m/%d/%y")
  imported_imputed_mfqs$Overall_date <- imported_imputed_mfqs$s_mfq_date
  
  CTDB_Data_Download <- merge.default(CTDB_Data_Download, imported_imputed_mfqs, all=TRUE)
  CTDB_Data_Download <- CTDB_Data_Download %>% select(ctdb_columns$new_name) %>% arrange(FIRST_NAME, LAST_NAME, Overall_date) 
  
  #****** Fixing names of some participants
  
  CTDB_Data_Download$FIRST_NAME <- gsub("\U0092", "", CTDB_Data_Download$FIRST_NAME, fixed=TRUE)
  CTDB_Data_Download$LAST_NAME <- gsub("\U0092", "", CTDB_Data_Download$LAST_NAME, fixed=TRUE)
  CTDB_Data_Download$LAST_NAME <- gsub("??", "", CTDB_Data_Download$LAST_NAME, fixed=TRUE)

  ctdb_names <- read_excel(paste0(database_location, "other_data_never_delete/ctdb_name_errors.xlsx"))
  for(e in seq_len(nrow(ctdb_names))) {
    iter4 <- as.numeric(e)
    # e = 1
    column <- as.character(ctdb_names[iter4,3])
    incorrect <- as.character(ctdb_names[iter4,1])
    correct <- as.character(ctdb_names[iter4,2])
    if (column=="LAST_NAME") {
      print("Fixing LAST_NAME")
      CTDB_Data_Download$LAST_NAME <- gsub(eval(incorrect), eval(correct), CTDB_Data_Download$LAST_NAME, fixed=TRUE)
    } else {
      print("Fixing FIRST_NAME")
      CTDB_Data_Download$FIRST_NAME <- gsub(eval(incorrect), eval(correct), CTDB_Data_Download$FIRST_NAME, fixed=TRUE)
      }
  }
  
  #****** Manual entry database
  
  manual_shaps <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "SHAPS", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date")  %>% mutate_all(as.character)
  manual_mfq <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "MFQ", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date") %>% mutate_all(as.character)
  manual_ari <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "ARI", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date") %>% mutate_all(as.character)
  manual_lsas <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "LSAS", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date") %>% mutate_all(as.character)
  manual_scared <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "SCARED", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date") %>% mutate_all(as.character)
  manual_ksadsdx <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "KSADS Dx checklist", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date")  %>% mutate_all(as.character)
  manual_ethnicity <- read_excel(paste0(database_location, "other_data_never_delete/ethnicity_list_KCs_CR_tracker.xlsx")) %>% mutate_all(as.character)
  
  manual_sets <- ls(pattern="manual_")
  manual_sets <- mget(manual_sets)
  manual_combined <- reduce(manual_sets, full_join)
  fill_names <- manual_combined %>% select(-Initials) %>% colnames()
  manual_combined <- manual_combined %>% 
    group_by(Initials, Overall_date) %>% 
    fill(., names(fill_names), .direction = "down") %>%
    fill(., names(fill_names), .direction = "up") %>%
    ungroup() %>% 
    distinct(., .keep_all = TRUE)
  manual_combined$Overall_date <- as.Date(manual_combined$Overall_date)
  manual_combined$c_ksadsdx_date <- as.Date(manual_combined$c_ksadsdx_date, "%d-%m-%Y")
  manual_combined$c_ksadsdx_epset_baseline_visit_date <- as.Date(manual_combined$c_ksadsdx_epset_baseline_visit_date, "%d-%m-%Y")
  
  # Clean up -------------------------------------------
  
  # ****** SDQ+
  
  SDQ_Data_Download[SDQ_Data_Download==-2] <- NA
  
  sdq_dates <- select(SDQ_Data_Download, matches("_date")) %>% colnames()
  SDQ_Data_Download[sdq_dates] <- lapply(SDQ_Data_Download[sdq_dates], as.Date, "%d-%m-%Y")
  
  #****** CTDB
  
  ctdb_dates <- select(CTDB_Data_Download, matches("_date")) %>% colnames()
  ctdb_dates <- c(ctdb_dates, "DOB") 
  CTDB_Data_Download[ctdb_dates] <- lapply(CTDB_Data_Download[ctdb_dates], as.Date, "%m/%d/%Y") 
  
  remove_unknown <- c('MRN', 'SDAN', 'FIRST_NAME', 'LAST_NAME', 'SEX', 
                      'Age_at_visit', 'Protocol', 'CTDB_visit_name')
  CTDB_Data_Download[remove_unknown] <- lapply(CTDB_Data_Download[remove_unknown], na_if, 'UNKNOWN')
  
  CTDB_Data_Download <- CTDB_Data_Download %>% filter(!str_detect(LAST_NAME, "test"))
  CTDB_Data_Download <- CTDB_Data_Download %>% filter(!str_detect(LAST_NAME, "TEST"))
  CTDB_Data_Download <- CTDB_Data_Download %>% filter(!str_detect(FIRST_NAME, "test"))
  CTDB_Data_Download <- CTDB_Data_Download %>% filter(!str_detect(FIRST_NAME, "TEST"))
  
  ctdb_numeric <- ctdb_columns %>% filter(num=='1') %>% select(new_name) %>% unlist()
  CTDB_Data_Download[ctdb_numeric] <- lapply(CTDB_Data_Download[ctdb_numeric], as.numeric) 
  CTDB_Data_Download[remove_unknown] <- lapply(CTDB_Data_Download[remove_unknown], na_if, 'NA')
  
  # Prep for combining CTDB & SDQ+ ---------------------------------------------------
  
  ctdb_Data_Download_reduced <- CTDB_Data_Download %>% 
    select(-MRN, -SDAN, -DOB, -SEX, -Age_at_visit, -Overall_date, -CTDB_visit_name) %>% 
    rename(Protocol_CTDB = "Protocol") 
  
  common_identifiers_child <- master_IRTA_latest %>% select(PLUSID, FIRST_NAME, LAST_NAME, SDAN, Initials) %>% 
    group_by(FIRST_NAME, LAST_NAME) %>% fill(PLUSID, SDAN, Initials, .direction = "down") %>% fill(PLUSID, SDAN, Initials, .direction = "up") %>% ungroup() %>% 
    group_by(Initials) %>% fill(PLUSID, SDAN, .direction = "down") %>% fill(PLUSID, SDAN, .direction = "up") %>% ungroup() %>% distinct(., .keep_all = TRUE)
  
  ctdb_w_plusid_child <- left_join(common_identifiers_child, ctdb_Data_Download_reduced, all=TRUE)
  
  common_identifiers_child_sib <- master_IRTA_latest %>% select(FIRST_NAME, LAST_NAME, Initials, PLUSID, IRTA_tracker, Sibling_Init, Sibling_Type) %>% 
    group_by(FIRST_NAME, LAST_NAME) %>% fill(Initials:Sibling_Type, .direction = "down") %>% fill(Initials:Sibling_Type, .direction = "up") %>% ungroup() %>% 
    group_by(Initials) %>% fill(PLUSID:Sibling_Type, .direction = "down") %>% fill(PLUSID:Sibling_Type, .direction = "up") %>% ungroup() %>% 
    filter(!is.na(Sibling_Init) | !is.na(Sibling_Type)) %>% filter(Sibling_Type=="1") %>% filter(IRTA_tracker!="REMOVED") %>% distinct(., .keep_all = TRUE) %>% 
    select(PLUSID, Initials, Sibling_Init)
  
  #****** CTDB parent name prep

  parent_names <- master_IRTA_latest %>% select(PLUSID, SDAN, FIRST_NAME, LAST_NAME, Initials, FIRST_NAME_P1, LAST_NAME_P1, FIRST_NAME_P2, LAST_NAME_P2) %>%
    group_by(FIRST_NAME, LAST_NAME) %>% 
    fill(PLUSID, SDAN, Initials:LAST_NAME_P2, .direction = "down") %>% fill(PLUSID, SDAN, Initials:LAST_NAME_P2, .direction = "up") %>% 
    ungroup() %>% distinct(., .keep_all = TRUE)
  parent_names[3:9] <- lapply(parent_names[3:9], str_to_upper)
  
  # parent 1:
  
  common_identifiers_parent1 <- parent_names %>% select(PLUSID, SDAN, FIRST_NAME, LAST_NAME, Initials, FIRST_NAME_P1, LAST_NAME_P1) %>%
    filter(!is.na(FIRST_NAME_P1)) %>% rename(FIRST_NAME_C = "FIRST_NAME", LAST_NAME_C = "LAST_NAME") %>% 
    rename(FIRST_NAME = "FIRST_NAME_P1", LAST_NAME = "LAST_NAME_P1") %>% distinct(., .keep_all = TRUE)
  ctdb_w_plusid_parent1 <- left_join(common_identifiers_parent1, ctdb_Data_Download_reduced, all=TRUE) %>%
    rename(FIRST_NAME_P1 = "FIRST_NAME", LAST_NAME_P1 = "LAST_NAME") %>% rename(FIRST_NAME = "FIRST_NAME_C", LAST_NAME = "LAST_NAME_C")

  # parent 2:
  
  common_identifiers_parent2 <- parent_names %>% select(PLUSID, SDAN, FIRST_NAME, LAST_NAME, Initials, FIRST_NAME_P2, LAST_NAME_P2) %>%
    filter(!is.na(FIRST_NAME_P2)) %>% rename(FIRST_NAME_C = "FIRST_NAME", LAST_NAME_C = "LAST_NAME") %>% 
    rename(FIRST_NAME = "FIRST_NAME_P2", LAST_NAME = "LAST_NAME_P2") %>% distinct(., .keep_all = TRUE)
  ctdb_w_plusid_parent2 <- left_join(common_identifiers_parent2, ctdb_Data_Download_reduced, all=TRUE) %>%
    rename(FIRST_NAME_P2 = "FIRST_NAME", LAST_NAME_P2 = "LAST_NAME") %>% rename(FIRST_NAME = "FIRST_NAME_C", LAST_NAME = "LAST_NAME_C")
  
  # combining these 
  
  ctdb_w_plusid_parent <- merge.default(ctdb_w_plusid_parent1, ctdb_w_plusid_parent2, all=TRUE) %>% 
    select(PLUSID, SDAN, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, matches("_mfq"), matches("_ari"), matches("_scared"), matches("_medsctdb"))

  # coalescing any information incorrectly entered under 'self', rather than 'parent': 
  
  parent_columns <- read_excel(paste0(database_location, "other_data_never_delete/ctdb_parent_column_fix.xlsx"))
    for(d in seq_len(nrow(parent_columns))) {
      iter5 <- as.numeric(d)
      # d=17
      parent <- parent_columns[d,2] %>% as.character
      child <- parent_columns[d,1] %>% as.character
      p <- which(colnames(ctdb_w_plusid_parent)==eval(parent)) %>% as.numeric()
      c <- which(colnames(ctdb_w_plusid_parent)==eval(child)) %>% as.numeric()
      ctdb_w_plusid_parent[[p]] <- coalesce(ctdb_w_plusid_parent[[p]], ctdb_w_plusid_parent[[c]])
    }
  
  ctdb_w_plusid_parent_reduced <- ctdb_w_plusid_parent %>% 
    select(PLUSID, SDAN, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, matches("p_mfq"), matches("p_ari"), matches("p_scared"), matches("_medsctdb"))

  # joining parent and child report 
  
  ctdb_w_plusid <- merge.default(ctdb_w_plusid_child, ctdb_w_plusid_parent_reduced, all=TRUE) %>% mutate(source = "CTDB")
  
  #****** 
  
  sdq_w_names <- left_join(common_identifiers_child, SDQ_Data_Download, all=TRUE) %>% 
    mutate(source = "SDQ") %>% filter(!is.na(Overall_date))
  
  #****** 
  
  manual_db_w_names <- left_join(common_identifiers_child, manual_combined, all=TRUE) %>% 
    mutate(source = "MANUAL") %>% filter(!is.na(Overall_date))
  
  #******   
  
  clinical_DB <- master_IRTA_latest %>% 
    select(FIRST_NAME, LAST_NAME, Initials, SDAN, DAWBA_ID, PLUSID, IRTA_tracker,
           FIRST_NAME_P1, LAST_NAME_P1, FIRST_NAME_P2, LAST_NAME_P2, Primary_clinician,
           SEX, DOB, Handedness, Participant_Type, Participant_Type2, Age_at_visit,
           Eligible, Clinical_Visit_Date, Clinical_Visit_Type, Clinical_Visit_Code, Clinical_Visit_Number,
           Scheduling_status, Protocol)
  
  clinical_DB_date <- clinical_DB %>% select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date)
  
  task_DB <- task_reshape_master_QC %>% 
    select(FIRST_NAME, LAST_NAME, Initials, SDAN, DAWBA_ID, PLUSID, IRTA_tracker,
           SEX, DOB, Handedness, Participant_Type, Participant_Type2, Age_at_visit,
           Eligible, Clinical_Visit_Date, Clinical_Visit_Type, Clinical_Visit_Code, Clinical_Visit_Number,
           Scheduling_status, Protocol, Task_Name, Task_Number, Task_Date, Task_Visit_Type, QC_tracker_tab_no, Include)
  
  task_DB$Task_Number <- lapply(task_DB$Task_Number, replace_na, '-9')
  task_DB_date <- task_DB %>% filter(Task_Number !="999" & Task_Number !="777") %>% 
    select(FIRST_NAME, LAST_NAME, PLUSID, Initials, Task_Name, Task_Date)
  task_DB$Task_Number <- lapply(task_DB$Task_Number, na_if, '-9')
  task_DB$Task_Number <- as.character(task_DB$Task_Number)

# Scoring SDQ+ and merging with CTDB --------------------------------------
  
#####
  # c_visittrack_ 
  
# The 5 scan measures -----------------------------------------------------

#####
# straightforward total sum
  
  tot_sum <- c('s_mfq_', 'p_mfq_', 's_ari1w_', 's_ari6m_', 'p_ari1w_', 'p_ari6m_', 's_mfq1w_', 'p_mfq1w_')
  
  for(i in seq_along(tot_sum)) {
    iter <- as.numeric(i)
    # iter=1
    measure_name <- tot_sum[iter]
    
      measure_temp_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches(measure_name)) %>% 
        filter(!is.na(Overall_date)) %>% 
        distinct(., .keep_all = TRUE)
      measure_temp_manual <- manual_db_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches(measure_name))
      measure_temp_sdq <- merge.default(measure_temp_sdq, measure_temp_manual, all=TRUE) %>% 
        group_by(Initials, Overall_date) %>% arrange(Initials, Overall_date, source) %>% slice(1) %>% ungroup()
    
    measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% ncol() %>% as.numeric()
    measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% apply(., 1, count_na)
    measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
    measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
    
    if (measure_name=="p_mfq_" | measure_name=="p_mfq1w_" | measure_name=="p_ari1w_" | measure_name=="p_ari6m_") {
      measure_temp_sdq[,9:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,9:ncol(measure_temp_sdq)], as.numeric)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% rowSums(na.rm=TRUE)
    } else {
      measure_temp_sdq[,7:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,7:ncol(measure_temp_sdq)], as.numeric)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% rowSums(na.rm=TRUE)
    }
    
    if (measure_name=="s_ari1w_") {measure_temp_sdq <- measure_temp_sdq %>% mutate(temptotal=(temptotal-s_ari1w_7_impairment))
    } else if (measure_name=="s_ari6m_") {measure_temp_sdq <- measure_temp_sdq %>% mutate(temptotal=(temptotal-s_ari6m_7_impairment))
    } else if (measure_name=="p_ari1w_") {measure_temp_sdq <- measure_temp_sdq %>% mutate(temptotal=(temptotal-p_ari1w_7_impairment))
    } else if (measure_name=="p_ari6m_") {measure_temp_sdq <- measure_temp_sdq %>% mutate(temptotal=(temptotal-p_ari6m_7_impairment))
    } else {measure_temp_sdq <- measure_temp_sdq}  
    
    measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(measure_name)) %>% complete.cases(.)
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"

    if (measure_name=="s_mfq1w_") {
      measure_temp_sdq[,7:19] <- sapply(measure_temp_sdq[,7:19], replace_na, '-9')
      measure_temp_sdq <- measure_temp_sdq %>% 
        filter(s_mfq1w_1_unhappy<4 & s_mfq1w_2_didnt_enjoy<4 & s_mfq1w_3_tired<4 & s_mfq1w_4_restless<4 &
                 s_mfq1w_5_no_good<4 & s_mfq1w_6_cried<4 & s_mfq1w_7_hard_think<4 & s_mfq1w_8_hate_myself<4 &
                 s_mfq1w_9_bad_person<4 & s_mfq1w_10_lonely<4 & s_mfq1w_11_nobody_love<4 & s_mfq1w_12_good_other_kid<4 &
                 s_mfq1w_13_all_wrong<4)
      measure_temp_sdq[,7:19] <- lapply(measure_temp_sdq[,7:19], na_if, '-9')
      measure_temp_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches(measure_name)) 
    } else if (measure_name=="p_mfq1w_") {
      measure_temp_sdq <- measure_temp_sdq
      measure_temp_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches(measure_name)) 
    } else {
      measure_temp_sdq <- measure_temp_sdq
      measure_temp_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches(measure_name)) %>% 
      rename(date_temp = ends_with("_date")) %>% rename(tempcomplete = ends_with("_complete")) %>% rename(temptotal = ends_with("_tot")) %>% 
        filter(!is.na(temptotal))
      }
    
    measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
    measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    
    measure_temp_combined <- merge.default(measure_temp_ctdb, measure_temp_sdq, all=TRUE) %>% 
      rename(measure_temp_source = source) %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, desc(temptotal), desc(measure_temp_source)) %>%
      slice(1) %>%
      ungroup()

    measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp_combined, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, matches(measure_name), date_temp, temptotal, measure_temp_source, tempcomplete)
    measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
    measure_temp_clinical <- measure_temp_clinical %>% 
      mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
      group_by(Initials, Clinical_Visit_Date) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(measurement_TDiff_abs<=60) %>% 
      select(-measurement_TDiff_abs)
    
    names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
    
    measure_temp_task <- merge.default(task_DB_date, measure_temp_combined, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, matches(measure_name), date_temp, temptotal, measure_temp_source, tempcomplete)
    measure_temp_task$measurement_TDiff <- as.numeric(difftime(measure_temp_task$Task_Date, measure_temp_task$date_temp, tz="", units = "days"))
    measure_temp_task <- measure_temp_task %>% 
      mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
      group_by(Initials, Task_Name, Task_Date) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      group_by(Initials, Task_Name, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, date_temp, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(measurement_TDiff_abs<=60) %>% 
      select(-measurement_TDiff_abs)
    
    names(measure_temp_task)[names(measure_temp_task) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_task)[names(measure_temp_task) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_task)[names(measure_temp_task) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_task)[names(measure_temp_task) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_task)[names(measure_temp_task) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    assign(paste0(measure_name, "subset_task"), measure_temp_task)
    
  }

#####
# unusual cases/those with subscales 
#####
# scared
    
  scared <- c('s_scared_', 'p_scared_')
  panic_subscale <- c("1_breathe|6_pass_out|9_look_nervous|12_feel_crazy|15_not_real|18_hr_fast|19_shaky|22_sweat|24_no_reason|27_choke|30_panic_attack|34_throw_up|38_dizzy")
  gad_subscale <- c("5_like_me|7_nervous|14_not_as_good|21_things_work_out|23_worry|28_worry_too_much|33_future|35_how_well_I_do|37_past")
  sep_subscale <- c("4_sleep_away_home|8_follow_parent|13_sleep_alone|16_nightmare_parents|20_nightmare|25_alone_at_home|29_away_from_family|31_worry_parents")
  social_subscale <- c("3_dislike_strangers|10_nervous_stranger|26_hard_to_talk|32_shy_strangers|39_others_watch_me|40_party|41_shy")
  school_subscale <- c("2_headache_school|11_stomachache_school|17_school|36_go_to_school")
  scared_subscales <- c('sep_subscale', 'panic_subscale', 'gad_subscale', 'social_subscale', 'school_subscale')
  
  for(i in seq_along(scared)) {
    iter <- as.numeric(i)
    # iter=2
    measure_name <- scared[iter]
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches(measure_name)) %>% 
      filter(!is.na(Overall_date)) %>% 
      distinct(., .keep_all = TRUE) 
    
    measure_temp_manual <- manual_db_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches(measure_name))
    measure_temp_sdq <- merge.default(measure_temp_sdq, measure_temp_manual, all=TRUE) %>% 
      group_by(Initials, Overall_date) %>% arrange(Initials, Overall_date, source) %>% slice(1) %>% ungroup()
    
    if (measure_name=="p_scared_") {
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq[,9:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,9:ncol(measure_temp_sdq)], as.numeric)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% rowSums(na.rm=TRUE)
      
      measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% complete.cases(.)
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"
      
    } else {
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(measure_name)) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(measure_name)) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq[,7:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,7:ncol(measure_temp_sdq)], as.numeric)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% rowSums(na.rm=TRUE)
      
      measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(measure_name)) %>% complete.cases(.)
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"
      
    }
    
    for(j in seq_along(scared_subscales)) {
    iter2 <- as.numeric(j)
      # iter2=5
      subscale_name <- scared_subscales[iter2]
      measure_temp_sdq$subscaletot <- measure_temp_sdq %>% select(matches(eval(parse(text=scared_subscales[iter2])))) %>% rowSums(na.rm=TRUE)
      names(measure_temp_sdq)[names(measure_temp_sdq) == "subscaletot"] <- (paste0(subscale_name, "_temp"))
    }

    measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
    measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    
    measure_temp_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches(measure_name)) %>% 
      rename(date_temp = ends_with("_date")) %>% rename(tempcomplete = ends_with("_complete")) %>% 
      rename(panic_subscale_temp = ends_with("panic_tot")) %>% rename(gad_subscale_temp = ends_with("gad_tot")) %>% 
      rename(sep_subscale_temp = ends_with("sep_tot")) %>% rename(social_subscale_temp = ends_with("social_tot")) %>% 
      rename(school_subscale_temp = ends_with("school_tot")) %>% rename(temptotal = ends_with("_tot")) %>% 
      filter(!is.na(temptotal))
    
    measure_temp_combined <- merge.default(measure_temp_ctdb, measure_temp_sdq, all=TRUE) %>% 
      rename(measure_temp_source = source) %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, desc(temptotal), desc(measure_temp_source)) %>%
      slice(1) %>% 
      ungroup()
    
    measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp_combined, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, matches(measure_name), date_temp, temptotal,
             panic_subscale_temp, sep_subscale_temp, social_subscale_temp, school_subscale_temp, gad_subscale_temp,
             measure_temp_source, tempcomplete)
    measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
    measure_temp_clinical <- measure_temp_clinical %>% 
      mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
      group_by(Initials, Clinical_Visit_Date) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(measurement_TDiff_abs<=60) %>% 
      select(-measurement_TDiff_abs)
  
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "panic_subscale_temp"] <- (paste0(measure_name, "panic_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "social_subscale_temp"] <- (paste0(measure_name, "social_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "sep_subscale_temp"] <- (paste0(measure_name, "sep_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "school_subscale_temp"] <- (paste0(measure_name, "school_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "gad_subscale_temp"] <- (paste0(measure_name, "gad_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
    
    measure_temp_task <- merge.default(task_DB_date, measure_temp_combined, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, matches(measure_name), date_temp, temptotal, 
             panic_subscale_temp, sep_subscale_temp, social_subscale_temp, school_subscale_temp, gad_subscale_temp,
             measure_temp_source, tempcomplete)
    measure_temp_task$measurement_TDiff <- as.numeric(difftime(measure_temp_task$Task_Date, measure_temp_task$date_temp, tz="", units = "days"))
    measure_temp_task <- measure_temp_task %>% 
      mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
      group_by(Initials, Task_Name, Task_Date) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      group_by(Initials, Task_Name, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, date_temp, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(measurement_TDiff_abs<=60) %>% 
      select(-measurement_TDiff_abs)
  
    names(measure_temp_task)[names(measure_temp_task) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_task)[names(measure_temp_task) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_task)[names(measure_temp_task) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_task)[names(measure_temp_task) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_task)[names(measure_temp_task) == "panic_subscale_temp"] <- (paste0(measure_name, "panic_tot"))
    names(measure_temp_task)[names(measure_temp_task) == "social_subscale_temp"] <- (paste0(measure_name, "social_tot"))
    names(measure_temp_task)[names(measure_temp_task) == "sep_subscale_temp"] <- (paste0(measure_name, "sep_tot"))
    names(measure_temp_task)[names(measure_temp_task) == "school_subscale_temp"] <- (paste0(measure_name, "school_tot"))
    names(measure_temp_task)[names(measure_temp_task) == "gad_subscale_temp"] <- (paste0(measure_name, "gad_tot"))
    names(measure_temp_task)[names(measure_temp_task) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    assign(paste0(measure_name, "subset_task"), measure_temp_task)
    
  }
  
#####
# lsas:
  
  s_lsas_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_lsas_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_lsas_manual <- manual_db_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_lsas_'))
  s_lsas_subset_sdq <- merge.default(s_lsas_subset_sdq, s_lsas_manual, all=TRUE)
    
  s_lsas_subset_sdq[,7:ncol(s_lsas_subset_sdq)] <- sapply(s_lsas_subset_sdq[,7:ncol(s_lsas_subset_sdq)], as.numeric) 
  
  s_lsas_subset_sdq$no_columns <- s_lsas_subset_sdq %>% select(matches('s_lsas_')) %>% ncol() %>% as.numeric()
  s_lsas_subset_sdq$NA_count <- s_lsas_subset_sdq %>% select(matches('s_lsas_')) %>% apply(., 1, count_na)
  s_lsas_subset_sdq$diff <- c(s_lsas_subset_sdq$no_columns - s_lsas_subset_sdq$NA_count)
  s_lsas_subset_sdq <- s_lsas_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_lsas_subset_sdq$s_lsas_tot <- s_lsas_subset_sdq %>% select(matches('s_lsas_')) %>% rowSums(na.rm=TRUE)
  
  s_lsas_subset_sdq$s_lsas_fear_tot <- s_lsas_subset_sdq %>% select(matches('_fear_')) %>% rowSums(na.rm=TRUE)
  s_lsas_subset_sdq$s_lsas_avoid_tot <- s_lsas_subset_sdq %>% select(matches('_avoid_')) %>% rowSums(na.rm=TRUE)
  lsas_performance <- c("_1_|_2_|_3_|_4_|_6_|_8_|_9_|_13_|_14_|_16_|_17_|_20_|_21_")
  s_lsas_subset_sdq$s_lsas_perform_tot <- s_lsas_subset_sdq %>% select(matches(lsas_performance)) %>% rowSums(na.rm=TRUE)
  lsas_social <- c("_5_|_7_|_10_|_11_|_12_|_15_|_18_|_19_|_22_|_23_|_24_")
  s_lsas_subset_sdq$s_lsas_social_tot <- s_lsas_subset_sdq %>% select(matches(lsas_social)) %>% rowSums(na.rm=TRUE)
  
  s_lsas_subset_sdq$s_lsas_complete <- s_lsas_subset_sdq %>% select(matches('s_lsas_')) %>% complete.cases(.)
  s_lsas_subset_sdq$s_lsas_complete[s_lsas_subset_sdq$s_lsas_complete=="FALSE"] <- "0"
  s_lsas_subset_sdq$s_lsas_complete[s_lsas_subset_sdq$s_lsas_complete=="TRUE"] <- "1"

  s_lsas_subset_sdq$s_lsas_date <- s_lsas_subset_sdq$Overall_date
  s_lsas_subset_sdq <- s_lsas_subset_sdq %>% select(-Overall_date)
  
  s_lsas_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches('s_lsas_')) %>% 
    filter(!is.na(s_lsas_date))
  
  s_lsas_subset <- merge.default(s_lsas_subset_ctdb, s_lsas_subset_sdq, all=TRUE) %>% 
    rename(s_lsas_source = source) %>% 
    group_by(Initials, s_lsas_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_lsas_date, desc(s_lsas_tot), desc(s_lsas_source)) %>%
    slice(1) %>% 
    ungroup()
  
  s_lsas_subset_clinical <- merge.default(clinical_DB_date, s_lsas_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_lsas_source, matches('s_lsas_'))
  s_lsas_subset_clinical$s_lsas_TDiff <- as.numeric(difftime(s_lsas_subset_clinical$Clinical_Visit_Date, s_lsas_subset_clinical$s_lsas_date, tz="", units = "days"))
  s_lsas_subset_clinical <- s_lsas_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_lsas_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_lsas_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_lsas_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  s_lsas_subset_task <- merge.default(task_DB_date, s_lsas_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, s_lsas_source, matches('s_lsas_'))
  s_lsas_subset_task$s_lsas_TDiff <- as.numeric(difftime(s_lsas_subset_task$Task_Date, s_lsas_subset_task$s_lsas_date, tz="", units = "days"))
  s_lsas_subset_task <- s_lsas_subset_task %>% 
    mutate(measurement_TDiff_abs=abs(s_lsas_TDiff)) %>% 
    group_by(Initials, Task_Name, Task_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, Task_Name, s_lsas_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, s_lsas_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
#####
# shaps
  
  s_shaps_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_shaps_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_shaps_manual <- manual_db_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_shaps_'))
  s_shaps_subset_sdq <- merge.default(s_shaps_subset_sdq, s_shaps_manual, all=TRUE)
  
  s_shaps_subset_sdq[,7:20] <- sapply(s_shaps_subset_sdq[,7:20], as.numeric) 
  
  s_shaps_subset_sdq$no_columns <- s_shaps_subset_sdq %>% select(matches('s_shaps_')) %>% ncol() %>% as.numeric()
  s_shaps_subset_sdq$NA_count <- s_shaps_subset_sdq %>% select(matches('s_shaps_')) %>% apply(., 1, count_na)
  s_shaps_subset_sdq$diff <- c(s_shaps_subset_sdq$no_columns - s_shaps_subset_sdq$NA_count)
  s_shaps_subset_sdq <- s_shaps_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  # recoding SHAPS scores from SDQ that were obtained before June 22nd 2018: coding went from 'strongly disagree'=0 - 'strongly agree'=3 -> 'strongly disagree'=3 - 'strongly agree'=0
  s_shaps_subset_sdq$date_temp <- s_shaps_subset_sdq$Overall_date
  s_shaps_subset_sdq$date_temp_diff <- as.numeric(difftime(as.Date("2018-06-22"), s_shaps_subset_sdq$date_temp, tz="", units = "days"))
  temp_before <- s_shaps_subset_sdq %>% filter(date_temp_diff>-1 & source=="SDQ") %>% select(-date_temp, -date_temp_diff)
  s_shaps_subset_sdq <- s_shaps_subset_sdq %>% filter(date_temp_diff<0 | source=="MANUAL") %>% select(-date_temp, -date_temp_diff)
  temp_before[,7:ncol(temp_before)]  <- lapply(temp_before[,7:ncol(temp_before)], FUN = function(x) recode(x, `3`=5, `2`=6, `1`=2, `0`=3, .missing = NULL))
  temp_before[,7:ncol(temp_before)]  <- lapply(temp_before[,7:ncol(temp_before)], FUN = function(x) recode(x, `5`=0, `6`=1, `2`=2, `3`=3, .missing = NULL))
  s_shaps_subset_sdq <- merge.default(s_shaps_subset_sdq, temp_before, all=TRUE)

  s_shaps_binary <- s_shaps_subset_sdq
  s_shaps_binary[,7:20]  <- lapply(s_shaps_binary[,7:20], FUN = function(x) recode(x, `0`=0, `1`=0, `2`=1, `3`=1, .missing = NULL))
  s_shaps_binary$s_shaps_binary_tot <- s_shaps_binary %>% select(matches('s_shaps_')) %>% rowSums(na.rm=TRUE) 
  s_shaps_binary <- s_shaps_binary %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, s_shaps_binary_tot)
  
  s_shaps_subset_sdq$s_shaps_tot <- s_shaps_subset_sdq %>% select(matches('s_shaps_')) %>% rowSums(na.rm=TRUE)
  
  s_shaps_subset_sdq <- left_join(s_shaps_subset_sdq, s_shaps_binary, all=TRUE)
  
  s_shaps_subset_sdq$s_shaps_complete <- s_shaps_subset_sdq %>% select(matches('s_shaps_')) %>% complete.cases(.)
  s_shaps_subset_sdq$s_shaps_complete[s_shaps_subset_sdq$s_shaps_complete=="FALSE"] <- "0"
  s_shaps_subset_sdq$s_shaps_complete[s_shaps_subset_sdq$s_shaps_complete=="TRUE"] <- "1"

  s_shaps_subset_sdq$s_shaps_date <- s_shaps_subset_sdq$Overall_date
  s_shaps_subset_sdq <- s_shaps_subset_sdq %>% select(-Overall_date)
  
  s_shaps_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches('s_shaps_')) %>% 
    filter(!is.na(s_shaps_date))
  s_shaps_subset_ctdb[,7:20]  <- lapply(s_shaps_subset_ctdb[,7:20], FUN = function(x) recode(x, `1`=0, `2`=1, `3`=2, `4`=3, .missing = NULL))
  
  s_shaps_subset <- merge.default(s_shaps_subset_ctdb, s_shaps_subset_sdq, all=TRUE) %>% 
    rename(s_shaps_source = source) %>% 
    group_by(Initials, s_shaps_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_shaps_date, desc(s_shaps_tot), desc(s_shaps_source)) %>%
    slice(1) %>% 
    ungroup()
  
  s_shaps_subset_clinical <- merge.default(clinical_DB_date, s_shaps_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_shaps_source, matches('s_shaps_'))
  s_shaps_subset_clinical$s_shaps_TDiff <- as.numeric(difftime(s_shaps_subset_clinical$Clinical_Visit_Date, s_shaps_subset_clinical$s_shaps_date, tz="", units = "days"))
  
  s_shaps_subset_clinical <- s_shaps_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_shaps_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_shaps_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_shaps_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  s_shaps_subset_task <- merge.default(task_DB_date, s_shaps_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, s_shaps_source, matches('s_shaps_'))
  s_shaps_subset_task$s_shaps_TDiff <- as.numeric(difftime(s_shaps_subset_task$Task_Date, s_shaps_subset_task$s_shaps_date, tz="", units = "days"))
  
  s_shaps_subset_task <- s_shaps_subset_task %>% 
    mutate(measurement_TDiff_abs=abs(s_shaps_TDiff)) %>% 
    group_by(Initials, Task_Name, Task_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, Task_Name, s_shaps_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, s_shaps_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
# Diagnosis ---------------------------------------------------------------

  fix_var <- c("c_ksadsdx_clin_name", "c_ksadsdx_visit_type",  "c_ksadsdx_eligibility",  "c_ksadsdx_eligibility_other", "c_ksadsdx_primary_dx",  "c_ksadsdx_primary_dx_other",  
               "c_ksadsdx_dx_detailed",  "c_ksadsdx_dx_detailed_descrip", "c_ksadsdx_treatment_notes",  "c_ksadsdx_notes_overall", "c_ksadsdx_epset_current_mdd", 
               "c_ksadsdx_epset_current_submdd", "c_ksadsdx_epset_current_mddsympt", "c_ksadsdx_epset_current_mania", "c_ksadsdx_epset_current_hypomania", "c_ksadsdx_comorbid_dx_old", 
               "c_ksadsdx_ongoing_other_comorbid_dx", "c_ksadsdx_how_interviewed")
  
  diagnosis_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('c_ksadsdx')) 
  
  diagnosis_manual <- manual_db_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('c_ksadsdx'))
  diagnosis_subset_sdq <- merge.default(diagnosis_subset_sdq, diagnosis_manual, all=TRUE)
  
  diagnosis_subset_sdq[fix_var] <- lapply(diagnosis_subset_sdq[fix_var], as.character)
  diagnosis_subset_sdq[fix_var] <- lapply(diagnosis_subset_sdq[fix_var], na_if, '')

  diagnosis_subset_sdq$c_ksadsdx_date <- as.Date(diagnosis_subset_sdq$c_ksadsdx_date)
  diagnosis_subset_sdq$c_ksadsdx_date <- coalesce(diagnosis_subset_sdq$c_ksadsdx_date, diagnosis_subset_sdq$Overall_date) 

  diagnosis_subset_sdq$no_columns <- diagnosis_subset_sdq %>% select(c_ksadsdx_eligibility, c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed, c_ksadsdx_comorbid_dx_old, c_ksadsdx_ongoing_other_comorbid_dx) %>% 
    ncol() %>% as.numeric()
  diagnosis_subset_sdq$NA_count <- diagnosis_subset_sdq %>% select(c_ksadsdx_eligibility, c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed, c_ksadsdx_comorbid_dx_old, c_ksadsdx_ongoing_other_comorbid_dx) %>% 
    apply(., 1, count_na)
  diagnosis_subset_sdq$diff <- c(diagnosis_subset_sdq$no_columns - diagnosis_subset_sdq$NA_count)
  diagnosis_subset_sdq <- diagnosis_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  diagnosis_subset_sdq <- diagnosis_subset_sdq %>% group_by(Initials, c_ksadsdx_date) %>% arrange(Initials, c_ksadsdx_date, source) %>%
    slice(1) %>% ungroup()%>% select(-Overall_date)
  
  diagnosis_subset_sdq[fix_var[2]] <- lapply(diagnosis_subset_sdq[fix_var[2]], FUN = function(x) recode(x, "0"="baseline", "1"="12 month FU", "2"="24 month FU", "9"="Inpatient Treatment", "10"="Outpatient Treatment", .missing = NULL))
  diagnosis_subset_sdq[fix_var[3]] <- lapply(diagnosis_subset_sdq[fix_var[3]], FUN = function(x) recode(x, "0"="Include", "1"="Include: can't scan", "5"="Excluded: does not meet criteria", "6"="Excluded: meets exclusionary criteria", "777"="Excluded: withdrew", .missing = NULL))
  diagnosis_subset_sdq[fix_var[5]] <- lapply(diagnosis_subset_sdq[fix_var[5]], FUN = function(x) recode(x, "1"="MDD", "2"="Sub-MDD", "3"="Healthy", "4"="Anxious", "5"="DMDD", "6"="ADHD", "777"="EXCLUDED", .missing = NULL))
  diagnosis_subset_sdq[fix_var[7]] <- lapply(diagnosis_subset_sdq[fix_var[7]], FUN = function(x) recode(x, "1"="Full_MDD", "2"="Sub_remit", "3"="Sub_hist", "4"="Sub_never", "5"="Remit_MDD", "6"="Hist_MDD", "7"="Remit_Sub", "8"="Hist_Sub", "9"="HV", "10"="Anxious", "11"="DMDD", "12"="Sub_DMDD", "13"="ADHD", "14"="Bipolar_I", "15"="Bipolar_II", "777"="EXCLUDED", .missing = NULL))
  diagnosis_subset_sdq[fix_var[11]] <- lapply(diagnosis_subset_sdq[fix_var[11]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[12]] <- lapply(diagnosis_subset_sdq[fix_var[12]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[13]] <- lapply(diagnosis_subset_sdq[fix_var[13]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[14]] <- lapply(diagnosis_subset_sdq[fix_var[14]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[15]] <- lapply(diagnosis_subset_sdq[fix_var[15]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[18]] <- lapply(diagnosis_subset_sdq[fix_var[18]], FUN = function(x) recode(x, "0"="face-to-face assessment", "1"="phone assessment", .missing = "face-to-face assessment"))
  
  fill_names <- diagnosis_subset_sdq %>% select(-Initials, -c_ksadsdx_date) %>% colnames()
  diagnosis_subset_sdq <- diagnosis_subset_sdq %>% group_by(Initials, c_ksadsdx_date) %>% fill(., names(fill_names), .direction = c("down")) %>%
    fill(., names(fill_names), .direction = c("up")) %>% ungroup() %>% distinct(., .keep_all = TRUE)
  
  # collapsing comorbid diagnoses into one variable 
  comorbid <- diagnosis_subset_sdq %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, c_ksadsdx_date, matches("_ongoing_"), c_ksadsdx_comorbid_dx_old) %>% 
    select(-c_ksadsdx_ongoing_other, -c_ksadsdx_ongoing_specific_phob_descrip)
  
  comorbid$c_ksadsdx_comorbid_dx_old <- gsub("[", "", comorbid$c_ksadsdx_comorbid_dx_old, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_old <- gsub("]", "", comorbid$c_ksadsdx_comorbid_dx_old, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_old <- gsub("'", "", comorbid$c_ksadsdx_comorbid_dx_old, fixed=TRUE)

  comorbid$c_ksadsdx_ongoing_mdd <- recode(comorbid$c_ksadsdx_ongoing_mdd, "1"="MDD", "0"="None", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_submdd <- recode(comorbid$c_ksadsdx_ongoing_submdd, "1"="Sub-MDD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_mania <- recode(comorbid$c_ksadsdx_ongoing_mania, "1"="Mania", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_hypomania <- recode(comorbid$c_ksadsdx_ongoing_hypomania, "1"="Hypomania", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_psychosis <- recode(comorbid$c_ksadsdx_ongoing_psychosis, "1"="Psychosis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_panic <- recode(comorbid$c_ksadsdx_ongoing_panic, "1"="Panic", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_sep_anx <- recode(comorbid$c_ksadsdx_ongoing_sep_anx, "1"="SepAnx", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_social <- recode(comorbid$c_ksadsdx_ongoing_social, "1"="SocPhobia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_specific <- recode(comorbid$c_ksadsdx_ongoing_specific, "1"="SpecificPhobia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_gad <- recode(comorbid$c_ksadsdx_ongoing_gad, "1"="GAD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_enuresis <- recode(comorbid$c_ksadsdx_ongoing_enuresis , "1"="Enuresis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_encopresis <- recode(comorbid$c_ksadsdx_ongoing_encopresis, "1"="Encopresis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_anorexia <- recode(comorbid$c_ksadsdx_ongoing_anorexia, "1"="Anorexia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_adhd<- recode(comorbid$c_ksadsdx_ongoing_adhd, "1"="ADHD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_odd <- recode(comorbid$c_ksadsdx_ongoing_odd, "1"="ODD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_cd <- recode(comorbid$c_ksadsdx_ongoing_cd, "1"="CD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_tic <- recode(comorbid$c_ksadsdx_ongoing_tic, "1"="Tic", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_cigarette <- recode(comorbid$c_ksadsdx_ongoing_cigarette, "1"="Nicotine", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_alcohol <- recode(comorbid$c_ksadsdx_ongoing_alcohol, "1"="Alcohol", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_sa <- recode(comorbid$c_ksadsdx_ongoing_sa, "1"="SA", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_ptsd <- recode(comorbid$c_ksadsdx_ongoing_ptsd, "1"="PTSD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_bdd <- recode(comorbid$c_ksadsdx_ongoing_bdd, "1"="BDD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_ocd <- recode(comorbid$c_ksadsdx_ongoing_ocd, "1"="OCD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_ongoing_eat <- recode(comorbid$c_ksadsdx_ongoing_eat, "1"="Eating", "0"=" ", .missing = NULL)

  comorbid$c_ksadsdx_ongoing_comorbid_combined <- paste(comorbid$c_ksadsdx_ongoing_mdd, comorbid$c_ksadsdx_ongoing_submdd, comorbid$c_ksadsdx_ongoing_mania, comorbid$c_ksadsdx_ongoing_hypomania, 
                         comorbid$c_ksadsdx_ongoing_psychosis, comorbid$c_ksadsdx_ongoing_panic, comorbid$c_ksadsdx_ongoing_sep_anx, comorbid$c_ksadsdx_ongoing_social, 
                         comorbid$c_ksadsdx_ongoing_specific, comorbid$c_ksadsdx_ongoing_gad, comorbid$c_ksadsdx_ongoing_enuresis, comorbid$c_ksadsdx_ongoing_encopresis, 
                         comorbid$c_ksadsdx_ongoing_anorexia, comorbid$c_ksadsdx_ongoing_adhd, comorbid$c_ksadsdx_ongoing_odd, comorbid$c_ksadsdx_ongoing_cd, 
                         comorbid$c_ksadsdx_ongoing_tic, comorbid$c_ksadsdx_ongoing_cigarette, comorbid$c_ksadsdx_ongoing_alcohol, comorbid$c_ksadsdx_ongoing_sa, 
                         comorbid$c_ksadsdx_ongoing_ptsd, comorbid$c_ksadsdx_ongoing_bdd, comorbid$c_ksadsdx_ongoing_ocd, comorbid$c_ksadsdx_ongoing_eat, sep=", ")

  for(i in seq_along(1:24)) {
    comorbid$c_ksadsdx_ongoing_comorbid_combined <- gsub("NA, ", "", comorbid$c_ksadsdx_ongoing_comorbid_combined, fixed=TRUE)
    comorbid$c_ksadsdx_ongoing_comorbid_combined <- gsub(" , ", "", comorbid$c_ksadsdx_ongoing_comorbid_combined, fixed=TRUE)
  }
  
  comorbid$c_ksadsdx_ongoing_comorbid_combined <- na_if(comorbid$c_ksadsdx_ongoing_comorbid_combined, "NA")
  comorbid$c_ksadsdx_ongoing_comorbid_combined <- gsub('.{3}$', '', comorbid$c_ksadsdx_ongoing_comorbid_combined)
  comorbid$c_ksadsdx_ongoing_comorbid_combined <- gsub('None, ', '', comorbid$c_ksadsdx_ongoing_comorbid_combined)
  comorbid$c_ksadsdx_comorbid_dx_combined <- paste("From KSADS DX checklist: 1) meeting full ongoing criteria: ", comorbid$c_ksadsdx_ongoing_comorbid_combined, "; 2) Other: ", comorbid$c_ksadsdx_ongoing_other_comorbid_dx, 
                          ". From old MDD form: ", comorbid$c_ksadsdx_comorbid_dx_old, sep="")
  
  comorbid$c_ksadsdx_comorbid_dx_combined <- gsub("From KSADS DX checklist: 1) meeting full ongoing criteria: NA; 2) Other: NA. From old MDD form: None", "None", comorbid$c_ksadsdx_comorbid_dx_combined, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_combined <- gsub("From KSADS DX checklist: 1) meeting full ongoing criteria: None; 2) Other: NA. From old MDD form: NA", "None", comorbid$c_ksadsdx_comorbid_dx_combined, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_combined <- na_if(comorbid$c_ksadsdx_comorbid_dx_combined, "From KSADS DX checklist: 1) meeting full ongoing criteria: NA; 2) Other: NA. From old MDD form: NA")
  
  comorbid$c_ksadsdx_comorbid_dx_combined <- gsub("From KSADS DX checklist: 1) meeting full ongoing criteria: NA; 2) Other: NA. ", "", comorbid$c_ksadsdx_comorbid_dx_combined, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_combined <- gsub(": 1) meeting full ongoing criteria: NA; 2) Other:", " under 'other comorbid disorders':", comorbid$c_ksadsdx_comorbid_dx_combined, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_combined <- gsub(" From old MDD form: NA", "", comorbid$c_ksadsdx_comorbid_dx_combined, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_combined <- gsub("; 2) Other: NA.", "", comorbid$c_ksadsdx_comorbid_dx_combined, fixed=TRUE)
  
  comorbid <- comorbid %>% select(PLUSID, Initials, c_ksadsdx_date, c_ksadsdx_comorbid_dx_combined, c_ksadsdx_ongoing_comorbid_combined, c_ksadsdx_comorbid_dx_old) %>% 
    group_by(Initials, c_ksadsdx_date) %>% slice(1) %>% ungroup()
  diagnosis_subset_sdq <- merge.default(diagnosis_subset_sdq, comorbid, all=TRUE) %>% group_by(PLUSID, c_ksadsdx_date) %>% slice(1) %>% ungroup()

  diagnosis_subset_task <- merge.default(task_DB_date, diagnosis_subset_sdq, all=TRUE) %>% 
    rename(c_ksadsdx_source = source) %>% 
    group_by(Initials, c_ksadsdx_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_ksadsdx_date) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, c_ksadsdx_date, matches("c_ksadsdx_"))
  
  diagnosis_subset_task$c_ksadsdx_TDiff <- as.numeric(difftime(diagnosis_subset_task$Task_Date, diagnosis_subset_task$c_ksadsdx_date, tz="", units = "days"))
  diagnosis_subset_task <- diagnosis_subset_task %>% 
    mutate(measurement_TDiff_abs=abs(c_ksadsdx_TDiff)) %>% 
    group_by(Initials, Task_Name, Task_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, Task_Name, c_ksadsdx_date) %>%
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, c_ksadsdx_date, measurement_TDiff_abs) %>%
    slice(1) %>%
    ungroup() %>%
    # filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  diagnosis_subset_clinical <- merge.default(clinical_DB_date, diagnosis_subset_sdq, all=TRUE) %>% 
    rename(c_ksadsdx_source = source) %>% 
    group_by(Initials, c_ksadsdx_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_ksadsdx_date) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_ksadsdx_date, matches('c_ksadsdx_'))
  
  diagnosis_subset_clinical$c_ksadsdx_TDiff <- as.numeric(difftime(diagnosis_subset_clinical$Clinical_Visit_Date, diagnosis_subset_clinical$c_ksadsdx_date, tz="", units = "days"))
  diagnosis_subset_clinical <- diagnosis_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(c_ksadsdx_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, c_ksadsdx_date) %>%
    arrange(FIRST_NAME, LAST_NAME, Initials, c_ksadsdx_date, measurement_TDiff_abs) %>%
    slice(1) %>%
    ungroup() %>%
    select(-measurement_TDiff_abs)

# IQ, handedness, tanner -------------------------------------------
  
#####
# IQ:
  
  c_wasi_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('c_wasi_')) %>% 
    distinct(., .keep_all = TRUE)
  
  c_wasi_subset_sdq[,7:9] <- sapply(c_wasi_subset_sdq[,7:9], as.character) 
  
  c_wasi_subset_sdq$no_columns <- c_wasi_subset_sdq %>% select(matches('c_wasi_')) %>% ncol() %>% as.numeric()
  c_wasi_subset_sdq$NA_count <- c_wasi_subset_sdq %>% select(matches('c_wasi_')) %>% apply(., 1, count_na)
  c_wasi_subset_sdq$diff <- c(c_wasi_subset_sdq$no_columns - c_wasi_subset_sdq$NA_count)
  c_wasi_subset_sdq <- c_wasi_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  c_wasi_subset_sdq$c_wasi_date <- c_wasi_subset_sdq$Overall_date
  c_wasi_subset_sdq <- c_wasi_subset_sdq %>% select(-Overall_date)
  
  c_wasi_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches('c_wasi_')) %>% 
    filter(!is.na(c_wasi_date))

  c_wasi_subset <- merge.default(c_wasi_subset_ctdb, c_wasi_subset_sdq, all=TRUE) %>% 
    rename(c_wasi_source = source) %>% 
    group_by(FIRST_NAME, LAST_NAME) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, desc(c_wasi_date), desc(c_wasi_iq), desc(c_wasi_source)) %>%
    slice(1) %>% 
    ungroup()
  
  c_wasi_subset_clinical <- merge.default(clinical_DB_date, c_wasi_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_wasi_source, c_wasi_date, matches('c_wasi_'))
  
  c_wasi_subset_task <- merge.default(task_DB_date, c_wasi_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, c_wasi_source, c_wasi_date, matches('c_wasi_'))
  
#####
# TANNER: 
  
  s_tanner_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_tanner_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_tanner_subset_sdq[,7:10] <- sapply(s_tanner_subset_sdq[,7:10], as.character) 
  
  s_tanner_subset_sdq$no_columns <- s_tanner_subset_sdq %>% select(matches('s_tanner_')) %>% ncol() %>% as.numeric()
  s_tanner_subset_sdq$NA_count <- s_tanner_subset_sdq %>% select(matches('s_tanner_')) %>% apply(., 1, count_na)
  s_tanner_subset_sdq$diff <- c(s_tanner_subset_sdq$no_columns - s_tanner_subset_sdq$NA_count)
  s_tanner_subset_sdq <- s_tanner_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_tanner_subset_sdq$s_tanner_date <- s_tanner_subset_sdq$Overall_date
  s_tanner_subset_sdq <- s_tanner_subset_sdq %>% select(-Overall_date)
  
  s_tanner_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches('s_tanner_')) %>% 
    filter(!is.na(s_tanner_date))
  
  s_tanner_subset <- merge.default(s_tanner_subset_ctdb, s_tanner_subset_sdq, all=TRUE) %>% 
    rename(s_tanner_source = source) %>% 
    group_by(Initials, s_tanner_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_tanner_date, desc(s_tanner_source)) %>%
    slice(1) %>% 
    ungroup()
  
  s_tanner_subset_clinical <- merge.default(clinical_DB_date, s_tanner_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_tanner_source, s_tanner_date, matches('s_tanner_'))
  s_tanner_subset_clinical$s_tanner_TDiff <- as.numeric(difftime(s_tanner_subset_clinical$Clinical_Visit_Date, s_tanner_subset_clinical$s_tanner_date, tz="", units = "days"))
  
  s_tanner_subset_clinical <- s_tanner_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_tanner_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    select(-measurement_TDiff_abs)
  
  s_tanner_subset_task <- merge.default(task_DB_date, s_tanner_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, s_tanner_source, s_tanner_date, matches('s_tanner_'))
  s_tanner_subset_task$s_tanner_TDiff <- as.numeric(difftime(s_tanner_subset_task$Task_Date, s_tanner_subset_task$s_tanner_date, tz="", units = "days"))
  
  s_tanner_subset_task <- s_tanner_subset_task %>% 
    mutate(measurement_TDiff_abs=abs(s_tanner_TDiff)) %>% 
    group_by(Initials, Task_Name, Task_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    select(-measurement_TDiff_abs)
  
#####
# Handedness - needs date 
  
  s_handedness_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_handedness_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_handedness_subset_sdq[,7:ncol(s_handedness_subset_sdq)] <- sapply(s_handedness_subset_sdq[,7:ncol(s_handedness_subset_sdq)], as.numeric) 
  
  s_handedness_subset_sdq$no_columns <- s_handedness_subset_sdq %>% select(matches('s_handedness_')) %>% ncol() %>% as.numeric()
  s_handedness_subset_sdq$NA_count <- s_handedness_subset_sdq %>% select(matches('s_handedness_')) %>% apply(., 1, count_na)
  s_handedness_subset_sdq$diff <- c(s_handedness_subset_sdq$no_columns - s_handedness_subset_sdq$NA_count)
  s_handedness_subset_sdq <- s_handedness_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  hand_columns <- s_handedness_subset_sdq %>% select(matches('s_handedness_')) %>% colnames(.)
  
  for(j in seq_along(hand_columns)) {
    iter2 <- as.numeric(j)
    # iter2=3
    hand_column_name <- as.character(hand_columns[iter2])
    
    # loop is recoding columns to the following: 
      # Always Left =0 = left + 2
      # Mostly Left =1 = left + 1 
      # Both Left and Right =2 = both + 1
      # Mostly Right =3 = right + 1
      # Always Right =4 = right + 2
    
    s_handedness_subset_sdq <- s_handedness_subset_sdq %>% 
      mutate(temp_left = ifelse(((!!sym(hand_column_name))==0), 2, 
                                ifelse(((!!sym(hand_column_name))==1), 1, 
                                       ifelse(((!!sym(hand_column_name))==2), 1, 0))))
    s_handedness_subset_sdq <- s_handedness_subset_sdq %>% 
      mutate(temp_right = ifelse(((!!sym(hand_column_name))==4), 2, 
                                ifelse(((!!sym(hand_column_name))==3), 1, 
                                       ifelse(((!!sym(hand_column_name))==2), 1, 0))))
    
    names(s_handedness_subset_sdq)[names(s_handedness_subset_sdq) == "temp_left"] <- (paste0("temp_left",iter2))
    names(s_handedness_subset_sdq)[names(s_handedness_subset_sdq) == "temp_right"] <- (paste0("temp_right",iter2))
  }
  
  s_handedness_subset_sdq$temp_left_tot <- s_handedness_subset_sdq %>% select(matches("temp_left")) %>% rowSums(na.rm=TRUE)
  s_handedness_subset_sdq$temp_right_tot <- s_handedness_subset_sdq %>% select(matches("temp_right")) %>% rowSums(na.rm=TRUE)
  s_handedness_subset_sdq <- s_handedness_subset_sdq %>% mutate(s_handedness_ehi_tot = 
        100*((temp_right_tot - temp_left_tot)/(temp_right_tot+temp_left_tot)))
  s_handedness_subset_sdq$s_handedness_ehi_tot <- round(s_handedness_subset_sdq$s_handedness_ehi_tot, digits=1)
  
  s_handedness_subset_sdq$s_handedness_ehi_complete <- s_handedness_subset_sdq %>% select(matches("s_handedness_")) %>% complete.cases(.)
  s_handedness_subset_sdq$s_handedness_ehi_complete[s_handedness_subset_sdq$s_handedness_ehi_complete=="FALSE"] <- "0"
  s_handedness_subset_sdq$s_handedness_ehi_complete[s_handedness_subset_sdq$s_handedness_ehi_complete=="TRUE"] <- "1"

  s_handedness_subset_sdq$s_handedness_ehi_date <- s_handedness_subset_sdq$Overall_date
  s_handedness_subset_sdq <- s_handedness_subset_sdq %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, s_handedness_ehi_tot, s_handedness_ehi_date, s_handedness_ehi_complete) %>% 
    distinct(., .keep_all = TRUE)
  
  s_handedness_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches('s_handedness')) %>% 
    filter(!is.na(s_handedness_ehi_date) | !is.na(s_handedness1_date))
  
  s_handedness_subset_ctdb$s_handedness_ehi_date <- coalesce(s_handedness_subset_ctdb$s_handedness_ehi_date, s_handedness_subset_ctdb$s_handedness1_date)
  s_handedness_subset_ctdb$s_handedness_ehi_complete <- coalesce(s_handedness_subset_ctdb$s_handedness_ehi_complete, s_handedness_subset_ctdb$s_handedness1_complete)
  s_handedness_subset_ctdb$s_handedness_ehi_tot <- coalesce(s_handedness_subset_ctdb$s_handedness_ehi_tot, s_handedness_subset_ctdb$s_handedness1_tot)
  s_handedness_subset_ctdb <- s_handedness_subset_ctdb %>% select(-s_handedness1_date, -s_handedness1_complete, -s_handedness1_tot) %>% 
    distinct(., .keep_all = TRUE)
  
  s_handedness_subset <- merge.default(s_handedness_subset_ctdb, s_handedness_subset_sdq, all=TRUE) %>% 
    rename(s_handedness_ehi_source = source) %>% 
    group_by(FIRST_NAME, LAST_NAME) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, desc(s_handedness_ehi_date), desc(s_handedness_ehi_tot), desc(s_handedness_ehi_source)) %>%
    slice(1) %>%
    ungroup()
  s_handedness_subset$s_handedness_ehi_tot <- as.numeric(s_handedness_subset$s_handedness_ehi_tot)
  
  s_handedness_subset <- s_handedness_subset %>% 
    mutate(s_handedness_ehi = ifelse(s_handedness_ehi_tot < -40, "LEFT", ifelse(s_handedness_ehi_tot >= 40, "RIGHT", "AMBIDEXTROUS")))
  
  s_handedness_subset_clinical <- merge.default(clinical_DB_date, s_handedness_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_handedness_ehi_source, s_handedness_ehi_date, matches('s_handedness_'))
  
  s_handedness_subset_task <- merge.default(task_DB_date, s_handedness_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, s_handedness_ehi_source, s_handedness_ehi_date, matches('s_handedness_'))
  
# Medication --------------------------------------------------------------
  
  ###
  
  c_medsclin_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Overall_date, source, c_medsclin_date, 
                                              c_medsclin_clinician_name, c_medsclin_person_completing:c_medsclin_othernotes) %>% 
    filter(!is.na(Overall_date)) %>% 
    distinct(., .keep_all = TRUE)
  
  c_medsclin_sdq$c_medsclin_date <- as.Date(c_medsclin_sdq$c_medsclin_date)
  c_medsclin_sdq$c_medsclin_date <- coalesce(c_medsclin_sdq$c_medsclin_date, c_medsclin_sdq$Overall_date) 
  c_medsclin_sdq <- c_medsclin_sdq %>% select(-Overall_date)

  c_medsclin_sdq[,7:ncol(c_medsclin_sdq)] <- sapply(c_medsclin_sdq[,7:ncol(c_medsclin_sdq)], as.character)
  c_medsclin_sdq[,7:ncol(c_medsclin_sdq)] <- sapply(c_medsclin_sdq[,7:ncol(c_medsclin_sdq)], na_if, 999)
  c_medsclin_sdq[,7:ncol(c_medsclin_sdq)] <- sapply(c_medsclin_sdq[,7:ncol(c_medsclin_sdq)], na_if, "") 

  c_medsclin_sdq$no_columns <- c_medsclin_sdq %>% select(c_medsclin_treatment_changes, matches("med1name")) %>% ncol() %>% as.numeric()
  c_medsclin_sdq$NA_count <- c_medsclin_sdq %>% select(c_medsclin_treatment_changes, matches("med1name")) %>% apply(., 1, count_na)
  c_medsclin_sdq$diff <- c(c_medsclin_sdq$no_columns - c_medsclin_sdq$NA_count)
  c_medsclin_sdq <- c_medsclin_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  ###

  c_medsclin1yr_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Overall_date, source, c_medsclin1yr_date, c_medsclin1yr_baseline_date,
                                           c_medsclin1yr_clinician_name, c_medsclin1yr_02_med1name:c_medsclin1yr_othernotes) %>% 
    filter(!is.na(Overall_date)) %>% 
    distinct(., .keep_all = TRUE)
  
  c_medsclin1yr_sdq$c_medsclin1yr_date <- as.Date(c_medsclin1yr_sdq$c_medsclin1yr_date)
  c_medsclin1yr_sdq$c_medsclin1yr_date <- coalesce(c_medsclin1yr_sdq$c_medsclin1yr_date, c_medsclin1yr_sdq$Overall_date) 
  c_medsclin1yr_sdq$c_medsclin1yr_baseline_date <- as.Date(c_medsclin1yr_sdq$c_medsclin1yr_baseline_date)
  c_medsclin1yr_sdq <- c_medsclin1yr_sdq %>% select(-Overall_date)
  
  c_medsclin1yr_sdq[,8:ncol(c_medsclin1yr_sdq)] <- sapply(c_medsclin1yr_sdq[,8:ncol(c_medsclin1yr_sdq)], as.character)
  c_medsclin1yr_sdq[,8:ncol(c_medsclin1yr_sdq)] <- sapply(c_medsclin1yr_sdq[,8:ncol(c_medsclin1yr_sdq)], na_if, 999)
  c_medsclin1yr_sdq[,8:ncol(c_medsclin1yr_sdq)] <- sapply(c_medsclin1yr_sdq[,8:ncol(c_medsclin1yr_sdq)], na_if, "") 
  
  c_medsclin1yr_sdq$no_columns <- c_medsclin1yr_sdq %>% select(matches("med1name")) %>% ncol() %>% as.numeric()
  c_medsclin1yr_sdq$NA_count <- c_medsclin1yr_sdq %>% select(matches("med1name")) %>% apply(., 1, count_na)
  c_medsclin1yr_sdq$diff <- c(c_medsclin1yr_sdq$no_columns - c_medsclin1yr_sdq$NA_count)
  c_medsclin1yr_sdq <- c_medsclin1yr_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)

  ###
  
  c_medsclin_sdq_task <- merge.default(task_DB_date, c_medsclin_sdq, all=TRUE) %>% 
    rename(c_medsclin_sdq_source = source) %>% 
    group_by(Initials, c_medsclin_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_medsclin_date) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, c_medsclin_date, matches("c_medsclin_"))
  
  c_medsclin_sdq_task$c_medsclin_TDiff <- as.numeric(difftime(c_medsclin_sdq_task$Task_Date, c_medsclin_sdq_task$c_medsclin_date, tz="", units = "days"))
  c_medsclin_sdq_task <- c_medsclin_sdq_task %>% 
    mutate(measurement_TDiff_abs=abs(c_medsclin_TDiff)) %>% 
    group_by(Initials, Task_Name, Task_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, Task_Name, c_medsclin_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, c_medsclin_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  c_medsclin_sdq_clinical <- merge.default(clinical_DB_date, c_medsclin_sdq, all=TRUE) %>% 
    rename(c_medsclin_sdq_source = source) %>% 
    group_by(Initials, c_medsclin_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_medsclin_date) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_medsclin_date, matches("c_medsclin_"))
  
  c_medsclin_sdq_clinical$c_medsclin_TDiff <- as.numeric(difftime(c_medsclin_sdq_clinical$Clinical_Visit_Date, c_medsclin_sdq_clinical$c_medsclin_date, tz="", units = "days"))
  c_medsclin_sdq_clinical <- c_medsclin_sdq_clinical %>% 
    mutate(measurement_TDiff_abs=abs(c_medsclin_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, c_medsclin_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_medsclin_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  ###
  
  c_medsclin1yr_sdq_task <- merge.default(task_DB_date, c_medsclin1yr_sdq, all=TRUE) %>% 
    rename(c_medsclin1yr_sdq_source = source) %>% 
    group_by(Initials, c_medsclin1yr_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_medsclin1yr_date) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, c_medsclin1yr_date, matches("c_medsclin1yr_"))
  
  c_medsclin1yr_sdq_task$c_medsclin1yr_TDiff <- as.numeric(difftime(c_medsclin1yr_sdq_task$Task_Date, c_medsclin1yr_sdq_task$c_medsclin1yr_date, tz="", units = "days"))
  c_medsclin1yr_sdq_task <- c_medsclin1yr_sdq_task %>% 
    mutate(measurement_TDiff_abs=abs(c_medsclin1yr_TDiff)) %>% 
    group_by(Initials, Task_Name, Task_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, Task_Name, c_medsclin1yr_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, c_medsclin1yr_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  c_medsclin1yr_sdq_clinical <- merge.default(clinical_DB_date, c_medsclin1yr_sdq, all=TRUE) %>% 
    rename(c_medsclin1yr_sdq_source = source) %>% 
    group_by(Initials, c_medsclin1yr_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_medsclin1yr_date) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_medsclin1yr_date, matches("c_medsclin1yr_"))
  
  c_medsclin1yr_sdq_clinical$c_medsclin1yr_TDiff <- as.numeric(difftime(c_medsclin1yr_sdq_clinical$Clinical_Visit_Date, c_medsclin1yr_sdq_clinical$c_medsclin1yr_date, tz="", units = "days"))
  c_medsclin1yr_sdq_clinical <- c_medsclin1yr_sdq_clinical %>% 
    mutate(measurement_TDiff_abs=abs(c_medsclin1yr_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, c_medsclin1yr_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_medsclin1yr_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
# Treatment measures that require scoring ---------------------------------

#####
# straightforward total sum
  
  tot_sum_clin <- c('c_cadam_', 's_chs_', 's_vadis_', 'c_cdrs_', 's_rumination_', 's_promis_', 's_mpss_', 's_bads_', 's_asswr_', 's_aai_', 'p_conners_', 's_cfs_')

  for(i in seq_along(tot_sum_clin)) {
    iter <- as.numeric(i)
    # iter=2
    measure_name <- tot_sum_clin[iter]
    
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Overall_date, source, matches(measure_name)) %>% 
      filter(!is.na(Overall_date)) %>% 
      distinct(., .keep_all = TRUE)
    
    if (measure_name=="s_chs_" | measure_name=="s_promis_" | measure_name=="s_mpss_" | measure_name=="s_bads_" | 
        measure_name=="s_asswr_" | measure_name=="s_aai_") {
      print("creating date variable for measure")
      measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
      measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    } else {
      print("date variable manually entered in SDQ+")
      measure_temp_sdq <- measure_temp_sdq %>% rename(date_temp = (paste0(measure_name, "date")))
      measure_temp_sdq$date_temp <- as.Date(measure_temp_sdq$date_temp)
      measure_temp_sdq$date_temp <- coalesce(measure_temp_sdq$date_temp, measure_temp_sdq$Overall_date) 
      measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
      }  
    
    measure_temp_sdq <- measure_temp_sdq %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
    
    if (measure_name=="s_promis_") {
      print("recoding measure")
      measure_temp_sdq <- measure_temp_sdq %>% 
        select(PLUSID:source, s_promis_1_restless, s_promis_4_falling_asleep, s_promis_5_troublesleeping, s_promis_8_stayingasleep,
               s_promis_2_satisfied, s_promis_3_refreshing, s_promis_6_enough_sleep, s_promis_7_quality)
      measure_temp_sdq[,7:10] <- lapply(measure_temp_sdq[,7:10], FUN = function(x) recode(x, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
      measure_temp_sdq[,11:14] <- lapply(measure_temp_sdq[,11:14], FUN = function(x) recode(x, `0`=5, `1`=9, `2`=8, `3`=2, `4`=1, .missing = NULL))
      measure_temp_sdq[,11:14] <- lapply(measure_temp_sdq[,11:14], FUN = function(x) recode(x, `5`=5, `9`=4, `8`=3, `2`=2, `1`=1, .missing = NULL))
    } else if (measure_name=="s_mpss_") {
      print("recoding measure")
      measure_temp_sdq[,7:ncol(measure_temp_sdq)]  <- lapply(measure_temp_sdq[,7:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `6`=7, `5`=6, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
    } else if (measure_name=="s_bads_") {
      print("recoding measure")
      measure_temp_sdq <- measure_temp_sdq %>% 
        select(PLUSID:source, s_bads_3_content, s_bads_4_array_activities, s_bads_5_good_decisions, s_bads_7_accomplish, s_bads_11_did_longterm_goal, 
               s_bads_12_did_worth_it, s_bads_23_structured_activities, #recode:
               s_bads_1_stayed_bed, s_bads_2_didnt_do, s_bads_6_not_accomplish, s_bads_8_avoid_something, s_bads_9_avoid_emotion, s_bads_10_not_to_think,
               s_bads_13_time_thinking, s_bads_14_not_tried_solutions, s_bads_15_think_past, s_bads_16_didnt_see_friends, s_bads_17_array_activities,
               s_bads_18_not_social, s_bads_19_pushed_people, s_bads_20_cut_off_from_people, s_bads_21_time_off, s_bads_22_not_active, s_bads_24_distract, 
               s_bads_25_felt_bad)
      measure_temp_sdq[,14:ncol(measure_temp_sdq)]  <- lapply(measure_temp_sdq[,14:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `0`=7, `1`=8, `2`=9, `3`=3, `4`=2, `5`=1, `6`=0, .missing = NULL))
      measure_temp_sdq[,14:ncol(measure_temp_sdq)]  <- lapply(measure_temp_sdq[,14:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `7`=6, `8`=5, `9`=4, `3`=3, `2`=2, `1`=1, `0`=0, .missing = NULL))
    } else if (measure_name=="s_chs_") {
      print("recoding measure")
      # recoding CHS scores from SDQ that were obtained before June 22nd 2018: coding went from 0-5 -> 1-6
      measure_temp_sdq$date_temp_diff <- as.numeric(difftime(as.Date("2018-06-22"), measure_temp_sdq$date_temp, tz="", units = "days"))
      temp_before <- measure_temp_sdq %>% filter(date_temp_diff>-1 & source=="SDQ") %>% select(-date_temp_diff)
      measure_temp_sdq <- measure_temp_sdq %>% filter(date_temp_diff<0 | source=="MANUAL") %>% select(-date_temp_diff)
      temp_before[,7:ncol(temp_before)] <- lapply(temp_before[,7:ncol(temp_before)], FUN = function(x) recode(x, `5`=6, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
      measure_temp_sdq <- merge.default(measure_temp_sdq, temp_before, all=TRUE)
    } else {
      print("no recoding necessary for this measure")
    }
    
    if (measure_name=="c_cadam_") {
      
      measure_temp_sdq[,11:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,11:ncol(measure_temp_sdq)], as.numeric)
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(c_cadam_1_sad_facial:c_cadam_15_leisure_activities) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(c_cadam_1_sad_facial:c_cadam_15_leisure_activities) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(c_cadam_1_sad_facial:c_cadam_15_leisure_activities) %>% rowSums(na.rm=TRUE)
      
    } else if (measure_name=="c_cdrs_") {
      
      measure_temp_sdq[,10:(ncol(measure_temp_sdq))] <- sapply(measure_temp_sdq[,10:(ncol(measure_temp_sdq))], as.numeric)
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(c_cdrs_1_depress_feel:c_cdrs_18_mood_liability) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(c_cdrs_1_depress_feel:c_cdrs_18_mood_liability) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)

      measure_temp_sdq <- measure_temp_sdq %>% rename(c_cdrs_tot_manual = c_cdrs_tot)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% 
        select(c_cdrs_1_depress_feel:c_cdrs_18_mood_liability) %>% rowSums(na.rm=TRUE)
      
    } else if (measure_name=="p_conners_") {
      
      measure_temp_sdq[,10:(ncol(measure_temp_sdq))] <- sapply(measure_temp_sdq[,10:(ncol(measure_temp_sdq))], as.numeric)
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(p_conners_1_angry:p_conners_80_blurts_answers) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(p_conners_1_angry:p_conners_80_blurts_answers) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(p_conners_1_angry:p_conners_80_blurts_answers) %>% rowSums(na.rm=TRUE)
      
    } else {
      
      measure_temp_sdq[,7:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,7:ncol(measure_temp_sdq)], as.numeric)
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(measure_name)) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(measure_name)) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% rowSums(na.rm=TRUE)
      
      measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(measure_name)) %>% complete.cases(.)
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"
      names(measure_temp_sdq)[names(measure_temp_sdq) == "tempcomplete"] <- (paste0(measure_name, "complete"))
      
    }
    
    measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp_sdq, all=TRUE) %>% 
      rename(measure_temp_source = source) %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, desc(temptotal), desc(measure_temp_source)) %>%
      ungroup() %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, matches(measure_name), date_temp, temptotal, measure_temp_source)
    
    measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
    measure_temp_clinical <- measure_temp_clinical %>% 
      mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
      group_by(Initials, Clinical_Visit_Date) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(measurement_TDiff_abs<=60) %>% 
      select(-measurement_TDiff_abs)
    
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
    
  }
  
#####
# unusual cases/those with subscales 

#####
# CASE - CHILD AND ADOLESCENT SURVEY OF EXPERIENCES

  CASE <- c('s_case_', 'p_case_')
  
  for(i in seq_along(CASE)) {
    iter <- as.numeric(i)
    # iter=2
    measure_name <- CASE[iter]
    
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches(measure_name)) %>% 
      distinct(., .keep_all = TRUE)
    
    measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
    measure_temp_sdq <- measure_temp_sdq %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, date_temp, matches(measure_name))
    measure_temp_sdq[,8:(ncol(measure_temp_sdq)-1)] <- sapply(measure_temp_sdq[,8:(ncol(measure_temp_sdq)-1)], as.numeric) 

    measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches("_if")) %>% ncol() %>% as.numeric()
    measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches("_if")) %>% apply(., 1, count_na)
    measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
    measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
    
    how_columns <- measure_temp_sdq %>% select(matches("_how")) %>% colnames()
    if_columns <- measure_temp_sdq %>% select(matches("_if")) %>% colnames()
    combined <- cbind(how_columns, if_columns)
    
    # recoding CASE scores from SDQ that were obtained before June 22nd 2018: '_how' item coding went from 0-5 -> 3, 2, 1, -1, -2, -3
    measure_temp_sdq$date_temp_diff <- as.numeric(difftime(as.Date("2018-06-22"), measure_temp_sdq$date_temp, tz="", units = "days"))
    temp_before <- measure_temp_sdq %>% filter(date_temp_diff>-1 & source=="SDQ") %>% select(-date_temp_diff)
    measure_temp_sdq <- measure_temp_sdq %>% filter(date_temp_diff<0 | source=="MANUAL") %>% select(-date_temp_diff)
    temp_before[how_columns] <- lapply(temp_before[how_columns], FUN = function(x) recode(x, `5`=-3, `4`=-2, `3`=-1, `2`=8, `1`=2, `0`=3, .missing = NULL))
    temp_before[how_columns] <- lapply(temp_before[how_columns], FUN = function(x) recode(x, `-3`=-3, `-2`=-2, `-1`=-1, `8`=1, `2`=2, `3`=3, .missing = NULL))
    measure_temp_sdq <- merge.default(measure_temp_sdq, temp_before, all=TRUE)

    measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches("_if")) %>% rowSums(na.rm=TRUE)
  
    for(j in seq_along(how_columns)) {
      iter2 <- as.numeric(j)
      # iter2=3
      how_column_name <- as.character(combined[iter2,1])
      if_column_name <- as.character(combined[iter2,2])
 
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_pos = ifelse(((!!sym(how_column_name))>0), (!!sym(if_column_name)), 0))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_pos"] <- (paste0("temp_pos",iter2))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_neg = ifelse(((!!sym(how_column_name))<0), (!!sym(if_column_name)), 0))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_neg"] <- (paste0("temp_neg",iter2))
      }
    
    measure_temp_sdq$temp_pos_tot <- measure_temp_sdq %>% select(matches("temp_pos")) %>% rowSums(na.rm=TRUE)
    measure_temp_sdq$temp_neg_tot <- measure_temp_sdq %>% select(matches("temp_neg")) %>% rowSums(na.rm=TRUE)

    measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches("_if")) %>% complete.cases(.)
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"
    
    measure_temp_task <- merge.default(task_DB_date, measure_temp_sdq, all=TRUE) %>% 
      rename(measure_temp_source = source) %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, desc(temptotal)) %>%
      ungroup() %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, date_temp, matches(measure_name), measure_temp_source, tempcomplete, 
             temptotal, temp_pos_tot, temp_neg_tot)
    
    measure_temp_task$measurement_TDiff <- as.numeric(difftime(measure_temp_task$Task_Date, measure_temp_task$date_temp, tz="", units = "days"))
    measure_temp_task <- measure_temp_task %>% 
      mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
      group_by(Initials, Task_Name, Task_Date) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      group_by(Initials, Task_Name, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, date_temp, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(measurement_TDiff_abs<=60) %>% 
      select(-measurement_TDiff_abs)
    
    names(measure_temp_task)[names(measure_temp_task) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_task)[names(measure_temp_task) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_task)[names(measure_temp_task) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_task)[names(measure_temp_task) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    names(measure_temp_task)[names(measure_temp_task) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_task)[names(measure_temp_task) == "temp_pos_tot"] <- (paste0(measure_name, "pos_tot"))
    names(measure_temp_task)[names(measure_temp_task) == "temp_neg_tot"] <- (paste0(measure_name, "neg_tot"))
    assign(paste0(measure_name, "subset_task"), measure_temp_task)
    
    measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp_sdq, all=TRUE) %>% 
      rename(measure_temp_source = source) %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, desc(temptotal)) %>%
      ungroup() %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, measure_temp_source, date_temp, matches(measure_name), 
             tempcomplete, temptotal, temp_pos_tot, temp_neg_tot)
    
    measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
    measure_temp_clinical <- measure_temp_clinical %>% 
      mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
      group_by(Initials, Clinical_Visit_Date) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(measurement_TDiff_abs<=60) %>% 
      select(-measurement_TDiff_abs)
    
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_pos_tot"] <- (paste0(measure_name, "_pos_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_neg_tot"] <- (paste0(measure_name, "_neg_tot"))
    assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
    
  }
  
#####
# SEQ - SEQ (SELF-EFFICACY QUESTIONNAIRE) child
  
  s_seq_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_seq_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_seq_subset_sdq[,7:ncol(s_seq_subset_sdq)] <- sapply(s_seq_subset_sdq[,7:ncol(s_seq_subset_sdq)], as.numeric) 
  
  s_seq_subset_sdq$no_columns <- s_seq_subset_sdq %>% select(matches('s_seq_')) %>% ncol() %>% as.numeric()
  s_seq_subset_sdq$NA_count <- s_seq_subset_sdq %>% select(matches('s_seq_')) %>% apply(., 1, count_na)
  s_seq_subset_sdq$diff <- c(s_seq_subset_sdq$no_columns - s_seq_subset_sdq$NA_count)
  s_seq_subset_sdq <- s_seq_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  # recoding SEQ scores from SDQ that were obtained before June 22nd 2018: coding went from 0-4 -> 1-5
  s_seq_subset_sdq$date_temp <- s_seq_subset_sdq$Overall_date
  s_seq_subset_sdq$date_temp_diff <- as.numeric(difftime(as.Date("2018-06-22"), s_seq_subset_sdq$date_temp, tz="", units = "days"))
  temp_before <- s_seq_subset_sdq %>% filter(date_temp_diff>-1 & source=="SDQ") %>% select(-date_temp, -date_temp_diff)
  s_seq_subset_sdq <- s_seq_subset_sdq %>% filter(date_temp_diff<0 | source=="MANUAL") %>% select(-date_temp, -date_temp_diff)
  temp_before[,7:ncol(temp_before)]  <- lapply(temp_before[,7:ncol(temp_before)], FUN = function(x) recode(x, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
  s_seq_subset_sdq <- merge.default(s_seq_subset_sdq, temp_before, all=TRUE)
  
  s_seq_subset_sdq$s_seq_tot <- s_seq_subset_sdq %>% select(matches('s_seq_')) %>% rowSums(na.rm=TRUE)
  
  s_seq_academic <- c("_1_|_4_|_7_|_10_|_13_|_16_|_19_|_22_")
  s_seq_subset_sdq$s_seq_academic_tot <- s_seq_subset_sdq %>% select(matches(s_seq_academic)) %>% rowSums(na.rm=TRUE)
  s_seq_social <- c("_2_|_6_|_8_|_11_|_14_|_17_|_20_|_23_")
  s_seq_subset_sdq$s_seq_social_tot <- s_seq_subset_sdq %>% select(matches(s_seq_social)) %>% rowSums(na.rm=TRUE)
  s_seq_emotional <- c("_3_|_5_|_9_|_12_|_15_|_18_|_21_|_24_")
  s_seq_subset_sdq$s_seq_emotional_tot <- s_seq_subset_sdq %>% select(matches(s_seq_emotional)) %>% rowSums(na.rm=TRUE)
  
  s_seq_subset_sdq$s_seq_complete <- s_seq_subset_sdq %>% select(matches('s_seq_')) %>% complete.cases(.)
  s_seq_subset_sdq$s_seq_complete[s_seq_subset_sdq$s_seq_complete=="FALSE"] <- "0"
  s_seq_subset_sdq$s_seq_complete[s_seq_subset_sdq$s_seq_complete=="TRUE"] <- "1"
  
  s_seq_subset_sdq$s_seq_date <- s_seq_subset_sdq$Overall_date
  s_seq_subset_sdq <- s_seq_subset_sdq %>% select(-Overall_date)
  
  s_seq_subset_clinical <- merge.default(clinical_DB_date, s_seq_subset_sdq, all=TRUE) %>% 
    rename(s_seq_source = source) %>% 
    group_by(Initials, s_seq_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_seq_date, desc(s_seq_tot), desc(s_seq_source)) %>%
    # slice(1) %>% 
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_seq_source, matches('s_seq_'))
  
  s_seq_subset_clinical$s_seq_TDiff <- as.numeric(difftime(s_seq_subset_clinical$Clinical_Visit_Date, s_seq_subset_clinical$s_seq_date, tz="", units = "days"))
  s_seq_subset_clinical <- s_seq_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_seq_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_seq_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_seq_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
#####
# FAD - FAMILY ASSESSMENT DEVICE

  FAD <- c('s_fad_', 'p_fad_')
  fad_normal <- c("_2_|_3_|_6_|_10_|_12_|_16_|_18_|_20_|_24_|_26_|_29_|_30_|_32_|_36_|_38_|_40_|_43_|_46_|_49_|_50_|_55_|_56_|_57_|_59_|_60_")
  fad_reverse <- c("_1_|_4_|_5_|_7_|_8_|_9_|_11_|_13_|_14_|_15_|_17_|_19_|_21_|_22_|_23_|_25_|_27_|_28_|_31_|_33_|_34_|_35_|_37_|_39_|_41_|
                   _42_|_44_|_45_|_47_|_48_|_51_|_52_|_53_|_54_|_58_")
  
  for(i in seq_along(FAD)) {
    iter <- as.numeric(i)
    # iter=2
    measure_name <- FAD[iter]
    
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches(measure_name)) %>% 
      distinct(., .keep_all = TRUE)
  
    measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(fad_normal), matches(fad_reverse)) %>% ncol() %>% as.numeric()
    measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(fad_normal), matches(fad_reverse)) %>% apply(., 1, count_na)
    measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
    measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)

    if (measure_name == "p_fad_") {
      measure_temp_sdq[,9:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,9:ncol(measure_temp_sdq)], as.numeric) 
      measure_temp_sdq <- measure_temp_sdq %>% select(PLUSID:Overall_date, p_fad_parent, p_fad_parent_other, matches(fad_normal), matches(fad_reverse))
      measure_temp_sdq[,34:ncol(measure_temp_sdq)] <- lapply(measure_temp_sdq[,34:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `1`=5, `2`=6, `3`=2, `4`=1, .missing = NULL))
      measure_temp_sdq[,34:ncol(measure_temp_sdq)] <- lapply(measure_temp_sdq[,34:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `5`=4, `6`=3, `2`=2, `1`=1, .missing = NULL))
    } else {
      measure_temp_sdq[,7:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,7:ncol(measure_temp_sdq)], as.numeric) 
      measure_temp_sdq <- measure_temp_sdq %>% select(PLUSID:Overall_date, matches(fad_normal), matches(fad_reverse))
      measure_temp_sdq[,32:ncol(measure_temp_sdq)] <- lapply(measure_temp_sdq[,32:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `1`=5, `2`=6, `3`=2, `4`=1, .missing = NULL))
      measure_temp_sdq[,32:ncol(measure_temp_sdq)] <- lapply(measure_temp_sdq[,32:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `5`=4, `6`=3, `2`=2, `1`=1, .missing = NULL))
    }
    
    problem_solving <- c("_2_|_12_|_24_|_38_|_50_|_60_")
    measure_temp_sdq$temp_problem_solving_tot <- measure_temp_sdq %>% select(matches(problem_solving)) %>% rowSums(na.rm=TRUE)
    comminication <- c("_1_|_3_|_14_|_18_|_22_|_29_|_35_|_43_|_52_|_59_")
    measure_temp_sdq$temp_comminication_tot <- measure_temp_sdq %>% select(matches(comminication)) %>% rowSums(na.rm=TRUE)
    roles <- c("_4_|_8_|_10_|_15_|_23_|_30_|_34_|_40_|_45_|_53_|_58_")
    measure_temp_sdq$temp_roles_tot <- measure_temp_sdq %>% select(matches(roles)) %>% rowSums(na.rm=TRUE)
    affective_response <- c("_9_|_19_|_28_|_39_|_49_|_57_")
    measure_temp_sdq$temp_affective_response_tot <- measure_temp_sdq %>% select(matches(affective_response)) %>% rowSums(na.rm=TRUE)
    behav_control <- c("_7_|_17_|_20_|_27_|_32_|_44_|_47_|_48_|_55_")
    measure_temp_sdq$temp_behav_control_tot <- measure_temp_sdq %>% select(matches(behav_control)) %>% rowSums(na.rm=TRUE)
    gen_functioning <- c("_1_|_6_|_11_|_16_|_21_|_26_|_31_|_36_|_41_|_46_|_51_|_56_")
    measure_temp_sdq$temp_gen_functioning_tot <- measure_temp_sdq %>% select(matches(gen_functioning)) %>% rowSums(na.rm=TRUE)
    
    measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(fad_normal), matches(fad_reverse)) %>% complete.cases(.)
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"
    
    measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
    measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    
    measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp_sdq, all=TRUE) %>% 
      rename(measure_temp_source = source) %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, desc(temp_gen_functioning_tot), desc(measure_temp_source)) %>%
      slice(1) %>% 
      ungroup() 
    
    measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
    measure_temp_clinical <- measure_temp_clinical %>% 
      mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
      group_by(Initials, Clinical_Visit_Date) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      group_by(Initials, date_temp) %>% 
      arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, measurement_TDiff_abs) %>% 
      slice(1) %>%
      ungroup() %>% 
      filter(measurement_TDiff_abs<=60) %>%
      select(-measurement_TDiff_abs)
    
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_problem_solving_tot"] <- (paste0(measure_name, "problem_solving_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_comminication_tot"] <- (paste0(measure_name, "comminication_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_roles_tot"] <- (paste0(measure_name, "roles_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_affective_response_tot"] <- (paste0(measure_name, "affective_response_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_behav_control_tot"] <- (paste0(measure_name, "behav_control_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_gen_functioning_tot"] <- (paste0(measure_name, "gen_functioning_tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
    
  }
 
#####
# FASA - FAMILY ACCOMODATION SCALE ANXIETY (parent report)
  
  p_fasa_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, p_fasa_responder, 
                                              p_fasa_responder_other, p_fasa_1_reassure:p_fasa_13_anxiety_worsen) %>% 
    distinct(., .keep_all = TRUE)
  
  p_fasa_subset_sdq[,9:ncol(p_fasa_subset_sdq)] <- sapply(p_fasa_subset_sdq[,9:ncol(p_fasa_subset_sdq)], as.numeric) 
  
  p_fasa_subset_sdq$no_columns <- p_fasa_subset_sdq %>% select(p_fasa_1_reassure:p_fasa_13_anxiety_worsen) %>% ncol() %>% as.numeric()
  p_fasa_subset_sdq$NA_count <- p_fasa_subset_sdq %>% select(p_fasa_1_reassure:p_fasa_13_anxiety_worsen) %>% apply(., 1, count_na)
  p_fasa_subset_sdq$diff <- c(p_fasa_subset_sdq$no_columns - p_fasa_subset_sdq$NA_count)
  p_fasa_subset_sdq <- p_fasa_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  p_fasa_subset_sdq$p_fasa_tot <- p_fasa_subset_sdq %>% select(p_fasa_1_reassure:p_fasa_13_anxiety_worsen) %>% rowSums(na.rm=TRUE)
  
  p_fasa_participation <- c("_1_|_2_|_3_|_4_|_5_")
  p_fasa_subset_sdq$p_fasa_participation_tot <- p_fasa_subset_sdq %>% select(matches(p_fasa_participation)) %>% rowSums(na.rm=TRUE)
  p_fasa_modification <- c("_6_|_7_|_8_|_9_")
  p_fasa_subset_sdq$p_fasa_modification_tot <- p_fasa_subset_sdq %>% select(matches(p_fasa_modification)) %>% rowSums(na.rm=TRUE)
  p_fasa_distress <- c("_10_|_11_|_12_|_13_")
  p_fasa_subset_sdq$p_fasa_distress_tot <- p_fasa_subset_sdq %>% select(matches(p_fasa_distress)) %>% rowSums(na.rm=TRUE)
  
  p_fasa_subset_sdq$p_fasa_complete <- p_fasa_subset_sdq %>% select(p_fasa_1_reassure:p_fasa_13_anxiety_worsen) %>% complete.cases(.)
  p_fasa_subset_sdq$p_fasa_complete[p_fasa_subset_sdq$p_fasa_complete=="FALSE"] <- "0"
  p_fasa_subset_sdq$p_fasa_complete[p_fasa_subset_sdq$p_fasa_complete=="TRUE"] <- "1"
  
  p_fasa_subset_sdq$p_fasa_date <- p_fasa_subset_sdq$Overall_date
  p_fasa_subset_sdq <- p_fasa_subset_sdq %>% select(-Overall_date)
  
  p_fasa_subset_clinical <- merge.default(clinical_DB_date, p_fasa_subset_sdq, all=TRUE) %>% 
    rename(p_fasa_source = source) %>% 
    group_by(Initials, p_fasa_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, p_fasa_date, desc(p_fasa_tot), desc(p_fasa_source)) %>%
    slice(1) %>% 
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, p_fasa_source, matches('p_fasa_'))
  
  p_fasa_subset_clinical$p_fasa_TDiff <- as.numeric(difftime(p_fasa_subset_clinical$Clinical_Visit_Date, p_fasa_subset_clinical$p_fasa_date, tz="", units = "days"))
  p_fasa_subset_clinical <- p_fasa_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(p_fasa_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, p_fasa_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, p_fasa_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
 
#####
# BA activities expectation & outcome
  
  s_baexpout_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_baexpout_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_baexpout_subset_sdq$s_baexpout_date <- as.Date(s_baexpout_subset_sdq$s_baexpout_date)
  s_baexpout_subset_sdq$s_baexpout_date <- coalesce(s_baexpout_subset_sdq$s_baexpout_date, s_baexpout_subset_sdq$Overall_date) 
  s_baexpout_subset_sdq <- s_baexpout_subset_sdq %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, s_baexpout_date, 
                                                            s_baexpout_clinician_name,
                                                            s_baexpout_weeks_treat:s_baexpout_act_1_type, s_baexpout_act_1_notes:s_baexpout_act_2_type,
                                                            s_baexpout_act_2_notes:s_baexpout_act_3_type, s_baexpout_act_3_notes:s_baexpout_act_4_type,
                                                            s_baexpout_act_4_notes:s_baexpout_act_5_type, s_baexpout_act_5_notes, s_baexpout_other_notes,
                                                            s_baexpout_act_1_moodbef:s_baexpout_act_1_again, s_baexpout_act_2_moodbef:s_baexpout_act_2_again, 
                                                            s_baexpout_act_3_moodbef:s_baexpout_act_3_again, s_baexpout_act_4_moodbef:s_baexpout_act_4_again, 
                                                            s_baexpout_act_5_moodbef:s_baexpout_act_5_again)
  
  s_baexpout_subset_sdq[,9:24] <- sapply(s_baexpout_subset_sdq[,9:24], as.character) 
  s_baexpout_subset_sdq[,25:ncol(s_baexpout_subset_sdq)] <- sapply(s_baexpout_subset_sdq[,25:ncol(s_baexpout_subset_sdq)], as.numeric) 
  
  s_baexpout_subset_sdq[,10]  <- lapply(s_baexpout_subset_sdq[,10], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  s_baexpout_subset_sdq[,13]  <- lapply(s_baexpout_subset_sdq[,13], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  s_baexpout_subset_sdq[,16]  <- lapply(s_baexpout_subset_sdq[,16], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  s_baexpout_subset_sdq[,19]  <- lapply(s_baexpout_subset_sdq[,19], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  s_baexpout_subset_sdq[,22]  <- lapply(s_baexpout_subset_sdq[,22], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  
  s_baexpout_subset_sdq$no_columns <- s_baexpout_subset_sdq %>% select(s_baexpout_act_1_moodbef:s_baexpout_act_5_again) %>% ncol() %>% as.numeric()
  s_baexpout_subset_sdq$NA_count <- s_baexpout_subset_sdq %>% select(s_baexpout_act_1_moodbef:s_baexpout_act_5_again) %>% apply(., 1, count_na)
  s_baexpout_subset_sdq$diff <- c(s_baexpout_subset_sdq$no_columns - s_baexpout_subset_sdq$NA_count)
  s_baexpout_subset_sdq <- s_baexpout_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  activities <- c("_1_", "_2_", "_3_", "_4_", "_5_")
  
  for(i in seq_along(activities)) {
    iter <- as.numeric(i)
    # iter=1
    activity_no <- as.character(activities[iter])
    
    s_baexpout_subset_sdq <- s_baexpout_subset_sdq %>% 
      mutate(temp_mood_diff = (!!sym(paste0("s_baexpout_act",activity_no,"moodaft"))) - (!!sym(paste0("s_baexpout_act",activity_no,"moodbef"))))
    names(s_baexpout_subset_sdq)[names(s_baexpout_subset_sdq) == "temp_mood_diff"] <- (paste0("s_baexpout_act",activity_no,"mood_diff"))
    
    s_baexpout_subset_sdq <- s_baexpout_subset_sdq %>% 
      mutate(temp_difficulty_diff = (!!sym(paste0("s_baexpout_act",activity_no,"diffaft"))) - (!!sym(paste0("s_baexpout_act",activity_no,"diffbef"))))
    names(s_baexpout_subset_sdq)[names(s_baexpout_subset_sdq) == "temp_difficulty_diff"] <- (paste0("s_baexpout_act",activity_no,"difficulty_diff"))
    
    s_baexpout_subset_sdq <- s_baexpout_subset_sdq %>% 
      mutate(temp_enjoyment_diff = (!!sym(paste0("s_baexpout_act",activity_no,"enjoyaft"))) - (!!sym(paste0("s_baexpout_act",activity_no,"enjoybef"))))
    names(s_baexpout_subset_sdq)[names(s_baexpout_subset_sdq) == "temp_enjoyment_diff"] <- (paste0("s_baexpout_act",activity_no,"enjoyment_diff"))

    s_baexpout_subset_sdq <- s_baexpout_subset_sdq %>% 
      mutate(temp_anxiety_diff = (!!sym(paste0("s_baexpout_act",activity_no,"anxaft"))) - (!!sym(paste0("s_baexpout_act",activity_no,"anxbef"))))
    names(s_baexpout_subset_sdq)[names(s_baexpout_subset_sdq) == "temp_anxiety_diff"] <- (paste0("s_baexpout_act",activity_no,"anxiety_diff"))
    
    s_baexpout_subset_sdq <- s_baexpout_subset_sdq %>% 
      mutate(temp_satisfaction_diff = (!!sym(paste0("s_baexpout_act",activity_no,"sataft"))) - (!!sym(paste0("s_baexpout_act",activity_no,"satbef"))))
    names(s_baexpout_subset_sdq)[names(s_baexpout_subset_sdq) == "temp_satisfaction_diff"] <- (paste0("s_baexpout_act",activity_no,"satisfaction_diff"))
    
  }
  
  s_baexpout_subset_clinical <- merge.default(clinical_DB_date, s_baexpout_subset_sdq, all=TRUE) %>% 
    rename(s_baexpout_source = source) %>% 
    group_by(Initials, s_baexpout_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_baexpout_date, desc(s_baexpout_act_1_enjoyment_diff)) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_baexpout_source, s_baexpout_date, s_baexpout_clinician_name, s_baexpout_weeks_treat,
           matches("_act_1_"), matches("_act_2_"), matches("_act_3_"), matches("_act_4_"), matches("_act_5_"), s_baexpout_other_notes)
  
  s_baexpout_subset_clinical$s_baexpout_TDiff <- as.numeric(difftime(s_baexpout_subset_clinical$Clinical_Visit_Date, s_baexpout_subset_clinical$s_baexpout_date, tz="", units = "days"))
  s_baexpout_subset_clinical <- s_baexpout_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_baexpout_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_baexpout_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_baexpout_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
#####
# SNAP
  
  c_snap_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('c_snap_')) %>% 
    distinct(., .keep_all = TRUE)
  
  c_snap_subset_sdq$c_snap_date <- as.Date(c_snap_subset_sdq$c_snap_date)
  c_snap_subset_sdq$c_snap_date <- coalesce(c_snap_subset_sdq$c_snap_date, c_snap_subset_sdq$Overall_date) 
  c_snap_subset_sdq <- c_snap_subset_sdq %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, c_snap_date, source, c_snap_clinician_name, c_snap_clinician_role,
                                                    c_snap_visit_type, c_snap_treatment_week, c_snap_1_fail_attention:c_snap_18_interrupts)
  
  c_snap_subset_sdq[,7:ncol(c_snap_subset_sdq)]  <- lapply(c_snap_subset_sdq[,7:ncol(c_snap_subset_sdq)], na_if, "")
  c_snap_subset_sdq[,11:ncol(c_snap_subset_sdq)] <- sapply(c_snap_subset_sdq[,11:ncol(c_snap_subset_sdq)], as.numeric) 
  
  c_snap_subset_sdq$no_columns <- c_snap_subset_sdq %>% select(matches('c_snap_')) %>% select(-c_snap_date) %>% ncol() %>% as.numeric()
  c_snap_subset_sdq$NA_count <- c_snap_subset_sdq %>% select(matches('c_snap_')) %>% select(-c_snap_date) %>% apply(., 1, count_na)
  c_snap_subset_sdq$diff <- c(c_snap_subset_sdq$no_columns - c_snap_subset_sdq$NA_count)
  c_snap_subset_sdq <- c_snap_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  c_snap_subset_sdq$c_snap_tot <- c_snap_subset_sdq %>% select(c_snap_1_fail_attention:c_snap_18_interrupts) %>% rowSums(na.rm=TRUE)
  
  c_snap_inattention <- c("_1_|_2_|_3_|_4_|_5_|_6_|_7_|_8_|_9_")
  c_snap_subset_sdq$c_snap_inattention_tot <- c_snap_subset_sdq %>% select(matches(c_snap_inattention)) %>% rowSums(na.rm=TRUE)
  c_snap_hyperactivity <- c("_10_|_11_|_12_|_13_|_14_|_15_16_|_17_|_18_")
  c_snap_subset_sdq$c_snap_hyperactivity_tot <- c_snap_subset_sdq %>% select(matches(c_snap_hyperactivity)) %>% rowSums(na.rm=TRUE)
  
  c_snap_subset_sdq$c_snap_complete <- c_snap_subset_sdq %>% select(matches('c_snap_')) %>% complete.cases(.)
  c_snap_subset_sdq$c_snap_complete[c_snap_subset_sdq$c_snap_complete=="FALSE"] <- "0"
  c_snap_subset_sdq$c_snap_complete[c_snap_subset_sdq$c_snap_complete=="TRUE"] <- "1"
  
  c_snap_subset_clinical <- merge.default(clinical_DB_date, c_snap_subset_sdq, all=TRUE) %>% 
    rename(c_snap_source = source) %>% 
    group_by(Initials, c_snap_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_snap_date, desc(c_snap_tot), desc(c_snap_source)) %>%
    slice(1) %>% 
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_snap_source, matches('c_snap_'))
  
  c_snap_subset_clinical$c_snap_TDiff <- as.numeric(difftime(c_snap_subset_clinical$Clinical_Visit_Date, c_snap_subset_clinical$c_snap_date, tz="", units = "days"))
  c_snap_subset_clinical <- c_snap_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(c_snap_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, c_snap_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_snap_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
#####
# CYBOCC

  ### clinician completed symptom assessment 
  
  c_cybocs_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, c_cybocs_date, matches('c_cybocs_ob_'), matches('c_cybocs_com_')) %>% 
    distinct(., .keep_all = TRUE)
  
  c_cybocs_subset_sdq[,8:ncol(c_cybocs_subset_sdq)] <- sapply(c_cybocs_subset_sdq[,8:ncol(c_cybocs_subset_sdq)], as.numeric) 
  
  c_cybocs_subset_sdq$no_columns <- c_cybocs_subset_sdq %>% select(matches('c_cybocs_')) %>% ncol() %>% as.numeric()
  c_cybocs_subset_sdq$NA_count <- c_cybocs_subset_sdq %>% select(matches('c_cybocs_')) %>% apply(., 1, count_na)
  c_cybocs_subset_sdq$diff <- c(c_cybocs_subset_sdq$no_columns - c_cybocs_subset_sdq$NA_count)
  c_cybocs_subset_sdq <- c_cybocs_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  c_cybocs_subset_sdq$c_cybocs_ob_tot <- c_cybocs_subset_sdq %>% select( matches('c_cybocs_ob_')) %>% rowSums(na.rm=TRUE)
  c_cybocs_subset_sdq$c_cybocs_com_tot <- c_cybocs_subset_sdq %>% select(matches('c_cybocs_com_')) %>% rowSums(na.rm=TRUE)
  
  c_cybocs_subset_sdq$c_cybocs_complete <- c_cybocs_subset_sdq %>% select(matches('c_cybocs_')) %>% complete.cases(.)
  c_cybocs_subset_sdq$c_cybocs_complete[c_cybocs_subset_sdq$c_cybocs_complete=="FALSE"] <- "0"
  c_cybocs_subset_sdq$c_cybocs_complete[c_cybocs_subset_sdq$c_cybocs_complete=="TRUE"] <- "1"
  c_cybocs_subset_sdq$c_cybocs_date <- as.Date(c_cybocs_subset_sdq$c_cybocs_date)
  c_cybocs_subset_sdq$c_cybocs_date <- coalesce(c_cybocs_subset_sdq$c_cybocs_date, c_cybocs_subset_sdq$Overall_date)
  c_cybocs_subset_sdq <- c_cybocs_subset_sdq %>% select(-Overall_date)
  
  c_cybocs_subset_clinical <- merge.default(clinical_DB_date, c_cybocs_subset_sdq, all=TRUE) %>% 
    rename(c_cybocs_source = source) %>% 
    group_by(Initials, c_cybocs_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_cybocs_date, desc(c_cybocs_ob_tot)) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_cybocs_date, c_cybocs_source, matches('c_cybocs_'))
  
  c_cybocs_subset_clinical$c_cybocs_TDiff <- as.numeric(difftime(c_cybocs_subset_clinical$Clinical_Visit_Date, c_cybocs_subset_clinical$c_cybocs_date, tz="", units = "days"))
  c_cybocs_subset_clinical <- c_cybocs_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(c_cybocs_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, c_cybocs_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_cybocs_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  ### self report symptom checklist 
  
  s_cybocs_list_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_cybocs_list_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_cybocs_list_subset_sdq$s_cybocs_list_date <- as.Date(s_cybocs_list_subset_sdq$s_cybocs_list_date)
  s_cybocs_list_subset_sdq$s_cybocs_list_date <- coalesce(s_cybocs_list_subset_sdq$s_cybocs_list_date, s_cybocs_list_subset_sdq$Overall_date) 
  
  s_cybocs_list_subset_sdq <- s_cybocs_list_subset_sdq %>% select(PLUSID:source, s_cybocs_list_date, matches("s_cybocs_list_com_"), matches("s_cybocs_list_ob_"))
  
  s_cybocs_list_subset_sdq[,7:ncol(s_cybocs_list_subset_sdq)]  <- lapply(s_cybocs_list_subset_sdq[,7:ncol(s_cybocs_list_subset_sdq)], as.character)
  s_cybocs_list_subset_sdq[,7:ncol(s_cybocs_list_subset_sdq)]  <- lapply(s_cybocs_list_subset_sdq[,7:ncol(s_cybocs_list_subset_sdq)], FUN = function(x) recode(x, `0`="Past", `1`="Current", `2`="Both Past & Current", `777`="Never", .missing = NULL))
  
  s_cybocs_list_subset_sdq$no_columns <- s_cybocs_list_subset_sdq %>% select(matches("s_cybocs_list_com_"), matches("s_cybocs_list_ob_")) %>% ncol() %>% as.numeric()
  s_cybocs_list_subset_sdq$NA_count <- s_cybocs_list_subset_sdq %>% select(matches("s_cybocs_list_com_"), matches("s_cybocs_list_ob_")) %>% apply(., 1, count_na)
  s_cybocs_list_subset_sdq$diff <- c(s_cybocs_list_subset_sdq$no_columns - s_cybocs_list_subset_sdq$NA_count)
  s_cybocs_list_subset_sdq <- s_cybocs_list_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_cybocs_list_subset_clinical <- merge.default(clinical_DB_date, s_cybocs_list_subset_sdq, all=TRUE) %>% 
    rename(s_cybocs_list_source = source) %>% 
    group_by(Initials, s_cybocs_list_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_cybocs_list_date) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_cybocs_list_source, matches('s_cybocs_list_'))
  
  s_cybocs_list_subset_clinical$s_cybocs_list_TDiff <- as.numeric(difftime(s_cybocs_list_subset_clinical$Clinical_Visit_Date, s_cybocs_list_subset_clinical$s_cybocs_list_date, tz="", units = "days"))
  s_cybocs_list_subset_clinical <- s_cybocs_list_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_cybocs_list_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_cybocs_list_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_cybocs_list_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs) 
 
#####
# BDD-YBOCS
  
  ### clinician completed symptom assessment 
  
  c_bddybocs_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('c_bddybocs_')) %>% 
    distinct(., .keep_all = TRUE)
  
  c_bddybocs_subset_sdq$c_bddybocs_date <- as.Date(c_bddybocs_subset_sdq$c_bddybocs_date)
  c_bddybocs_subset_sdq$c_bddybocs_date <- coalesce(c_bddybocs_subset_sdq$c_bddybocs_date, c_bddybocs_subset_sdq$Overall_date) 
  
  c_bddybocs_subset_sdq <- c_bddybocs_subset_sdq %>% 
    select(PLUSID:source, c_bddybocs_date, c_bddybocs_Comments, c_bddybocs_12_avoidanceB, c_bddybocs_2_interference_body_defectB,
           c_bddybocs_5_control_over_thoughtsA, c_bddybocs_6789_activities, c_bddybocs_6789_other, c_bddybocs_7_interferenceB,
           c_bddybocs_1_time_thoughts_body_defect, c_bddybocs_2_interference_body_defectA, c_bddybocs_3_distress_body_defect,
           c_bddybocs_4_resistance_against_thoughts, c_bddybocs_5_control_over_thoughts, c_bddybocs_6_timespent, c_bddybocs_7_interference, 
           c_bddybocs_8_distress_activities, c_bddybocs_9_resistance_against_compulsions, c_bddybocs_10_control_over_behavior, c_bddybocs_11_insight, 
           c_bddybocs_12_avoidance)

  c_bddybocs_subset_sdq[,14:ncol(c_bddybocs_subset_sdq)] <- sapply(c_bddybocs_subset_sdq[,14:ncol(c_bddybocs_subset_sdq)], as.numeric) 
  
  c_bddybocs_subset_sdq$no_columns <- c_bddybocs_subset_sdq %>% 
    select(c_bddybocs_1_time_thoughts_body_defect, c_bddybocs_2_interference_body_defectA, c_bddybocs_3_distress_body_defect,
           c_bddybocs_4_resistance_against_thoughts, c_bddybocs_5_control_over_thoughts, c_bddybocs_6_timespent, c_bddybocs_7_interference, 
           c_bddybocs_8_distress_activities, c_bddybocs_9_resistance_against_compulsions, c_bddybocs_10_control_over_behavior, c_bddybocs_11_insight, 
           c_bddybocs_12_avoidance) %>% ncol() %>% as.numeric()
  c_bddybocs_subset_sdq$NA_count <- c_bddybocs_subset_sdq %>% 
    select(c_bddybocs_1_time_thoughts_body_defect, c_bddybocs_2_interference_body_defectA, c_bddybocs_3_distress_body_defect,
           c_bddybocs_4_resistance_against_thoughts, c_bddybocs_5_control_over_thoughts, c_bddybocs_6_timespent, c_bddybocs_7_interference, 
           c_bddybocs_8_distress_activities, c_bddybocs_9_resistance_against_compulsions, c_bddybocs_10_control_over_behavior, c_bddybocs_11_insight, 
           c_bddybocs_12_avoidance) %>% apply(., 1, count_na)
  c_bddybocs_subset_sdq$diff <- c(c_bddybocs_subset_sdq$no_columns - c_bddybocs_subset_sdq$NA_count)
  c_bddybocs_subset_sdq <- c_bddybocs_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  c_bddybocs_subset_sdq$c_bddybocs_tot <- c_bddybocs_subset_sdq %>% 
    select(c_bddybocs_1_time_thoughts_body_defect, c_bddybocs_2_interference_body_defectA, c_bddybocs_3_distress_body_defect,
           c_bddybocs_4_resistance_against_thoughts, c_bddybocs_5_control_over_thoughts, c_bddybocs_6_timespent, c_bddybocs_7_interference, 
           c_bddybocs_8_distress_activities, c_bddybocs_9_resistance_against_compulsions, c_bddybocs_10_control_over_behavior, c_bddybocs_11_insight, 
           c_bddybocs_12_avoidance) %>% rowSums(na.rm=TRUE)
  
  c_bddybocs_subset_sdq$c_bddybocs_complete <- c_bddybocs_subset_sdq %>% 
    select(c_bddybocs_1_time_thoughts_body_defect, c_bddybocs_2_interference_body_defectA, c_bddybocs_3_distress_body_defect,
           c_bddybocs_4_resistance_against_thoughts, c_bddybocs_5_control_over_thoughts, c_bddybocs_6_timespent, c_bddybocs_7_interference, 
           c_bddybocs_8_distress_activities, c_bddybocs_9_resistance_against_compulsions, c_bddybocs_10_control_over_behavior, c_bddybocs_11_insight, 
           c_bddybocs_12_avoidance) %>% complete.cases(.)
  c_bddybocs_subset_sdq$c_bddybocs_complete[c_bddybocs_subset_sdq$c_bddybocs_complete=="FALSE"] <- "0"
  c_bddybocs_subset_sdq$c_bddybocs_complete[c_bddybocs_subset_sdq$c_bddybocs_complete=="TRUE"] <- "1"
  
  c_bddybocs_subset_clinical <- merge.default(clinical_DB_date, c_bddybocs_subset_sdq, all=TRUE) %>% 
    rename(c_bddybocs_source = source) %>% 
    group_by(Initials, c_bddybocs_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_bddybocs_date, desc(c_bddybocs_tot)) %>%
    fill(c_bddybocs_Comments:c_bddybocs_7_interferenceB, .direction = "down") %>%
    fill(c_bddybocs_Comments:c_bddybocs_7_interferenceB, .direction = "up") %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_bddybocs_source, c_bddybocs_date, 
           c_bddybocs_1_time_thoughts_body_defect, c_bddybocs_2_interference_body_defectA, c_bddybocs_2_interference_body_defectB, 
           c_bddybocs_3_distress_body_defect, c_bddybocs_4_resistance_against_thoughts, c_bddybocs_5_control_over_thoughts, 
           c_bddybocs_5_control_over_thoughtsA, c_bddybocs_6789_activities, c_bddybocs_6789_other, c_bddybocs_6_timespent, 
           c_bddybocs_7_interference, c_bddybocs_7_interferenceB, c_bddybocs_8_distress_activities, c_bddybocs_9_resistance_against_compulsions, 
           c_bddybocs_10_control_over_behavior, c_bddybocs_11_insight, c_bddybocs_12_avoidance, c_bddybocs_12_avoidanceB, c_bddybocs_Comments, 
           c_bddybocs_tot)
  
  c_bddybocs_subset_clinical$c_bddybocs_TDiff <- as.numeric(difftime(c_bddybocs_subset_clinical$Clinical_Visit_Date, c_bddybocs_subset_clinical$c_bddybocs_date, tz="", units = "days"))
  c_bddybocs_subset_clinical <- c_bddybocs_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(c_bddybocs_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, c_bddybocs_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, c_bddybocs_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  ### self report symptom checklist 
  
  s_bddybocs_list_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_bddybocs_list_')) %>% 
    distinct(., .keep_all = TRUE)

  s_bddybocs_list_subset_sdq$s_bddybocs_list_date <- as.Date(s_bddybocs_list_subset_sdq$s_bddybocs_list_date)
  s_bddybocs_list_subset_sdq$s_bddybocs_list_date <- coalesce(s_bddybocs_list_subset_sdq$s_bddybocs_list_date, s_bddybocs_list_subset_sdq$Overall_date) 
  s_bddybocs_list_subset_sdq <- s_bddybocs_list_subset_sdq %>% select(-Overall_date)
  
  s_bddybocs_list_subset_sdq[,7:ncol(s_bddybocs_list_subset_sdq)]  <- lapply(s_bddybocs_list_subset_sdq[,7:ncol(s_bddybocs_list_subset_sdq)], as.character)
  s_bddybocs_list_subset_sdq[,7:ncol(s_bddybocs_list_subset_sdq)]  <- lapply(s_bddybocs_list_subset_sdq[,7:ncol(s_bddybocs_list_subset_sdq)], FUN = function(x) recode(x, `0`="Past", `1`="Current", `2`="Both Past & Current", `77 7`="Never", .missing = NULL))
  s_bddybocs_list_subset_sdq[,7:ncol(s_bddybocs_list_subset_sdq)]  <- lapply(s_bddybocs_list_subset_sdq[,7:ncol(s_bddybocs_list_subset_sdq)], na_if, "")
  
  s_bddybocs_list_subset_sdq$no_columns <- s_bddybocs_list_subset_sdq %>% select(s_bddybocs_list_face:s_bddybocs_list_height) %>% ncol() %>% as.numeric()
  s_bddybocs_list_subset_sdq$NA_count <- s_bddybocs_list_subset_sdq %>% select(s_bddybocs_list_face:s_bddybocs_list_height) %>% apply(., 1, count_na)
  s_bddybocs_list_subset_sdq$diff <- c(s_bddybocs_list_subset_sdq$no_columns - s_bddybocs_list_subset_sdq$NA_count)
  s_bddybocs_list_subset_sdq <- s_bddybocs_list_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_bddybocs_list_subset_clinical <- merge.default(clinical_DB_date, s_bddybocs_list_subset_sdq, all=TRUE) %>% 
    rename(s_bddybocs_list_source = source) %>% 
    group_by(Initials, s_bddybocs_list_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_bddybocs_list_date) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_bddybocs_list_source, matches('s_bddybocs_list_'))
  
  s_bddybocs_list_subset_clinical$s_bddybocs_list_TDiff <- as.numeric(difftime(s_bddybocs_list_subset_clinical$Clinical_Visit_Date, s_bddybocs_list_subset_clinical$s_bddybocs_list_date, tz="", units = "days"))
  s_bddybocs_list_subset_clinical <- s_bddybocs_list_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_bddybocs_list_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_bddybocs_list_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_bddybocs_list_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)

#####
# CHoCIR - OBSESSIONAL COMPULSIVE INVENTORY-REVISED

  CHoCIR <- c('s_chocir_', 'p_chocir_')
  
  for(i in seq_along(CHoCIR)) {
    iter <- as.numeric(i)
    # iter=1
  measure_name <- CHoCIR[iter]
  
  measure_temp_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches(measure_name)) %>% 
    distinct(., .keep_all = TRUE)

  measure_temp_sdq[,7:38] <- sapply(measure_temp_sdq[,7:38], as.numeric) 
  measure_temp_sdq[,7:38] <- sapply(measure_temp_sdq[,7:38], na_if, '777') 
  
  measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches("_1_washing_hands"):matches("_38_avoiding_because_of_thoughts")) %>% ncol() %>% as.numeric()
  measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches("_1_washing_hands"):matches("_38_avoiding_because_of_thoughts")) %>% apply(., 1, count_na)
  measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
  measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)

  chocir_compulsion_symptom <- c("_1_|_2_|_3_|_4_|_5_|_6_|_7_|_8_|_9_|_10_")
  measure_temp_sdq$temp_compulsion_symptom_tot <- measure_temp_sdq %>% select(matches(chocir_compulsion_symptom)) %>% rowSums(na.rm=TRUE)
  chocir_compulsion_impairment <- c("_14_|_15_|_16_|_17_|_18_|_19_")
  measure_temp_sdq$temp_compulsion_impairment_tot <- measure_temp_sdq %>% select(matches(chocir_compulsion_impairment)) %>% rowSums(na.rm=TRUE)
  chocir_obsession_symptom <- c("_20_|_21_|_22_|_23_|_24_|_25_|_26_|_27_|_28_|_29_")
  measure_temp_sdq$temp_obsession_symptom_tot <- measure_temp_sdq %>% select(matches(chocir_obsession_symptom)) %>% rowSums(na.rm=TRUE)
  chocir_obsession_impairment <- c("_33_|_34_|_35_|_36_|_37_|_38_")
  measure_temp_sdq$temp_obsession_impairment_tot <- measure_temp_sdq %>% select(matches(chocir_obsession_impairment)) %>% rowSums(na.rm=TRUE)
  measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_symptom_tot = (temp_compulsion_symptom_tot + temp_obsession_symptom_tot), 
                                                        temp_impairment_tot = (temp_compulsion_impairment_tot + temp_obsession_impairment_tot))
  
  measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches("_1_washing_hands"):matches("_38_avoiding_because_of_thoughts")) %>% complete.cases(.)
  measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
  measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"
  
  measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
  measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
  
  measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp_sdq, all=TRUE) %>% 
    rename(measure_temp_source = source) %>% 
    group_by(Initials, date_temp) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, desc(temp_impairment_tot)) %>%
    fill(matches("_habit_1"):matches("_thought_3"), .direction = "down") %>%
    fill(matches("_habit_1"):matches("_thought_3"), .direction = "up") %>%
    ungroup() 
  
  measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
  measure_temp_clinical <- measure_temp_clinical %>% 
    mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, date_temp) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_compulsion_symptom_tot"] <- (paste0(measure_name, "compulsion_symptom_tot"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_compulsion_impairment_tot"] <- (paste0(measure_name, "compulsion_impairment_tot"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_obsession_symptom_tot"] <- (paste0(measure_name, "obsession_symptom_tot"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_obsession_impairment_tot"] <- (paste0(measure_name, "obsession_impairment_tot"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_symptom_tot"] <- (paste0(measure_name, "symptom_tot"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "temp_impairment_tot"] <- (paste0(measure_name, "impairment_tot"))
  names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
  assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
  
  }
  
#####
# CPSS - THE CHILD PTSD SYMPTOM SCALE
  
  s_cpss_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches('s_cpss_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_cpss_subset_sdq$s_cpss_date <- as.Date(s_cpss_subset_sdq$s_cpss_date)
  s_cpss_subset_sdq$s_cpss_date <- coalesce(s_cpss_subset_sdq$s_cpss_date, s_cpss_subset_sdq$Overall_date) 
  s_cpss_subset_sdq <- s_cpss_subset_sdq %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, s_cpss_date, source, s_cpss_event,
                                                    s_cpss_time_since, s_cpss_1_thoughts:s_cpss_24_happiness)
  
  s_cpss_subset_sdq[,9:ncol(s_cpss_subset_sdq)] <- sapply(s_cpss_subset_sdq[,9:ncol(s_cpss_subset_sdq)], as.numeric) 
  
  s_cpss_subset_sdq$no_columns <- s_cpss_subset_sdq %>% select(s_cpss_1_thoughts:s_cpss_24_happiness) %>% ncol() %>% as.numeric()
  s_cpss_subset_sdq$NA_count <- s_cpss_subset_sdq %>% select(s_cpss_1_thoughts:s_cpss_24_happiness) %>% apply(., 1, count_na)
  s_cpss_subset_sdq$diff <- c(s_cpss_subset_sdq$no_columns - s_cpss_subset_sdq$NA_count)
  s_cpss_subset_sdq <- s_cpss_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_cpss_subset_sdq$s_cpss_tot <- s_cpss_subset_sdq %>% select(s_cpss_1_thoughts:s_cpss_24_happiness) %>% rowSums(na.rm=TRUE)
  
  s_cpss_reexperiencing <- c("_1_|_2_|_3_|_4_|_5_")
  s_cpss_subset_sdq$s_cpss_reexperiencing_tot <- s_cpss_subset_sdq %>% select(matches(s_cpss_reexperiencing)) %>% rowSums(na.rm=TRUE)
  s_cpss_avoidance <- c("_6_|_7_|_8_|_9_|_10_|_11_|_12_")
  s_cpss_subset_sdq$s_cpss_avoidance_tot <- s_cpss_subset_sdq %>% select(matches(s_cpss_avoidance)) %>% rowSums(na.rm=TRUE)
  s_cpss_hyperarousal <- c("_13_|_14_|_15_|_16_|_17_")
  s_cpss_subset_sdq$s_cpss_hyperarousal_tot <- s_cpss_subset_sdq %>% select(matches(s_cpss_hyperarousal)) %>% rowSums(na.rm=TRUE)
  s_cpss_impairment <- c("_18_|_19_|_20_|_21_|_22_|_23_|_24_")
  s_cpss_subset_sdq$s_cpss_impairment_tot <- s_cpss_subset_sdq %>% select(matches(s_cpss_impairment)) %>% rowSums(na.rm=TRUE)
  
  s_cpss_subset_sdq$s_cpss_complete <- s_cpss_subset_sdq %>% select(matches('s_cpss_')) %>% complete.cases(.)
  s_cpss_subset_sdq$s_cpss_complete[s_cpss_subset_sdq$s_cpss_complete=="FALSE"] <- "0"
  s_cpss_subset_sdq$s_cpss_complete[s_cpss_subset_sdq$s_cpss_complete=="TRUE"] <- "1"
  
  s_cpss_subset_clinical <- merge.default(clinical_DB_date, s_cpss_subset_sdq, all=TRUE) %>% 
    rename(s_cpss_source = source) %>% 
    group_by(Initials, s_cpss_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_cpss_date, desc(s_cpss_tot), desc(s_cpss_source)) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_cpss_source, matches('s_cpss_'))
  
  s_cpss_subset_clinical$s_cpss_TDiff <- as.numeric(difftime(s_cpss_subset_clinical$Clinical_Visit_Date, s_cpss_subset_clinical$s_cpss_date, tz="", units = "days"))
  s_cpss_subset_clinical <- s_cpss_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_cpss_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_cpss_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_cpss_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
#####
# ASCQ - ADOLESCENT SOCIAL COGNITIONS QUESTIONNAIRE

  s_ascq_subset_sdq <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Overall_date, source, matches('s_ascq_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_ascq_subset_sdq[,7:ncol(s_ascq_subset_sdq)] <- sapply(s_ascq_subset_sdq[,7:ncol(s_ascq_subset_sdq)], as.numeric) 
  s_ascq_subset_sdq[,7:35]  <- lapply(s_ascq_subset_sdq[,7:35], FUN = function(x) recode(x, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
  
  s_ascq_subset_sdq$no_columns <- s_ascq_subset_sdq %>% select(matches('s_ascq_')) %>% ncol() %>% as.numeric()
  s_ascq_subset_sdq$NA_count <- s_ascq_subset_sdq %>% select(matches('s_ascq_')) %>% apply(., 1, count_na)
  s_ascq_subset_sdq$diff <- c(s_ascq_subset_sdq$no_columns - s_ascq_subset_sdq$NA_count)
  s_ascq_subset_sdq <- s_ascq_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  s_ascq_subset_sdq <- s_ascq_subset_sdq %>% filter_at(., vars(ends_with("_often")), all_vars(. <6))
  
  s_ascq_subset_sdq$s_ascq_oten_tot <- s_ascq_subset_sdq %>% select(ends_with("_often")) %>% rowMeans(na.rm=TRUE) %>% round(digits=2)
  s_ascq_subset_sdq$s_ascq_believe_tot <- s_ascq_subset_sdq %>% select(ends_with("_believe")) %>% rowMeans(na.rm=TRUE) %>% round(digits=2)
  
  s_ascq_subset_sdq$s_ascq_complete <- s_ascq_subset_sdq %>% select(s_ascq_1_speak_often:s_ascq_29_laugh_at_me_believe) %>% complete.cases(.)
  s_ascq_subset_sdq$s_ascq_complete[s_ascq_subset_sdq$s_ascq_complete=="FALSE"] <- "0"
  s_ascq_subset_sdq$s_ascq_complete[s_ascq_subset_sdq$s_ascq_complete=="TRUE"] <- "1"
  
  s_ascq_subset_sdq$s_ascq_date <- s_ascq_subset_sdq$Overall_date
  s_ascq_subset_sdq <- s_ascq_subset_sdq %>% select(-Overall_date)
  
  s_ascq_subset_clinical <- merge.default(clinical_DB_date, s_ascq_subset_sdq, all=TRUE) %>% 
    rename(s_ascq_source = source) %>% 
    group_by(Initials, s_ascq_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_ascq_date, desc(s_ascq_oten_tot)) %>%
    ungroup() %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_ascq_source, s_ascq_date, matches('s_ascq_'))
  
  s_ascq_subset_clinical$s_ascq_TDiff <- as.numeric(difftime(s_ascq_subset_clinical$Clinical_Visit_Date, s_ascq_subset_clinical$s_ascq_date, tz="", units = "days"))
  s_ascq_subset_clinical <- s_ascq_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_ascq_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_ascq_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_ascq_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
# Measures where no scoring is necessary ----------------------------------
  
  variables_no_scoring <- c('c_family_hist_', 'p_demo_eval_', 'p_demo_screen_', 'c_blood_', 's_menstruation_', 
                            's_middebrief_', 's_mar_', 's_rsdebrief_', 's_mmidebrief_', 's_medsscan_', 's_after_ba_', 
                            's_before_ba_', 's_srsors_', 'c_inpatient_ratings_', 'c_cgas_', 'c_cgi_', 's_fua_', 'p_fua_', 
                            'ksads_', 'p_dawba_bdd_', 's_dawba_bdd_', 's_medsctdb_', 'c_family_interview_')
  
  for(i in seq_along(variables_no_scoring)) {
    iter <- as.numeric(i)
    # iter=23
    measure_name <- variables_no_scoring[iter]
    print(paste("************************LOOP = ", measure_name))

    if (measure_name=="s_medsctdb_") {
      measure_temp <- ctdb_w_plusid %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, s_medsctdb_list, s_medsctdb_changes, s_medsctdb_date) %>% 
        filter(!is.na(s_medsctdb_date))
    } else if (measure_name=="c_family_interview_") {
      measure_temp <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Overall_date, source, matches(measure_name)) %>% 
        filter(!is.na(Overall_date)) %>% 
        distinct(., .keep_all = TRUE)
      measure_temp <- merge.default(measure_temp, common_identifiers_child_sib, all=TRUE) %>% 
        select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Sibling_Init, source, matches(measure_name))
    } else {
      measure_temp <- sdq_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Overall_date, source, matches(measure_name)) %>% 
        filter(!is.na(Overall_date)) %>% 
        distinct(., .keep_all = TRUE)
    }

    if (measure_name=="p_demo_eval_" | measure_name=="s_menstruation_" | measure_name=="s_middebrief_" | measure_name=="s_mar_" |
        measure_name=="s_fua_" | measure_name=="p_fua_") {
      print("creating date variable for measure")
      measure_temp$date_temp <- measure_temp$Overall_date
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
      measure_temp[,7:ncol(measure_temp)]  <- lapply(measure_temp[,7:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="p_demo_screen_") {
      print("creating date variable for measure")
      measure_temp$date_temp <- measure_temp$Overall_date
      measure_temp$p_demo_screen_date_mri <- as.Date(measure_temp$p_demo_screen_date_mri)
      measure_temp$p_demo_screen_date_ekg <- as.Date(measure_temp$p_demo_screen_date_ekg)
      measure_temp$p_demo_screen_date_eeg <- as.Date(measure_temp$p_demo_screen_date_eeg)
      measure_temp$p_demo_screen_date_ct <- as.Date(measure_temp$p_demo_screen_date_ct)
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
      fix_na_cols <- measure_temp %>% select(matches(measure_name)) %>% 
        select(-p_demo_screen_date_ekg, -p_demo_screen_date_eeg, -p_demo_screen_date_ct, -p_demo_screen_date_mri) %>% colnames()
      measure_temp[fix_na_cols]  <- lapply(measure_temp[fix_na_cols], na_if, "")
    } else if (measure_name=="s_mmidebrief_" | measure_name=="c_inpatient_ratings_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = (paste0(measure_name, "1_date")))
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
      measure_temp[,7:ncol(measure_temp)]  <- lapply(measure_temp[,7:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="ksads_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = c_ksads_date)
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
      measure_temp[,7:ncol(measure_temp)]  <- lapply(measure_temp[,7:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="c_cgi_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = (paste0(measure_name, "date")))
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp$c_cgi_date_completed_by_clinician <- as.Date(measure_temp$c_cgi_date_completed_by_clinician)
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
      measure_temp[,8:ncol(measure_temp)]  <- lapply(measure_temp[,8:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="s_medsctdb_") {
      print("date included in CTDB pull")
      measure_temp <- measure_temp %>% rename(date_temp = s_medsctdb_date)
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
      measure_temp[,7:ncol(measure_temp)]  <- lapply(measure_temp[,7:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="c_blood_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = c_blood_draw_date)
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
      measure_temp[,7:ncol(measure_temp)]  <- lapply(measure_temp[,7:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="c_family_interview_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = "c_family_interview_date")
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date)
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, Sibling_Init, date_temp, source, matches(measure_name))
      measure_temp[,8:ncol(measure_temp)]  <- lapply(measure_temp[,8:ncol(measure_temp)], na_if, "")
    } else {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = (paste0(measure_name, "date")))
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp <- measure_temp %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, date_temp, source, matches(measure_name))
      measure_temp[,7:ncol(measure_temp)]  <- lapply(measure_temp[,7:ncol(measure_temp)], na_if, "")
    }  
      
    if (measure_name=="p_demo_eval_"){
      measure_temp$p_demo_eval_6_race <- gsub("while", "white",measure_temp$p_demo_eval_6_race)
      measure_temp_manual <- manual_db_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, Overall_date, matches(measure_name)) %>% 
        rename(date_temp="Overall_date") %>% filter(!is.na(p_demo_eval_6_race) | !is.na(p_demo_eval_7_hispanic))
      measure_temp <- merge.default(measure_temp, measure_temp_manual, all=TRUE)
    }
      
    if (measure_name=="s_medsctdb_") {
      
      print("CTDB measure - no need to filter cases")
      measure_temp$s_medsctdb_list <- na_if(measure_temp$s_medsctdb_list, "N/A")
      measure_temp$s_medsctdb_list <- na_if(measure_temp$s_medsctdb_list, "n/a")
      measure_temp$s_medsctdb_list <- gsub("none", "None", measure_temp$s_medsctdb_list)
      measure_temp$s_medsctdb_list <- gsub("non", "None", measure_temp$s_medsctdb_list)
      
    } else {
      
      print("SDQ+ meausure - excluding incomplete cases")
  
      measure_temp$no_columns <- measure_temp %>% select(matches(measure_name)) %>% select(-matches("date")) %>% ncol() %>% as.numeric()
      measure_temp$NA_count <- measure_temp %>% select(matches(measure_name)) %>% select(-matches("date")) %>% apply(., 1, count_na)
      measure_temp$diff <- c(measure_temp$no_columns - measure_temp$NA_count)
      measure_temp <- measure_temp %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp$tempcomplete <- measure_temp %>% select(matches(measure_name)) %>% complete.cases(.)
      measure_temp$tempcomplete[measure_temp$tempcomplete=="FALSE"] <- "0"
      measure_temp$tempcomplete[measure_temp$tempcomplete=="TRUE"] <- "1"
      
    }
    
    # if (measure_name=="c_family_interview_") {
    #   dummy1 <- measure_temp %>% select(-Initials) %>% rename(Initials="Sibling_Init") %>% filter(!is.na(Initials))
    #   measure_temp <- measure_temp %>% select(-Sibling_Init) %>% merge.default(., dummy1, all=TRUE)
    #   rm(dummy1) 
    # }
    
    if (measure_name=="p_demo_eval_"){
      fill_names <- measure_temp %>% select(-Initials) %>% colnames()
      measure_temp <- measure_temp %>% group_by(Initials) %>% arrange(Initials, desc(source)) %>%
        fill(., names(fill_names), .direction = "down") %>% fill(., names(fill_names), .direction = "up") %>% slice(1) %>% ungroup()
    }

    if (measure_name=="s_after_ba_" | measure_name=="s_before_ba_" | measure_name=="s_srsors_" | 
        measure_name=="c_inpatient_" | measure_name=="c_cgi_" | measure_name=="s_fua_" | measure_name=="p_fua_" |
        measure_name=="p_dawba_bdd_" | measure_name=="s_dawba_bdd_" | measure_name=="c_family_interview_" ) {
      
      print("clinical measure only - not for tasks database")
      
    } else if (measure_name=="s_medsctdb_") {
      
      print("creating tasks subset")
      
      measure_temp_task <- merge.default(task_DB_date, measure_temp, all=TRUE) %>% 
        rename(measure_temp_source = source) %>% 
        group_by(Initials, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, date_temp) %>%
        ungroup() %>% 
        select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, date_temp, matches(measure_name), measure_temp_source)
      
      measure_temp_task$measurement_TDiff <- as.numeric(difftime(measure_temp_task$Task_Date, measure_temp_task$date_temp, tz="", units = "days"))
      measure_temp_task <- measure_temp_task %>% 
        mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
        group_by(Initials, Task_Name, Task_Date) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        group_by(Initials, Task_Name, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, date_temp, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        filter(measurement_TDiff_abs<=60) %>% 
        select(-measurement_TDiff_abs)
      
      names(measure_temp_task)[names(measure_temp_task) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
      names(measure_temp_task)[names(measure_temp_task) == "date_temp"] <- (paste0(measure_name, "date"))
      names(measure_temp_task)[names(measure_temp_task) == "measure_temp_source"] <- (paste0(measure_name, "source"))
      assign(paste0(measure_name, "subset_task"), measure_temp_task)
      
    } else if (measure_name=="p_demo_eval_" | measure_name=="p_demo_screen_" | measure_name=="c_family_hist_" |
               measure_name=="c_ksadsdx_" | measure_name=="ksads_") {
      
      print("creating tasks subset")
      
      measure_temp_task <- merge.default(task_DB_date, measure_temp, all=TRUE) %>% 
        rename(measure_temp_source = source) %>% 
        group_by(Initials, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, date_temp) %>%
        ungroup() %>% 
        select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, date_temp, matches(measure_name), measure_temp_source, tempcomplete)
      
      measure_temp_task$measurement_TDiff <- as.numeric(difftime(measure_temp_task$Task_Date, measure_temp_task$date_temp, tz="", units = "days"))
      measure_temp_task <- measure_temp_task %>% 
        mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
        group_by(Initials, Task_Name, Task_Date) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        select(-measurement_TDiff_abs)
      
      if (measure_name=="ksads_") {
        names(measure_temp_task)[names(measure_temp_task) == "tempcomplete"] <- (paste0("c_", measure_name, "complete"))
        names(measure_temp_task)[names(measure_temp_task) == "measurement_TDiff"] <- (paste0("c_", measure_name, "TDiff"))
        names(measure_temp_task)[names(measure_temp_task) == "date_temp"] <- (paste0("c_", measure_name, "date"))
        names(measure_temp_task)[names(measure_temp_task) == "measure_temp_source"] <- (paste0("c_", measure_name, "source"))
        assign(paste0("c_", measure_name, "subset_task"), measure_temp_task)
      } else {
        names(measure_temp_task)[names(measure_temp_task) == "tempcomplete"] <- (paste0(measure_name, "complete"))
        names(measure_temp_task)[names(measure_temp_task) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
        names(measure_temp_task)[names(measure_temp_task) == "date_temp"] <- (paste0(measure_name, "date"))
        names(measure_temp_task)[names(measure_temp_task) == "measure_temp_source"] <- (paste0(measure_name, "source"))
        assign(paste0(measure_name, "subset_task"), measure_temp_task)
      }
    
      } else {
      
      print("creating tasks subset")
      
      measure_temp_task <- merge.default(task_DB_date, measure_temp, all=TRUE) %>% 
        rename(measure_temp_source = source) %>% 
        group_by(Initials, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, date_temp) %>%
        ungroup() %>% 
        select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, date_temp, matches(measure_name), measure_temp_source, tempcomplete)
      
      measure_temp_task$measurement_TDiff <- as.numeric(difftime(measure_temp_task$Task_Date, measure_temp_task$date_temp, tz="", units = "days"))
      measure_temp_task <- measure_temp_task %>% 
        mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
        group_by(Initials, Task_Name, Task_Date) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        group_by(Initials, Task_Name, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, date_temp, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        filter(measurement_TDiff_abs<=60) %>% 
        select(-measurement_TDiff_abs)
      
      names(measure_temp_task)[names(measure_temp_task) == "tempcomplete"] <- (paste0(measure_name, "complete"))
      names(measure_temp_task)[names(measure_temp_task) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
      names(measure_temp_task)[names(measure_temp_task) == "date_temp"] <- (paste0(measure_name, "date"))
      names(measure_temp_task)[names(measure_temp_task) == "measure_temp_source"] <- (paste0(measure_name, "source"))
      assign(paste0(measure_name, "subset_task"), measure_temp_task)
    }
    
    if (measure_name=="s_middebrief_" | measure_name=="s_mar_" | measure_name=="s_rsdebrief_" | 
        measure_name=="s_mmidebrief_" | measure_name=="s_medsscan_") {
      
      print("task measure only - not for clinical database")
      
    } else if (measure_name=="s_medsctdb_") {
      
      print("creating clinical subset")
      
      measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp, all=TRUE) %>% 
        rename(measure_temp_source = source) %>% 
        group_by(Initials, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, date_temp) %>%
        ungroup() %>% 
        select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, date_temp, matches(measure_name), measure_temp_source)
      
      measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
      measure_temp_clinical <- measure_temp_clinical %>% 
        mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
        group_by(Initials, Clinical_Visit_Date) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        group_by(Initials, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        filter(measurement_TDiff_abs<=60) %>% 
        select(-measurement_TDiff_abs)
      
      names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
      names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
      names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
      assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
      
    } else if (measure_name=="p_demo_eval_" | measure_name=="p_demo_screen_" | measure_name=="c_family_hist_" |
      measure_name=="c_ksadsdx_" | measure_name=="ksads_" | measure_name=="c_family_interview_" ) {
      
      print("creating clinical subset")
      
      measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp, all=TRUE) %>% 
        rename(measure_temp_source = source) %>% 
        group_by(Initials, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, date_temp) %>%
        ungroup() %>% 
        select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, date_temp, matches(measure_name), measure_temp_source, tempcomplete)
      
      measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
      measure_temp_clinical <- measure_temp_clinical %>% 
        mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
        group_by(Initials, Clinical_Visit_Date) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        select(-measurement_TDiff_abs)
      
      if (measure_name=="ksads_") {
        names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0("c_", measure_name, "complete"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0("c_", measure_name, "TDiff"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0("c_", measure_name, "date"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0("c_", measure_name, "source"))
        assign(paste0("c_", measure_name, "subset_clinical"), measure_temp_clinical)
      } else {
        names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
        assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
      }
      
    } else {
      
      print("creating clinical subset")
      
      measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp, all=TRUE) %>% 
        rename(measure_temp_source = source) %>% 
        group_by(Initials, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, date_temp) %>%
        ungroup() %>% 
        select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, date_temp, matches(measure_name), measure_temp_source, tempcomplete)
      
      measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
      measure_temp_clinical <- measure_temp_clinical %>% 
        mutate(measurement_TDiff_abs=abs(measurement_TDiff)) %>% 
        group_by(Initials, Clinical_Visit_Date) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        group_by(Initials, date_temp) %>% 
        arrange(FIRST_NAME, LAST_NAME, Initials, date_temp, measurement_TDiff_abs) %>% 
        slice(1) %>%
        ungroup() %>% 
        filter(measurement_TDiff_abs<=60) %>% 
        select(-measurement_TDiff_abs)
      
        names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
        names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
        assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
      
      }
  } 

# Creating clical and CBT databases & exporting --------------------------

rm(measure_temp_clinical)
clinic_sets <- ls(pattern="_clinical")
clinic_sets <- c("clinical_DB", clinic_sets)
clinic_sets <- mget(clinic_sets)

# merge & tidy up

Psychometrics_treatment <- reduce(clinic_sets, full_join)
fill_names <- Psychometrics_treatment %>% select(-Initials, -Clinical_Visit_Date) %>% colnames()
# str(Psychometrics_treatment, list.len=ncol(Psychometrics_treatment))
Psychometrics_treatment <- Psychometrics_treatment %>%
  group_by(Initials, Clinical_Visit_Date) %>%
  arrange(Initials, Clinical_Visit_Date) %>% 
  fill(., names(fill_names), .direction = "down") %>%
  fill(., names(fill_names), .direction = "up") %>%
  ungroup() %>%
  group_by(Initials) %>%
  fill(., matches("p_demo_eval_"), matches("p_demo_screen_"), matches("c_family_hist_"), matches("c_ksadsdx_"), matches("c_ksads_"),
       matches("c_wasi_"), matches("s_tanner_"), matches("s_handedness_"), .direction = "down") %>%
  fill(., matches("p_demo_eval_"), matches("p_demo_screen_"), matches("c_family_hist_"), matches("c_ksadsdx_"), matches("c_ksads_"),
       matches("c_wasi_"), matches("s_tanner_"), matches("s_handedness_"), .direction = "up") %>%
  ungroup() %>% 
  distinct(., .keep_all = TRUE)

############# CBT subset

# selecting columns 

cbt_columns <- read_excel(paste0(database_location, "other_data_never_delete/names_cbt_datebase.xlsx"))
CBT_report <- Psychometrics_treatment %>% 
  select(cbt_columns$select, matches("s_fua_"), matches("p_fua_"), matches("s_before_ba_"), matches("s_after_ba_"), matches("s_baexpout_"),
         s_after_ba_sess_come_again, matches("s_menstruation_"), matches("s_medsctdb_"), matches("c_medsclin_"), -matches("_TDiff"), -matches("_complete"), 
         -matches("_source")) %>% arrange(LAST_NAME, Initials, FIRST_NAME, Clinical_Visit_Date) %>%
  filter(str_detect(Clinical_Visit_Code, "o")) %>% select(-s_fua_date, -p_fua_date, -s_before_ba_date, -s_after_ba_date, -s_baexpout_date, -s_menstruation_date, -c_medsclin_date)

# insert code to filter out future dates
CBT_report$TDiff <- as.numeric(difftime(CBT_report$Clinical_Visit_Date, todays_date_formatted, tz="", units = "days"))
CBT_report <- CBT_report %>% filter(TDiff<1) %>% select(-TDiff)

CBT_report$s_mfq1w_tot <- coalesce(CBT_report$s_mfq1w_tot, CBT_report$s_mfq_tot)
CBT_report$s_ari1w_tot <- coalesce(CBT_report$s_ari1w_tot, CBT_report$s_ari6m_tot)
CBT_report$p_mfq1w_tot <- coalesce(CBT_report$p_mfq1w_tot, CBT_report$p_mfq_tot)
CBT_report$p_ari1w_tot <- coalesce(CBT_report$p_ari1w_tot, CBT_report$p_ari6m_tot)
CBT_report$p_mfq1w_parent <- coalesce(CBT_report$p_mfq1w_parent, CBT_report$p_mfq_parent)
CBT_report$p_ari1w_parent <- coalesce(CBT_report$p_ari1w_parent, CBT_report$p_ari6m_parent)
CBT_report <- CBT_report %>% select(-matches("_ari6m_"), -matches("_mfq_"))

# CBT_report2 <- CBT_report2 %>% group_by(Initials) %>% slice(c(1, n())) %>% ungroup()

# recoding & calculating variables

CBT_report$Eligible <- recode(CBT_report$Eligible, "0"="Include", "5"="Excluded: does not meet criteria",
                              "6"="Excluded: meets exclusionary criteria (substance use, psychosis, etc.)",
                              "7"="Did not or withdrew assent/consent", "8"="Ruled as ineligible for treatment during baseline assessment (didn't meet inclusionary or met exclusionary criteria)",
                              "9"="Patient (or parent) withdrew from treatment", "10"="Excluded after commencing treatment: some treatment received before participant was later excluded (e.g. bad scanner, now meets exclusionary criteria, etc.)",
                              "11"="Completed treatment", .missing = NULL)

parent_report <- select(CBT_report, matches("_parent")) %>% colnames()
CBT_report[parent_report] <- lapply(CBT_report[parent_report], as.character)
CBT_report[parent_report] <- lapply(CBT_report[parent_report], FUN = function(x) recode(x, "0"="Other", "1"="Mother", "2"="Father", .missing = "Unknown"))
parent_report <- parent_report[parent_report != "p_fad_parent"]

parent_col_spread <- CBT_report %>% select(Initials, Clinical_Visit_Date, Clinical_Visit_Type, Clinical_Visit_Code, Clinical_Visit_Number, 
                                           matches("p_mfq"), matches("p_ari"), matches("p_scared")) %>% distinct(., .keep_all = TRUE)
dummy <- tibble(Initials= c("DUMMY"), p_mfq1w_parent= c("Father", "Mother", "Unknown"), p_scared_parent= c("Father", "Mother", "Unknown"), p_ari1w_parent= c("Father", "Mother", "Unknown"))
parent_col_spread <- merge.default(parent_col_spread, dummy, all=TRUE)

for(q in seq_along(parent_report)) {
  iter6 <- as.numeric(q)
  # iter6=1
  parent_measure <- c(parent_report[iter6])
  parent_measure_tot <- gsub("parent", "tot", parent_measure, fixed=TRUE)

  parent_col_spread <- parent_col_spread %>% spread(eval(parse(text=parent_measure)), eval(parse(text=parent_measure_tot)), fill=NA)

  father_report <- gsub("p_", "p_f_", parent_measure_tot, fixed=TRUE)
  mother_report <- gsub("p_", "p_m_", parent_measure_tot, fixed=TRUE)
  unknown_report <- gsub("p_", "p_u_", parent_measure_tot, fixed=TRUE)
  names(parent_col_spread)[names(parent_col_spread) == "Father"] <- (paste0(father_report))
  names(parent_col_spread)[names(parent_col_spread) == "Mother"] <- (paste0(mother_report))
  names(parent_col_spread)[names(parent_col_spread) == "Unknown"] <- (paste0(unknown_report))
}

CBT_report <- merge.default(CBT_report, parent_col_spread, all=TRUE) %>% filter(Initials!="DUMMY")

ba_rating_columns <- CBT_report %>% select(matches("s_before_ba_"), matches("s_after_ba_")) %>% colnames()
ba_rating_columns <- ba_rating_columns[ba_rating_columns != "s_before_ba_clinician_name"]
ba_rating_columns <- ba_rating_columns[ba_rating_columns != "s_after_ba_clinician_name"]

CBT_report[ba_rating_columns] <- lapply(CBT_report[ba_rating_columns], as.numeric)
CBT_report <- CBT_report %>% mutate(s_ba_sess_mood_diff = (s_after_ba_mood - s_before_ba_mood), s_ba_sess_difficulty_diff = (s_after_ba_difficult - s_before_ba_difficult),
                                    s_ba_sess_enjoy_diff = (s_after_ba_enjoy - s_before_ba_enjoy), s_ba_sess_anxiety_diff = (s_after_ba_anx - s_before_ba_anx),
                                    s_ba_sess_satisfaction_diff = (s_after_ba_sat - s_before_ba_sat)
                                    # s_ba_week_enjoy_diff = (s_after_ba_week_expected_enjoyment - s_before_ba_week_actual_enjoyment), # cannot include this until I resolve how I will have to
                                    # take the expected weekly enjoyment from the previous week from the actual enjoyment of the present week - i.e. 1 row previous
                                )

CBT_report <- CBT_report %>% select(Initials, FIRST_NAME:Eligible, Clinical_Visit_Date:Clinical_Visit_Number, Scheduling_status:c_ksadsdx_ongoing_comorbid_combined, matches("_mfq"), 
                                    matches("_scared"), matches("_ari"), s_shaps_tot:s_rumination_tot, matches("s_fua_"), matches("p_fua_"), matches("s_before_ba_"), 
                                    matches("s_after_ba_"), matches("_ba_sess_"), matches("s_baexpout_"), matches("s_menstruation_"), matches("s_medsctdb_"), matches("c_medsclin_")) %>%
  select(-matches("s_baexpout_act_3_"), -matches("s_baexpout_act_4_"), -matches("s_baexpout_act_5_")) %>% 
  arrange(LAST_NAME, Initials, Clinical_Visit_Date)

parent_report <- select(CBT_report, matches("_parent")) %>% colnames()
CBT_report[parent_report] <- lapply(CBT_report[parent_report], na_if, "Unknown")

############# Inpatient subset

# selecting columns

inpatient_columns <- read_excel(paste0(database_location, "other_data_never_delete/names_inpatient_datebase.xlsx"))
MATCH_tracker <- Psychometrics_treatment %>% select(inpatient_columns$select, matches("c_medsclin_"), -matches("_TDiff"), -matches("_complete"),
                                                 -matches("_source")) %>% arrange(LAST_NAME, Initials, FIRST_NAME, Clinical_Visit_Date) %>%
  filter(Clinical_Visit_Code=="i") %>% select(-c_medsclin_date)

# insert code to filter out future dates
MATCH_tracker$TDiff <- as.numeric(difftime(MATCH_tracker$Clinical_Visit_Date, todays_date_formatted, tz="", units = "days"))
MATCH_tracker <- MATCH_tracker %>% filter(TDiff<1) %>% select(-TDiff)

MATCH_tracker$s_mfq1w_tot <- coalesce(MATCH_tracker$s_mfq1w_tot, MATCH_tracker$s_mfq_tot)
MATCH_tracker$s_ari1w_tot <- coalesce(MATCH_tracker$s_ari1w_tot, MATCH_tracker$s_ari6m_tot)
MATCH_tracker$p_mfq1w_tot <- coalesce(MATCH_tracker$p_mfq1w_tot, MATCH_tracker$p_mfq_tot)
MATCH_tracker$p_ari1w_tot <- coalesce(MATCH_tracker$p_ari1w_tot, MATCH_tracker$p_ari6m_tot)
MATCH_tracker <- MATCH_tracker %>% select(-matches("_ari6m_"), -matches("_mfq_"))

# recoding & calculating variables

MATCH_tracker$Eligible <- recode(MATCH_tracker$Eligible, "0"="Include", "5"="Excluded: does not meet criteria",
                              "6"="Excluded: meets exclusionary criteria (substance use, psychosis, etc.)",
                              "7"="Did not or withdrew assent/consent", "8"="Ruled as ineligible for treatment during baseline assessment (didn't meet inclusionary or met exclusionary criteria)",
                              "9"="Patient (or parent) withdrew from treatment", "10"="Excluded after commencing treatment: some treatment received before participant was later excluded (e.g. bad scanner, now meets exclusionary criteria, etc.)",
                              "11"="Completed treatment", .missing = NULL)
############# exporting

# Psychometrics_treatment <- Psychometrics_treatment %>% select(-FIRST_NAME_P1, -LAST_NAME_P1, -FIRST_NAME_P2, -LAST_NAME_P2)
Psychometrics_treatment %>% write_xlsx(paste0(database_location, "MASTER_DATABASE_CLINICAL.xlsx"))
Psychometrics_treatment %>% write_xlsx(paste0(folder_backup, "MASTER_DATABASE_CLINICAL_", todays_date_formatted, ".xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(database_location), pattern = "^MASTER_DATABASE_CLINICAL.xlsx", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(database_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'MASTER_DATABASE_CLINICAL'")
} else {
  print("Conflict: exporting as 'MASTER_DATABASE_CLINICAL_updated'")
  Psychometrics_treatment %>% write_xlsx(paste0(database_location,"MASTER_DATABASE_CLINICAL_updated.xlsx"))
}

CBT_report %>% write_xlsx(paste0(CBT_location, "MASTER_DATABASE_CBT.xlsx"))
CBT_report %>% write_xlsx(paste0(CBT_backup, "MASTER_DATABASE_CBT_", todays_date_formatted, ".xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(CBT_location), pattern = "^MASTER_DATABASE_CBT.xlsx", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(CBT_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'MASTER_DATABASE_CBT'")
} else {
  print("Conflict: exporting as 'MASTER_DATABASE_CBT_updated'")
  CBT_report %>% write_xlsx(paste0(CBT_location, "MASTER_DATABASE_CBT_updated.xlsx"))
}

MATCH_tracker %>% write_xlsx(paste0(inpatient_location, "MASTER_DATABASE_Inpatient.xlsx"))
MATCH_tracker %>% write_xlsx(paste0(inpatient_backup, "MASTER_DATABASE_Inpatient_", todays_date_formatted, ".xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(inpatient_location), pattern = "^MASTER_DATABASE_Inpatient.xlsx", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(inpatient_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'MASTER_DATABASE_Inpatient'")
} else {
  print("Conflict: exporting as 'MASTER_DATABASE_Inpatient_updated'")
  MATCH_tracker %>% write_xlsx(paste0(inpatient_location,"MASTER_DATABASE_Inpatient_updated.xlsx"))
}

# Creating tasks database & exporting ------------------------------------

rm(max_tasks, measure_temp_task)
task_sets <- ls(pattern="_task")
task_sets <- c("task_DB", task_sets)
task_sets <- mget(task_sets)

# merge & tidy up

Psychometrics_behav <- reduce(task_sets, full_join)
fill_names <- Psychometrics_behav %>% select(-Initials, -Task_Name, -Task_Date, -Task_Number) %>% colnames()
# str(Psychometrics_behav, list.len=ncol(Psychometrics_behav))
Psychometrics_behav <- Psychometrics_behav %>%
  group_by(Initials, Task_Name, Task_Date, Task_Number) %>%
  arrange(Initials, Task_Name, Task_Date) %>% 
  fill(., names(fill_names), .direction = c("down")) %>%
  fill(., names(fill_names), .direction = c("up")) %>%
  ungroup() %>%
  group_by(Initials) %>%
  fill(., matches("p_demo_eval_"), matches("p_demo_screen_"), matches("c_family_hist_"), matches("c_ksadsdx_"), matches("c_ksads_"),
       matches("c_wasi_"), matches("s_tanner_"), matches("s_handedness_"), .direction = "down") %>%
  fill(., matches("p_demo_eval_"), matches("p_demo_screen_"), matches("c_family_hist_"), matches("c_ksadsdx_"), matches("c_ksads_"),
       matches("c_wasi_"), matches("s_tanner_"), matches("s_handedness_"), .direction = "up") %>%
  ungroup() %>%
  distinct(., .keep_all = TRUE)

# exporting

Psychometrics_behav %>% write_xlsx(paste0(database_location, "MASTER_DATABASE_BEHAVIOURAL.xlsx"))
Psychometrics_behav %>% write_xlsx(paste0(database_location, "Backup/MASTER_DATABASE_BEHAVIOURAL_", todays_date_formatted, ".xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(database_location), pattern = "^MASTER_DATABASE_BEHAVIOURAL.xlsx", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(database_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'MASTER_DATABASE_BEHAVIOURAL'")
} else {
  print("Conflict: exporting as 'MASTER_DATABASE_BEHAVIOURAL_updated'")
  Psychometrics_behav %>% write_xlsx(paste0(database_location,"MASTER_DATABASE_BEHAVIOURAL_updated.xlsx"))
}

# Identifying missing cases -----------------------------------------------

# check <- Psychometrics_behav %>% 
#   filter((is.na(s_mfq_tot) & is.na(s_mfq1w_tot)) | (is.na(p_mfq_tot) & is.na(p_mfq1w_tot)) | 
#     (is.na(s_ari1w_tot) & is.na(s_ari6m_tot)) | (is.na(p_ari1w_tot) & is.na(p_ari6m_tot)) |
#            is.na(s_scared_tot) |is.na(p_scared_tot) |is.na(s_shaps_tot) |is.na(s_lsas_tot)) %>% 
#   select(FIRST_NAME:Include, s_mfq_tot, p_mfq_tot, s_ari1w_tot, p_ari1w_tot,
#          s_scared_tot, p_scared_tot, s_shaps_tot, s_lsas_tot) %>% 
#   filter(Task_Number !="777" & Task_Number !="999") %>% filter(Task_Name != "Resting_state_scan")
# 
# list_names <- check %>% select(FIRST_NAME, LAST_NAME, Initials) %>% distinct(., .keep_all = TRUE)
# list_names$assigned_irta <- c(as.character(rep("SK", 101)), as.character(rep("LG", 101)))
# check <- merge.default(check, list_names, all=TRUE)
# check %>% write_xlsx(paste0(database_location, "MASTER_DATABASE_task_missing.xlsx"))

# Removing unnecessary variables ------------------------------------------

rm(list=ls(pattern="subset"))
rm(list=ls(pattern="clinical"))
rm(list=ls(pattern="task"))
rm(list=ls(pattern="iter"))
rm(list=ls(pattern="manual"))
rm(list=ls(pattern="common_identifiers"))
rm(list=ls(pattern="data_"))
rm(list=ls(pattern="parent_"))
rm(measure_temp_combined, tot_sum, s_shaps_binary, imported_imputed_mfqs, gen_functioning, hand_columns, father_report, mother_report, column, 
   measure_name, panic_subscale, sep_subscale, social_subscale, gad_subscale, school_subscale, j, subscale_name, i, lsas_performance, lsas_social, hand_column_name, e, d,
   fix_var, remove_unknown, variables_no_scoring, CASE, CHoCIR, activities, activity_no, behav_control, c_snap_hyperactivity, ba_rating_columns, unknown_report,
   c_snap_inattention, comminication, chocir_compulsion_impairment, chocir_compulsion_symptom, chocir_obsession_impairment, chocir_obsession_symptom, sdq_columns, 
   affective_response, FAD, fad_normal, fad_reverse, if_column_name, if_columns, p_fasa_modification, p_fasa_distress, p_fasa_participation, sdq_w_names, sdq_dates, 
   s_cpss_avoidance, s_cpss_hyperarousal, s_cpss_impairment, s_cpss_reexperiencing, s_seq_academic, s_seq_emotional, s_seq_social, how_column_name, 
   how_columns, roles, tot_sum_clin, problem_solving, scared, scared_subscales, cbt_columns, inpatient_columns, clinic_sets, combined, comorbid, file_save_check_time, 
   measure_temp, parent, child, p, c, q, incorrect, correct, old_dx_temp, old_ksads_checklist, old_mdd_form, dummy, imputed_mfqs, temp_before, file_save_check, 
   file_save_check_combined, ctdb_columns, ctdb_Data_Download_reduced, ctdb_dates, ctdb_names, ctdb_numeric, ctdb_w_plusid, ctdb_w_plusid_child, ctdb_w_plusid_parent, 
   ctdb_w_plusid_parent1, ctdb_w_plusid_parent2, measure_temp_ctdb, c_medsclin_sdq, measure_temp_sdq, fill_names, fix_na_cols, c_medsclin1yr_sdq)
rm(SDQ_Data_Download_raw, SDQ_Data_Download, CTDB_Data_Download)
