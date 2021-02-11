  
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
  
  # LE adding check for latest SDQ pull file size, accessibility, and readability, print warning message if not downloaded correctly 

  sdq_file_size <- file.size(paste0(sdq_pull, latest_sdq_pull, ".txt"))
  if(sdq_file_size == 0){
    warning("The latest SDQ pull was not downloaded correctly, issue with file size")
  }
  
  if(file.access(paste0(sdq_pull, latest_sdq_pull, ".txt"), mode = 0) != 0){
    warning("The lastest SDQ pull was not downloaded correctly, issue with file accessibility")
  }
  
  if(file.access(paste0(sdq_pull, latest_sdq_pull, ".txt"), mode = 4) != 0){
    warning("The lastest SDQ pull was not downloaded correctly, issue with reading file")
  }
  
  # Read in SDQ data from latest pull

  SDQ_Data_Download_raw <- read.delim(paste0(sdq_pull, latest_sdq_pull, ".txt"),  quote="",  
                                    encoding="UTF-8", row.names = NULL, header = TRUE, stringsAsFactors = FALSE) %>% mutate_all(as.character)
  
  #****** adding special cases
  
  # LE Check for files in the other_data_never_delete directory
  
  never_delete_location <- paste0(database_location, 'other_data_never_delete/')
  never_delete_files <- list.files(never_delete_location)
  
  for (filename in never_delete_files){
    if(file.size(paste0(never_delete_location, filename)) == 0){
      warning("At least one file in the other_data_never_delete directory was not downloaded correctly, issue with file size")
    }
    if(file.access(paste0(never_delete_location, filename), mode = 0) != 0){
      warning("At least one file in the other_data_never_delete directory was not downloaded correctly, issue with file accessibility")
    }
    if(file.access(paste0(never_delete_location, filename), mode = 4) != 0){
      warning("At least one file in the other_data_never_delete directory was not downloaded correctly, issue with reading file")
    }
  }
  
  SDQ_Data_Download_raw <- SDQ_Data_Download_raw %>% filter(PlusIID != "4711-5358-6649-5157")
  SDQ_Data_Download_raw <- SDQ_Data_Download_raw %>% filter(PlusIID != "8768-8233-7459-5808")
  SDQ_Data_Download_raw <- SDQ_Data_Download_raw %>% filter(PlusIID != "2738-0093-0639-3598")
  imported_data_23495 <- read_excel(paste0(database_location, 'other_data_never_delete/4711-5358-6649-5157_23495_pull_03222019_01-M-0192.xlsx')) %>% 
    mutate_all(as.character) # this is the data we pulled from SDQ for the participant who signed into 0192 as an adult
  imported_data_23544 <- read_excel(paste0(database_location, 'other_data_never_delete/8768-8233-7459-5808_23544_pull_05222019_02-M-0021.xlsx')) %>% 
    mutate_all(as.character)
  imported_data_22279 <- read_excel(paste0(database_location, 'other_data_never_delete/2738-0093-0639-3598_22279_pull_07242019_02-M-0186.xlsx')) %>% 
    mutate_all(as.character)
  imported_hyphen_issue <- read_excel(paste0(database_location, 'other_data_never_delete/data_plusids_without_hyphens.xlsx')) %>% mutate_all(as.character)
  
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, imported_data_23495, all=TRUE)
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, imported_data_23544, all=TRUE)
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, imported_data_22279, all=TRUE)
  SDQ_Data_Download_raw <- merge.default(SDQ_Data_Download_raw, imported_hyphen_issue, all=TRUE)
  
  SDQ_Data_Download_raw$Overall_date <- as.Date(SDQ_Data_Download_raw$modedate, "%Y-%m-%d")

  # changing column names

  sdq_columns <- read_excel(paste0(database_location, "other_data_never_delete/sdq_column_names_and_descriptions.xlsx"))
  # setnames(SDQ_Data_dateadded_fixed, old=c(sdq_columns$old_name), new=c(sdq_columns$new_name), skip_absent=TRUE)
  setnames(SDQ_Data_Download_raw, old=c(sdq_columns$old_name), new=c(sdq_columns$new_name))
  
  SDQ_Data_Download <- SDQ_Data_Download_raw %>% select(sdq_columns$new_name) %>% arrange(PLUSID, Overall_date) %>% select(-SID)
  
  sdq_dates <- SDQ_Data_Download %>% select(matches("_date")) %>% select(-Overall_date) %>% colnames()
  SDQ_Data_Download[sdq_dates] <- lapply(SDQ_Data_Download[sdq_dates], as.Date, "%d-%m-%Y")

  #****** old dx checklist & mdd form (from before these were combined on sdq & the mdd form removed)
  
  old_mdd_form <- read_excel(paste0(database_location, "other_data_never_delete/diagnosis_old_mdd_updated_comorbid.xlsx")) %>% 
    select(-starts_with("x"), -Entry_date, -SDAN, -Initials) %>% mutate_all(as.character)
  old_mdd_form$Overall_date <- as.Date(old_mdd_form$Overall_date)
  date_variables <- c("c_ksadsdx_date", "c_ksadsdx_epset_baseline_visit_date")
  old_mdd_form[date_variables] <- lapply(old_mdd_form[date_variables], as.Date, "%d-%m-%Y")
  SDQ_Data_Download <- merge.default(SDQ_Data_Download, old_mdd_form, all=TRUE)

  #****** CTDB = where data is stored for those not in 0037
  
  # LE adding check for latest CTDB pull file size, accessibility, and readability, print warning message if not downloaded correctly 
  
  ctdb_file_size <- file.size(paste0(ctdb_pull, latest_ctdb_pull, ".xlsx"))
  if(ctdb_file_size == 0){
    warning("The latest CTDB pull was not downloaded correctly, issue with file size")
  }
  
  if(file.access(paste0(ctdb_pull, latest_ctdb_pull, ".xlsx"), mode = 0) != 0){
    warning("The lastest CTDB pull was not downloaded correctly, issue with file accessibility")
  }
  
  if(file.access(paste0(ctdb_pull, latest_ctdb_pull, ".xlsx"), mode = 4) != 0){
    warning("The lastest CTDB pull was not downloaded correctly, issue with reading file")
  }
  
  # Read in CTDB data from latest pull
  
  CTDB_Data_Download <- read_excel(paste0(ctdb_pull, latest_ctdb_pull, ".xlsx"), sheet = 'Data_and_Scores') %>% mutate(source = "CTDB")
  
  # changing column names
  
  ctdb_columns <- read_excel(paste0(database_location, "other_data_never_delete/ctdb_column_names_and_descriptions.xlsx"))
  #setnames(CTDB_Data_Download, old=c(ctdb_columns$old_name), new=c(ctdb_columns$new_name), skip_absent=TRUE)
  setnames(CTDB_Data_Download, old=c(ctdb_columns$old_name), new=c(ctdb_columns$new_name))
  
  CTDB_Data_Download$Overall_date <- as.Date(CTDB_Data_Download$Overall_date, tz="", "%Y-%m-%d")
  CTDB_Data_Download <- CTDB_Data_Download %>% select(ctdb_columns$new_name) %>% arrange(FIRST_NAME, LAST_NAME, Overall_date) 
  
  # Fixing names of some participants
  
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
  
  # clean up
  
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

  #****** Manual entry database
  
  # LE adding check for manual entry database file size, accessibility, and readability, print warning message if not downloaded correctly 
  
  if(file.size(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx")) == 0){
    warning("The manual entry database was not downloaded correctly, issue with file size")
  }
  
  if(file.access(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), mode = 0) != 0){
    warning("The manual entry database was not downloaded correctly, issue with file accessibility")
  }
  
  if(file.access(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), mode = 4) != 0){
    warning("The manual entry database was not downloaded correctly, issue with reading file")
  }
  
  # Read in manual entry database
  
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
  manual_ksads <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "KSADS_Depression", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date")  %>% mutate_all(as.character)
  manual_bdd <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "Eval BDD screen", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date")  %>% mutate_all(as.character)
  manual_medication <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "Clinician medication forms", skip=2) %>% 
    select(-starts_with("x"), -Entry_date, -matches("c_medsclin1yr_")) %>% rename(Overall_date = "Measure_date")  %>% mutate_all(as.character)
  manual_ascq <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "ASCQ", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(Overall_date = "Measure_date") %>% mutate_all(as.character)
  
  manual_ethnicity <- read_excel(paste0(database_location, "other_data_never_delete/ethnicity_list_KCs_CR_tracker.xlsx")) %>% mutate_all(as.character)
  
  manual_sets <- ls(pattern="manual_")
  manual_sets <- mget(manual_sets)
  manual_combined <- reduce(manual_sets, full_join)
  fill_names <- manual_combined %>% select(-Initials, -Overall_date) %>% colnames()
  manual_combined <- manual_combined %>% group_by(Initials, Overall_date) %>% 
    fill(., all_of(fill_names), .direction = "down") %>% fill(., all_of(fill_names), .direction = "up") %>%
    # fill(., fill_names, .direction = "down") %>% fill(., fill_names, .direction = "up") %>%
    ungroup() %>% distinct(., .keep_all = TRUE) %>% mutate(source = "MANUAL")
  
  date_variables <- manual_combined %>% select(matches("_date")) %>% select(-Overall_date) %>% colnames()
  manual_combined$Overall_date <- as.Date(manual_combined$Overall_date)
  manual_combined[date_variables] <- lapply(manual_combined[date_variables], as.Date, "%d-%m-%Y")
  
  # Imputed MFQ data
  
  imported_imputed_mfqs <- read_excel(paste0(database_location, 'other_data_never_delete/imputed_mfq_06172020.xlsx')) %>% 
    mutate(source = "IMPUTED") %>% select(-s_mfq_date, -p_mfq_date)
  imported_imputed_scareds <- read_excel(paste0(database_location, 'other_data_never_delete/imputed_scared_07022020.xlsx')) %>% 
    mutate(source = "IMPUTED") %>% select(-s_scared_date, -p_scared_date, -s_scared_source, -p_scared_source)
  imported_imputed_aris <- read_excel(paste0(database_location, 'other_data_never_delete/imputed_ari_07022020.xlsx')) %>% 
    mutate(source = "IMPUTED") %>% select(-s_ari1w_date, -p_ari1w_date, -s_ari1w_source, -p_ari1w_source)
  imported_imputed_shaps <- read_excel(paste0(database_location, 'other_data_never_delete/imputed_shaps_07022020.xlsx')) %>% 
    mutate(source = "IMPUTED") %>% select(-s_shaps_date, -s_shaps_source)
  imported_imputed_lsas <- read_excel(paste0(database_location, 'other_data_never_delete/imputed_lsas_07022020.xlsx')) %>% 
    mutate(source = "IMPUTED") %>% select(-s_lsas_date, -s_lsas_source)
  
  imported_imputed_mfqs$Overall_date <- as.Date(imported_imputed_mfqs$Overall_date)
  imported_imputed_scareds$Overall_date <- as.Date(imported_imputed_scareds$Overall_date)
  imported_imputed_aris$Overall_date <- as.Date(imported_imputed_aris$Overall_date)
  imported_imputed_shaps$Overall_date <- as.Date(imported_imputed_shaps$Overall_date)
  imported_imputed_lsas$Overall_date <- as.Date(imported_imputed_lsas$Overall_date)
  
  manual_combined <- merge.default(manual_combined, imported_imputed_mfqs, all=TRUE) %>% merge.default(., imported_imputed_scareds, all=TRUE) %>% 
    merge.default(., imported_imputed_aris, all=TRUE) %>% merge.default(., imported_imputed_shaps, all=TRUE) %>% merge.default(., imported_imputed_lsas, all=TRUE)
  
  # Daily MFQ
  
  manual_daily_mfq <- read_excel(paste0(database_location, "other_data_never_delete/inpatient_daily_mfq_manual_entry_2020-03-03.xlsx")) %>% 
    mutate_all(as.character) %>% mutate(source = "MANUAL") 
  manual_daily_mfq$s_mfq1d_date <- as.Date(manual_daily_mfq$s_mfq1d_date)
  manual_daily_mfq$s_mfq1d_time <- as.ITime(manual_daily_mfq$s_mfq1d_time)
  
  # DAWBA suicide manual entry 
  
  manual_dawba_suicide <- read_excel(paste0(database_location, "other_data_never_delete/dawba_suicide_manual_entry_2020-03-16.xlsx")) %>% 
    mutate_all(as.character) %>% rename(date_temp="s_suicide_date") %>% mutate(source = "MANUAL")
  manual_dawba_suicide$date_temp <- as.Date(manual_dawba_suicide$date_temp)
  
  # Tic disorder measure

  manual_ygtss <- read_excel(paste0(database_location, "Manual data entry/MANUAL_ENTRY_DATABASE.xlsx"), sheet = "YGTSS Tics", skip=2) %>% 
    select(-starts_with("x"), -Entry_date) %>% rename(c_ygtss_date = "Measure_date") %>% mutate_all(as.character) %>% mutate(c_ygtss_source = "MANUAL") 
  manual_ygtss$c_ygtss_date <- as.Date(manual_ygtss$c_ygtss_date)
  
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
    filter(!is.na(Sibling_Init) | !is.na(Sibling_Type)) %>% distinct(., .keep_all = TRUE) %>% select(PLUSID, Initials, Sibling_Init)
  
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
    select(PLUSID, SDAN, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches("_mfq"), matches("_ari"), matches("_scared"), matches("_medsctdb"))

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
    select(PLUSID, SDAN, FIRST_NAME, LAST_NAME, Initials, Protocol_CTDB, source, matches("p_mfq"), matches("p_ari"), matches("p_scared"), matches("_medsctdb"))

  # joining parent and child report 
  
  ctdb_w_plusid <- merge.default(ctdb_w_plusid_child, ctdb_w_plusid_parent_reduced, all=TRUE)
  
  #****** 
  
  sdq_w_names <- left_join(common_identifiers_child, SDQ_Data_Download, all=TRUE) %>% 
    mutate(source = "SDQ") %>% filter(!is.na(Overall_date))
  
  #****** 
  
  manual_db_w_names <- merge.default(common_identifiers_child, manual_combined, all=TRUE) %>% group_by(Initials) %>% 
    fill(., PLUSID, SDAN, FIRST_NAME, LAST_NAME, .direction = "down") %>% fill(., PLUSID, SDAN, FIRST_NAME, LAST_NAME, .direction = "up") %>% 
    ungroup() %>% filter(!is.na(Overall_date))
  
  manual_suicide_w_names <- left_join(common_identifiers_child, manual_dawba_suicide, all=TRUE) %>% filter(!is.na(date_temp))
  
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
  
  clinical_DB_date_before_covid <- clinical_DB_date %>% filter(Clinical_Visit_Date < "2020-03-16")
  clinical_DB_date_since_covid <- clinical_DB_date %>% filter(Clinical_Visit_Date >= "2020-03-16")
  task_DB_date_before_covid <- task_DB_date %>% filter(Task_Date < "2020-03-16")
  task_DB_date_since_covid <- task_DB_date %>% filter(Task_Date >= "2020-03-16")

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
    print(paste("************************LOOP = ", measure_name))
    
      measure_temp_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name)) %>% 
        filter(!is.na(Overall_date)) %>% 
        distinct(., .keep_all = TRUE)
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff) 
      
      measure_temp_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name))
      
      measure_temp_manual$no_columns <- measure_temp_manual %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% ncol() %>% as.numeric()
      measure_temp_manual$NA_count <- measure_temp_manual %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% apply(., 1, count_na)
      measure_temp_manual$diff <- c(measure_temp_manual$no_columns - measure_temp_manual$NA_count)
      measure_temp_manual <- measure_temp_manual %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq <- merge.default(measure_temp_sdq, measure_temp_manual, all=TRUE) %>% 
        group_by(Initials, Overall_date) %>% arrange(Initials, Overall_date, source) %>% slice(1) %>% ungroup()
    
    if (measure_name=="p_mfq_" | measure_name=="p_mfq1w_" | measure_name=="p_ari1w_" | measure_name=="p_ari6m_") {
      measure_temp_sdq[,7:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,7:ncol(measure_temp_sdq)], as.numeric)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% rowSums(na.rm=TRUE)
    } else {
      measure_temp_sdq[,5:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,5:ncol(measure_temp_sdq)], as.numeric)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% rowSums(na.rm=TRUE)
    }
    
    if (measure_name=="s_ari1w_") {measure_temp_sdq <- measure_temp_sdq %>% mutate(temptotal=(temptotal-s_ari1w_7_impairment))
    } else if (measure_name=="s_ari6m_") {measure_temp_sdq <- measure_temp_sdq %>% mutate(temptotal=(temptotal-s_ari6m_7_impairment))
    } else if (measure_name=="p_ari1w_") {measure_temp_sdq <- measure_temp_sdq %>% mutate(temptotal=(temptotal-p_ari1w_7_impairment))
    } else if (measure_name=="p_ari6m_") {measure_temp_sdq <- measure_temp_sdq %>% mutate(temptotal=(temptotal-p_ari6m_7_impairment))
    } else {measure_temp_sdq <- measure_temp_sdq}  
    
    measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-matches("_parent")) %>% complete.cases(.)
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"

    if (measure_name=="s_mfq1w_") {
      measure_temp_sdq[,5:17] <- sapply(measure_temp_sdq[,5:17], replace_na, '-9')
      measure_temp_sdq <- measure_temp_sdq %>% 
        filter(s_mfq1w_1_unhappy<4 & s_mfq1w_2_didnt_enjoy<4 & s_mfq1w_3_tired<4 & s_mfq1w_4_restless<4 &
                 s_mfq1w_5_no_good<4 & s_mfq1w_6_cried<4 & s_mfq1w_7_hard_think<4 & s_mfq1w_8_hate_myself<4 &
                 s_mfq1w_9_bad_person<4 & s_mfq1w_10_lonely<4 & s_mfq1w_11_nobody_love<4 & s_mfq1w_12_good_other_kid<4 &
                 s_mfq1w_13_all_wrong<4)
      measure_temp_sdq[,5:17] <- lapply(measure_temp_sdq[,5:17], na_if, '-9')
      measure_temp_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches(measure_name)) 
    } else if (measure_name=="p_mfq1w_") {
      measure_temp_sdq <- measure_temp_sdq
      measure_temp_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches(measure_name)) 
    } else {
      measure_temp_sdq <- measure_temp_sdq
      measure_temp_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches(measure_name)) %>% 
        rename(date_temp = ends_with("_date")) %>% rename(tempcomplete = ends_with("_complete")) %>% rename(temptotal = ends_with("_tot")) %>% 
        filter(!is.na(temptotal))
      }
    
    measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
    measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    
    measure_temp_combined <- merge.default(measure_temp_ctdb, measure_temp_sdq, all=TRUE) %>% 
      rename(measure_temp_source = source) %>% 
      group_by(Initials, date_temp) %>% 
      arrange(Initials, date_temp, desc(temptotal), desc(measure_temp_source)) %>%
      slice(1) %>%
      ungroup()
    
    measure_temp_before_covid <- measure_temp_combined %>% filter(date_temp < "2020-03-16")
    measure_temp_since_covid <- measure_temp_combined %>% filter(date_temp >= "2020-03-16")
    
    measure_temp_clinical1 <- merge.default(clinical_DB_date_before_covid, measure_temp_before_covid, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, matches(measure_name), date_temp, temptotal, measure_temp_source, tempcomplete)
    measure_temp_clinical1$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical1$Clinical_Visit_Date, measure_temp_clinical1$date_temp, tz="", units = "days"))
    measure_temp_clinical1 <- measure_temp_clinical1 %>%
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
    
    measure_temp_clinical2 <- merge.default(clinical_DB_date_since_covid, measure_temp_since_covid, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, matches(measure_name), date_temp, temptotal, measure_temp_source, tempcomplete)
    measure_temp_clinical2$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical2$Clinical_Visit_Date, measure_temp_clinical2$date_temp, tz="", units = "days"))
    measure_temp_clinical2$Clinical_Visit_Date_temp <- (measure_temp_clinical2$Clinical_Visit_Date - as.difftime(3, unit="days"))
    measure_temp_clinical2 <- measure_temp_clinical2 %>%
      filter(date_temp >= Clinical_Visit_Date_temp) %>%
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
      select(-measurement_TDiff_abs, -Clinical_Visit_Date_temp)
    
    measure_temp_clinical <- merge.default(measure_temp_clinical1, measure_temp_clinical2, all = TRUE)
    names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
    
    measure_temp_task1 <- merge.default(task_DB_date_before_covid, measure_temp_before_covid, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, matches(measure_name), date_temp, temptotal, measure_temp_source, tempcomplete)
    measure_temp_task1$measurement_TDiff <- as.numeric(difftime(measure_temp_task1$Task_Date, measure_temp_task1$date_temp, tz="", units = "days"))
    measure_temp_task1 <- measure_temp_task1 %>% 
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

    measure_temp_task2 <- merge.default(task_DB_date_since_covid, measure_temp_since_covid, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, matches(measure_name), date_temp, temptotal, measure_temp_source, tempcomplete)
    measure_temp_task2$measurement_TDiff <- as.numeric(difftime(measure_temp_task2$Task_Date, measure_temp_task2$date_temp, tz="", units = "days"))
    measure_temp_task2$Task_Date_temp <- (measure_temp_task2$Task_Date - as.difftime(3, unit="days"))
    measure_temp_task2 <- measure_temp_task2 %>% 
      filter(date_temp >= Task_Date_temp) %>%
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
      select(-measurement_TDiff_abs, -Task_Date_temp)
    
    measure_temp_task <- merge.default(measure_temp_task1, measure_temp_task2, all = TRUE)
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
    print(paste("************************LOOP = ", measure_name))
    
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name)) %>% 
      filter(!is.na(Overall_date)) %>% 
      distinct(., .keep_all = TRUE) 
    measure_temp_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name))
    
    if (measure_name=="p_scared_") {
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_manual$no_columns <- measure_temp_manual %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% ncol() %>% as.numeric()
      measure_temp_manual$NA_count <- measure_temp_manual %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% apply(., 1, count_na)
      measure_temp_manual$diff <- c(measure_temp_manual$no_columns - measure_temp_manual$NA_count)
      measure_temp_manual <- measure_temp_manual %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)

      measure_temp_sdq <- merge.default(measure_temp_sdq, measure_temp_manual, all=TRUE) %>% 
        group_by(Initials, Overall_date) %>% arrange(Initials, Overall_date, source) %>% slice(1) %>% ungroup()
      
      measure_temp_sdq[,7:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,7:ncol(measure_temp_sdq)], as.numeric)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% rowSums(na.rm=TRUE)
      
      measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(measure_name)) %>% select(-p_scared_parent, -p_scared_parent_other) %>% complete.cases(.)
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"
      
    } else {
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(measure_name)) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(measure_name)) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_manual$no_columns <- measure_temp_manual %>% select(matches(measure_name)) %>% ncol() %>% as.numeric()
      measure_temp_manual$NA_count <- measure_temp_manual %>% select(matches(measure_name)) %>% apply(., 1, count_na)
      measure_temp_manual$diff <- c(measure_temp_manual$no_columns - measure_temp_manual$NA_count)
      measure_temp_manual <- measure_temp_manual %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq <- merge.default(measure_temp_sdq, measure_temp_manual, all=TRUE) %>% 
        group_by(Initials, Overall_date) %>% arrange(Initials, Overall_date, source) %>% slice(1) %>% ungroup()
      
      measure_temp_sdq[,5:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,5:ncol(measure_temp_sdq)], as.numeric)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(measure_name)) %>% rowSums(na.rm=TRUE)
      
      measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(measure_name)) %>% complete.cases(.)
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
      measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"
      
    }
    
    for(j in seq_along(scared_subscales)) {
    iter2 <- as.numeric(j)
      # iter2=5
      subscale_name <- scared_subscales[iter2]
      print(paste("************************LOOP = ", measure_name, subscale_name))
      
      measure_temp_sdq$subscaletot <- measure_temp_sdq %>% select(matches(eval(parse(text=scared_subscales[iter2])))) %>% rowSums(na.rm=TRUE)
      names(measure_temp_sdq)[names(measure_temp_sdq) == "subscaletot"] <- (paste0(subscale_name, "_temp"))
    }

    measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
    measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    
    measure_temp_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches(measure_name)) %>% 
      rename(date_temp = ends_with("_date")) %>% rename(tempcomplete = ends_with("_complete")) %>% 
      rename(panic_subscale_temp = ends_with("panic_tot")) %>% rename(gad_subscale_temp = ends_with("gad_tot")) %>% 
      rename(sep_subscale_temp = ends_with("sep_tot")) %>% rename(social_subscale_temp = ends_with("social_tot")) %>% 
      rename(school_subscale_temp = ends_with("school_tot")) %>% rename(temptotal = ends_with("_tot")) %>% 
      filter(!is.na(temptotal))
    
    measure_temp_combined <- merge.default(measure_temp_ctdb, measure_temp_sdq, all=TRUE) %>% 
      rename(measure_temp_source = source) %>% 
      group_by(Initials, date_temp) %>% 
      arrange(Initials, date_temp, desc(temptotal), desc(measure_temp_source)) %>%
      slice(1) %>% 
      ungroup()
    
    measure_temp_before_covid <- measure_temp_combined %>% filter(date_temp < "2020-03-16")
    measure_temp_since_covid <- measure_temp_combined %>% filter(date_temp >= "2020-03-16")
    
    measure_temp_clinical1 <- merge.default(clinical_DB_date_before_covid, measure_temp_before_covid, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, matches(measure_name), date_temp, temptotal,
             panic_subscale_temp, sep_subscale_temp, social_subscale_temp, school_subscale_temp, gad_subscale_temp,
             measure_temp_source, tempcomplete)
    measure_temp_clinical1$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical1$Clinical_Visit_Date, measure_temp_clinical1$date_temp, tz="", units = "days"))
    measure_temp_clinical1 <- measure_temp_clinical1 %>%
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

    measure_temp_clinical2 <- merge.default(clinical_DB_date_since_covid, measure_temp_since_covid, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, matches(measure_name), date_temp, temptotal,
             panic_subscale_temp, sep_subscale_temp, social_subscale_temp, school_subscale_temp, gad_subscale_temp,
             measure_temp_source, tempcomplete)
    measure_temp_clinical2$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical2$Clinical_Visit_Date, measure_temp_clinical2$date_temp, tz="", units = "days"))
    measure_temp_clinical2$Clinical_Visit_Date_temp <- (measure_temp_clinical2$Clinical_Visit_Date - as.difftime(3, unit="days"))
    measure_temp_clinical2 <- measure_temp_clinical2 %>%
      filter(date_temp >= Clinical_Visit_Date_temp) %>%
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
      select(-measurement_TDiff_abs, -Clinical_Visit_Date_temp)
  
    measure_temp_clinical <- merge.default(measure_temp_clinical1, measure_temp_clinical2, all = TRUE)
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
    
    measure_temp_task1 <- merge.default(task_DB_date_before_covid, measure_temp_before_covid, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, matches(measure_name), date_temp, temptotal, 
             panic_subscale_temp, sep_subscale_temp, social_subscale_temp, school_subscale_temp, gad_subscale_temp,
             measure_temp_source, tempcomplete)
    measure_temp_task1$measurement_TDiff <- as.numeric(difftime(measure_temp_task1$Task_Date, measure_temp_task1$date_temp, tz="", units = "days"))
    measure_temp_task1 <- measure_temp_task1 %>%
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

    measure_temp_task2 <- merge.default(task_DB_date_since_covid, measure_temp_since_covid, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, matches(measure_name), date_temp, temptotal, 
             panic_subscale_temp, sep_subscale_temp, social_subscale_temp, school_subscale_temp, gad_subscale_temp,
             measure_temp_source, tempcomplete)
    measure_temp_task2$measurement_TDiff <- as.numeric(difftime(measure_temp_task2$Task_Date, measure_temp_task2$date_temp, tz="", units = "days"))
    measure_temp_task2$Task_Date_temp <- (measure_temp_task2$Task_Date - as.difftime(3, unit="days"))
    measure_temp_task2 <- measure_temp_task2 %>%
      filter(date_temp >= Task_Date_temp) %>%
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
      select(-measurement_TDiff_abs, -Task_Date_temp)
      
    measure_temp_task <- merge.default(measure_temp_task1, measure_temp_task2, all = TRUE)
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
  
  s_lsas_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_lsas_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_lsas_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_lsas_'))
  s_lsas_subset_sdq <- merge.default(s_lsas_subset_sdq, s_lsas_manual, all=TRUE)
    
  s_lsas_subset_sdq[,5:ncol(s_lsas_subset_sdq)] <- sapply(s_lsas_subset_sdq[,5:ncol(s_lsas_subset_sdq)], as.numeric) 
  
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
  
  s_lsas_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches('s_lsas_')) %>% 
    filter(!is.na(s_lsas_date))
  
  s_lsas_subset <- merge.default(s_lsas_subset_ctdb, s_lsas_subset_sdq, all=TRUE) %>% 
    rename(s_lsas_source = source) %>% 
    group_by(Initials, s_lsas_date) %>% 
    arrange(Initials, s_lsas_date, desc(s_lsas_tot), desc(s_lsas_source)) %>%
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
# lsas adolescent:
  
  s_lsasad_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_lsasad_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_lsasad_subset_sdq[,5:ncol(s_lsasad_subset_sdq)] <- sapply(s_lsasad_subset_sdq[,5:ncol(s_lsasad_subset_sdq)], as.numeric) 
  
  s_lsasad_subset_sdq$no_columns <- s_lsasad_subset_sdq %>% select(matches('s_lsasad_')) %>% ncol() %>% as.numeric()
  s_lsasad_subset_sdq$NA_count <- s_lsasad_subset_sdq %>% select(matches('s_lsasad_')) %>% apply(., 1, count_na)
  s_lsasad_subset_sdq$diff <- c(s_lsasad_subset_sdq$no_columns - s_lsasad_subset_sdq$NA_count)
  s_lsasad_subset_sdq <- s_lsasad_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_lsasad_subset_sdq$s_lsasad_tot <- s_lsasad_subset_sdq %>% select(matches('s_lsasad_')) %>% rowSums(na.rm=TRUE)
  
  lsas_performance <- c("_2_|_3_|_5_|_7_|_10_|_11_|_13_|_14_|_15_|_16_|_21_|_24_")
  s_lsasad_subset_sdq$s_lsasad_perform_fear_tot <- s_lsasad_subset_sdq %>% select(matches('_fear')) %>% select(matches(lsas_performance)) %>% rowSums(na.rm=TRUE)
  s_lsasad_subset_sdq$s_lsasad_perform_avoid_tot <- s_lsasad_subset_sdq %>% select(matches('_avoid')) %>% select(matches(lsas_performance))  %>% rowSums(na.rm=TRUE)
  lsas_social <- c("_1_|_4_|_6_|_8_|_9_|_12_|_17_|_18_|_19_|_20_|_22_|_23_")
  s_lsasad_subset_sdq$s_lsasad_social_fear_tot <- s_lsasad_subset_sdq %>% select(matches('_fear')) %>% select(matches(lsas_social)) %>% rowSums(na.rm=TRUE)
  s_lsasad_subset_sdq$s_lsasad_social_avoid_tot <- s_lsasad_subset_sdq %>% select(matches('_avoid')) %>% select(matches(lsas_social)) %>% rowSums(na.rm=TRUE)
  
  s_lsasad_subset_sdq$s_lsasad_complete <- s_lsasad_subset_sdq %>% select(matches('s_lsasad_')) %>% complete.cases(.)
  s_lsasad_subset_sdq$s_lsasad_complete[s_lsasad_subset_sdq$s_lsasad_complete=="FALSE"] <- "0"
  s_lsasad_subset_sdq$s_lsasad_complete[s_lsasad_subset_sdq$s_lsasad_complete=="TRUE"] <- "1"
  
  s_lsasad_subset_sdq$s_lsasad_date <- s_lsasad_subset_sdq$Overall_date
  s_lsasad_subset_sdq <- s_lsasad_subset_sdq %>% select(-Overall_date)

  s_lsasad_subset_clinical <- merge.default(clinical_DB_date, s_lsasad_subset_sdq, all=TRUE) %>% rename(s_lsasad_source = source) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, s_lsasad_source, matches('s_lsasad_'))
  s_lsasad_subset_clinical$s_lsasad_TDiff <- as.numeric(difftime(s_lsasad_subset_clinical$Clinical_Visit_Date, s_lsasad_subset_clinical$s_lsasad_date, tz="", units = "days"))
  s_lsasad_subset_clinical <- s_lsasad_subset_clinical %>% 
    mutate(measurement_TDiff_abs=abs(s_lsasad_TDiff)) %>% 
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_lsasad_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, s_lsasad_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
  s_lsasad_subset_task <- merge.default(task_DB_date, s_lsasad_subset_sdq, all=TRUE) %>% rename(s_lsasad_source = source) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, s_lsasad_source, matches('s_lsasad_'))
  s_lsasad_subset_task$s_lsasad_TDiff <- as.numeric(difftime(s_lsasad_subset_task$Task_Date, s_lsasad_subset_task$s_lsasad_date, tz="", units = "days"))
  s_lsasad_subset_task <- s_lsasad_subset_task %>% 
    mutate(measurement_TDiff_abs=abs(s_lsasad_TDiff)) %>% 
    group_by(Initials, Task_Name, Task_Date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, Task_Date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, Task_Name, s_lsasad_date) %>% 
    arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, s_lsasad_date, measurement_TDiff_abs) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(measurement_TDiff_abs<=60) %>% 
    select(-measurement_TDiff_abs)
  
#####
# shaps
  
  s_shaps_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_shaps_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_shaps_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_shaps_'))
  s_shaps_subset_sdq <- merge.default(s_shaps_subset_sdq, s_shaps_manual, all=TRUE)
  
  s_shaps_subset_sdq[,5:18] <- sapply(s_shaps_subset_sdq[,5:18], as.numeric) 
  
  s_shaps_subset_sdq$no_columns <- s_shaps_subset_sdq %>% select(matches('s_shaps_')) %>% ncol() %>% as.numeric()
  s_shaps_subset_sdq$NA_count <- s_shaps_subset_sdq %>% select(matches('s_shaps_')) %>% apply(., 1, count_na)
  s_shaps_subset_sdq$diff <- c(s_shaps_subset_sdq$no_columns - s_shaps_subset_sdq$NA_count)
  s_shaps_subset_sdq <- s_shaps_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  # recoding SHAPS scores from SDQ that were obtained before June 22nd 2018: coding went from 'strongly disagree'=0 - 'strongly agree'=3 -> 'strongly disagree'=3 - 'strongly agree'=0
  s_shaps_subset_sdq$date_temp <- s_shaps_subset_sdq$Overall_date
  s_shaps_subset_sdq$date_temp_diff <- as.numeric(difftime(as.Date("2018-06-22"), s_shaps_subset_sdq$date_temp, tz="", units = "days"))
  temp_before <- s_shaps_subset_sdq %>% filter(date_temp_diff>-1 & source=="SDQ") %>% select(-date_temp, -date_temp_diff)
  s_shaps_subset_sdq <- s_shaps_subset_sdq %>% filter(date_temp_diff<0 | source=="MANUAL") %>% select(-date_temp, -date_temp_diff)
  temp_before[,5:ncol(temp_before)]  <- lapply(temp_before[,5:ncol(temp_before)], FUN = function(x) recode(x, `3`=0, `2`=1, `1`=2, `0`=3, .missing = NULL))
  s_shaps_subset_sdq <- merge.default(s_shaps_subset_sdq, temp_before, all=TRUE)

  s_shaps_binary <- s_shaps_subset_sdq
  s_shaps_binary[,5:18]  <- lapply(s_shaps_binary[,5:18], FUN = function(x) recode(x, `0`=0, `1`=0, `2`=1, `3`=1, .missing = NULL))
  s_shaps_binary$s_shaps_binary_tot <- s_shaps_binary %>% select(matches('s_shaps_')) %>% rowSums(na.rm=TRUE) 
  s_shaps_binary <- s_shaps_binary %>% select(PLUSID, Initials, source, Overall_date, s_shaps_binary_tot)
  
  s_shaps_subset_sdq$s_shaps_tot <- s_shaps_subset_sdq %>% select(matches('s_shaps_')) %>% rowSums(na.rm=TRUE)
  
  s_shaps_subset_sdq <- left_join(s_shaps_subset_sdq, s_shaps_binary, all=TRUE)
  
  s_shaps_subset_sdq$s_shaps_complete <- s_shaps_subset_sdq %>% select(matches('s_shaps_')) %>% complete.cases(.)
  s_shaps_subset_sdq$s_shaps_complete[s_shaps_subset_sdq$s_shaps_complete=="FALSE"] <- "0"
  s_shaps_subset_sdq$s_shaps_complete[s_shaps_subset_sdq$s_shaps_complete=="TRUE"] <- "1"

  s_shaps_subset_sdq$s_shaps_date <- s_shaps_subset_sdq$Overall_date
  s_shaps_subset_sdq <- s_shaps_subset_sdq %>% select(-Overall_date)
  
  s_shaps_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches('s_shaps_')) %>% 
    filter(!is.na(s_shaps_date))
  s_shaps_subset_ctdb[,5:18]  <- lapply(s_shaps_subset_ctdb[,5:18], FUN = function(x) recode(x, `1`=0, `2`=1, `3`=2, `4`=3, .missing = NULL))
  
  s_shaps_subset <- merge.default(s_shaps_subset_ctdb, s_shaps_subset_sdq, all=TRUE) %>% 
    rename(s_shaps_source = source) %>% 
    group_by(Initials, s_shaps_date) %>% 
    arrange(Initials, s_shaps_date, desc(s_shaps_tot), desc(s_shaps_source)) %>%
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
               "c_ksadsdx_dx_detailed",  "c_ksadsdx_dx_detailed_descrip", "c_ksadsdx_treatment_week_no",  "c_ksadsdx_treatment_notes",  "c_ksadsdx_notes_overall", "c_ksadsdx_epset_current_mdd", 
               "c_ksadsdx_epset_current_submdd", "c_ksadsdx_epset_current_mddsympt", "c_ksadsdx_epset_current_mania", "c_ksadsdx_epset_current_hypomania", "c_ksadsdx_comorbid_dx_old", 
               "c_ksadsdx_ongoing_other_comorbid_dx", "c_ksadsdx_how_interviewed", "c_ksadsdx_ongoing_specific_phob_descrip", 
               "c_ksadsdx_current_specific_phob_descrip", "c_ksadsdx_lifetime_specific_phob_descrip", 
               "c_ksadsdx_current_other_comorbid_dx", "c_ksadsdx_lifetime_other_comorbid_dx")
  
  diagnosis_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('c_ksadsdx')) %>% mutate_all(as.character)
  diagnosis_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('c_ksadsdx')) %>% mutate_all(as.character)
  diagnosis_subset_sdq <- merge.default(diagnosis_subset_sdq, diagnosis_manual, all=TRUE)
  
  diagnosis_subset_sdq[fix_var] <- lapply(diagnosis_subset_sdq[fix_var], na_if, '')
  diagnosis_subset_sdq$c_ksadsdx_date <- as.Date(diagnosis_subset_sdq$c_ksadsdx_date)
  diagnosis_subset_sdq$Overall_date <- as.Date(diagnosis_subset_sdq$Overall_date)
  diagnosis_subset_sdq$c_ksadsdx_epset_baseline_visit_date <- as.Date(diagnosis_subset_sdq$c_ksadsdx_epset_baseline_visit_date)
  diagnosis_subset_sdq$c_ksadsdx_date <- coalesce(diagnosis_subset_sdq$c_ksadsdx_date, diagnosis_subset_sdq$Overall_date) 

  diagnosis_subset_sdq$no_columns <- diagnosis_subset_sdq %>% select(c_ksadsdx_clin_name, c_ksadsdx_visit_type, c_ksadsdx_eligibility, c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed) %>% 
    ncol() %>% as.numeric()
  diagnosis_subset_sdq$NA_count <- diagnosis_subset_sdq %>% select(c_ksadsdx_clin_name, c_ksadsdx_visit_type, c_ksadsdx_eligibility, c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed) %>% 
    apply(., 1, count_na)
  diagnosis_subset_sdq$diff <- c(diagnosis_subset_sdq$no_columns - diagnosis_subset_sdq$NA_count)
  diagnosis_subset_sdq <- diagnosis_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  diagnosis_subset_sdq <- diagnosis_subset_sdq %>% group_by(Initials, c_ksadsdx_date) %>%
    fill(c_ksadsdx_visit_type:c_ksadsdx_lifetime_communication, .direction = "down") %>%
    fill(c_ksadsdx_visit_type:c_ksadsdx_lifetime_communication, .direction = "up") %>%
    distinct(., .keep_all = TRUE) %>% arrange(Initials, c_ksadsdx_date, source) %>%
    slice(1) %>% ungroup() %>% select(-Overall_date)
  
  diagnosis_subset_sdq[fix_var[2]] <- lapply(diagnosis_subset_sdq[fix_var[2]], FUN = function(x) recode(x, "0"="baseline", "1"="12 month FU", "2"="24 month FU",
    "3"="36 month FU", "4"="48 month FU", "9"="Inpatient Treatment", "10"="Outpatient Treatment", .missing = NULL))
  diagnosis_subset_sdq[fix_var[3]] <- lapply(diagnosis_subset_sdq[fix_var[3]], FUN = function(x) recode(x, "0"="Include", "1"="Include: can't scan", 
    "5"="Excluded: does not meet criteria", "6"="Excluded: meets exclusionary criteria", "777"="Excluded: withdrew", "7"="Excluded: withdrew", .missing = NULL))
  diagnosis_subset_sdq[fix_var[5]] <- lapply(diagnosis_subset_sdq[fix_var[5]], FUN = function(x) recode(x, "1"="MDD", "2"="Sub-MDD", "3"="Healthy", "4"="Anxious", 
    "5"="DMDD", "6"="ADHD", "777"="EXCLUDED", .missing = NULL))
  diagnosis_subset_sdq[fix_var[7]] <- lapply(diagnosis_subset_sdq[fix_var[7]], FUN = function(x) recode(x, "1"="Full_MDD", "2"="Sub_remit", "3"="Sub_hist", 
    "4"="Sub_never", "5"="Remit_MDD", "6"="Hist_MDD", "7"="Remit_Sub", "8"="Hist_Sub", "9"="HV", "10"="Anxious", "11"="DMDD", "12"="Sub_DMDD", "13"="ADHD", 
    "14"="Bipolar_I", "15"="Bipolar_II", "777"="EXCLUDED", .missing = NULL))
  diagnosis_subset_sdq[fix_var[12]] <- lapply(diagnosis_subset_sdq[fix_var[12]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[13]] <- lapply(diagnosis_subset_sdq[fix_var[13]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[14]] <- lapply(diagnosis_subset_sdq[fix_var[14]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[15]] <- lapply(diagnosis_subset_sdq[fix_var[15]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[16]] <- lapply(diagnosis_subset_sdq[fix_var[16]], FUN = function(x) recode(x, "1"="yes", "2"="no", .missing = NULL))
  diagnosis_subset_sdq[fix_var[19]] <- lapply(diagnosis_subset_sdq[fix_var[19]], FUN = function(x) recode(x, "0"="face-to-face assessment", "1"="phone assessment", 
    "2"="videoconference", .missing = "face-to-face assessment"))
  
  # fill_names <- diagnosis_subset_sdq %>% select(-Initials, -c_ksadsdx_date) %>% colnames()
  # diagnosis_subset_sdq <- diagnosis_subset_sdq %>% group_by(Initials, c_ksadsdx_date) %>% fill(., names(fill_names), .direction = c("down")) %>%
  #   fill(., names(fill_names), .direction = c("up")) %>% ungroup() %>% distinct(., .keep_all = TRUE)
  
  # collapsing comorbid diagnoses into one variable 
  comorbid <- diagnosis_subset_sdq %>% select(PLUSID, Initials, c_ksadsdx_date, matches("_ongoing_"), matches("_current_"),
                  matches("_lifetime_"), c_ksadsdx_comorbid_dx_old)
  
  comorbid$c_ksadsdx_comorbid_dx_old <- gsub("[", "", comorbid$c_ksadsdx_comorbid_dx_old, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_old <- gsub("]", "", comorbid$c_ksadsdx_comorbid_dx_old, fixed=TRUE)
  comorbid$c_ksadsdx_comorbid_dx_old <- gsub("'", "", comorbid$c_ksadsdx_comorbid_dx_old, fixed=TRUE)

  comorbid$c_ksadsdx_ongoing_mdd <- recode(comorbid$c_ksadsdx_ongoing_mdd, "1"="MDD", "0"=" ", .missing = NULL)
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
  comorbid$c_ksadsdx_ongoing_communication <- recode(comorbid$c_ksadsdx_ongoing_communication, "1"="Communication", "0"=" ", .missing = NULL)
  
  comorbid$c_ksadsdx_current_mdd <- recode(comorbid$c_ksadsdx_current_mdd, "1"="MDD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_submdd <- recode(comorbid$c_ksadsdx_current_submdd, "1"="Sub-MDD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_mania <- recode(comorbid$c_ksadsdx_current_mania, "1"="Mania", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_hypomania <- recode(comorbid$c_ksadsdx_current_hypomania, "1"="Hypomania", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_psychosis <- recode(comorbid$c_ksadsdx_current_psychosis, "1"="Psychosis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_panic <- recode(comorbid$c_ksadsdx_current_panic, "1"="Panic", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_sep_anx <- recode(comorbid$c_ksadsdx_current_sep_anx, "1"="SepAnx", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_social <- recode(comorbid$c_ksadsdx_current_social, "1"="SocPhobia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_specific <- recode(comorbid$c_ksadsdx_current_specific, "1"="SpecificPhobia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_gad <- recode(comorbid$c_ksadsdx_current_gad, "1"="GAD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_enuresis <- recode(comorbid$c_ksadsdx_current_enuresis , "1"="Enuresis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_encopresis <- recode(comorbid$c_ksadsdx_current_encopresis, "1"="Encopresis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_anorexia <- recode(comorbid$c_ksadsdx_current_anorexia, "1"="Anorexia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_adhd<- recode(comorbid$c_ksadsdx_current_adhd, "1"="ADHD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_odd <- recode(comorbid$c_ksadsdx_current_odd, "1"="ODD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_cd <- recode(comorbid$c_ksadsdx_current_cd, "1"="CD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_tic <- recode(comorbid$c_ksadsdx_current_tic, "1"="Tic", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_cigarette <- recode(comorbid$c_ksadsdx_current_cigarette, "1"="Nicotine", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_alcohol <- recode(comorbid$c_ksadsdx_current_alcohol, "1"="Alcohol", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_sa <- recode(comorbid$c_ksadsdx_current_sa, "1"="SA", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_ptsd <- recode(comorbid$c_ksadsdx_current_ptsd, "1"="PTSD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_bdd <- recode(comorbid$c_ksadsdx_current_bdd, "1"="BDD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_ocd <- recode(comorbid$c_ksadsdx_current_ocd, "1"="OCD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_eat <- recode(comorbid$c_ksadsdx_current_eat, "1"="Eating", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_current_communication <- recode(comorbid$c_ksadsdx_current_communication, "1"="Communication", "0"=" ", .missing = NULL)
  
  comorbid$c_ksadsdx_lifetime_mdd <- recode(comorbid$c_ksadsdx_lifetime_mdd, "1"="MDD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_submdd <- recode(comorbid$c_ksadsdx_lifetime_submdd, "1"="Sub-MDD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_mania <- recode(comorbid$c_ksadsdx_lifetime_mania, "1"="Mania", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_hypomania <- recode(comorbid$c_ksadsdx_lifetime_hypomania, "1"="Hypomania", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_psychosis <- recode(comorbid$c_ksadsdx_lifetime_psychosis, "1"="Psychosis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_panic <- recode(comorbid$c_ksadsdx_lifetime_panic, "1"="Panic", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_sep_anx <- recode(comorbid$c_ksadsdx_lifetime_sep_anx, "1"="SepAnx", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_social <- recode(comorbid$c_ksadsdx_lifetime_social, "1"="SocPhobia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_specific <- recode(comorbid$c_ksadsdx_lifetime_specific, "1"="SpecificPhobia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_gad <- recode(comorbid$c_ksadsdx_lifetime_gad, "1"="GAD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_enuresis <- recode(comorbid$c_ksadsdx_lifetime_enuresis , "1"="Enuresis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_encopresis <- recode(comorbid$c_ksadsdx_lifetime_encopresis, "1"="Encopresis", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_anorexia <- recode(comorbid$c_ksadsdx_lifetime_anorexia, "1"="Anorexia", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_adhd<- recode(comorbid$c_ksadsdx_lifetime_adhd, "1"="ADHD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_odd <- recode(comorbid$c_ksadsdx_lifetime_odd, "1"="ODD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_cd <- recode(comorbid$c_ksadsdx_lifetime_cd, "1"="CD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_tic <- recode(comorbid$c_ksadsdx_lifetime_tic, "1"="Tic", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_cigarette <- recode(comorbid$c_ksadsdx_lifetime_cigarette, "1"="Nicotine", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_alcohol <- recode(comorbid$c_ksadsdx_lifetime_alcohol, "1"="Alcohol", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_sa <- recode(comorbid$c_ksadsdx_lifetime_sa, "1"="SA", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_ptsd <- recode(comorbid$c_ksadsdx_lifetime_ptsd, "1"="PTSD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_bdd <- recode(comorbid$c_ksadsdx_lifetime_bdd, "1"="BDD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_ocd <- recode(comorbid$c_ksadsdx_lifetime_ocd, "1"="OCD", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_eat <- recode(comorbid$c_ksadsdx_lifetime_eat, "1"="Eating", "0"=" ", .missing = NULL)
  comorbid$c_ksadsdx_lifetime_communication <- recode(comorbid$c_ksadsdx_lifetime_communication, "1"="Communication", "0"=" ", .missing = NULL)
  
  comorbid$c_ksadsdx_ongoing_comorbid_dx_all <- paste(comorbid$c_ksadsdx_ongoing_mania, comorbid$c_ksadsdx_ongoing_hypomania, 
                         comorbid$c_ksadsdx_ongoing_psychosis, comorbid$c_ksadsdx_ongoing_panic, comorbid$c_ksadsdx_ongoing_sep_anx, comorbid$c_ksadsdx_ongoing_social, 
                         comorbid$c_ksadsdx_ongoing_specific, comorbid$c_ksadsdx_ongoing_gad, comorbid$c_ksadsdx_ongoing_enuresis, comorbid$c_ksadsdx_ongoing_encopresis, 
                         comorbid$c_ksadsdx_ongoing_anorexia, comorbid$c_ksadsdx_ongoing_adhd, comorbid$c_ksadsdx_ongoing_odd, comorbid$c_ksadsdx_ongoing_cd, 
                         comorbid$c_ksadsdx_ongoing_tic, comorbid$c_ksadsdx_ongoing_cigarette, comorbid$c_ksadsdx_ongoing_alcohol, comorbid$c_ksadsdx_ongoing_sa, 
                         comorbid$c_ksadsdx_ongoing_ptsd, comorbid$c_ksadsdx_ongoing_bdd, comorbid$c_ksadsdx_ongoing_ocd, comorbid$c_ksadsdx_ongoing_eat, 
                         comorbid$c_ksadsdx_ongoing_communication, comorbid$c_ksadsdx_ongoing_other_comorbid_dx, sep=", ")
  
  comorbid$c_ksadsdx_current_comorbid_dx_all <- paste(comorbid$c_ksadsdx_current_mania, comorbid$c_ksadsdx_current_hypomania, 
                         comorbid$c_ksadsdx_current_psychosis, comorbid$c_ksadsdx_current_panic, comorbid$c_ksadsdx_current_sep_anx, comorbid$c_ksadsdx_current_social, 
                         comorbid$c_ksadsdx_current_specific, comorbid$c_ksadsdx_current_gad, comorbid$c_ksadsdx_current_enuresis, comorbid$c_ksadsdx_current_encopresis, 
                         comorbid$c_ksadsdx_current_anorexia, comorbid$c_ksadsdx_current_adhd, comorbid$c_ksadsdx_current_odd, comorbid$c_ksadsdx_current_cd, 
                         comorbid$c_ksadsdx_current_tic, comorbid$c_ksadsdx_current_cigarette, comorbid$c_ksadsdx_current_alcohol, comorbid$c_ksadsdx_current_sa, 
                         comorbid$c_ksadsdx_current_ptsd, comorbid$c_ksadsdx_current_bdd, comorbid$c_ksadsdx_current_ocd, comorbid$c_ksadsdx_current_eat, 
                         comorbid$c_ksadsdx_current_communication, comorbid$c_ksadsdx_current_other_comorbid_dx, sep=", ")
  
  comorbid$c_ksadsdx_lifetime_comorbid_dx_all <- paste(comorbid$c_ksadsdx_lifetime_mania, comorbid$c_ksadsdx_lifetime_hypomania, 
                         comorbid$c_ksadsdx_lifetime_psychosis, comorbid$c_ksadsdx_lifetime_panic, comorbid$c_ksadsdx_lifetime_sep_anx, comorbid$c_ksadsdx_lifetime_social, 
                         comorbid$c_ksadsdx_lifetime_specific, comorbid$c_ksadsdx_lifetime_gad, comorbid$c_ksadsdx_lifetime_enuresis, comorbid$c_ksadsdx_lifetime_encopresis, 
                         comorbid$c_ksadsdx_lifetime_anorexia, comorbid$c_ksadsdx_lifetime_adhd, comorbid$c_ksadsdx_lifetime_odd, comorbid$c_ksadsdx_lifetime_cd, 
                         comorbid$c_ksadsdx_lifetime_tic, comorbid$c_ksadsdx_lifetime_cigarette, comorbid$c_ksadsdx_lifetime_alcohol, comorbid$c_ksadsdx_lifetime_sa, 
                         comorbid$c_ksadsdx_lifetime_ptsd, comorbid$c_ksadsdx_lifetime_bdd, comorbid$c_ksadsdx_lifetime_ocd, comorbid$c_ksadsdx_lifetime_eat, 
                         comorbid$c_ksadsdx_lifetime_communication, comorbid$c_ksadsdx_lifetime_other_comorbid_dx, sep=", ")

  for(i in seq_along(1:26)) {
    comorbid$c_ksadsdx_ongoing_comorbid_dx_all <- gsub("NA, ", "", comorbid$c_ksadsdx_ongoing_comorbid_dx_all, fixed=TRUE)
    comorbid$c_ksadsdx_ongoing_comorbid_dx_all <- gsub(" , ", "", comorbid$c_ksadsdx_ongoing_comorbid_dx_all, fixed=TRUE)
    comorbid$c_ksadsdx_current_comorbid_dx_all <- gsub("NA, ", "", comorbid$c_ksadsdx_current_comorbid_dx_all, fixed=TRUE)
    comorbid$c_ksadsdx_current_comorbid_dx_all <- gsub(" , ", "", comorbid$c_ksadsdx_current_comorbid_dx_all, fixed=TRUE)
    comorbid$c_ksadsdx_lifetime_comorbid_dx_all <- gsub("NA, ", "", comorbid$c_ksadsdx_lifetime_comorbid_dx_all, fixed=TRUE)
    comorbid$c_ksadsdx_lifetime_comorbid_dx_all <- gsub(" , ", "", comorbid$c_ksadsdx_lifetime_comorbid_dx_all, fixed=TRUE)
  }
  
  comorbid$c_ksadsdx_ongoing_comorbid_dx_all <- gsub(", NA", "", comorbid$c_ksadsdx_ongoing_comorbid_dx_all, fixed=TRUE)
  comorbid$c_ksadsdx_current_comorbid_dx_all <- gsub(", NA", "", comorbid$c_ksadsdx_current_comorbid_dx_all, fixed=TRUE)
  comorbid$c_ksadsdx_lifetime_comorbid_dx_all <- gsub(", NA", "", comorbid$c_ksadsdx_lifetime_comorbid_dx_all, fixed=TRUE)
  comorbid$c_ksadsdx_ongoing_comorbid_dx_all <- na_if(comorbid$c_ksadsdx_ongoing_comorbid_dx_all, "NA")
  comorbid$c_ksadsdx_current_comorbid_dx_all <- na_if(comorbid$c_ksadsdx_current_comorbid_dx_all, "NA")
  comorbid$c_ksadsdx_lifetime_comorbid_dx_all <- na_if(comorbid$c_ksadsdx_lifetime_comorbid_dx_all, "NA")
  
  # populating blanks with 'None', if applicable (i.e. clinician specified no comorbid dx in old version of questionnaire)
  comorbid$c_ksadsdx_ongoing_comorbid_dx_all <- coalesce(comorbid$c_ksadsdx_ongoing_comorbid_dx_all, comorbid$c_ksadsdx_comorbid_dx_old)
  comorbid$c_ksadsdx_current_comorbid_dx_all <- coalesce(comorbid$c_ksadsdx_current_comorbid_dx_all, comorbid$c_ksadsdx_comorbid_dx_old)
  comorbid$c_ksadsdx_lifetime_comorbid_dx_all <- coalesce(comorbid$c_ksadsdx_lifetime_comorbid_dx_all, comorbid$c_ksadsdx_comorbid_dx_old)
  
  comorbid <- comorbid %>% select(PLUSID, Initials, c_ksadsdx_date, matches("_comorbid_dx_all")) %>% 
    group_by(Initials, c_ksadsdx_date) %>% slice(1) %>% ungroup()
  diagnosis_subset_sdq <- merge.default(diagnosis_subset_sdq, comorbid, all=TRUE) 

  na_names_dx <- diagnosis_subset_sdq %>% select(matches("c_ksadsdx_")) %>% select(-matches("_date")) %>% colnames()
  diagnosis_subset_sdq[na_names_dx] <- lapply(diagnosis_subset_sdq[na_names_dx], replace_na, "999")
  
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
    # group_by(Initials, Task_Name, c_ksadsdx_date) %>%
    # arrange(FIRST_NAME, LAST_NAME, Initials, Task_Name, c_ksadsdx_date, measurement_TDiff_abs) %>%
    # slice(1) %>%
    # ungroup() %>%
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
    # group_by(Initials, c_ksadsdx_date) %>%
    # arrange(FIRST_NAME, LAST_NAME, Initials, c_ksadsdx_date, measurement_TDiff_abs) %>%
    # slice(1) %>%
    # ungroup() %>%
    select(-measurement_TDiff_abs)

# IQ, handedness, tanner -------------------------------------------
  
#####
# IQ:
  
  c_wasi_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('c_wasi_')) %>% 
    distinct(., .keep_all = TRUE)
  
  c_wasi_subset_sdq[,5:7] <- sapply(c_wasi_subset_sdq[,5:7], as.character) 
  
  c_wasi_subset_sdq$no_columns <- c_wasi_subset_sdq %>% select(matches('c_wasi_')) %>% ncol() %>% as.numeric()
  c_wasi_subset_sdq$NA_count <- c_wasi_subset_sdq %>% select(matches('c_wasi_')) %>% apply(., 1, count_na)
  c_wasi_subset_sdq$diff <- c(c_wasi_subset_sdq$no_columns - c_wasi_subset_sdq$NA_count)
  c_wasi_subset_sdq <- c_wasi_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  c_wasi_subset_sdq$c_wasi_date <- c_wasi_subset_sdq$Overall_date
  c_wasi_subset_sdq <- c_wasi_subset_sdq %>% select(-Overall_date)
  
  c_wasi_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches('c_wasi_')) %>% 
    filter(!is.na(c_wasi_date))

  c_wasi_subset <- merge.default(c_wasi_subset_ctdb, c_wasi_subset_sdq, all=TRUE) %>% 
    rename(c_wasi_source = source) %>% 
    group_by(Initials) %>% 
    arrange(Initials, desc(c_wasi_date), desc(c_wasi_iq), desc(c_wasi_source)) %>%
    slice(1) %>% 
    ungroup()
  
  na_names_iq <- c_wasi_subset %>% select(matches("c_wasi_")) %>% select(-matches("_date")) %>% colnames()
  c_wasi_subset[na_names_iq] <- lapply(c_wasi_subset[na_names_iq], replace_na, "999")
  
  c_wasi_subset_clinical <- merge.default(clinical_DB_date, c_wasi_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_wasi_source, c_wasi_date, matches('c_wasi_'))
  
  c_wasi_subset_task <- merge.default(task_DB_date, c_wasi_subset, all=TRUE) %>% 
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Task_Name, Task_Date, c_wasi_source, c_wasi_date, matches('c_wasi_'))
  
#####
# TANNER: 
  
  s_tanner_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_tanner_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_tanner_subset_sdq[,5:8] <- sapply(s_tanner_subset_sdq[,5:8], as.character) 
  
  s_tanner_subset_sdq$no_columns <- s_tanner_subset_sdq %>% select(matches('s_tanner_')) %>% ncol() %>% as.numeric()
  s_tanner_subset_sdq$NA_count <- s_tanner_subset_sdq %>% select(matches('s_tanner_')) %>% apply(., 1, count_na)
  s_tanner_subset_sdq$diff <- c(s_tanner_subset_sdq$no_columns - s_tanner_subset_sdq$NA_count)
  s_tanner_subset_sdq <- s_tanner_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_tanner_subset_sdq$s_tanner_date <- s_tanner_subset_sdq$Overall_date
  s_tanner_subset_sdq <- s_tanner_subset_sdq %>% select(-Overall_date)
  
  s_tanner_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches('s_tanner_')) %>% 
    filter(!is.na(s_tanner_date))
  
  s_tanner_subset <- merge.default(s_tanner_subset_ctdb, s_tanner_subset_sdq, all=TRUE) %>% 
    rename(s_tanner_source = source) %>% 
    group_by(Initials, s_tanner_date) %>% 
    arrange(Initials, s_tanner_date, desc(s_tanner_source)) %>%
    slice(1) %>% 
    ungroup()
  
  na_names_tanner <- s_tanner_subset %>% select(matches("s_tanner_")) %>% select(-matches("_date")) %>% colnames()
  s_tanner_subset[na_names_tanner] <- lapply(s_tanner_subset[na_names_tanner], replace_na, "999")
  
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
  
  s_handedness_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_handedness_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_handedness_subset_sdq[,5:ncol(s_handedness_subset_sdq)] <- sapply(s_handedness_subset_sdq[,5:ncol(s_handedness_subset_sdq)], as.numeric) 
  
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
  s_handedness_subset_sdq <- s_handedness_subset_sdq %>% select(PLUSID, Initials, source, s_handedness_ehi_tot, s_handedness_ehi_date, s_handedness_ehi_complete) %>% 
    distinct(., .keep_all = TRUE)
  
  s_handedness_subset_ctdb <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, matches('s_handedness')) %>% 
    filter(!is.na(s_handedness_ehi_date) | !is.na(s_handedness1_date))
  
  s_handedness_subset_ctdb$s_handedness_ehi_date <- coalesce(s_handedness_subset_ctdb$s_handedness_ehi_date, s_handedness_subset_ctdb$s_handedness1_date)
  s_handedness_subset_ctdb$s_handedness_ehi_complete <- coalesce(s_handedness_subset_ctdb$s_handedness_ehi_complete, s_handedness_subset_ctdb$s_handedness1_complete)
  s_handedness_subset_ctdb$s_handedness_ehi_tot <- coalesce(s_handedness_subset_ctdb$s_handedness_ehi_tot, s_handedness_subset_ctdb$s_handedness1_tot)
  s_handedness_subset_ctdb <- s_handedness_subset_ctdb %>% select(-s_handedness1_date, -s_handedness1_complete, -s_handedness1_tot) %>% 
    distinct(., .keep_all = TRUE)
  
  s_handedness_subset <- merge.default(s_handedness_subset_ctdb, s_handedness_subset_sdq, all=TRUE) %>% 
    rename(s_handedness_ehi_source = source) %>% 
    group_by(Initials) %>% 
    arrange(Initials, desc(s_handedness_ehi_date), desc(s_handedness_ehi_tot), desc(s_handedness_ehi_source)) %>%
    slice(1) %>%
    ungroup()
  
  na_names_handedness <- s_handedness_subset %>% select(matches("s_handedness_")) %>% select(-matches("_date")) %>% colnames()
  s_handedness_subset[na_names_handedness] <- lapply(s_handedness_subset[na_names_handedness], replace_na, "999")
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
    filter(!is.na(Overall_date)) %>% distinct(., .keep_all = TRUE) 
  
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
  
  c_medsclin_manual <- manual_db_w_names %>% select(PLUSID, FIRST_NAME, LAST_NAME, Initials, source, c_medsclin_date, 
                                                    c_medsclin_clinician_name, c_medsclin_person_completing:c_medsclin_othernotes) %>% 
    filter(!is.na(c_medsclin_date))
  c_medsclin_manual$c_medsclin_date <- as.Date(c_medsclin_manual$c_medsclin_date, "%Y-%m-%d")
  c_medsclin_combined <- merge.default(c_medsclin_sdq, c_medsclin_manual, all = TRUE)
  
  ###

  c_medsclin1yr_sdq <- sdq_w_names %>% select(PLUSID, Initials, Overall_date, source, c_medsclin1yr_date, c_medsclin1yr_baseline_date,
                                           c_medsclin1yr_clinician_name, c_medsclin1yr_02_med1name:c_medsclin1yr_othernotes) %>% 
    filter(!is.na(Overall_date)) %>% distinct(., .keep_all = TRUE) 
  
  c_medsclin1yr_sdq$c_medsclin1yr_date <- as.Date(c_medsclin1yr_sdq$c_medsclin1yr_date)
  c_medsclin1yr_sdq$c_medsclin1yr_date <- coalesce(c_medsclin1yr_sdq$c_medsclin1yr_date, c_medsclin1yr_sdq$Overall_date) 
  c_medsclin1yr_sdq$c_medsclin1yr_baseline_date <- as.Date(c_medsclin1yr_sdq$c_medsclin1yr_baseline_date)
  c_medsclin1yr_sdq <- c_medsclin1yr_sdq %>% select(-Overall_date)
  
  c_medsclin1yr_sdq[,6:ncol(c_medsclin1yr_sdq)] <- sapply(c_medsclin1yr_sdq[,6:ncol(c_medsclin1yr_sdq)], as.character)
  c_medsclin1yr_sdq[,6:ncol(c_medsclin1yr_sdq)] <- sapply(c_medsclin1yr_sdq[,6:ncol(c_medsclin1yr_sdq)], na_if, 999)
  c_medsclin1yr_sdq[,6:ncol(c_medsclin1yr_sdq)] <- sapply(c_medsclin1yr_sdq[,6:ncol(c_medsclin1yr_sdq)], na_if, "") 
  
  c_medsclin1yr_sdq$no_columns <- c_medsclin1yr_sdq %>% select(matches("med1name")) %>% ncol() %>% as.numeric()
  c_medsclin1yr_sdq$NA_count <- c_medsclin1yr_sdq %>% select(matches("med1name")) %>% apply(., 1, count_na)
  c_medsclin1yr_sdq$diff <- c(c_medsclin1yr_sdq$no_columns - c_medsclin1yr_sdq$NA_count)
  c_medsclin1yr_sdq <- c_medsclin1yr_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  ###
  
  c_medsclin_sdq_task <- merge.default(task_DB_date, c_medsclin_combined, all=TRUE) %>% 
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
  
  c_medsclin_sdq_clinical <- merge.default(clinical_DB_date, c_medsclin_combined, all=TRUE) %>% 
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
    # iter=13
    measure_name <- tot_sum_clin[iter]
    print(paste("************************LOOP = ", measure_name))
    
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, Initials, Overall_date, source, matches(measure_name)) %>% 
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
    
    measure_temp_sdq <- measure_temp_sdq %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
    
    if (measure_name=="s_promis_") {
      print("recoding measure")
      measure_temp_sdq <- measure_temp_sdq %>% 
        select(PLUSID:source, s_promis_1_restless, s_promis_4_falling_asleep, s_promis_5_troublesleeping, s_promis_8_stayingasleep,
               s_promis_2_satisfied, s_promis_3_refreshing, s_promis_6_enough_sleep, s_promis_7_quality)
      measure_temp_sdq[,5:8] <- lapply(measure_temp_sdq[,5:8], FUN = function(x) recode(x, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
      measure_temp_sdq[,9:12] <- lapply(measure_temp_sdq[,9:12], FUN = function(x) recode(x, `0`=5, `1`=4, `2`=3, `3`=2, `4`=1, .missing = NULL))
    } else if (measure_name=="s_mpss_") {
      print("recoding measure")
      measure_temp_sdq[,5:ncol(measure_temp_sdq)]  <- lapply(measure_temp_sdq[,5:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `6`=7, `5`=6, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
    } else if (measure_name=="s_bads_") {
      print("recoding measure")
      measure_temp_sdq <- measure_temp_sdq %>% 
        select(PLUSID:source, s_bads_3_content, s_bads_4_array_activities, s_bads_5_good_decisions, s_bads_7_accomplish, s_bads_11_did_longterm_goal, 
               s_bads_12_did_worth_it, s_bads_23_structured_activities, #recode:
               s_bads_1_stayed_bed, s_bads_2_didnt_do, s_bads_6_not_accomplish, s_bads_8_avoid_something, s_bads_9_avoid_emotion, s_bads_10_not_to_think,
               s_bads_13_time_thinking, s_bads_14_not_tried_solutions, s_bads_15_think_past, s_bads_16_didnt_see_friends, s_bads_17_array_activities,
               s_bads_18_not_social, s_bads_19_pushed_people, s_bads_20_cut_off_from_people, s_bads_21_time_off, s_bads_22_not_active, s_bads_24_distract, 
               s_bads_25_felt_bad)
      measure_temp_sdq[,12:ncol(measure_temp_sdq)]  <- lapply(measure_temp_sdq[,12:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `0`=6, `1`=5, `2`=4, `3`=3, `4`=2, `5`=1, `6`=0, .missing = NULL))
    } else if (measure_name=="s_chs_") {
      print("recoding measure")
      # recoding CHS scores from SDQ that were obtained before June 22nd 2018: coding went from 0-5 -> 1-6
      measure_temp_sdq$date_temp_diff <- as.numeric(difftime(as.Date("2018-06-22"), measure_temp_sdq$date_temp, tz="", units = "days"))
      temp_before <- measure_temp_sdq %>% filter(date_temp_diff>-1 & source=="SDQ") %>% select(-date_temp_diff)
      measure_temp_sdq <- measure_temp_sdq %>% filter(date_temp_diff<0 | source=="MANUAL") %>% select(-date_temp_diff)
      temp_before[,5:ncol(temp_before)] <- lapply(temp_before[,5:ncol(temp_before)], FUN = function(x) recode(x, `5`=6, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
      measure_temp_sdq <- merge.default(measure_temp_sdq, temp_before, all=TRUE)
    } else {
      print("no recoding necessary for this measure")
    }
    
    if (measure_name=="c_cadam_") {
      
      measure_temp_sdq[,9:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,9:ncol(measure_temp_sdq)], as.numeric)
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(c_cadam_1_sad_facial:c_cadam_15_leisure_activities) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(c_cadam_1_sad_facial:c_cadam_15_leisure_activities) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(c_cadam_1_sad_facial:c_cadam_15_leisure_activities) %>% rowSums(na.rm=TRUE)
      
    } else if (measure_name=="c_cdrs_") {
      
      measure_temp_sdq[,8:(ncol(measure_temp_sdq))] <- sapply(measure_temp_sdq[,8:(ncol(measure_temp_sdq))], as.numeric)
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(c_cdrs_1_depress_feel:c_cdrs_18_mood_liability) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(c_cdrs_1_depress_feel:c_cdrs_18_mood_liability) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)

      measure_temp_sdq <- measure_temp_sdq %>% rename(c_cdrs_tot_manual = c_cdrs_tot)
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% 
        select(c_cdrs_1_depress_feel:c_cdrs_18_mood_liability) %>% rowSums(na.rm=TRUE)
      
    } else if (measure_name=="p_conners_") {
      
      measure_temp_sdq[,8:(ncol(measure_temp_sdq))] <- sapply(measure_temp_sdq[,8:(ncol(measure_temp_sdq))], as.numeric)
      
      measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(p_conners_1_angry:p_conners_80_blurts_answers) %>% ncol() %>% as.numeric()
      measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(p_conners_1_angry:p_conners_80_blurts_answers) %>% apply(., 1, count_na)
      measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
      measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
      
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(p_conners_1_angry:p_conners_80_blurts_answers) %>% rowSums(na.rm=TRUE)
      
    } else {
      
      measure_temp_sdq[,5:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,5:ncol(measure_temp_sdq)], as.numeric)
      
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
    # iter=1
    measure_name <- CASE[iter]
    print(paste("************************LOOP = ", measure_name))
    
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name)) %>% 
      distinct(., .keep_all = TRUE)
    
    measure_temp_sdq$date_temp <- measure_temp_sdq$Overall_date
    measure_temp_sdq <- measure_temp_sdq %>% select(PLUSID, Initials, source, date_temp, matches(measure_name))
    measure_temp_sdq[,6:(ncol(measure_temp_sdq)-1)] <- sapply(measure_temp_sdq[,6:(ncol(measure_temp_sdq)-1)], as.numeric) 

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
    temp_before[how_columns] <- lapply(temp_before[how_columns], FUN = function(x) recode(x, `5`=-3, `4`=-2, `3`=-1, `2`=1, `1`=2, `0`=3, .missing = NULL))
    measure_temp_sdq <- merge.default(measure_temp_sdq, temp_before, all=TRUE)

    measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches("_if")) %>% rowSums(na.rm=TRUE)
  
    for(j in seq_along(how_columns)) {
      iter2 <- as.numeric(j)
      # iter2=3
      how_column_name <- as.character(combined[iter2,1])
      if_column_name <- as.character(combined[iter2,2])
      print(paste("************************LOOP = ", measure_name, how_column_name))
 
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_pos = ifelse(((!!sym(how_column_name))>0), 1, 0))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_pos"] <- (paste0("temp_pos",iter2))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_neg = ifelse(((!!sym(how_column_name))<0), 1, 0))
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
  
  s_seq_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_seq_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_seq_subset_sdq[,5:ncol(s_seq_subset_sdq)] <- sapply(s_seq_subset_sdq[,5:ncol(s_seq_subset_sdq)], as.numeric) 
  
  s_seq_subset_sdq$no_columns <- s_seq_subset_sdq %>% select(matches('s_seq_')) %>% ncol() %>% as.numeric()
  s_seq_subset_sdq$NA_count <- s_seq_subset_sdq %>% select(matches('s_seq_')) %>% apply(., 1, count_na)
  s_seq_subset_sdq$diff <- c(s_seq_subset_sdq$no_columns - s_seq_subset_sdq$NA_count)
  s_seq_subset_sdq <- s_seq_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  # recoding SEQ scores from SDQ that were obtained before June 22nd 2018: coding went from 0-4 -> 1-5
  s_seq_subset_sdq$date_temp <- s_seq_subset_sdq$Overall_date
  s_seq_subset_sdq$date_temp_diff <- as.numeric(difftime(as.Date("2018-06-22"), s_seq_subset_sdq$date_temp, tz="", units = "days"))
  temp_before <- s_seq_subset_sdq %>% filter(date_temp_diff>-1 & source=="SDQ") %>% select(-date_temp, -date_temp_diff)
  s_seq_subset_sdq <- s_seq_subset_sdq %>% filter(date_temp_diff<0 | source=="MANUAL") %>% select(-date_temp, -date_temp_diff)
  temp_before[,5:ncol(temp_before)]  <- lapply(temp_before[,5:ncol(temp_before)], FUN = function(x) recode(x, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))
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
    
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name)) %>% 
      distinct(., .keep_all = TRUE)
  
    measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(fad_normal), matches(fad_reverse)) %>% ncol() %>% as.numeric()
    measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(fad_normal), matches(fad_reverse)) %>% apply(., 1, count_na)
    measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
    measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)

    if (measure_name == "p_fad_") {
      measure_temp_sdq[,7:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,7:ncol(measure_temp_sdq)], as.numeric) 
      measure_temp_sdq <- measure_temp_sdq %>% select(PLUSID:Overall_date, p_fad_parent, p_fad_parent_other, matches(fad_normal), matches(fad_reverse))
      measure_temp_sdq[,32:ncol(measure_temp_sdq)] <- lapply(measure_temp_sdq[,32:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `1`=4, `2`=3, `3`=2, `4`=1, .missing = NULL))
    } else {
      measure_temp_sdq[,5:ncol(measure_temp_sdq)] <- sapply(measure_temp_sdq[,5:ncol(measure_temp_sdq)], as.numeric) 
      measure_temp_sdq <- measure_temp_sdq %>% select(PLUSID:Overall_date, matches(fad_normal), matches(fad_reverse))
      measure_temp_sdq[,30:ncol(measure_temp_sdq)] <- lapply(measure_temp_sdq[,30:ncol(measure_temp_sdq)], FUN = function(x) recode(x, `1`=4, `2`=3, `3`=2, `4`=1, .missing = NULL))
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
  
  p_fasa_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, p_fasa_responder, 
                                              p_fasa_responder_other, p_fasa_1_reassure:p_fasa_13_anxiety_worsen) %>% 
    distinct(., .keep_all = TRUE)
  
  p_fasa_subset_sdq[,7:ncol(p_fasa_subset_sdq)] <- sapply(p_fasa_subset_sdq[,7:ncol(p_fasa_subset_sdq)], as.numeric) 
  
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
  
  s_baexpout_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_baexpout_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_baexpout_subset_sdq$s_baexpout_date <- as.Date(s_baexpout_subset_sdq$s_baexpout_date)
  s_baexpout_subset_sdq$s_baexpout_date <- coalesce(s_baexpout_subset_sdq$s_baexpout_date, s_baexpout_subset_sdq$Overall_date) 
  s_baexpout_subset_sdq <- s_baexpout_subset_sdq %>% select(PLUSID, Initials, source, s_baexpout_date, 
                                                            s_baexpout_clinician_name,
                                                            s_baexpout_weeks_treat:s_baexpout_act_1_type, s_baexpout_act_1_notes:s_baexpout_act_2_type,
                                                            s_baexpout_act_2_notes:s_baexpout_act_3_type, s_baexpout_act_3_notes:s_baexpout_act_4_type,
                                                            s_baexpout_act_4_notes:s_baexpout_act_5_type, s_baexpout_act_5_notes, s_baexpout_other_notes,
                                                            s_baexpout_act_1_moodbef:s_baexpout_act_1_again, s_baexpout_act_2_moodbef:s_baexpout_act_2_again, 
                                                            s_baexpout_act_3_moodbef:s_baexpout_act_3_again, s_baexpout_act_4_moodbef:s_baexpout_act_4_again, 
                                                            s_baexpout_act_5_moodbef:s_baexpout_act_5_again)
  
  s_baexpout_subset_sdq[,7:22] <- sapply(s_baexpout_subset_sdq[,7:22], as.character) 
  s_baexpout_subset_sdq[,23:ncol(s_baexpout_subset_sdq)] <- sapply(s_baexpout_subset_sdq[,23:ncol(s_baexpout_subset_sdq)], as.numeric) 
  
  s_baexpout_subset_sdq[,8]  <- lapply(s_baexpout_subset_sdq[,8], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  s_baexpout_subset_sdq[,11]  <- lapply(s_baexpout_subset_sdq[,11], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  s_baexpout_subset_sdq[,14]  <- lapply(s_baexpout_subset_sdq[,14], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  s_baexpout_subset_sdq[,17]  <- lapply(s_baexpout_subset_sdq[,17], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  s_baexpout_subset_sdq[,20]  <- lapply(s_baexpout_subset_sdq[,20], FUN = function(x) recode(x, `1`= "Something active", `2`= "Something artistic", `3`= "Something musical", `4`= "Something funny",`5`= "Something relaxing",`6`= "Something social", .missing = NULL))
  
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
  
  c_snap_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('c_snap_')) %>% 
    distinct(., .keep_all = TRUE)
  
  c_snap_subset_sdq$c_snap_date <- as.Date(c_snap_subset_sdq$c_snap_date)
  c_snap_subset_sdq$c_snap_date <- coalesce(c_snap_subset_sdq$c_snap_date, c_snap_subset_sdq$Overall_date) 
  c_snap_subset_sdq <- c_snap_subset_sdq %>% select(PLUSID, Initials, c_snap_date, source, c_snap_clinician_name, c_snap_clinician_role,
                                                    c_snap_visit_type, c_snap_treatment_week, c_snap_1_fail_attention:c_snap_18_interrupts)
  
  c_snap_subset_sdq[,5:ncol(c_snap_subset_sdq)]  <- lapply(c_snap_subset_sdq[,5:ncol(c_snap_subset_sdq)], na_if, "")
  c_snap_subset_sdq[,9:ncol(c_snap_subset_sdq)] <- sapply(c_snap_subset_sdq[,9:ncol(c_snap_subset_sdq)], as.numeric) 
  
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
  
  c_cybocs_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, c_cybocs_date, matches('c_cybocs_ob_'), matches('c_cybocs_com_')) %>% 
    distinct(., .keep_all = TRUE)
  
  c_cybocs_subset_sdq[,6:ncol(c_cybocs_subset_sdq)] <- sapply(c_cybocs_subset_sdq[,6:ncol(c_cybocs_subset_sdq)], as.numeric) 
  
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
  
  s_cybocs_list_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_cybocs_list_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_cybocs_list_subset_sdq$s_cybocs_list_date <- as.Date(s_cybocs_list_subset_sdq$s_cybocs_list_date)
  s_cybocs_list_subset_sdq$s_cybocs_list_date <- coalesce(s_cybocs_list_subset_sdq$s_cybocs_list_date, s_cybocs_list_subset_sdq$Overall_date) 
  
  s_cybocs_list_subset_sdq <- s_cybocs_list_subset_sdq %>% select(PLUSID:source, s_cybocs_list_date, matches("s_cybocs_list_com_"), matches("s_cybocs_list_ob_"))
  
  s_cybocs_list_subset_sdq[,5:ncol(s_cybocs_list_subset_sdq)]  <- lapply(s_cybocs_list_subset_sdq[,5:ncol(s_cybocs_list_subset_sdq)], as.character)
  s_cybocs_list_subset_sdq[,5:ncol(s_cybocs_list_subset_sdq)]  <- lapply(s_cybocs_list_subset_sdq[,5:ncol(s_cybocs_list_subset_sdq)], FUN = function(x) recode(x, `0`="Past", `1`="Current", `2`="Both Past & Current", `777`="Never", .missing = NULL))
  
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
  
  c_bddybocs_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('c_bddybocs_')) %>% 
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

  c_bddybocs_subset_sdq[,12:ncol(c_bddybocs_subset_sdq)] <- sapply(c_bddybocs_subset_sdq[,12:ncol(c_bddybocs_subset_sdq)], as.numeric) 
  
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
  
  s_bddybocs_list_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_bddybocs_list_')) %>% 
    distinct(., .keep_all = TRUE)

  s_bddybocs_list_subset_sdq$s_bddybocs_list_date <- as.Date(s_bddybocs_list_subset_sdq$s_bddybocs_list_date)
  s_bddybocs_list_subset_sdq$s_bddybocs_list_date <- coalesce(s_bddybocs_list_subset_sdq$s_bddybocs_list_date, s_bddybocs_list_subset_sdq$Overall_date) 
  s_bddybocs_list_subset_sdq <- s_bddybocs_list_subset_sdq %>% select(-Overall_date)
  
  s_bddybocs_list_subset_sdq[,5:ncol(s_bddybocs_list_subset_sdq)]  <- lapply(s_bddybocs_list_subset_sdq[,5:ncol(s_bddybocs_list_subset_sdq)], as.character)
  s_bddybocs_list_subset_sdq[,5:ncol(s_bddybocs_list_subset_sdq)]  <- lapply(s_bddybocs_list_subset_sdq[,5:ncol(s_bddybocs_list_subset_sdq)], FUN = function(x) recode(x, `0`="Past", `1`="Current", `2`="Both Past & Current", `77 7`="Never", .missing = NULL))
  s_bddybocs_list_subset_sdq[,5:ncol(s_bddybocs_list_subset_sdq)]  <- lapply(s_bddybocs_list_subset_sdq[,5:ncol(s_bddybocs_list_subset_sdq)], na_if, "")
  
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
  
  measure_temp_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name)) %>% 
    distinct(., .keep_all = TRUE)

  measure_temp_sdq[,5:36] <- sapply(measure_temp_sdq[,5:36], as.numeric) 
  measure_temp_sdq[,5:36] <- sapply(measure_temp_sdq[,5:36], na_if, '777') 
  
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
  
  s_cpss_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_cpss_')) %>% 
    distinct(., .keep_all = TRUE)
  
  s_cpss_subset_sdq$s_cpss_date <- as.Date(s_cpss_subset_sdq$s_cpss_date)
  s_cpss_subset_sdq$s_cpss_date <- coalesce(s_cpss_subset_sdq$s_cpss_date, s_cpss_subset_sdq$Overall_date) 
  s_cpss_subset_sdq <- s_cpss_subset_sdq %>% select(PLUSID, Initials, s_cpss_date, source, s_cpss_event,
                                                    s_cpss_time_since, s_cpss_1_thoughts:s_cpss_24_happiness)
  
  s_cpss_subset_sdq[,7:ncol(s_cpss_subset_sdq)] <- sapply(s_cpss_subset_sdq[,7:ncol(s_cpss_subset_sdq)], as.numeric) 
  
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

  s_ascq_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, Overall_date, source, matches('s_ascq_')) %>% 
    distinct(., .keep_all = TRUE)
  
  # removing the two participants that mixed up the 'often' and 'believe' columns 
  # s_ascq_subset_dummy <- s_ascq_subset_sdq %>%
  #   filter((PLUSID=='1131-3732-4602-4203' & Overall_date=="2018-08-20") | (PLUSID=='6527-2908-1008-7627' & Overall_date=="2019-05-29"))
  # s_ascq_subset_sdq <- s_ascq_subset_sdq %>% filter_at(., vars(ends_with("_often")), all_vars(. <6))
  
  # add code to merge manual information in 
  s_ascq_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_ascq_'))
  s_ascq_subset_sdq <- merge.default(s_ascq_subset_sdq, s_ascq_manual, all=TRUE)
  
  s_ascq_subset_sdq[,5:ncol(s_ascq_subset_sdq)] <- sapply(s_ascq_subset_sdq[,5:ncol(s_ascq_subset_sdq)], as.numeric) 
  s_ascq_subset_sdq[,5:33]  <- lapply(s_ascq_subset_sdq[,5:33], FUN = function(x) recode(x, `4`=5, `3`=4, `2`=3, `1`=2, `0`=1, .missing = NULL))

  s_ascq_subset_sdq$no_columns <- s_ascq_subset_sdq %>% select(matches('s_ascq_')) %>% ncol() %>% as.numeric()
  s_ascq_subset_sdq$NA_count <- s_ascq_subset_sdq %>% select(matches('s_ascq_')) %>% apply(., 1, count_na)
  s_ascq_subset_sdq$diff <- c(s_ascq_subset_sdq$no_columns - s_ascq_subset_sdq$NA_count)
  s_ascq_subset_sdq <- s_ascq_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_ascq_subset_sdq <- s_ascq_subset_sdq %>% group_by(Initials, Overall_date) %>% arrange(Initials, Overall_date, source) %>% slice(1) %>% ungroup()
  
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
  
#####
# YGTSS - Yale Global Tic Severity Scale
  
  c_ygtss_subset_sdq <- manual_ygtss %>% select(Initials, SDAN, c_ygtss_date, c_ygtss_source, c_ygtss_number_motor:c_ygtss_impairment) %>%
    distinct(., .keep_all = TRUE)

  c_ygtss_subset_sdq[,5:ncol(c_ygtss_subset_sdq)] <- sapply(c_ygtss_subset_sdq[,5:ncol(c_ygtss_subset_sdq)], as.numeric)

  c_ygtss_subset_sdq$no_columns <- c_ygtss_subset_sdq %>% select(c_ygtss_number_motor:c_ygtss_impairment) %>% ncol() %>% as.numeric()
  c_ygtss_subset_sdq$NA_count <- c_ygtss_subset_sdq %>% select(c_ygtss_number_motor:c_ygtss_impairment) %>% apply(., 1, count_na)
  c_ygtss_subset_sdq$diff <- c(c_ygtss_subset_sdq$no_columns - c_ygtss_subset_sdq$NA_count)
  c_ygtss_subset_sdq <- c_ygtss_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  c_ygtss_subset_sdq$c_ygtss_severity_tot <- c_ygtss_subset_sdq %>% select(c_ygtss_number_motor:c_ygtss_interference_phonic) %>% rowSums(na.rm=TRUE)
  c_ygtss_subset_sdq$c_ygtss_tot <- c_ygtss_subset_sdq %>% select(c_ygtss_number_motor:c_ygtss_impairment) %>% rowSums(na.rm=TRUE)

  c_ygtss_subset_sdq$c_ygtss_complete <- c_ygtss_subset_sdq %>% select(c_ygtss_number_motor:c_ygtss_impairment) %>% complete.cases(.)
  c_ygtss_subset_sdq$c_ygtss_complete[c_ygtss_subset_sdq$c_ygtss_complete=="FALSE"] <- "0"
  c_ygtss_subset_sdq$c_ygtss_complete[c_ygtss_subset_sdq$c_ygtss_complete=="TRUE"] <- "1"

  c_ygtss_subset_clinical <- merge.default(clinical_DB_date, c_ygtss_subset_sdq, all=TRUE) %>% arrange(Initials, c_ygtss_date) %>%
    select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, c_ygtss_source, c_ygtss_date, matches('c_ygtss_'))

  c_ygtss_subset_clinical$c_ygtss_TDiff <- as.numeric(difftime(c_ygtss_subset_clinical$Clinical_Visit_Date, c_ygtss_subset_clinical$c_ygtss_date, tz="", units = "days"))
  c_ygtss_subset_clinical <- c_ygtss_subset_clinical %>%
    mutate(measurement_TDiff_abs=abs(c_ygtss_TDiff)) %>%
    group_by(Initials, Clinical_Visit_Date) %>%
    arrange(FIRST_NAME, LAST_NAME, Initials, Clinical_Visit_Date, measurement_TDiff_abs) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(Initials, c_ygtss_date) %>%
    arrange(FIRST_NAME, LAST_NAME, Initials, c_ygtss_date, measurement_TDiff_abs) %>%
    slice(1) %>%
    ungroup() %>%
    filter(measurement_TDiff_abs<=60) %>%
    select(-measurement_TDiff_abs)

#####
# COVID19 questionnaire
  
  s_covid19_temp_sdq <- sdq_w_names %>% select(PLUSID, Initials, Overall_date, source, matches("s_covid19_")) %>% 
    filter(!is.na(Overall_date)) %>% rename(s_covid19_date = "Overall_date") %>% rename(s_covid19_source = "source") %>% 
    distinct(., .keep_all = TRUE)
  
  covid_recode <- c("s_covid19_3b_fun", "s_covid19_3c_enjoy", "s_covid19_4c_sd_difficult", "s_covid19_9_hopeful_end", "s_covid19_10_hopeful_vaccine", "s_covid19_14_relationships_family", "s_covid19_15_relationships_friends")
  s_covid19_temp_sdq[covid_recode]  <- lapply(s_covid19_temp_sdq[covid_recode], FUN = function(x) recode(x, `10`=1, `9`=2, `8`=3, `7`=4, `6`=5, `5`=6, `4`=7, `3`=8, `2`=9, `1`=10, .missing = NULL))
  s_covid19_temp_sdq$s_covid19_18_less_sleep <- recode(s_covid19_temp_sdq$s_covid19_18_sleep, `10`=0, `9`=0, `8`=0, `7`=0, `6`=0, `5`=1, `4`=2, `3`=4, `2`=8, `1`=10)
  s_covid19_temp_sdq$s_covid19_18_more_sleep <- recode(s_covid19_temp_sdq$s_covid19_18_sleep, `10`=10, `9`=8, `8`=6, `7`=4, `6`=2, `5`=1, `4`=0, `3`=0, `2`=0, `1`=0)
  s_covid19_temp_sdq <- s_covid19_temp_sdq %>% select(PLUSID:s_covid19_17_mental_health, s_covid19_18_less_sleep, s_covid19_18_more_sleep, s_covid19_19_active:s_covid19_22_other_comments)

  s_covid19_temp_sdq[,5:22] <- sapply(s_covid19_temp_sdq[,5:22], as.numeric)
  s_covid19_temp_sdq[,24:32] <- sapply(s_covid19_temp_sdq[,24:32], as.numeric)
  s_covid19_temp_sdq[,23] <- sapply(s_covid19_temp_sdq[,23], na_if, "")
  s_covid19_temp_sdq[,33] <- sapply(s_covid19_temp_sdq[,33], na_if, "")
  
  s_covid19_temp_sdq$no_columns <- s_covid19_temp_sdq %>% select(s_covid19_1_worried_self:s_covid19_10_hopeful_vaccine, s_covid19_14_relationships_family:s_covid19_21_distress) %>% ncol() %>% as.numeric()
  s_covid19_temp_sdq$NA_count <- s_covid19_temp_sdq %>% select(s_covid19_1_worried_self:s_covid19_10_hopeful_vaccine, s_covid19_14_relationships_family:s_covid19_21_distress) %>% apply(., 1, count_na)
  s_covid19_temp_sdq$diff <- c(s_covid19_temp_sdq$no_columns - s_covid19_temp_sdq$NA_count)
  s_covid19_temp_sdq <- s_covid19_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_covid19_temp_sdq$s_covid19_tot <- s_covid19_temp_sdq %>% select(s_covid19_1_worried_self:s_covid19_10_hopeful_vaccine, s_covid19_14_relationships_family:s_covid19_21_distress) %>% rowSums(na.rm=TRUE)
  
  s_covid19_temp_sdq$s_covid19_complete <- s_covid19_temp_sdq %>% select(s_covid19_1_worried_self:s_covid19_10_hopeful_vaccine, s_covid19_14_relationships_family:s_covid19_21_distress) %>% complete.cases(.)
  s_covid19_temp_sdq$s_covid19_complete[s_covid19_temp_sdq$s_covid19_complete=="FALSE"] <- "0"
  s_covid19_temp_sdq$s_covid19_complete[s_covid19_temp_sdq$s_covid19_complete=="TRUE"] <- "1"
  
  s_covid19_sdq_clinical <- merge.default(clinical_DB_date, s_covid19_temp_sdq, all=TRUE) %>% arrange(Initials, s_covid19_date) %>%
    select(Initials, PLUSID, Clinical_Visit_Date, matches("s_covid19_"))
  
  s_covid19_sdq_clinical$s_covid19_TDiff <- as.numeric(difftime(s_covid19_sdq_clinical$s_covid19_date, s_covid19_sdq_clinical$Clinical_Visit_Date, tz="", units = "days"))
  s_covid19_sdq_clinical$Clinical_Visit_Date_temp <- (s_covid19_sdq_clinical$Clinical_Visit_Date - as.difftime(3, unit="days"))
  s_covid19_sdq_clinical <- s_covid19_sdq_clinical %>%
    filter(s_covid19_date >= Clinical_Visit_Date_temp) %>%
    group_by(Initials, Clinical_Visit_Date) %>% 
    arrange(Initials, Clinical_Visit_Date, s_covid19_TDiff) %>% 
    slice(1) %>%
    ungroup() %>% 
    group_by(Initials, s_covid19_date) %>% 
    arrange(Initials, s_covid19_date, s_covid19_TDiff) %>% 
    slice(1) %>%
    ungroup() %>% 
    filter(s_covid19_TDiff<=60) %>% 
    select(-Clinical_Visit_Date_temp)
  
#####
# CRISIS questionnaire
  
  crisis_measures <- c('s_crisis_base_', 's_crisis_3m_', 's_crisis_fu_', 'p_crisis_fu_', 'p_crisis_base_', 'p_crisis_3m_', 's_scaredshort_', 'p_scaredshort_', 'mmi_recovery_')
  
  for(i in seq_along(crisis_measures)) {
    iter <- as.numeric(i)
    # iter=1
    measure_name <- crisis_measures[iter]
    print(paste("************************LOOP = ", measure_name))
    
    measure_temp_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name)) %>% 
      filter(!is.na(Overall_date)) %>% rename(measure_temp_source = "source") %>% distinct(., .keep_all = TRUE) 
    
    if (measure_name=="s_crisis_base_" | measure_name=="p_crisis_base_" | measure_name=="s_crisis_fu_" | measure_name=="p_crisis_fu_") {
      measure_temp_sdq[,5:10] <- lapply(measure_temp_sdq[,5:10], na_if, "")
      measure_temp_sdq[,11:16] <- lapply(measure_temp_sdq[,11:16], as.numeric)
      measure_temp_sdq[,17] <- lapply(measure_temp_sdq[,17], na_if, "")
      measure_temp_sdq[,18:54] <- lapply(measure_temp_sdq[,18:54], as.numeric)
      measure_temp_sdq[,55:58] <- lapply(measure_temp_sdq[,55:58], na_if, "")
      
      # recoding items "32_sad" & "33_anhedonia" - reverse scoring to flip from 0, 1, 2, 3, 4 to 4, 3, 2, 1, 0
      measure_temp_sdq[,43:44] <- lapply(measure_temp_sdq[,43:44], FUN = function(x) recode(x, `4`=0, `3`=1, `2`=2, `1`=3, `0`=4, .missing = NULL))
      
      measure_temp_sdq$date_temp <- as.Date(measure_temp_sdq$Overall_date)
      measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    } else if (measure_name=="s_crisis_3m_" | measure_name=="p_crisis_3m_") {
      measure_temp_sdq[,5:20] <- lapply(measure_temp_sdq[,5:20], as.numeric)
      
      # recoding items "32_sad" & "33_anhedonia" - reverse scoring to flip from 0, 1, 2, 3, 4 to 4, 3, 2, 1, 0
      measure_temp_sdq[,9:10] <- lapply(measure_temp_sdq[,9:10], FUN = function(x) recode(x, `4`=0, `3`=1, `2`=2, `1`=3, `0`=4, .missing = NULL)) 
      
      measure_temp_sdq$date_temp <- as.Date(measure_temp_sdq$Overall_date)
      measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    } else if (measure_name=="s_scaredshort_" | measure_name=="p_scaredshort_") {
      measure_temp_sdq[,5:13] <- lapply(measure_temp_sdq[,5:13], as.numeric)
      measure_temp_sdq$date_temp <- as.Date(measure_temp_sdq$Overall_date)
      measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date)
    } else {
      measure_temp_sdq[,5] <- lapply(measure_temp_sdq[,5], as.numeric)
      measure_temp_sdq[,6] <- lapply(measure_temp_sdq[,6], na_if, "")
      measure_temp_sdq$date_temp <- as.Date(measure_temp_sdq$mmi_recovery_date)
      measure_temp_sdq$date_temp <- coalesce(measure_temp_sdq$date_temp, measure_temp_sdq$Overall_date)
      measure_temp_sdq <- measure_temp_sdq %>% select(-Overall_date, -mmi_recovery_date)
    }
    
    measure_temp_sdq$no_columns <- measure_temp_sdq %>% select(matches(measure_name)) %>% ncol() %>% as.numeric()
    measure_temp_sdq$NA_count <- measure_temp_sdq %>% select(matches(measure_name)) %>% apply(., 1, count_na)
    measure_temp_sdq$diff <- c(measure_temp_sdq$no_columns - measure_temp_sdq$NA_count)
    measure_temp_sdq <- measure_temp_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
    
    if (measure_name=="s_crisis_base_" | measure_name=="p_crisis_base_" | measure_name=="s_crisis_fu_" | measure_name=="p_crisis_fu_") {
      
      fill_in <- measure_temp_sdq %>% select(matches("_3_symptoms"), matches("_5_family_events"), matches("_44_support")) %>% colnames()
      measure_temp_sdq[fill_in] <- lapply(measure_temp_sdq[fill_in], replace_na, 0)

      temp_1_exposed <- as.character(paste0(measure_name, "1_exposed"))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_1_exposed_pos_test = ifelse(str_detect((!!sym(temp_1_exposed)), "pos_test"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_1_exposed_diagnosis = ifelse(str_detect((!!sym(temp_1_exposed)), "diagnosis"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_1_exposed_symptoms = ifelse(str_detect((!!sym(temp_1_exposed)), "symptoms"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_1_exposed_no = ifelse(str_detect((!!sym(temp_1_exposed)), "no"), 1, 0))
      measure_temp_sdq$temp_1_exposed_tot <- measure_temp_sdq %>% 
        select(temp_1_exposed_pos_test, temp_1_exposed_diagnosis, temp_1_exposed_symptoms) %>% rowSums(na.rm=TRUE)
      
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_1_exposed_pos_test"] <- (paste0(measure_name, "1_exposed_pos_test"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_1_exposed_diagnosis"] <- (paste0(measure_name, "1_exposed_diagnosis"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_1_exposed_symptoms"] <- (paste0(measure_name, "1_exposed_symptoms"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_1_exposed_no"] <- (paste0(measure_name, "1_exposed_no"))
      
      temp_2_self_diagnosis <- as.character(paste0(measure_name, "2_self_diagnosis"))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_2_self_diagnosis_pos_test = ifelse(str_detect((!!sym(temp_2_self_diagnosis)), "pos_test"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_2_self_diagnosis_diagnosis = ifelse(str_detect((!!sym(temp_2_self_diagnosis)), "diagnosis"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_2_self_diagnosis_symptoms = ifelse(str_detect((!!sym(temp_2_self_diagnosis)), "symptoms"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_2_self_diagnosis_no = ifelse(str_detect((!!sym(temp_2_self_diagnosis)), "no"), 1, 0))
      measure_temp_sdq$temp_2_self_diagnosis_tot <- measure_temp_sdq %>% 
        select(temp_2_self_diagnosis_pos_test, temp_2_self_diagnosis_diagnosis, temp_2_self_diagnosis_symptoms) %>% rowSums(na.rm=TRUE)
      
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_2_self_diagnosis_pos_test"] <- (paste0(measure_name, "2_self_diagnosis_pos_test"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_2_self_diagnosis_diagnosis"] <- (paste0(measure_name, "2_self_diagnosis_diagnosis"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_2_self_diagnosis_symptoms"] <- (paste0(measure_name, "2_self_diagnosis_symptoms"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_2_self_diagnosis_no"] <- (paste0(measure_name, "2_self_diagnosis_no"))
      
      temp_3_symptoms <- as.character(paste0(measure_name, "3_symptoms"))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_3_symptoms_fever = ifelse(str_detect((!!sym(temp_3_symptoms)), "fever"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_3_symptoms_cough = ifelse(str_detect((!!sym(temp_3_symptoms)), "cough"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_3_symptoms_short_breath = ifelse(str_detect((!!sym(temp_3_symptoms)), "short_breath"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_3_symptoms_sore_throat = ifelse(str_detect((!!sym(temp_3_symptoms)), "sore_throat"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_3_symptoms_fatigue = ifelse(str_detect((!!sym(temp_3_symptoms)), "fatigue"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_3_symptoms_taste = ifelse(str_detect((!!sym(temp_3_symptoms)), "taste"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_3_symptoms_other = ifelse(str_detect((!!sym(temp_3_symptoms)), "other"), 1, 0))
      measure_temp_sdq$temp_3_symptoms_tot <- measure_temp_sdq %>% select(matches("3_symptoms_")) %>% rowSums(na.rm=TRUE)
      
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_3_symptoms_fever"] <- (paste0(measure_name, "3_symptoms_fever"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_3_symptoms_cough"] <- (paste0(measure_name, "3_symptoms_cough"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_3_symptoms_short_breath"] <- (paste0(measure_name, "3_symptoms_short_breath"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_3_symptoms_sore_throat"] <- (paste0(measure_name, "3_symptoms_sore_throat"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_3_symptoms_fatigue"] <- (paste0(measure_name, "3_symptoms_fatigue"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_3_symptoms_taste"] <- (paste0(measure_name, "3_symptoms_taste"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_3_symptoms_other"] <- (paste0(measure_name, "3_symptoms_other"))
      
      temp_4_family_diagnosis <- as.character(paste0(measure_name, "4_family_diagnosis"))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_4_family_diagnosis_yes_household = ifelse(str_detect((!!sym(temp_4_family_diagnosis)), "yes_household"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_4_family_diagnosis_yes_non_household = ifelse(str_detect((!!sym(temp_4_family_diagnosis)), "yes_non_household"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_4_family_diagnosis_no = ifelse(str_detect((!!sym(temp_4_family_diagnosis)), "no"), 1, 0))
      measure_temp_sdq$temp_4_family_diagnosis_tot <- measure_temp_sdq %>%
        select(temp_4_family_diagnosis_yes_household, temp_4_family_diagnosis_yes_non_household) %>% rowSums(na.rm=TRUE)
      
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_4_family_diagnosis_yes_household"] <- (paste0(measure_name, "4_family_diagnosis_yes_household"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_4_family_diagnosis_yes_non_household"] <- (paste0(measure_name, "4_family_diagnosis_yes_non_household"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_4_family_diagnosis_no"] <- (paste0(measure_name, "4_family_diagnosis_no"))
      
      temp_5_family_events <- as.character(paste0(measure_name, "5_family_events"))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_5_family_events_ill = ifelse(str_detect((!!sym(temp_5_family_events)), "ill"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_5_family_events_hospitalized = ifelse(str_detect((!!sym(temp_5_family_events)), "hospitalized"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_5_family_events_quarantine_w_sympt = ifelse(str_detect((!!sym(temp_5_family_events)), "quarantine_w_sympt"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_5_family_events_quarantine_no_sympt = ifelse(str_detect((!!sym(temp_5_family_events)), "quarantine_no_sympt"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_5_family_events_lost_job = ifelse(str_detect((!!sym(temp_5_family_events)), "lost_job"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_5_family_events_loss_earnings = ifelse(str_detect((!!sym(temp_5_family_events)), "loss_earnings"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_5_family_events_died = ifelse(str_detect((!!sym(temp_5_family_events)), "died"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_5_family_events_no = ifelse(str_detect((!!sym(temp_5_family_events)), "no"), 1, 0))
      measure_temp_sdq$temp_5_family_events_tot <- measure_temp_sdq %>%
        select(temp_5_family_events_ill, temp_5_family_events_hospitalized, temp_5_family_events_quarantine_w_sympt, 
               temp_5_family_events_quarantine_no_sympt, temp_5_family_events_died) %>% rowSums(na.rm=TRUE)
      
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_ill"] <- (paste0(measure_name, "5_family_events_ill"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_hospitalized"] <- (paste0(measure_name, "5_family_events_hospitalized"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_quarantine_w_sympt"] <- (paste0(measure_name, "5_family_events_quarantine_w_sympt"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_quarantine_no_sympt"] <- (paste0(measure_name, "5_family_events_quarantine_no_sympt"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_lost_job"] <- (paste0(measure_name, "5_family_events_lost_job"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_loss_earnings"] <- (paste0(measure_name, "5_family_events_loss_earnings"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_died"] <- (paste0(measure_name, "5_family_events_died"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_no"] <- (paste0(measure_name, "5_family_events_no"))
      
      temp_44_support <- as.character(paste0(measure_name, "44_support"))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_resource = ifelse(str_detect((!!sym(temp_44_support)), "resource"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_tutoring = ifelse(str_detect((!!sym(temp_44_support)), "tutoring"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_mentoring = ifelse(str_detect((!!sym(temp_44_support)), "mentoring"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_activity = ifelse(str_detect((!!sym(temp_44_support)), "activity"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_volunteer = ifelse(str_detect((!!sym(temp_44_support)), "volunteer"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_psychotherapy = ifelse(str_detect((!!sym(temp_44_support)), "psychotherapy"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_psychiatrist = ifelse(str_detect((!!sym(temp_44_support)), "psychiatrist"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_occup_therapy = ifelse(str_detect((!!sym(temp_44_support)), "occup_therapy"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_speech = ifelse(str_detect((!!sym(temp_44_support)), "speech"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_sports = ifelse(str_detect((!!sym(temp_44_support)), "sports"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_medical = ifelse(str_detect((!!sym(temp_44_support)), "medical"), 1, 0))
      measure_temp_sdq <- measure_temp_sdq %>% mutate(temp_44_support_other = ifelse(str_detect((!!sym(temp_44_support)), "other"), 1, 0))
      
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_resource"] <- (paste0(measure_name, "44_support_resource"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_tutoring"] <- (paste0(measure_name, "44_support_tutoring"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_mentoring"] <- (paste0(measure_name, "44_support_mentoring"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_activity"] <- (paste0(measure_name, "44_support_activity"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_volunteer"] <- (paste0(measure_name, "44_support_volunteer"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_psychotherapy"] <- (paste0(measure_name, "44_support_psychotherapy"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_psychiatrist"] <- (paste0(measure_name, "44_support_psychiatrist"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_occup_therapy"] <- (paste0(measure_name, "44_support_occup_therapy"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_speech"] <- (paste0(measure_name, "44_support_speech"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_sports"] <- (paste0(measure_name, "44_support_sports"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_medical"] <- (paste0(measure_name, "44_support_medical"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_44_support_other"] <- (paste0(measure_name, "44_support_other"))

      measure_temp_sdq$temp_all_exposure_tot <- measure_temp_sdq %>%
        select(temp_1_exposed_tot, temp_2_self_diagnosis_tot, temp_3_symptoms_tot, temp_4_family_diagnosis_tot, temp_5_family_events_tot) %>% rowSums(na.rm=TRUE)
      
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_1_exposed_tot"] <- (paste0(measure_name, "1_exposed_tot"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_2_self_diagnosis_tot"] <- (paste0(measure_name, "2_self_diagnosis_tot"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_3_symptoms_tot"] <- (paste0(measure_name, "3_symptoms_tot"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_4_family_diagnosis_tot"] <- (paste0(measure_name, "4_family_diagnosis_tot"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_5_family_events_tot"] <- (paste0(measure_name, "5_family_events_tot"))
      names(measure_temp_sdq)[names(measure_temp_sdq) == "temp_all_exposure_tot"] <- (paste0(measure_name, "all_exposure_tot"))
      
    }
    
    if (measure_name=="s_scaredshort_" | measure_name=="p_scaredshort_") {
      #24:41 are scared related, 42:45 are ptsd related
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches(paste0(measure_name,"24_no_reason")):matches(paste0(measure_name,"41_shy")))%>% rowSums(na.rm=TRUE)
    } else if (measure_name=="mmi_recovery_") {
      measure_temp_sdq$temptotal <- c(0)
    } else {
      measure_temp_sdq$temptotal <- measure_temp_sdq %>% select(matches("_31_gen_worry"):matches("_40_rumination")) %>% rowSums(na.rm=TRUE)
    }
    
    measure_temp_sdq$tempcomplete <- measure_temp_sdq %>% select(matches(measure_name)) %>% complete.cases(.)
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="FALSE"] <- "0"
    measure_temp_sdq$tempcomplete[measure_temp_sdq$tempcomplete=="TRUE"] <- "1"

    measure_temp_clinical <- merge.default(clinical_DB_date, measure_temp_sdq, all=TRUE) %>% 
      select(FIRST_NAME, LAST_NAME, Initials, PLUSID, Clinical_Visit_Date, matches(measure_name), date_temp, temptotal, measure_temp_source, tempcomplete)
    
    measure_temp_clinical$measurement_TDiff <- as.numeric(difftime(measure_temp_clinical$Clinical_Visit_Date, measure_temp_clinical$date_temp, tz="", units = "days"))
    measure_temp_clinical$Clinical_Visit_Date_temp <- (measure_temp_clinical$Clinical_Visit_Date - as.difftime(3, unit="days"))
    measure_temp_clinical <- measure_temp_clinical %>%
      filter(date_temp >= Clinical_Visit_Date_temp) %>%
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
      select(-measurement_TDiff_abs, -Clinical_Visit_Date_temp)
    
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_clinical)[names(measure_temp_clinical) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    if (measure_name=="mmi_recovery_") {measure_temp_clinical <- measure_temp_clinical %>% select(-mmi_recovery_tot)}
    assign(paste0(measure_name, "subset_clinical"), measure_temp_clinical)
    
    measure_temp_task <- merge.default(task_DB_date, measure_temp_sdq, all=TRUE) %>% select(FIRST_NAME, LAST_NAME, Initials, 
             PLUSID, Task_Name, Task_Date, matches(measure_name), date_temp, temptotal, measure_temp_source, tempcomplete)
    measure_temp_task$measurement_TDiff <- as.numeric(difftime(measure_temp_task$Task_Date, measure_temp_task$date_temp, tz="", units = "days"))
    measure_temp_task$Task_Date_temp <- (measure_temp_task$Task_Date - as.difftime(3, unit="days"))
    measure_temp_task <- measure_temp_task %>%
      filter(date_temp >= Task_Date_temp) %>%
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
      select(-measurement_TDiff_abs, -Task_Date_temp)
    
    names(measure_temp_task)[names(measure_temp_task) == "measurement_TDiff"] <- (paste0(measure_name, "TDiff"))
    names(measure_temp_task)[names(measure_temp_task) == "tempcomplete"] <- (paste0(measure_name, "complete"))
    names(measure_temp_task)[names(measure_temp_task) == "temptotal"] <- (paste0(measure_name, "tot"))
    names(measure_temp_task)[names(measure_temp_task) == "date_temp"] <- (paste0(measure_name, "date"))
    names(measure_temp_task)[names(measure_temp_task) == "measure_temp_source"] <- (paste0(measure_name, "source"))
    if (measure_name=="mmi_recovery_") {measure_temp_task <- measure_temp_task %>% select(-mmi_recovery_tot)}
    assign(paste0(measure_name, "subset_task"), measure_temp_task)
    
  }
  
#####
# Daily MFQ - inpatients only 
  
  s_mfq1d_subset_sdq <- sdq_w_names %>% select(PLUSID, Initials, source, Overall_date, matches('s_mfq1d_')) %>% 
    distinct(., .keep_all = TRUE) %>% rename(s_mfq1d_date = Overall_date)
  manual_daily_mfq <- manual_daily_mfq %>% select(PLUSID, Initials, source, s_mfq1d_date, s_mfq1d_adjusted_date, s_mfq1d_1_unhappy:s_mfq1d_13_all_wrong, s_mfq1d_manual_tot)
  
  s_mfq1d_subset_sdq[,5:17] <- sapply(s_mfq1d_subset_sdq[,5:17], as.numeric) 
  manual_daily_mfq[,6:19] <- sapply(manual_daily_mfq[,6:19], as.numeric)
  manual_daily_mfq[,6:19] <- sapply(manual_daily_mfq[,6:19], round, 0)
  
  s_mfq1d_subset_sdq$no_columns <- s_mfq1d_subset_sdq %>% select(s_mfq1d_1_unhappy:s_mfq1d_13_all_wrong) %>% ncol() %>% as.numeric()
  s_mfq1d_subset_sdq$NA_count <- s_mfq1d_subset_sdq %>% select(s_mfq1d_1_unhappy:s_mfq1d_13_all_wrong) %>% apply(., 1, count_na)
  s_mfq1d_subset_sdq$diff <- c(s_mfq1d_subset_sdq$no_columns - s_mfq1d_subset_sdq$NA_count)
  s_mfq1d_subset_sdq <- s_mfq1d_subset_sdq %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
  
  s_mfq1d_subset <- merge.default(s_mfq1d_subset_sdq, manual_daily_mfq, all=TRUE)
  
  s_mfq1d_subset$s_mfq1d_tot <- s_mfq1d_subset %>% select(s_mfq1d_1_unhappy:s_mfq1d_13_all_wrong) %>% rowSums(na.rm=TRUE)
  
  s_mfq1d_subset$s_mfq1d_complete <- s_mfq1d_subset %>% select(s_mfq1d_1_unhappy:s_mfq1d_13_all_wrong) %>% complete.cases(.)
  s_mfq1d_subset$s_mfq1d_complete[s_mfq1d_subset$s_mfq1d_complete=="FALSE"] <- "0"
  s_mfq1d_subset$s_mfq1d_complete[s_mfq1d_subset$s_mfq1d_complete=="TRUE"] <- "1"
  
  demo_daily_mfq <- master_IRTA_latest %>% arrange(Initials, desc(Clinical_Visit_Date)) %>% filter(Clinical_Visit_Code=="i") %>% 
    select(FIRST_NAME, LAST_NAME, Initials, SDAN, DAWBA_ID, PLUSID, IRTA_tracker, Eligible, SEX, DOB, Handedness) %>% 
    group_by(Initials) %>% slice(1) %>% ungroup()
  
  s_mfq1d_final <- merge.default(demo_daily_mfq, s_mfq1d_subset, all=TRUE) %>% rename(s_mfq1d_source = source)
  
# Measures where no scoring is necessary ----------------------------------
  
  variables_no_scoring <- c('c_family_hist_', 'p_demo_screen_background_', 'p_demo_screen_', 'c_blood_', 's_menstruation_', 
                            's_middebrief_', 's_mar_', 's_rsdebrief_', 's_mmidebrief_', 's_medsscan_', 's_after_ba_', 
                            's_before_ba_', 's_srsors_', 'c_inpatient_ratings_', 'c_cgas_', 'c_cgi_', 's_fua_', 'p_fua_', 
                            'ksads_', 'p_dawba_bdd_', 's_dawba_bdd_', 's_medsctdb_', 'c_family_interview_', 's_suicide_')
  
  for(i in seq_along(variables_no_scoring)) {
    iter <- as.numeric(i)
    # iter=23
    measure_name <- variables_no_scoring[iter]
    print(paste("************************LOOP = ", measure_name))

    if (measure_name=="s_medsctdb_") {
      measure_temp <- ctdb_w_plusid %>% select(PLUSID, Initials, Protocol_CTDB, source, s_medsctdb_list, s_medsctdb_changes, s_medsctdb_date) %>% 
        filter(!is.na(s_medsctdb_date))
    } else if (measure_name=="c_family_interview_") {
      measure_temp <- sdq_w_names %>% select(PLUSID, Initials, Overall_date, source, matches(measure_name)) %>% 
        filter(!is.na(Overall_date)) %>% 
        distinct(., .keep_all = TRUE)
      measure_temp <- merge.default(measure_temp, common_identifiers_child_sib, all=TRUE) %>% 
        select(PLUSID, Initials, Overall_date, Sibling_Init, source, matches(measure_name))
    } else if (measure_name=="p_demo_screen_") {
      measure_temp <- sdq_w_names %>% select(PLUSID, Initials, Overall_date, source, matches(measure_name)) %>% 
        select(-matches('p_demo_screen_background_')) %>% filter(!is.na(Overall_date)) %>% 
        distinct(., .keep_all = TRUE)
    } else {
      measure_temp <- sdq_w_names %>% select(PLUSID, Initials, Overall_date, source, matches(measure_name)) %>% 
        filter(!is.na(Overall_date)) %>% 
        distinct(., .keep_all = TRUE)
    }

    if (measure_name=="p_demo_screen_background_" | measure_name=="s_menstruation_" | measure_name=="s_middebrief_" | measure_name=="s_mar_" |
        measure_name=="s_fua_" | measure_name=="p_fua_" | measure_name=="s_suicide_") {
      print("creating date variable for measure")
      measure_temp$date_temp <- measure_temp$Overall_date
      measure_temp <- measure_temp %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
      measure_temp[,5:ncol(measure_temp)]  <- lapply(measure_temp[,5:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="p_demo_screen_") {
      print("creating date variable for measure")
      measure_temp$date_temp <- measure_temp$Overall_date
      measure_temp$p_demo_screen_date_mri <- as.Date(measure_temp$p_demo_screen_date_mri)
      measure_temp$p_demo_screen_date_ekg <- as.Date(measure_temp$p_demo_screen_date_ekg)
      measure_temp$p_demo_screen_date_eeg <- as.Date(measure_temp$p_demo_screen_date_eeg)
      measure_temp$p_demo_screen_date_ct <- as.Date(measure_temp$p_demo_screen_date_ct)
      measure_temp <- measure_temp %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
      fix_na_cols <- measure_temp %>% select(matches(measure_name)) %>% 
        select(-p_demo_screen_date_ekg, -p_demo_screen_date_eeg, -p_demo_screen_date_ct, -p_demo_screen_date_mri) %>% colnames()
      measure_temp[fix_na_cols]  <- lapply(measure_temp[fix_na_cols], na_if, "")
    } else if (measure_name=="s_mmidebrief_" | measure_name=="c_inpatient_ratings_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = (paste0(measure_name, "1_date")))
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp <- measure_temp %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
      measure_temp[,5:ncol(measure_temp)]  <- lapply(measure_temp[,5:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="ksads_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = c_ksads_date)
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp <- measure_temp %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
      measure_temp[,5:ncol(measure_temp)]  <- lapply(measure_temp[,5:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="c_cgi_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = (paste0(measure_name, "date")))
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp$c_cgi_date_completed_by_clinician <- as.Date(measure_temp$c_cgi_date_completed_by_clinician)
      measure_temp <- measure_temp %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
      measure_temp[,6:ncol(measure_temp)]  <- lapply(measure_temp[,6:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="s_medsctdb_") {
      print("date included in CTDB pull")
      measure_temp <- measure_temp %>% rename(date_temp = s_medsctdb_date)
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp <- measure_temp %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
      measure_temp[,5:ncol(measure_temp)]  <- lapply(measure_temp[,5:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="c_blood_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = c_blood_draw_date)
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp <- measure_temp %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
      measure_temp[,5:ncol(measure_temp)]  <- lapply(measure_temp[,5:ncol(measure_temp)], na_if, "")
    } else if (measure_name=="c_family_interview_") {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = "c_family_interview_date")
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date)
      measure_temp <- measure_temp %>% select(PLUSID, Initials, Sibling_Init, date_temp, source, matches(measure_name))
      measure_temp[,6:ncol(measure_temp)]  <- lapply(measure_temp[,6:ncol(measure_temp)], na_if, "")
    } else {
      print("date variable manually entered in SDQ+")
      measure_temp <- measure_temp %>% rename(date_temp = (paste0(measure_name, "date")))
      measure_temp$date_temp <- as.Date(measure_temp$date_temp)
      measure_temp$date_temp <- coalesce(measure_temp$date_temp, measure_temp$Overall_date) 
      measure_temp <- measure_temp %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
      measure_temp[,5:ncol(measure_temp)]  <- lapply(measure_temp[,5:ncol(measure_temp)], na_if, "")
    }  
      
    if (measure_name=="p_demo_screen_background_"){
      measure_temp$p_demo_screen_background_race <- gsub("while", "white", measure_temp$p_demo_screen_background_race)
      measure_temp_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, Overall_date, matches(measure_name)) %>% 
        rename(date_temp="Overall_date") %>% filter(!is.na(p_demo_screen_background_race) | !is.na(p_demo_screen_background_hispanic))
      measure_temp <- merge.default(measure_temp, measure_temp_manual, all=TRUE)
    } else if (measure_name=="s_suicide_"){
      measure_temp <- merge.default(measure_temp, manual_suicide_w_names, all=TRUE) %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
    } else if (measure_name=='p_dawba_bdd_' | measure_name=='s_dawba_bdd_') {
      measure_temp_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, matches(measure_name)) %>% 
        rename(date_temp = (paste0(measure_name, "date"))) %>% filter(!is.na(date_temp))
      measure_temp <- merge.default(measure_temp, measure_temp_manual, all=TRUE) %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
    } else if (measure_name=='ksads_') {
      measure_temp_manual <- manual_db_w_names %>% select(PLUSID, Initials, source, matches(measure_name)) %>% 
        rename(date_temp = "c_ksads_date") %>% filter(!is.na(date_temp))
      measure_temp <- merge.default(measure_temp, measure_temp_manual, all=TRUE) %>% select(PLUSID, Initials, date_temp, source, matches(measure_name))
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
    
    if (measure_name=="c_family_interview_") {
      sibling <- measure_temp %>% filter(!is.na(Sibling_Init)) %>% 
        select(Sibling_Init, date_temp, source, matches(measure_name), tempcomplete) %>% rename(Initials = "Sibling_Init")
      sibling <- master_IRTA_latest %>% select(Initials, PLUSID) %>% distinct(., .keep_all = TRUE) %>% 
        filter(Initials %in% sibling$Initials) %>% merge.default(sibling, ., all = TRUE)
      measure_temp <- merge.default(measure_temp, sibling, all=TRUE) %>% 
        select(PLUSID, Initials, date_temp, source, matches(measure_name), tempcomplete)
      # add row to replace nas for 'c_family_interview_type' with 'complete'
      #September 10, 2020. Georgia changed c_family_type to include "gave_up", "refusal", and "complete"
      #any data before that was NA we replaced with "complete" except the ones that IRTAs modified to be "gave_up" and
      #"refusal"
      measure_temp$c_family_interview_type <- sapply(measure_temp$c_family_interview_type, replace_na, "complete")
                                   
    }
    
    if (measure_name=="p_demo_screen_background_"){
      fill_names <- measure_temp %>% select(-Initials) %>% colnames()
      measure_temp <- measure_temp %>% group_by(Initials) %>% arrange(Initials, desc(source)) %>%
        fill(., all_of(fill_names), .direction = "down") %>% fill(., all_of(fill_names), .direction = "up") %>% slice(1) %>% ungroup()
    }
    
    if (measure_name=="p_demo_screen_background_" | measure_name=="p_demo_screen_" | measure_name=="c_family_interview_" | measure_name=="ksads_") {
      
      measure_name2 <- gsub("_", "", measure_name, fixed=TRUE)
      na_names_temp <- measure_temp %>% select(matches(measure_name)) %>% select(-matches("_date")) %>% colnames()
      measure_temp[na_names_temp] <- lapply(measure_temp[na_names_temp], replace_na, "999")
      assign(paste0("na_names_", measure_name2), na_names_temp)
      rm(measure_name2)
      
    }

    if (measure_name=="s_after_ba_" | measure_name=="s_before_ba_" | measure_name=="s_srsors_" | 
        measure_name=="c_inpatient_" | measure_name=="c_cgi_" | measure_name=="s_fua_" | measure_name=="p_fua_" |
        measure_name=="p_dawba_bdd_" | measure_name=="s_dawba_bdd_" | measure_name=="c_family_interview_" | measure_name=="s_suicide_") {
      
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
      
    } else if (measure_name=="p_demo_screen_background_" | measure_name=="p_demo_screen_" | measure_name=="c_family_hist_" |
               measure_name=="ksads_") {
      
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
      
    } else if (measure_name=="p_demo_screen_background_" | measure_name=="p_demo_screen_" | measure_name=="c_family_hist_" |
      measure_name=="ksads_" | measure_name=="c_family_interview_") {
      
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

rm(measure_temp_clinical, measure_temp_clinical1, measure_temp_clinical2)
clinic_sets <- ls(pattern="_clinical")
clinic_sets <- c("clinical_DB", clinic_sets)
clinic_sets <- mget(clinic_sets)

# merge & tidy up

Psychometrics_treatment <- reduce(clinic_sets, full_join)
# # str(Psychometrics_treatment, list.len=ncol(Psychometrics_treatment))
Psychometrics_treatment <- Psychometrics_treatment %>%
  group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date) %>%
  fill(., matches("p_demo_screen_background_"), matches("p_demo_screen_"), matches("c_family_hist_"), matches("c_ksadsdx_"), matches("c_ksads_"),
       matches("c_wasi_"), matches("s_tanner_"), matches("s_handedness_"), .direction = "down") %>%
  fill(., matches("p_demo_screen_background_"), matches("p_demo_screen_"), matches("c_family_hist_"), matches("c_wasi_"), matches("s_handedness_"), .direction = "up") %>%
  ungroup() %>%
  distinct(., .keep_all = TRUE) %>% 
  filter(!is.na(IRTA_tracker))

# fill_names <- Psychometrics_treatment %>% select(-Initials, -Clinical_Visit_Date) %>% colnames()
# duplicates <- Psychometrics_treatment %>% group_by(Initials, Clinical_Visit_Date) %>% filter(n()>1) %>% filter(!is.na(Clinical_Visit_Date))
# Psychometrics_treatment <- Psychometrics_treatment %>%
#   group_by(Initials, Clinical_Visit_Date) %>%
#   arrange(Initials, Clinical_Visit_Date) %>%
#   fill(., all_of(fill_names1), .direction = "down") %>%
#   fill(., all_of(fill_names), .direction = "up") %>%
#   ungroup()

na_names_combined <- c(na_names_dx, na_names_iq, na_names_tanner, na_names_handedness, na_names_cfamilyinterview,
                       na_names_pdemoscreenbackground, na_names_pdemoscreen, na_names_ksads)
Psychometrics_treatment[na_names_combined] <- lapply(Psychometrics_treatment[na_names_combined], na_if, "999")

############# CBT subset

# selecting columns 
cbt_columns <- read_excel(paste0(database_location, "other_data_never_delete/names_cbt_datebase.xlsx"))
current_outpatients <- Psychometrics_treatment %>% filter(str_detect(Clinical_Visit_Code, "o")) %>% group_by(Initials) %>% arrange(Clinical_Visit_Date) %>% slice(n()) %>% ungroup() %>% 
  select(Initials, SDAN, Age_at_visit, SEX, c_ksadsdx_dx_detailed, IRTA_tracker)
outpatient_list <- c(current_outpatients$Initials)
CBT_report <- Psychometrics_treatment %>% 
  select(cbt_columns$select, matches("s_fua_"), matches("p_fua_"), matches("s_srsors_"),
    # matches("s_before_ba_"), matches("s_after_ba_"), matches("s_baexpout_"), s_after_ba_sess_come_again, 
    matches("s_menstruation_"), matches("s_medsctdb_"), matches("c_medsclin_")) %>% 
  arrange(LAST_NAME, Initials, FIRST_NAME, Clinical_Visit_Date) %>% filter(Initials %in% outpatient_list) %>% 
  select(-matches("_TDiff"), -matches("_complete"), -matches("_source"), -s_fua_date, -p_fua_date, -s_srsors_date, -s_menstruation_date, -c_medsclin_date) %>% 
  filter(str_detect(Clinical_Visit_Code, "o") | Clinical_Visit_Type=="c1")

# insert code to filter out future dates
CBT_report$TDiff <- as.numeric(difftime(CBT_report$Clinical_Visit_Date, todays_date_formatted, tz="", units = "days"))
CBT_report <- CBT_report %>% filter(TDiff<1) %>% select(-TDiff)

# merging columns 
CBT_report$s_mfq1w_tot <- coalesce(as.character(CBT_report$s_mfq1w_tot), as.character(CBT_report$s_mfq_tot))
CBT_report$s_ari1w_tot <- coalesce(as.character(CBT_report$s_ari1w_tot), as.character(CBT_report$s_ari6m_tot))
CBT_report$p_mfq1w_tot <- coalesce(as.character(CBT_report$p_mfq1w_tot), as.character(CBT_report$p_mfq_tot))
CBT_report$p_ari1w_tot <- coalesce(as.character(CBT_report$p_ari1w_tot), as.character(CBT_report$p_ari6m_tot))
CBT_report$p_mfq1w_parent <- coalesce(as.character(CBT_report$p_mfq1w_parent), as.character(CBT_report$p_mfq_parent))
CBT_report$p_ari1w_parent <- coalesce(as.character(CBT_report$p_ari1w_parent), as.character(CBT_report$p_ari6m_parent))
CBT_report <- CBT_report %>% select(-matches("_ari6m_"), -matches("_mfq_"))

# accounting for measures not being given at both eval & pre-int
CBT_report_preint <- CBT_report %>% filter(str_detect(Clinical_Visit_Type, "o0") | Clinical_Visit_Type=="c1") %>% group_by(Initials) %>% arrange(Clinical_Visit_Code) %>% 
  fill(., c_ksadsdx_eligibility:c_medsclin_othernotes, .direction="down") %>% ungroup() %>% filter(str_detect(Clinical_Visit_Type, "o0"))
CBT_report <- CBT_report %>% filter(!str_detect(Clinical_Visit_Type, "o0") & Clinical_Visit_Type!="c1") %>% merge.default(., CBT_report_preint, all = TRUE)

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

# no longer need the BA measures to be in the CBT database 
# ba_rating_columns <- CBT_report %>% select(matches("s_before_ba_"), matches("s_after_ba_")) %>% colnames()
# ba_rating_columns <- ba_rating_columns[ba_rating_columns != "s_before_ba_clinician_name"]
# ba_rating_columns <- ba_rating_columns[ba_rating_columns != "s_after_ba_clinician_name"]
# 
# CBT_report[ba_rating_columns] <- lapply(CBT_report[ba_rating_columns], as.numeric)
# CBT_report <- CBT_report %>% mutate(s_ba_sess_mood_diff = (s_after_ba_mood - s_before_ba_mood), s_ba_sess_difficulty_diff = (s_after_ba_difficult - s_before_ba_difficult),
#                                     s_ba_sess_enjoy_diff = (s_after_ba_enjoy - s_before_ba_enjoy), s_ba_sess_anxiety_diff = (s_after_ba_anx - s_before_ba_anx),
#                                     s_ba_sess_satisfaction_diff = (s_after_ba_sat - s_before_ba_sat)
#                                     # s_ba_week_enjoy_diff = (s_after_ba_week_expected_enjoyment - s_before_ba_week_actual_enjoyment), # cannot include this until I resolve how I will have to
#                                     # take the expected weekly enjoyment from the previous week from the actual enjoyment of the present week - i.e. 1 row previous
#                                 )

CBT_report <- CBT_report %>% select(Initials, FIRST_NAME:Eligible, Clinical_Visit_Date:Clinical_Visit_Number, 
  Scheduling_status:c_ksadsdx_lifetime_comorbid_dx_all, matches("_mfq"), matches("_scared"), matches("_ari"), s_shaps_tot:c_ygtss_tot, 
  matches("s_fua_"), matches("p_fua_"), matches("s_srsors_"),
  # matches("s_before_ba_"), matches("s_after_ba_"), matches("_ba_sess_"), matches("s_baexpout_"), 
  matches("s_menstruation_"), matches("s_medsctdb_"), matches("c_medsclin_")) %>%
  # select(-matches("s_baexpout_act_3_"), -matches("s_baexpout_act_4_"), -matches("s_baexpout_act_5_")) %>% 
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

MATCH_tracker$s_mfq1w_tot <- coalesce(as.character(MATCH_tracker$s_mfq1w_tot), as.character(MATCH_tracker$s_mfq_tot))
MATCH_tracker$s_ari1w_tot <- coalesce(as.character(MATCH_tracker$s_ari1w_tot), as.character(MATCH_tracker$s_ari6m_tot))
MATCH_tracker$p_mfq1w_tot <- coalesce(as.character(MATCH_tracker$p_mfq1w_tot), as.character(MATCH_tracker$p_mfq_tot))
MATCH_tracker$p_ari1w_tot <- coalesce(as.character(MATCH_tracker$p_ari1w_tot), as.character(MATCH_tracker$p_ari6m_tot))
MATCH_tracker <- MATCH_tracker %>% select(-matches("_ari6m_"), -matches("_mfq_"))

# recoding & calculating variables

MATCH_tracker$Eligible <- recode(MATCH_tracker$Eligible, "0"="Include", "5"="Excluded: does not meet criteria",
                              "6"="Excluded: meets exclusionary criteria (substance use, psychosis, etc.)",
                              "7"="Did not or withdrew assent/consent", "8"="Ruled as ineligible for treatment during baseline assessment (didn't meet inclusionary or met exclusionary criteria)",
                              "9"="Patient (or parent) withdrew from treatment", "10"="Excluded after commencing treatment: some treatment received before participant was later excluded (e.g. bad scanner, now meets exclusionary criteria, etc.)",
                              "11"="Completed treatment", .missing = NULL)

s_mfq1d_final$Eligible <- recode(s_mfq1d_final$Eligible, "0"="Include", "5"="Excluded: does not meet criteria",
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

#Chana requested only the ones that have scheduling set to 3 to show up on the supervision report
CBT_report = CBT_report %>% filter(Scheduling_status == 3)
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

s_mfq1d_final %>% write_xlsx(paste0(inpatient_location, "MASTER_DATABASE_daily_MFQ.xlsx"))
s_mfq1d_final %>% write_xlsx(paste0(inpatient_backup, "MASTER_DATABASE_daily_MFQ_", todays_date_formatted, ".xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(inpatient_location), pattern = "^MASTER_DATABASE_daily_MFQ.xlsx", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(inpatient_location, file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'MASTER_DATABASE_daily_MFQ'")
} else {
  print("Conflict: exporting as 'MASTER_DATABASE_daily_MFQ_updated'")
  s_mfq1d_final %>% write_xlsx(paste0(inpatient_location,"MASTER_DATABASE_daily_MFQ_updated.xlsx"))
}

#### CRISIS sub-dataset 
fill_names <- c("SDAN", "PLUSID", "Participant_Type2", "IRTA_tracker", "c_ksadsdx_primary_dx", "c_ksadsdx_dx_detailed")
measures_expected <- task_reshape_master_QC %>% filter(str_detect(Task_Name, "easures")) %>% filter(Task_Date > "2020-03-17") %>% 
  select(Initials, SDAN, PLUSID, Participant_Type2, IRTA_tracker, Clinical_Visit_Date, Clinical_Visit_Type, Task_Name, Task_Date) %>% mutate(Review_status = 0)
measures_completed <- Psychometrics_treatment %>% select(Initials, SDAN, PLUSID, Participant_Type2, IRTA_tracker, Clinical_Visit_Date, Clinical_Visit_Type, 
        c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed, s_covid19_tot,	s_covid19_date,	s_covid19_22_other_comments,
        s_crisis_base_1_exposed, s_crisis_base_2_self_diagnosis, s_crisis_base_3_symptoms, s_crisis_base_4_family_diagnosis, s_crisis_base_5_family_events, 
        p_crisis_base_1_exposed, p_crisis_base_2_self_diagnosis, p_crisis_base_3_symptoms, p_crisis_base_4_family_diagnosis, p_crisis_base_5_family_events, 
        s_crisis_fu_1_exposed, s_crisis_fu_2_self_diagnosis, s_crisis_fu_3_symptoms, s_crisis_fu_4_family_diagnosis, s_crisis_fu_5_family_events, 
        p_crisis_fu_1_exposed, p_crisis_fu_2_self_diagnosis, p_crisis_fu_3_symptoms, p_crisis_fu_4_family_diagnosis, p_crisis_fu_5_family_events, 
        s_mfq1w_tot, s_mfq1w_date, s_mfq_tot, s_mfq_date, s_scared_tot, s_scared_date, s_scaredshort_tot, s_scaredshort_date, 
        p_mfq1w_tot, p_mfq1w_date, p_mfq_tot, p_mfq_date, p_scaredshort_tot, p_scaredshort_date, 
        s_crisis_3m_tot, s_crisis_3m_date, s_crisis_base_tot, s_crisis_base_date, s_crisis_fu_tot, s_crisis_fu_date,
        p_crisis_3m_tot, p_crisis_3m_date, p_crisis_base_tot, p_crisis_base_date, p_crisis_fu_tot, p_crisis_fu_date,
        s_crisis_base_3b_symptoms_describe, s_crisis_base_12_positive_describe, s_crisis_base_44_support, s_crisis_base_44_support_other, 
        s_crisis_base_additional_concerns, s_crisis_base_additional_comments, 
        p_crisis_base_3b_symptoms_describe, p_crisis_base_12_positive_describe, p_crisis_base_44_support, p_crisis_base_44_support_other, 
        p_crisis_base_additional_concerns, p_crisis_base_additional_comments,
        s_crisis_fu_3b_symptoms_describe, s_crisis_fu_12_positive_describe, s_crisis_fu_44_support, s_crisis_fu_44_support_other, 
        s_crisis_fu_additional_concerns, s_crisis_fu_additional_comments, 
        p_crisis_fu_3b_symptoms_describe, p_crisis_fu_12_positive_describe, 
        p_crisis_fu_44_support, p_crisis_fu_44_support_other, p_crisis_fu_additional_concerns, p_crisis_fu_additional_comments) %>% 
  filter(s_mfq1w_date  > "2020-03-17" | s_scaredshort_date > "2020-03-17" | s_crisis_base_date > "2020-03-17" | s_crisis_3m_date > "2020-03-17" |
         s_mfq_date  > "2020-03-17" | p_mfq_date  > "2020-03-17" | p_mfq1w_date  > "2020-03-17" | p_crisis_base_date > "2020-03-17" | 
         p_crisis_3m_date > "2020-03-17" | s_crisis_fu_date > "2020-03-17" | p_crisis_fu_date > "2020-03-17") %>% mutate(Review_status = 0) %>% 
  mutate(Review_notes = NA) %>% mutate(Clinician_reviewed = NA) %>% mutate(Clinician_notes = NA) %>% mutate(Clinician_assigned = NA) %>% 
  mutate(Clinician_followup = NA) %>% mutate(Clinician_discussion = NA) %>% mutate(Clinician_call_date = NA)
measures_dataset_w_missing <- merge.default(measures_expected, measures_completed, all=TRUE) %>% group_by(Initials) %>% 
  fill(., all_of(fill_names), .direction = "up") %>% fill(., all_of(fill_names), .direction = "down") %>% ungroup()
measures_dataset_w_missing$Clinical_Visit_Date <- as.Date(measures_dataset_w_missing$Clinical_Visit_Date)

contact_info <- master_IRTA_latest %>% group_by(Initials) %>% arrange(Clinical_Visit_Date) %>% slice(n()) %>% select(Initials, FIRST_NAME, LAST_NAME, Child_Phone_Number, Parent_Contact_Number) %>% 
  filter(Initials %in% measures_dataset_w_missing$Initials) %>% ungroup()

# QCing
not_tracked <- measures_dataset_w_missing %>% filter(is.na(Task_Name)) %>% mutate(QC_note1 = "Missing from IRTA tracker")
measure_incomplete <- measures_dataset_w_missing %>% filter(is.na(s_mfq1w_tot) | is.na(s_mfq_tot) | is.na(s_crisis_base_tot) | is.na(s_crisis_3m_tot) | is.na(s_scaredshort_tot)) %>% 
  mutate(QC_note2 = "At least one questionnaire not completed")
measures_dataset_qc <- merge.default(not_tracked, measure_incomplete, all=TRUE)

measures_dataset_qc$QC_notes <- paste(measures_dataset_qc$QC_note1, measures_dataset_qc$QC_note2, sep = "; ")
measures_dataset_qc <- measures_dataset_qc %>% select(Initials, IRTA_tracker, Clinical_Visit_Date, QC_notes)
measures_dataset_qc$QC_notes <- gsub("NA; ", "", measures_dataset_qc$QC_notes, fixed=TRUE)
measures_dataset_qc$QC_notes <- gsub("; NA", "", measures_dataset_qc$QC_notes, fixed=TRUE)
measures_dataset_qc$QC_notes <- gsub("NA", "", measures_dataset_qc$QC_notes, fixed=TRUE)

# T-2 timepoints

smfq1w_tminus2_long <- Psychometrics_treatment %>% select(Initials, SDAN, PLUSID, Clinical_Visit_Date, s_mfq1w_tot, s_mfq_tot, s_mfq1w_date, s_mfq_date) %>% 
  filter(!is.na(s_mfq1w_tot) | !is.na(s_mfq_tot)) 
smfq1w_tminus2_long$s_mfq1w_tot <- coalesce(smfq1w_tminus2_long$s_mfq1w_tot, smfq1w_tminus2_long$s_mfq_tot) %>% round(., digits = 0)
smfq1w_tminus2_long$s_mfq1w_date <- coalesce(smfq1w_tminus2_long$s_mfq1w_date, smfq1w_tminus2_long$s_mfq_date)
smfq1w_tminus2_long <- smfq1w_tminus2_long %>% filter(s_mfq1w_date < "2020-03-17") %>% group_by(Initials) %>% 
  arrange(desc(s_mfq1w_date)) %>% slice(1:2) %>% ungroup() %>% select(-s_mfq_date, -s_mfq_tot) %>% distinct(., .keep_all = TRUE)

sscared_tminus2_long <- Psychometrics_treatment %>% select(Initials, SDAN, PLUSID, Clinical_Visit_Date, s_scared_tot, s_scared_date) %>% 
  filter(!is.na(s_scared_tot)) %>% mutate(s_scared_tot = round(s_scared_tot, digits = 0))
sscared_tminus2_long <- sscared_tminus2_long %>% filter(s_scared_date < "2020-03-17") %>% group_by(Initials) %>% 
  arrange(desc(s_scared_date)) %>% slice(1:2) %>% ungroup() %>% distinct(., .keep_all = TRUE)

measures_combined_long <- merge.default(smfq1w_tminus2_long, sscared_tminus2_long, all=TRUE) %>% 
  # merge.default(., sari1w_tminus2_long, all=TRUE) %>% 
  distinct(., .keep_all = TRUE) %>% filter(Initials %in% measures_dataset_w_missing$Initials)

# Combining  
measures_dataset_w_qc <- left_join(measures_dataset_w_missing, measures_dataset_qc) %>% merge.default(contact_info) 

fill_names <- c("SDAN", "PLUSID", "IRTA_tracker", "FIRST_NAME", "LAST_NAME", "Child_Phone_Number", "Parent_Contact_Number", "c_ksadsdx_primary_dx", "c_ksadsdx_dx_detailed")
measures_dataset_tminus2_long <- merge.default(measures_dataset_w_qc, measures_combined_long, all=TRUE) %>% mutate(s_covid19_22_other_comments = as.character(s_covid19_22_other_comments))
measures_dataset_tminus2_long$s_mfq_tot <- coalesce(measures_dataset_tminus2_long$s_mfq_tot, measures_dataset_tminus2_long$s_mfq1w_tot)
measures_dataset_tminus2_long$s_mfq_date <- coalesce(measures_dataset_tminus2_long$s_mfq_date, measures_dataset_tminus2_long$s_mfq1w_date)
measures_dataset_tminus2_long$p_mfq_tot <- coalesce(measures_dataset_tminus2_long$p_mfq_tot, measures_dataset_tminus2_long$p_mfq1w_tot)
measures_dataset_tminus2_long$p_mfq_date <- coalesce(measures_dataset_tminus2_long$p_mfq_date, measures_dataset_tminus2_long$p_mfq1w_date)
measures_dataset_tminus2_long <- measures_dataset_tminus2_long %>% select(-matches("_mfq1w_")) %>% group_by(Initials) %>% 
  fill(., all_of(fill_names), .direction = "down") %>% fill(., all_of(fill_names), .direction = "up") %>% ungroup()

# adding auto flagged variables:
crisis_na_names <- c("s_crisis_base_tot", "p_crisis_base_tot", "s_crisis_fu_tot", "p_crisis_fu_tot", "s_mfq_tot", "p_mfq_tot")
measures_dataset_tminus2_long[crisis_na_names] <- lapply(measures_dataset_tminus2_long[crisis_na_names], replace_na, "-999")
measures_dataset_tminus2_long[crisis_na_names] <- lapply(measures_dataset_tminus2_long[crisis_na_names], as.numeric)
measures_dataset_tminus2_long <- measures_dataset_tminus2_long %>% 
  mutate(CRISIS_flag1 = ifelse((s_crisis_base_tot>26 | s_crisis_fu_tot>26), 1, 0)) %>% 
  mutate(CRISIS_flag2 = ifelse((p_crisis_base_tot>26 | p_crisis_fu_tot>26), 1, 0)) %>% 
  mutate(MFQ_flag1 = ifelse((s_mfq_tot>11 & p_mfq_tot<=11), 1, ifelse((p_mfq_tot>11 & s_mfq_tot<=11), 2, ifelse((s_mfq_tot>11 & p_mfq_tot>11), 3, NA))))
measures_dataset_tminus2_long[crisis_na_names] <- lapply(measures_dataset_tminus2_long[crisis_na_names], na_if, "-999")
measures_dataset_tminus2_long[crisis_na_names] <- lapply(measures_dataset_tminus2_long[crisis_na_names], as.numeric)

mfq_change_s <- measures_dataset_tminus2_long %>% select(Initials, Clinical_Visit_Date, s_mfq_tot) %>% filter(!is.na(s_mfq_tot)) %>% 
  distinct(., .keep_all = TRUE) %>% group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date) %>% 
  mutate(s_mfq_tot_prev = lag(s_mfq_tot, n = 1, default = NA)) %>% select(-s_mfq_tot)
mfq_change_p <- measures_dataset_tminus2_long %>% select(Initials, Clinical_Visit_Date, p_mfq_tot) %>% filter(!is.na(p_mfq_tot)) %>% 
  distinct(., .keep_all = TRUE) %>% group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date) %>% 
  mutate(p_mfq_tot_prev = lag(p_mfq_tot, n = 1, default = NA)) %>% select(-p_mfq_tot)
measures_dataset_tminus2_long <- merge.default(measures_dataset_tminus2_long, mfq_change_s, all=TRUE) %>% 
  merge.default(., mfq_change_p, all=TRUE) %>% mutate(s_mfq_diff = (s_mfq_tot - s_mfq_tot_prev)) %>% 
  mutate(p_mfq_diff = (p_mfq_tot - p_mfq_tot_prev))

crisis_na_names <- c("s_mfq_diff", "p_mfq_diff")
measures_dataset_tminus2_long[crisis_na_names] <- lapply(measures_dataset_tminus2_long[crisis_na_names], replace_na, "-999")
measures_dataset_tminus2_long[crisis_na_names] <- lapply(measures_dataset_tminus2_long[crisis_na_names], as.numeric)
measures_dataset_tminus2_long <- measures_dataset_tminus2_long %>% 
  mutate(MFQ_flag2 = ifelse((s_mfq_diff>3 & p_mfq_diff<=3), 1, ifelse((p_mfq_diff>3 & s_mfq_diff<=3), 2,
          ifelse((s_mfq_diff>3 & p_mfq_diff>3), 3, NA))))
measures_dataset_tminus2_long[crisis_na_names] <- lapply(measures_dataset_tminus2_long[crisis_na_names], na_if, "-999")
measures_dataset_tminus2_long[crisis_na_names] <- lapply(measures_dataset_tminus2_long[crisis_na_names], as.numeric)

# measures_dataset_tminus2_long$CRISIS_flag <- pmax(measures_dataset_tminus2_long$CRISIS_flag1, measures_dataset_tminus2_long$CRISIS_flag2)

measures_dataset_tminus2_long$CRISIS_flag1 <- na_if(measures_dataset_tminus2_long$CRISIS_flag1, 0)
measures_dataset_tminus2_long$CRISIS_flag2 <- na_if(measures_dataset_tminus2_long$CRISIS_flag2, 0)
measures_dataset_tminus2_long$CRISIS_flag1 <- recode(measures_dataset_tminus2_long$CRISIS_flag1, `1`="Child CRISIS above 26")
measures_dataset_tminus2_long$CRISIS_flag2 <- recode(measures_dataset_tminus2_long$CRISIS_flag2, `1`="Parent CRISIS above 26")
measures_dataset_tminus2_long$CRISIS_flag <- paste(measures_dataset_tminus2_long$CRISIS_flag1, measures_dataset_tminus2_long$CRISIS_flag2, sep="; ")
measures_dataset_tminus2_long$CRISIS_flag <- gsub("; NA", "", measures_dataset_tminus2_long$CRISIS_flag, fixed=TRUE)
measures_dataset_tminus2_long$CRISIS_flag <- gsub("NA; ", "", measures_dataset_tminus2_long$CRISIS_flag, fixed=TRUE)
measures_dataset_tminus2_long$CRISIS_flag <- na_if(measures_dataset_tminus2_long$CRISIS_flag, "NA")
measures_dataset_tminus2_long$MFQ_flag1 <- recode(measures_dataset_tminus2_long$MFQ_flag1, `1`="Child MFQ >= 12", `2`="Parent MFQ >= 12", `3`="Child & Parent MFQ >= 12")
measures_dataset_tminus2_long$MFQ_flag2 <- recode(measures_dataset_tminus2_long$MFQ_flag2, `1`="Child MFQ jumped by 4 or more", 
                                       `2`="Parent MFQ jumped by 4 or more", `3`="Child & Parent jumped by 4 or more")
measures_dataset_tminus2_long$MFQ_flag <- paste(measures_dataset_tminus2_long$MFQ_flag1, measures_dataset_tminus2_long$MFQ_flag2, sep="; ")
measures_dataset_tminus2_long$MFQ_flag <- gsub("; NA", "", measures_dataset_tminus2_long$MFQ_flag, fixed=TRUE)
measures_dataset_tminus2_long$MFQ_flag <- gsub("NA; ", "", measures_dataset_tminus2_long$MFQ_flag, fixed=TRUE)
measures_dataset_tminus2_long$MFQ_flag <- na_if(measures_dataset_tminus2_long$MFQ_flag, "NA")

# comparing to previous day's sheet: 
prev_crisis_file <- list.files(path = paste0(database_location, "COVID19/"), pattern = "^CRISIS_subset_20", all.files = FALSE,
                               full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
prev_crisis_file_time <- file.mtime(paste0(database_location, "COVID19/", prev_crisis_file)) %>% as.Date()
prev_crisis_combined <- tibble(File=c(prev_crisis_file), Date=c(prev_crisis_file_time))
prev_crisis_combined$date_diff <- as.numeric(difftime(todays_date_formatted, prev_crisis_combined$Date, tz="", units = "days"))
prev_crisis_combined <- prev_crisis_combined %>% arrange(date_diff) %>% filter(date_diff>0) %>% slice(1)
prev_crisis_database <- read_excel(paste0(database_location, "COVID19/", prev_crisis_combined[1])) %>% 
  select(Initials, Clinical_Visit_Date, matches("_tot"), matches("_date"), -Task_Date) %>% mutate(source1="OLD")
hist_check <- measures_dataset_tminus2_long %>% select(Initials, Clinical_Visit_Date, matches("_tot"), matches("_date"), -Task_Date) %>% 
  mutate(source2="NEW") %>% merge.default(., prev_crisis_database, all=TRUE) %>% filter(is.na(source1) | is.na(source2)) %>% 
  filter(!is.na(source2)) %>% select(Initials, Clinical_Visit_Date) %>% distinct(., .keep_all = TRUE) %>% mutate(New_info="Yes")
measures_dataset_tminus2_long <- left_join(measures_dataset_tminus2_long, hist_check)

# final reorder
fill_names <- c("SDAN", "PLUSID", "IRTA_tracker", "FIRST_NAME", "LAST_NAME", "Child_Phone_Number", "Parent_Contact_Number", "c_ksadsdx_primary_dx", "c_ksadsdx_dx_detailed")
measures_dataset_tminus2_long <- measures_dataset_tminus2_long %>% 
  select(IRTA_tracker, Initials, SDAN, PLUSID, FIRST_NAME, LAST_NAME, Child_Phone_Number, Parent_Contact_Number,
         c_ksadsdx_primary_dx, c_ksadsdx_dx_detailed, Clinical_Visit_Date, Task_Name, Task_Date, s_covid19_tot, s_covid19_date, s_covid19_22_other_comments,
         s_crisis_base_1_exposed, s_crisis_base_2_self_diagnosis, s_crisis_base_3_symptoms, s_crisis_base_4_family_diagnosis, s_crisis_base_5_family_events, 
         p_crisis_base_1_exposed, p_crisis_base_2_self_diagnosis, p_crisis_base_3_symptoms, p_crisis_base_4_family_diagnosis, p_crisis_base_5_family_events, 
         s_crisis_fu_1_exposed, s_crisis_fu_2_self_diagnosis, s_crisis_fu_3_symptoms, s_crisis_fu_4_family_diagnosis, s_crisis_fu_5_family_events, 
         p_crisis_fu_1_exposed, p_crisis_fu_2_self_diagnosis, p_crisis_fu_3_symptoms, p_crisis_fu_4_family_diagnosis, p_crisis_fu_5_family_events, 
         s_mfq_tot, s_mfq_diff, s_mfq_date, s_scared_tot, s_scared_date, s_scaredshort_tot, s_scaredshort_date, 
         p_mfq_tot, p_mfq_diff, p_mfq_date, p_scaredshort_tot, p_scaredshort_date, 
         s_crisis_3m_tot, s_crisis_3m_date, s_crisis_base_tot, s_crisis_base_date, s_crisis_fu_tot, s_crisis_fu_date,
         p_crisis_3m_tot, p_crisis_3m_date, p_crisis_base_tot, p_crisis_base_date, p_crisis_fu_tot, p_crisis_fu_date,
         s_crisis_base_3b_symptoms_describe, s_crisis_base_12_positive_describe, s_crisis_base_44_support, s_crisis_base_44_support_other, 
         s_crisis_base_additional_concerns, s_crisis_base_additional_comments, 
         p_crisis_base_3b_symptoms_describe, p_crisis_base_12_positive_describe, p_crisis_base_44_support, p_crisis_base_44_support_other, 
         p_crisis_base_additional_concerns, p_crisis_base_additional_comments,
         s_crisis_fu_3b_symptoms_describe, s_crisis_fu_12_positive_describe, s_crisis_fu_44_support, s_crisis_fu_44_support_other, 
         s_crisis_fu_additional_concerns, s_crisis_fu_additional_comments, 
         p_crisis_fu_3b_symptoms_describe, p_crisis_fu_12_positive_describe, 
         p_crisis_fu_44_support, p_crisis_fu_44_support_other, p_crisis_fu_additional_concerns, p_crisis_fu_additional_comments,
         QC_notes, New_info, CRISIS_flag, MFQ_flag, Review_status, Review_notes, Clinician_reviewed, Clinician_discussion, Clinician_assigned, 
         Clinician_call_date, Clinician_notes, Clinician_followup) %>% 
  group_by(Initials) %>% fill(., all_of(fill_names), .direction = "up") %>% fill(., all_of(fill_names), .direction = "down") %>% 
  ungroup() %>% distinct(., .keep_all = TRUE)


if (file.exists(paste0(database_location, "COVID19/CRISIS_subset_", todays_date_formatted, ".xlsx"))){
  print("CRISIS file already exported today")
} else {
  print("CRISIS file exported")
  measures_dataset_tminus2_long %>% write_xlsx(paste0(database_location, "COVID19/CRISIS_subset_", todays_date_formatted, ".xlsx"))
}

# Creating tasks database & exporting ------------------------------------

rm(max_tasks, measure_temp_task, measure_temp_task1, measure_temp_task2)
task_sets <- ls(pattern="_task")
task_sets <- c("task_DB", task_sets)
task_sets <- mget(task_sets)

# merge & tidy up

Psychometrics_behav <- reduce(task_sets, full_join)
# fill_names <- Psychometrics_behav %>% select(-Initials, -Task_Name, -Task_Date, -Task_Number) %>% colnames()
# str(Psychometrics_behav, list.len=ncol(Psychometrics_behav))
Psychometrics_behav <- Psychometrics_behav %>%
  # group_by(Initials, Task_Name, Task_Date, Task_Number) %>%
  # arrange(Initials, Task_Name, Task_Date) %>% 
  # fill(., all_of(fill_names), .direction = c("down")) %>%
  # fill(., all_of(fill_names), .direction = c("up")) %>%
  # ungroup() %>%
  group_by(Initials) %>% arrange(Initials, Clinical_Visit_Date) %>% 
  fill(., matches("p_demo_screen_background_"), matches("p_demo_screen_"), matches("c_family_hist_"), matches("c_ksadsdx_"), matches("c_ksads_"),
       matches("c_wasi_"), matches("s_tanner_"), matches("s_handedness_"), .direction = "down") %>%
  fill(., matches("p_demo_screen_background_"), matches("p_demo_screen_"), matches("c_family_hist_"),
    matches("c_wasi_"), matches("s_handedness_"), .direction = "up") %>%
  ungroup() %>%
  distinct(., .keep_all = TRUE) %>% 
  filter(!is.na(IRTA_tracker))

na_names_combined <- c(na_names_dx, na_names_iq, na_names_tanner, na_names_handedness,
                       na_names_pdemoscreenbackground, na_names_pdemoscreen, na_names_ksads)
Psychometrics_behav[na_names_combined] <- lapply(Psychometrics_behav[na_names_combined], na_if, "999")

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

# historical database checks ----------------------------------------------

if (weekdays(as.Date(todays_date_formatted))=="Wednesday") {
  print("Today is Wednesday -> creating historical task database change report")

  # importing previous version for comparison 
  prev_db_file <- list.files(path = paste0(database_location, "Backup/"), pattern = "^MASTER_DATABASE_BEHAVIOURAL", all.files = FALSE,
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  prev_db_file_time <- file.mtime(paste0(database_location, "Backup/", prev_db_file)) %>% as.Date()
  prev_db_combined <- tibble(File=c(prev_db_file), Date=c(prev_db_file_time))
  prev_db_combined$date_diff <- as.numeric(difftime(last_week_date_formatted, prev_db_combined$Date, tz="", units = "days"))
  prev_db_combined$day_of_week <- weekdays(as.Date(prev_db_combined$Date))
  prev_db_combined <- prev_db_combined %>% filter(day_of_week=="Wednesday") %>% arrange(date_diff) %>% filter(date_diff>=0) %>% slice(1)
  prev_behav_database <- read_excel(paste0(database_location, "Backup/", prev_db_combined[1])) %>%
    select(Initials, Task_Name, Task_Date, matches("_mfq_tot"), matches("_mfq_date"), matches("_ari1w_tot"), matches("_ari1w_date"), 
           matches("_scared_tot"), matches("_scared_date"), matches("s_lsas_tot"), matches("s_lsas_date"),
           matches("s_shaps_tot"), matches("s_shaps_date"), s_medsscan_date, s_medsscan_med1name, s_medsscan_med1dose) %>% mutate(source2="OLD")
  date_variabes <- prev_behav_database %>% select(matches("_Date"), matches("_date")) %>% colnames()
  prev_behav_database[date_variabes] <- lapply(prev_behav_database[date_variabes], as.Date)
  
  # checking the 5 core measures
  hist_check_measures <- c("s_mfq_", "p_mfq_", "s_ari1w_", "p_ari1w_", "s_scared_", "p_scared_", "s_lsas_", "s_shaps_")
  for(i in seq_along(hist_check_measures)) {
    iter <- as.numeric(i)
    # iter=1
    measure_name_tot <- paste(hist_check_measures[iter], "tot", sep="")
    measure_name_date <- paste(hist_check_measures[iter], "date", sep="")
    
    old_temp <- prev_behav_database %>% select(Initials, Task_Name, Task_Date, matches(measure_name_tot), matches(measure_name_date))
    old_temp[measure_name_tot] <- lapply(old_temp[measure_name_tot], replace_na, 999)
    old_temp[measure_name_tot] <- lapply(old_temp[measure_name_tot], round, digits=0)
    names(old_temp)[names(old_temp) == measure_name_tot] <- (paste0(measure_name_tot, "_old"))
    names(old_temp)[names(old_temp) == measure_name_date] <- (paste0(measure_name_date, "_old"))
    
    new_temp <- Psychometrics_behav %>% select(Initials, Task_Name, Task_Date, matches(measure_name_tot), matches(measure_name_date)) 
    new_temp[measure_name_tot] <- lapply(new_temp[measure_name_tot], replace_na, 999)
    new_temp[measure_name_tot] <- lapply(new_temp[measure_name_tot], round, digits=0)
    
    combined_temp <- merge.default(old_temp, new_temp, all=TRUE)  %>%
      filter(eval(parse(text=measure_name_tot)) != eval(parse(text=paste0(measure_name_tot, "_old"))) | 
               eval(parse(text=measure_name_date)) != eval(parse(text=paste0(measure_name_date, "_old")))) %>% 
      filter(eval(parse(text=paste0(measure_name_tot, "_old")))!=999) %>%
      mutate(reason = ifelse((eval(parse(text=measure_name_tot))==999), 1, 0)) %>% mutate(reason=as.character(reason))
    combined_temp[measure_name_tot] <- lapply(combined_temp[measure_name_tot], na_if, 999)
    
    if (hist_check_measures[iter]=="s_mfq_") {
      combined_temp$reason <- recode(combined_temp$reason, `1`="MFQ (self) was present, now missing", `0`="MFQ (self) score/date change") 
    } else if (hist_check_measures[iter]=="p_mfq_") {
      combined_temp$reason <- recode(combined_temp$reason, `1`="MFQ (parent) was present, now missing", `0`="MFQ (parent) score/date change")
    } else if (hist_check_measures[iter]=="s_ari1w_") {
      combined_temp$reason <- recode(combined_temp$reason, `1`="ARI (self) was present, now missing", `0`="ARI (self) score/date change")
    } else if (hist_check_measures[iter]=="p_ari1w_") {
      combined_temp$reason <- recode(combined_temp$reason, `1`="ARI (parent) was present, now missing", `0`="ARI (parent) score/date change")
    } else if (hist_check_measures[iter]=="s_scared_") {
      combined_temp$reason <- recode(combined_temp$reason, `1`="SCARED (self) was present, now missing", `0`="SCARED (self) score/date change")
    } else if (hist_check_measures[iter]=="p_scared_") {
      combined_temp$reason <- recode(combined_temp$reason, `1`="SCARED (parent) was present, now missing", `0`="SCARED (parent) score/date change")
    } else if (hist_check_measures[iter]=="s_lsas_") {
      combined_temp$reason <- recode(combined_temp$reason, `1`="LSAS was present, now missing", `0`="LSAS score/date change")
    } else if (hist_check_measures[iter]=="s_shaps_") {
      combined_temp$reason <- recode(combined_temp$reason, `1`="SHAPS was present, now missing", `0`="SHAPS score/date change")
    }
    
    names(combined_temp)[names(combined_temp) == "reason"] <- (paste0("reason_", iter))
    assign(paste0(hist_check_measures[iter], "subset_historical"), combined_temp)
    
  }
  
  # checking the scan medication form 
  old_temp <- prev_behav_database %>% select(Initials, Task_Name, Task_Date, matches("s_medsscan")) %>% 
    rename(s_medsscan_date_old="s_medsscan_date", s_medsscan_med1name_old="s_medsscan_med1name", s_medsscan_med1dose_old="s_medsscan_med1dose")
  old_temp[,5:6] <- lapply(old_temp[,5:6], replace_na, 999)
  
  new_temp <- Psychometrics_behav %>% select(Initials, Task_Name, Task_Date, s_medsscan_date, s_medsscan_med1name, s_medsscan_med1dose)
  new_temp[,5:6] <- lapply(new_temp[,5:6], replace_na, 999)
  
  meds_subset_historical <- merge.default(old_temp, new_temp, all=TRUE) %>%
    filter(s_medsscan_med1name != s_medsscan_med1name_old | s_medsscan_med1dose != s_medsscan_med1dose_old | 
             s_medsscan_date != s_medsscan_date_old) %>% filter(s_medsscan_med1name_old != 999) %>%
    mutate(reason_9 = ifelse((s_medsscan_med1name==999), 1, 0)) %>% mutate(reason_9=as.character(reason_9))
  meds_subset_historical[,5:6] <- lapply(meds_subset_historical[,5:6], na_if, 999)
  meds_subset_historical$reason_9 <- recode(meds_subset_historical$reason_9, `1`="Scan med info was present, now missing", `0`="Scan med info/date change") 
  
  # then merge all together & export for checking 
  historical_sets <- ls(pattern="_historical")
  historical_sets <- mget(historical_sets)
  historical_check_all_combined <- reduce(historical_sets, full_join)
  
  historical_check_all_combined$reason <- paste(historical_check_all_combined$reason_1, historical_check_all_combined$reason_2, historical_check_all_combined$reason_3, 
                                                historical_check_all_combined$reason_4, historical_check_all_combined$reason_5, historical_check_all_combined$reason_6,
                                                historical_check_all_combined$reason_7, historical_check_all_combined$reason_8, historical_check_all_combined$reason_9, sep="; ")
  historical_check_all_combined$reason <- gsub("NA; ", "", historical_check_all_combined$reason, fixed=TRUE)
  historical_check_all_combined$reason <- gsub("; NA", "", historical_check_all_combined$reason, fixed=TRUE)
  historical_check_all_combined$reason <- gsub("NA", "", historical_check_all_combined$reason, fixed=TRUE)
  historical_check_all_combined <- historical_check_all_combined %>% select(-matches("reason_"))
  
  historical_check_all_combined %>% write_xlsx(paste0(database_location, "Backup/MASTER_DATABASE_BEHAVIOURAL_CHANGE_", todays_date_formatted, ".xlsx"))
  rm(prev_behav_database, prev_db_combined, prev_db_file, prev_db_file_time, measure_name_date, measure_name_tot)
  
} else {
  print("Historical task database change report not produced - only produced on Wednesdays")
  }

# CRISIS sub-set & descriptives for Argyris -------------------------------

suppressPackageStartupMessages(library(kableExtra))
render(paste0(scripts, 'Reports/crisis_dataset_description.Rmd'),
       output_format = "html_document", output_file = 'CRISIS_completion_numbers', output_dir = paste0(database_location, 'COVID19/'))
detach(package:kableExtra)

CRISIS_data_presentation_final %>% write_csv(paste0(database_location, "COVID19/CRISIS_subset_tminus1.csv"))
CRISIS_data_presentation_final %>% write_xlsx(paste0(database_location, "COVID19/CRISIS_subset_tminus1.xlsx"))

# checking saved properly
file_save_check <- list.files(path = paste0(database_location, "COVID19/"), pattern = "^CRISIS_subset_tminus1.csv", all.files = FALSE,
                              full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_save_check_time <- file.mtime(paste0(database_location, "COVID19/", file_save_check)) %>% as.Date()
file_save_check_combined <- tibble(File=c(file_save_check), Date=c(file_save_check_time)) 
file_save_check_combined$date_diff <- as.numeric(difftime(todays_date_formatted, file_save_check_combined$Date, tz="", units = "days"))

if (file_save_check_combined$date_diff[1]==0) {
  print("Exported as 'CRISIS_subset_tminus1'")
} else {
  print("Conflict: exporting as 'CRISIS_subset_tminus1_updated'")
  CRISIS_data_presentation_final %>% write_csv(paste0(database_location, "COVID19/CRISIS_subset_tminus1_updated.csv"))
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
rm(list=ls(pattern="measures"))
rm(list=ls(pattern="iter"))
rm(list=ls(pattern="manual"))
rm(list=ls(pattern="common_identifiers"))
rm(list=ls(pattern="data_"))
rm(list=ls(pattern="na_names_"))
rm(list=ls(pattern="parent_"))
rm(list=ls(pattern="historical"))
rm(list=ls(pattern="imported_imputed"))
rm(list=ls(pattern="temp"))
rm(tot_sum, s_shaps_binary, imported_imputed_mfqs, gen_functioning, hand_columns, father_report, mother_report, column, numeric_variables, 
   measure_name, panic_subscale, sep_subscale, social_subscale, gad_subscale, school_subscale, j, subscale_name, i, lsas_performance, lsas_social, hand_column_name, e, d, b, prev_crisis_file, prev_crisis_combined, prev_crisis_database, 
   fix_var, remove_unknown, variables_no_scoring, CASE, CHoCIR, activities, activity_no, behav_control, c_snap_hyperactivity, ba_rating_columns, unknown_report, fill_in, crisis_na_names, 
   c_snap_inattention, comminication, chocir_compulsion_impairment, chocir_compulsion_symptom, chocir_obsession_impairment, chocir_obsession_symptom, sdq_columns, date_variables,
   affective_response, FAD, fad_normal, fad_reverse, if_column_name, if_columns, p_fasa_modification, p_fasa_distress, p_fasa_participation, sdq_w_names, sdq_dates, 
   s_cpss_avoidance, s_cpss_hyperarousal, s_cpss_impairment, s_cpss_reexperiencing, s_seq_academic, s_seq_emotional, s_seq_social, how_column_name, task_DB, task_DB_date, 
   how_columns, roles, tot_sum_clin, problem_solving, scared, scared_subscales, cbt_columns, inpatient_columns, clinic_sets, combined, comorbid, file_save_check_time, sibling, f, 
   parent, child, p, c, q, incorrect, correct, dummy, file_save_check, task_sets, 
   file_save_check_combined, ctdb_columns, ctdb_Data_Download_reduced, ctdb_dates, ctdb_names, ctdb_numeric, ctdb_w_plusid, ctdb_w_plusid_child, ctdb_w_plusid_parent,
   ctdb_w_plusid_parent1, ctdb_w_plusid_parent2, c_medsclin_sdq, fill_names, fix_na_cols, c_medsclin1yr_sdq, demo_daily_mfq, imported_hyphen_issue, c_medsclin_combined, old_mdd_form, 
   not_tracked, covid_recode, sscared_tminus2_long, smfq1w_tminus2_long, contact_info, measure_incomplete, c_medsclin1yr_sdq_task, c_medsclin_sdq_task, current_outpatients, hist_check, split1, 
   task_DB_date_before_covid, task_DB_date_since_covid, CBT_report_preint, mfq_change_s, mfq_change_p)
rm(SDQ_Data_Download_raw, SDQ_Data_Download, CTDB_Data_Download) # SDQ_Data_dateadded_fixed, SDQ_Data_dateadded_missing, SDQ_Data_dateadded_uk, SDQ_Data_dateadded_usa
