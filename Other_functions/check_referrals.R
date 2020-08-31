
# importing data ----------------------------------------------------------

if (exists("master_IRTA_latest")==FALSE) {
  irta_master_file <- list.files(path = paste0(IRTA_tracker_location), pattern = "^MASTER_IRTA_DATABASE", all.files = FALSE,
    full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
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

if (exists("master_IRTA_oldest_screens_latest")==FALSE) {
  irta_old_screens_file <- list.files(path = paste0(IRTA_tracker_location), pattern = "^OLD_REFERRALS_DATABASE", all.files = FALSE,
    full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  irta_old_screens_file_time <- file.mtime(paste0(IRTA_tracker_location, "/", irta_old_screens_file)) %>% as.Date()
  irta_old_screens_combined <- tibble(File=c(irta_old_screens_file), Date=c(irta_old_screens_file_time)) %>% arrange(desc(Date)) %>% slice(1)
  master_IRTA_oldest_screens_latest <- read_excel(paste0(IRTA_tracker_location, irta_old_screens_combined[1]))
  date_variabes <- c("DOB", "Screening_Start_Date", "Referral_Date", "Consent_Date", "Clinical_Visit_Date", "Clinicals_date", "Overall_date")
  master_IRTA_oldest_screens_latest[date_variabes] <- lapply(master_IRTA_oldest_screens_latest[date_variabes], as.Date) 
  rm(date_variabes, irta_old_screens_file, irta_old_screens_file_time, irta_old_screens_combined)
} else {
  print("IRTA tracker screens OLD already imported")
}

if (exists("master_IRTA_screens_latest")==FALSE) {
  irta_ongoing_screens_file <- list.files(path = paste0(IRTA_tracker_location), pattern = "^REFERRAL_AND_SCREENING_DATABASE", all.files = FALSE,
    full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  irta_ongoing_screens_file_time <- file.mtime(paste0(IRTA_tracker_location, "/", irta_ongoing_screens_file)) %>% as.Date()
  irta_ongoing_screens_combined <- tibble(File=c(irta_ongoing_screens_file), Date=c(irta_ongoing_screens_file_time)) %>% 
    arrange(desc(Date)) %>% slice(1)
  master_IRTA_screens_latest <- read_excel(paste0(IRTA_tracker_location, irta_ongoing_screens_combined[1]))
  date_variabes <- c("DOB", "Screening_Start_Date", "Referral_Date", "Consent_Date", "Clinical_Visit_Date", "Clinicals_date", "Overall_date")
  master_IRTA_screens_latest[date_variabes] <- lapply(master_IRTA_screens_latest[date_variabes], as.Date) 
  rm(date_variabes, irta_ongoing_screens_file, irta_ongoing_screens_file_time, irta_ongoing_screens_combined)
} else {
  print("IRTA tracker screens current already imported")
}

# contact info for all our participants - to check against new referrals 

active <- master_IRTA_latest %>% select(IRTA_tracker, Initials, FIRST_NAME, LAST_NAME, Child_Phone_Number, Child_Email,
    Referral_Informant_Name, Referral_Informant_Email, Referral_Informant_Phone,
    Parent_Name, FIRST_NAME_P1, LAST_NAME_P1, Parent_Contact_Number, Parent_Email, Street_address, State, Zip, 
    Second_Parent_Name, FIRST_NAME_P2, LAST_NAME_P2, Second_Parent_Contact_Number, Second_Parent_Email, Second_Parent_Address) %>% 
  group_by(Initials) %>% fill(., FIRST_NAME:Second_Parent_Address, .direction = "down") %>% 
  fill(., FIRST_NAME:Second_Parent_Address, .direction = "up") %>% slice(n()) %>% ungroup() %>% mutate(source = "active")
old_screens <- master_IRTA_oldest_screens_latest %>% select(IRTA_tracker, Initials, FIRST_NAME, LAST_NAME, Child_Phone_Number, Child_Email,
    Referral_Informant_Name, Referral_Informant_Email, Referral_Informant_Phone,
    Parent_Name, FIRST_NAME_P1, LAST_NAME_P1, Parent_Contact_Number, Parent_Email, Street_address, State, Zip, 
    Second_Parent_Name, FIRST_NAME_P2, LAST_NAME_P2, Second_Parent_Contact_Number, Second_Parent_Email, Second_Parent_Address) %>% 
  distinct(., .keep_all = TRUE) %>% mutate(source = "old screens")

# merge & clean up 

temp_combined <- merge.default(active, old_screens, all=TRUE) 

split1 <- temp_combined %>% filter(!is.na(Initials)) %>% group_by(Initials) %>% fill(., FIRST_NAME:Second_Parent_Address, .direction = "down") %>% 
  fill(., FIRST_NAME:Second_Parent_Address, .direction = "up") %>% slice(n()) %>% ungroup()
split2 <- temp_combined %>% filter(is.na(Initials))
all_to_check_against <- merge.default(split1, split2, all=TRUE)

all_to_check_against$no_columns <- all_to_check_against %>% select(Initials:Second_Parent_Address) %>% select(-matches("_parent")) %>% ncol() %>% as.numeric()
all_to_check_against$NA_count <- all_to_check_against %>% select(Initials:Second_Parent_Address) %>% select(-matches("_parent")) %>% apply(., 1, count_na)
all_to_check_against$diff <- c(all_to_check_against$no_columns - all_to_check_against$NA_count)
all_to_check_against <- all_to_check_against %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)

# extracting info to check against ----------------------------------------

# phone numbers 
existing_phone <- all_to_check_against %>% select(IRTA_tracker, Initials, Child_Phone_Number, Referral_Informant_Phone, 
  Parent_Contact_Number, Second_Parent_Contact_Number)
existing_phone_long <- pivot_longer(existing_phone, cols=c("Child_Phone_Number", "Referral_Informant_Phone", 
  "Parent_Contact_Number", "Second_Parent_Contact_Number"), names_to = "type", values_to = "Contact_num") %>% 
  select(IRTA_tracker, Initials, Contact_num) %>% filter(!is.na(Contact_num)) %>% mutate(row_id = row_number())

existing_phone_temp_a <- str_split_fixed(existing_phone_long$Contact_num, pattern = c(";"), n=Inf) %>% as.data.frame() %>% mutate(row_id = row_number())
existing_phone_temp_b <- str_split_fixed(existing_phone_temp_a$V1, pattern = c(":"), n=Inf) %>% as.data.frame() %>% mutate(row_id = row_number())
existing_phone_temp_c <- str_split_fixed(existing_phone_temp_a$V2, pattern = c(":"), n=Inf) %>%
  as.data.frame() %>% mutate(row_id = row_number()) %>% rename(V3 = "V1") %>% rename(V4 = "V2")
existing_phone_combined <- merge.default(existing_phone_long, existing_phone_temp_b, all = TRUE) %>% 
  merge.default(., existing_phone_temp_c, all = TRUE) %>% select(-Contact_num)
existing_phone_long2 <- pivot_longer(existing_phone_combined, cols=c("V1", "V2", "V3", "V4"), names_to = "type", values_to = "Contact_num") 
existing_phone_long2$Contact_num <- gsub(" ", "", existing_phone_long2$Contact_num)
existing_phone_long2$Contact_num  <- na_if(existing_phone_long2$Contact_num , "")
existing_phone_long2 <- existing_phone_long2 %>% select(IRTA_tracker, Initials, Contact_num) %>% filter(str_detect(Contact_num, "-")) %>% 
  distinct(., .keep_all = TRUE) %>% mutate(source1 = "existing")

# emails
existing_email <- all_to_check_against %>% select(IRTA_tracker, Initials, Child_Email, Referral_Informant_Email, Parent_Email, Second_Parent_Email)
existing_email_long <- pivot_longer(existing_email, cols=c("Child_Email", "Referral_Informant_Email", "Parent_Email", "Second_Parent_Email"), 
  names_to = "type", values_to = "Contact_email") %>% select(IRTA_tracker, Initials, Contact_email) %>% filter(!is.na(Contact_email)) %>% mutate(row_id = row_number())

existing_email_temp_a <- str_split_fixed(existing_email_long$Contact_email, pattern = c(";"), n=Inf) %>% as.data.frame() %>% mutate(row_id = row_number())
existing_email_combined <- merge.default(existing_email_long, existing_email_temp_a, all = TRUE) %>% select(row_id, IRTA_tracker, Initials, V1, V2)
existing_email_long2 <- pivot_longer(existing_email_combined, cols=c("V1", "V2"), names_to = "type", values_to = "Contact_email") 
existing_email_long2$Contact_email <- gsub(" ", "", existing_email_long2$Contact_email)
existing_email_long2$Contact_email  <- na_if(existing_email_long2$Contact_email , "")
existing_email_long2 <- existing_email_long2 %>% select(IRTA_tracker, Initials, Contact_email) %>% filter(str_detect(Contact_email, "@")) %>% 
  distinct(., .keep_all = TRUE) %>% mutate(source1 = "existing")
existing_email_long2$Contact_email <- tolower(existing_email_long2$Contact_email)

# address
existing_address <- all_to_check_against %>% select(IRTA_tracker, Initials, Street_address, State, Zip, Second_Parent_Address) %>% 
  mutate(row_id = row_number())
existing_address$First_Parent_Address <- paste(existing_address$Street_address, existing_address$State, existing_address$Zip, sep=" ")

existing_address_long <- pivot_longer(existing_address, cols=c("First_Parent_Address", "Second_Parent_Address"), 
  names_to = "type", values_to = "Address") %>% select(IRTA_tracker, Initials, Address) %>% filter(!is.na(Address))
existing_address_long$Address <- gsub(",", "", existing_address_long$Address)
existing_address_long$Address <- gsub("NA ", "", existing_address_long$Address)
existing_address_long$Address <- gsub(" NA", "", existing_address_long$Address)
existing_address_long$Address <- na_if(existing_address_long$Address, "NA")
existing_address_long <- existing_address_long %>% filter(!is.na(Address)) %>% distinct(., .keep_all = TRUE) %>% mutate(source1 = "existing")

# parent name
existing_name_p <- all_to_check_against %>% select(IRTA_tracker, Initials, Parent_Name, FIRST_NAME_P1, LAST_NAME_P1, Referral_Informant_Name, 
  Second_Parent_Name, FIRST_NAME_P2, LAST_NAME_P2)
existing_name_p$Parent_Name1 <- paste(existing_name_p$FIRST_NAME_P1, existing_name_p$LAST_NAME_P1, sep=" ")
existing_name_p$Parent_Name2 <- paste(existing_name_p$FIRST_NAME_P2, existing_name_p$LAST_NAME_P2, sep=" ")

existing_name_p_long <- pivot_longer(existing_name_p, 
  cols=c("Parent_Name1", "Parent_Name2", "Parent_Name", "Second_Parent_Name", "Referral_Informant_Name"), 
  names_to = "type", values_to = "Parent_Name_combined") %>% select(IRTA_tracker, Initials, Parent_Name_combined) %>% filter(!is.na(Parent_Name_combined))
existing_name_p_long$Parent_Name_combined <- toupper(existing_name_p_long$Parent_Name_combined)
existing_name_p_long$Parent_Name_combined <- gsub("NA ", "", existing_name_p_long$Parent_Name_combined)
existing_name_p_long$Parent_Name_combined <- gsub(" NA", "", existing_name_p_long$Parent_Name_combined)
existing_name_p_long$Parent_Name_combined <- na_if(existing_name_p_long$Parent_Name_combined, "NA")
existing_name_p_long <- existing_name_p_long %>% distinct(., .keep_all = TRUE) %>% filter(!is.na(Parent_Name_combined)) %>% mutate(source1 = "existing")

# child name
existing_name_c <- all_to_check_against %>% select(IRTA_tracker, Initials, FIRST_NAME, LAST_NAME) 
existing_name_c$Child_Name <- paste(existing_name_c$FIRST_NAME, existing_name_c$LAST_NAME, sep=" ")
existing_name_c$Child_Name <- gsub("NA ", "", existing_name_c$Child_Name)
existing_name_c$Child_Name <- gsub(" NA", "", existing_name_c$Child_Name)
existing_name_c$Child_Name <- na_if(existing_name_c$Child_Name, "NA")
existing_name_c <- existing_name_c %>% select(IRTA_tracker, Initials, Child_Name) %>% distinct(., .keep_all = TRUE) %>% filter(!is.na(Initials)) %>% 
  mutate(source1 = "existing")

existing_initials <- all_to_check_against %>% select(IRTA_tracker, Initials) %>% distinct(., .keep_all = TRUE) %>% filter(!is.na(Initials)) %>% 
  mutate(source1 = "existing")

# screening data to check against existing --------------------------------

# current screens 

current_screens <- master_IRTA_screens_latest %>% select(IRTA_tracker, Initials, FIRST_NAME, LAST_NAME, Child_Phone_Number, Child_Email,
  Referral_Informant_Name, Referral_Informant_Email, Referral_Informant_Phone,
  Parent_Name, FIRST_NAME_P1, LAST_NAME_P1, Parent_Contact_Number, Parent_Email, Street_address, State, Zip, 
  Second_Parent_Name, FIRST_NAME_P2, LAST_NAME_P2, Second_Parent_Contact_Number, Second_Parent_Email, Second_Parent_Address) %>%
  distinct(., .keep_all = TRUE) %>% mutate(source = "current screens")

# screening phone 

screening_phone <- current_screens %>% select(IRTA_tracker, Initials, Child_Phone_Number, Referral_Informant_Phone, 
  Parent_Contact_Number, Second_Parent_Contact_Number)
screening_phone_long <- pivot_longer(screening_phone, cols=c("Child_Phone_Number", "Referral_Informant_Phone", 
  "Parent_Contact_Number", "Second_Parent_Contact_Number"), names_to = "type", values_to = "Contact_num") %>% 
  select(IRTA_tracker, Initials, Contact_num) %>% filter(!is.na(Contact_num)) %>% mutate(row_id = row_number())

screening_phone_temp_a <- str_split_fixed(screening_phone_long$Contact_num, pattern = c(";"), n=Inf) %>% as.data.frame() %>% mutate(row_id = row_number())
screening_phone_temp_b <- str_split_fixed(screening_phone_temp_a$V1, pattern = c(":"), n=Inf) %>% as.data.frame() %>% mutate(row_id = row_number()) %>% 
  mutate_all(as.character)

possibleError <- tryCatch(
 screening_phone_temp_c <- str_split_fixed(screening_phone_temp_a$V2, pattern = c(":"), n=Inf) %>% as.data.frame() %>% 
    mutate(row_id = row_number()) %>% mutate_all(as.character),
 error=function(e) e)
if (inherits(possibleError, "error")) {
  print("no second phone number")
} else if (nrow(screening_phone_temp_c)==0) {
  print("no second phone number")
  rm(screening_phone_temp_c)
} else {
  screening_phone_temp_c <- str_split_fixed(screening_phone_temp_a$V2, pattern = c(":"), n=Inf) %>% as.data.frame() %>% 
    mutate(row_id = row_number()) %>% mutate_all(as.character)
}

if(exists("screening_phone_temp_c") == TRUE) {

  possibleError2 <- tryCatch(
    screening_phone_temp_c %>% select(V2),
    error=function(e) e)
  if(inherits(possibleError2, "error")) {
    screening_phone_temp_c <- screening_phone_temp_c %>% rename(V3 = "V1")
  } else {
    screening_phone_temp_c <- screening_phone_temp_c %>% rename(V3 = "V1") %>% rename(V4 = "V2")
  }
  screening_phone_combined <- merge.default(screening_phone_long, screening_phone_temp_b, all = TRUE) %>% 
    merge.default(., screening_phone_temp_c, all = TRUE) %>% select(-Contact_num)
  
} else {
  screening_phone_combined <- merge.default(screening_phone_long, screening_phone_temp_b, all = TRUE) %>% select(-Contact_num)
}

screening_phone_long2 <- pivot_longer(screening_phone_combined, cols=c(matches("V")), names_to = "type", values_to = "Contact_num") 
screening_phone_long2$Contact_num <- gsub(" ", "", screening_phone_long2$Contact_num)
screening_phone_long2$Contact_num  <- na_if(screening_phone_long2$Contact_num , "")
screening_phone_long2 <- screening_phone_long2 %>% select(IRTA_tracker, Initials, Contact_num) %>% filter(str_detect(Contact_num, "-")) %>% 
  rename(IRTA_screen = "IRTA_tracker") %>% distinct(., .keep_all = TRUE) %>% mutate(source2 = "screening")

# screening email 
screening_email <- current_screens %>% select(IRTA_tracker, Initials, Child_Email, Referral_Informant_Email, Parent_Email, Second_Parent_Email) 
screening_email_long <- pivot_longer(screening_email, cols=c("Child_Email", "Referral_Informant_Email", "Parent_Email", "Second_Parent_Email"), 
  names_to = "type", values_to = "Contact_email") %>% select(IRTA_tracker, Initials, Contact_email) %>% distinct(., .keep_all = TRUE) %>% 
  filter(!is.na(Contact_email)) %>% mutate(row_id = row_number())
screening_email_long$Contact_email <- tolower(screening_email_long$Contact_email)
screening_email_temp <- str_split_fixed(screening_email_long$Contact_email, pattern = c(";"), n=Inf) %>% as.data.frame() %>% mutate(row_id = row_number())
screening_email_combined <- merge.default(screening_email_long, screening_email_temp, all = TRUE) %>% select(-Contact_email)

# possibleError <- tryCatch(
#   error=function(e) e)
# if(inherits(possibleError, "error")) {
# }

possibleError3 <- tryCatch(
  screening_email_long2 <- pivot_longer(screening_email_combined, cols=c("V1", "V2"), names_to = "type", values_to = "Contact_email") %>% 
    select(IRTA_tracker, Initials, Contact_email) %>% distinct(., .keep_all = TRUE) %>% filter(str_detect(Contact_email, "@")) %>% 
    rename(IRTA_screen = "IRTA_tracker") %>% distinct(., .keep_all = TRUE) %>% mutate(source2 = "screening"),
  error=function(e) e)
if(inherits(possibleError3, "error")) {
  screening_email_long2 <- screening_email_combined %>% rename(Contact_email = "V1") %>% 
    select(IRTA_tracker, Initials, Contact_email) %>% distinct(., .keep_all = TRUE) %>% filter(str_detect(Contact_email, "@")) %>% 
    rename(IRTA_screen = "IRTA_tracker") %>% distinct(., .keep_all = TRUE) %>% mutate(source2 = "screening")
} else {
  screening_email_long2 <- pivot_longer(screening_email_combined, cols=c("V1", "V2"), names_to = "type", values_to = "Contact_email") %>% 
    select(IRTA_tracker, Initials, Contact_email) %>% distinct(., .keep_all = TRUE) %>% filter(str_detect(Contact_email, "@")) %>% 
    rename(IRTA_screen = "IRTA_tracker") %>% distinct(., .keep_all = TRUE) %>% mutate(source2 = "screening")
}
screening_email_long2$Contact_email <- gsub(" ", "", screening_email_long2$Contact_email)

# screening address 
screening_address <- current_screens %>% select(IRTA_tracker, Initials, Street_address, State, Zip, Second_Parent_Address) %>% 
  mutate(row_id = row_number())
screening_address$First_Parent_Address <- paste(screening_address$Street_address, screening_address$State, screening_address$Zip, sep=" ")

screening_address_long <- pivot_longer(screening_address, cols=c("First_Parent_Address", "Second_Parent_Address"), 
  names_to = "type", values_to = "Address") %>% select(IRTA_tracker, Initials, Address) %>% filter(!is.na(Address))
screening_address_long$Address <- gsub(",", "", screening_address_long$Address)
screening_address_long$Address <- gsub("NA ", "", screening_address_long$Address)
screening_address_long$Address <- gsub(" NA", "", screening_address_long$Address)
screening_address_long$Address <- na_if(screening_address_long$Address, "NA")
screening_address_long <- screening_address_long %>% filter(!is.na(Address)) %>% distinct(., .keep_all = TRUE) %>% 
  rename(IRTA_screen = "IRTA_tracker") %>% mutate(source2 = "screening")

# screening parent name 
screening_name_p <- current_screens %>% select(IRTA_tracker, Initials, Parent_Name, FIRST_NAME_P1, LAST_NAME_P1, Referral_Informant_Name, 
  Second_Parent_Name, FIRST_NAME_P2, LAST_NAME_P2)
screening_name_p$Parent_Name1 <- paste(screening_name_p$FIRST_NAME_P1, screening_name_p$LAST_NAME_P1, sep=" ")
screening_name_p$Parent_Name2 <- paste(screening_name_p$FIRST_NAME_P2, screening_name_p$LAST_NAME_P2, sep=" ")

screening_name_p_long <- pivot_longer(screening_name_p, 
  cols=c("Parent_Name1", "Parent_Name2", "Parent_Name", "Second_Parent_Name", "Referral_Informant_Name"), 
  names_to = "type", values_to = "Parent_Name_combined") %>% select(IRTA_tracker, Initials, Parent_Name_combined) %>% filter(!is.na(Parent_Name_combined))
screening_name_p_long$Parent_Name_combined <- toupper(screening_name_p_long$Parent_Name_combined)
screening_name_p_long$Parent_Name_combined <- gsub("NA ", "", screening_name_p_long$Parent_Name_combined)
screening_name_p_long$Parent_Name_combined <- gsub(" NA", "", screening_name_p_long$Parent_Name_combined)
screening_name_p_long$Parent_Name_combined <- na_if(screening_name_p_long$Parent_Name_combined, "NA")
screening_name_p_long <- screening_name_p_long %>% distinct(., .keep_all = TRUE) %>% filter(!is.na(Parent_Name_combined)) %>% 
  rename(IRTA_screen = "IRTA_tracker") %>% mutate(source2 = "screening")

# screening child name 

# child name
screening_name_c <- current_screens %>% select(IRTA_tracker, Initials, FIRST_NAME, LAST_NAME) 
screening_name_c$Child_Name <- paste(screening_name_c$FIRST_NAME, screening_name_c$LAST_NAME, sep=" ")
screening_name_c$Child_Name <- gsub("NA ", "", screening_name_c$Child_Name)
screening_name_c$Child_Name <- gsub(" NA", "", screening_name_c$Child_Name)
screening_name_c$Child_Name <- na_if(screening_name_c$Child_Name, "NA")
screening_name_c <- screening_name_c %>% select(IRTA_tracker, Initials, Child_Name) %>% distinct(., .keep_all = TRUE) %>% 
  filter(!is.na(Child_Name)) %>% rename(IRTA_screen = "IRTA_tracker") %>% mutate(source2 = "screening")

screening_initials <- current_screens %>% select(Initials, IRTA_tracker) %>% distinct(., .keep_all = TRUE) %>% 
  filter(!is.na(Initials)) %>% rename(IRTA_screen = "IRTA_tracker") %>% mutate(source2 = "screening")

# checking the screening against the existing -----------------------------

phone <- merge.default(existing_phone_long2, screening_phone_long2, all = TRUE) %>% filter(!is.na(source1) & !is.na(source2))
email <- merge.default(existing_email_long2, screening_email_long2, all = TRUE) %>% filter(!is.na(source1) & !is.na(source2))
address <- merge.default(existing_address_long, screening_address_long, all = TRUE) %>% filter(!is.na(source1) & !is.na(source2))
parent_name <- merge.default(existing_name_p_long, screening_name_p_long, all = TRUE) %>% filter(!is.na(source1) & !is.na(source2))
child_name <- merge.default(existing_name_c, screening_name_c, all = TRUE) %>% filter(!is.na(source1) & !is.na(source2))
initials <- merge.default(existing_initials, screening_initials, all = TRUE) %>% filter(!is.na(source1) & !is.na(source2))

all_repeats <- merge.default(initials, phone, all = TRUE) %>% merge.default(., email, all = TRUE) %>% merge.default(., address, all = TRUE) %>% 
  merge.default(., parent_name, all = TRUE) %>% merge.default(., child_name, all = TRUE) %>% select(-source1, -source2)

all_repeats %>% write_xlsx(paste0(IRTA_tracker_location,"QCing/Screens_already_in_DB_", todays_date_formatted, ".xlsx"))
 
rm(split1, split2, screening_address, screening_address_long, screening_email, screening_email_combined, screening_email_long, 
  screening_email_long2, screening_email_temp, screening_initials, screening_name_c, screening_name_p, screening_name_p_long,
  screening_phone, screening_phone_combined, screening_phone_long, screening_phone_long2, screening_phone_temp_a, screening_phone_temp_b,
  screening_phone_temp_c, active, address, all_to_check_against, child_name, email, existing_address, existing_address_long, 
  existing_email, existing_email_combined, existing_email_long, existing_email_long2, existing_email_temp_a, existing_initials, 
  existing_name_c, existing_name_p, existing_name_p_long, existing_phone, existing_phone_combined, existing_phone_long, 
  existing_phone_long2, existing_phone_temp_a, existing_phone_temp_b, existing_phone_temp_c, initials, phone, parent_name, 
  current_screens, old_screens, temp_combined)
