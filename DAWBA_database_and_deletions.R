
#***********DAWBA = diagnostic information from participant screening

# to do: 

# Loading patient info ----------------------------------------------------

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

if (exists("master_IRTA_oldest_screens_latest")==FALSE) {
  irta_old_screens_file <- list.files(path = paste0(IRTA_tracker_location), pattern = "^OLD_REFERRALS_DATABASE", all.files = FALSE,
                                      full.names = FALSE, recursive = FALSE,
                                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  irta_old_screens_file_time <- file.mtime(paste0(IRTA_tracker_location, "/", irta_old_screens_file)) %>% as.Date()
  irta_old_screens_combined <- tibble(File=c(irta_old_screens_file), Date=c(irta_old_screens_file_time)) %>% arrange(desc(Date)) %>% slice(1)
  master_IRTA_oldest_screens_latest <- read_excel(paste0(IRTA_tracker_location, irta_old_screens_combined[1]))
  date_variabes <- c("DOB", "Screening_Start_Date", "Referral_Date", "Consent_Date", "Clinical_Visit_Date", "Clinicals_date", "Overall_date")
  master_IRTA_oldest_screens_latest[date_variabes] <- lapply(master_IRTA_oldest_screens_latest[date_variabes], as.Date) 
  rm(date_variabes, irta_old_screens_file, irta_old_screens_file_time, irta_old_screens_combined)
} else {
  print("master IRTA tracker + QC info already imported")
}

if (exists("master_IRTA_screens_latest")==FALSE) {
  irta_ongoing_screens_file <- list.files(path = paste0(IRTA_tracker_location), pattern = "^REFERRAL_AND_SCREENING_DATABASE", all.files = FALSE,
                                          full.names = FALSE, recursive = FALSE,
                                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  irta_ongoing_screens_file_time <- file.mtime(paste0(IRTA_tracker_location, "/", irta_ongoing_screens_file)) %>% as.Date()
  irta_ongoing_screens_combined <- tibble(File=c(irta_ongoing_screens_file), Date=c(irta_ongoing_screens_file_time)) %>% 
    arrange(desc(Date)) %>% slice(1)
  master_IRTA_screens_latest <- read_excel(paste0(IRTA_tracker_location, irta_ongoing_screens_combined[1]))
  date_variabes <- c("DOB", "Screening_Start_Date", "Referral_Date", "Consent_Date", "Clinical_Visit_Date", "Clinicals_date", "Overall_date")
  master_IRTA_screens_latest[date_variabes] <- lapply(master_IRTA_screens_latest[date_variabes], as.Date) 
  rm(date_variabes, irta_ongoing_screens_file, irta_ongoing_screens_file_time, irta_ongoing_screens_combined)
} else {
  print("master IRTA tracker + QC info already imported")
}

# Isolating participant info I need & merging -----------------------------

master_IRTA_identifiers <- master_IRTA_latest %>% 
  select(FIRST_NAME, LAST_NAME, Initials, DAWBA_ID, PLUSID, SDAN, IRTA_tracker, SEX, DOB, Participant_Type2, 
         Clinical_Visit_Date, Screening_Start_Date, Referral_Date, Overall_date, 
         Eligible, Eligibility_notes, Scheduling_status, Scheduling_status_notes, 
         Parent_e_consented, Child_e_assented, Parent_DAWBA_completed, Child_DAWBA_completed, DAWBA_completed) %>% 
  group_by(Initials) %>% fill(., DAWBA_ID, PLUSID, .direction = c("down")) %>% fill(., DAWBA_ID, PLUSID, .direction = c("up")) %>%
  arrange(Initials, Overall_date) %>% filter(1:n() == 1) %>% ungroup() %>% 
  mutate(Source = "Current participants") %>% filter(!is.na(DAWBA_ID) | !is.na(PLUSID)) 

master_current_screen_identifiers <- master_IRTA_screens_latest %>% 
  select(FIRST_NAME, LAST_NAME, Initials, DAWBA_ID, PLUSID, SDAN, IRTA_tracker, SEX, DOB, Participant_Type2, 
         Clinical_Visit_Date, Screening_Start_Date, Referral_Date, Overall_date, 
         Eligible, Eligibility_notes, Scheduling_status, Scheduling_status_notes, 
         Parent_e_consented, Child_e_assented, Parent_DAWBA_completed, Child_DAWBA_completed, DAWBA_completed) %>% 
  mutate(Source = "Current screens") %>% filter(!is.na(DAWBA_ID) | !is.na(PLUSID))

master_old_screen_identifiers <- master_IRTA_oldest_screens_latest %>% 
  select(FIRST_NAME, LAST_NAME, Initials, DAWBA_ID, PLUSID, SDAN, IRTA_tracker, SEX, DOB, Participant_Type2, 
         Clinical_Visit_Date, Screening_Start_Date, Referral_Date, Overall_date, 
         Eligible, Eligibility_notes, Scheduling_status, Scheduling_status_notes, 
         Parent_e_consented, Child_e_assented, Parent_DAWBA_completed, Child_DAWBA_completed, DAWBA_completed) %>% 
  mutate(Source = "OLD screens") %>% filter(!is.na(DAWBA_ID) | !is.na(PLUSID))

participant_identifiers_combined <- merge.default(master_IRTA_identifiers, master_current_screen_identifiers, all=TRUE) %>% 
  merge.default(., master_old_screen_identifiers, all=TRUE)
fill_names <- participant_identifiers_combined %>% select(-Initials) %>% colnames()
participant_identifiers_combined <- participant_identifiers_combined %>% group_by(Initials) %>% 
  fill(., names(fill_names), .direction = c("down")) %>% 
  fill(., names(fill_names), .direction = c("up")) %>% 
  arrange(Initials, Source) %>% filter(1:n() == 1) %>% ungroup()

# finish the split below

split1 <- colsplit(participant_identifiers_combined$DAWBA_ID, "/", names = c("DAWBA1", "DAWBA2"))
participant_identifiers_combined <- cbind(participant_identifiers_combined, split1) 
participant_identifiers_combined <- melt(data = participant_identifiers_combined, id.vars = 
  c("FIRST_NAME", "LAST_NAME", "Initials", "PLUSID", "SDAN", "IRTA_tracker", "SEX", "DOB", "Participant_Type2", "Clinical_Visit_Date", "Screening_Start_Date", 
    "Referral_Date", "Overall_date", "Parent_e_consented", "Child_e_assented", "Parent_DAWBA_completed", "Child_DAWBA_completed", "DAWBA_completed",
    "Eligible", "Eligibility_notes", "Scheduling_status", "Scheduling_status_notes", "Source"), measure.vars = c("DAWBA1", "DAWBA2")) %>% 
  rename(DAWBA_ID = value) %>% mutate(DAWBA_ID = as.character(DAWBA_ID)) %>% filter(variable=="DAWBA1" | !is.na(DAWBA_ID)) %>% select(-variable)

# DAWBA import ------------------------------------------------------------

# new DAWBA import:

DAWBA_Data_Download_raw <- read.delim(paste0(dawba_pull, latest_dawba_pull, ".csv"),  sep="\t", quote="", encoding="UTF-8", row.names = NULL, header = TRUE, stringsAsFactors = FALSE)

DAWBA_Data_Download_raw[DAWBA_Data_Download_raw==-2] <- NA
DAWBA_Data_Download_raw %>% write_xlsx(paste0(dawba_pull, "old/DAWBA_", todays_date_formatted, "_raw.xlsx"))

# dawba_col_names <-  DAWBA_Data_Download_raw %>% colnames() %>% as.data.frame()
# dawba_col_names %>% write_xlsx(paste0(dawba_pull, "DAWBA_colnames_", todays_date_formatted, ".xlsx"))

# importing existing DAWBA archive:

DAWBA_Archive <- read_excel(paste0(database_location, "other_data_never_delete/dawba_archive_raw.xlsx")) 

# merging old & new, clean up & then save new DAWBA archive 

dawba_combined <- merge.default(DAWBA_Archive, DAWBA_Data_Download_raw, all=TRUE) %>% 
  filter(sid !="234110") %>% filter(sid !="234111") %>% filter(sid !="234112") %>% filter(sid !="234113") %>% filter(sid !="None")
fill_names <- dawba_combined %>% select(-sid) %>% colnames()
dawba_combined[fill_names] <- lapply(dawba_combined[fill_names], na_if, "")
dawba_combined <- dawba_combined %>% 
  group_by(sid) %>%
  fill(., names(fill_names), .direction = "down") %>%
  fill(., names(fill_names), .direction = "up") %>%
  ungroup() %>%
  distinct(., .keep_all = TRUE)

dawba_combined %>% write_xlsx(paste0(dawba_pull, "old/dawba_archive_raw_", todays_date_formatted, ".xlsx"))
dawba_combined %>% write_xlsx(paste0(database_location, "other_data_never_delete/dawba_archive_raw.xlsx"))

# reducing down to the DAWBA variables we're interested in right now

dawba_columns <- read_excel(paste0(database_location, "other_data_never_delete/dawba_column_names_and_descriptions.xlsx"))
setnames(dawba_combined, old=c(dawba_columns$old_name), new=c(dawba_columns$new_name), skip_absent=TRUE)

DAWBA_Data_Download <- dawba_combined %>% select(dawba_columns$new_name) %>% arrange(DAWBA_ID) 

# Clean up -------------------------------------------

DAWBA_Data_Download$DAWBA_SEX[DAWBA_Data_Download$DAWBA_SEX==1] <- 'MALE'
DAWBA_Data_Download$DAWBA_SEX[DAWBA_Data_Download$DAWBA_SEX==2] <- 'FEMALE'

dawba_date_variables <- c("p_dawba_sdq_date", "s_dawba_sdq_date")
DAWBA_Data_Download[dawba_date_variables] <- lapply(DAWBA_Data_Download[dawba_date_variables], as.Date, "%d.%m.%y")

dawba_w_names <- merge.default(participant_identifiers_combined, DAWBA_Data_Download, all=TRUE) %>% filter(!is.na(Initials) | !is.na(dawba_logins))

dawba_w_names$Eligible <- recode(dawba_w_names$Eligible, "0"="Include", 
                         "1"="Include: can't scan (braces, etc.)","2"="On hold: contact again after specified amount of time","3"="On hold: low priority",
                         "4"="Excluded: cannot be reached or scheduled, all contact options exhausted ",
                         "5"="Excluded: does not meet criteria", 
                         "6"="Excluded: meets exclusionary criteria (substance use, psychosis, etc.)", 
                         "7"="Did not or withdrew assent/consent", "8"="Ruled as ineligible for treatment during baseline assessment (didn't meet inclusionary or met exclusionary criteria)", 
                         "9"="Patient (or parent) withdrew from treatment", "10"="Excluded after commencing treatment: some treatment received before participant was later excluded (e.g. bad scanner, now meets exclusionary criteria, etc.)", 
                         "11"="Completed treatment", .missing = NULL)

# Adding BDD predictions --------------------------------------------------

#####
# Parent

# criterion a = a lot of worry beyond normal (pz1) plus a lot of worry about a specific body part (pz2a-pz2i)

pca <- dawba_w_names %>% select(DAWBA_ID, PLUSID, Initials, p_dawba_bdd_1_concerns_appearance, matches("p_dawba_bdd_2"), -p_dawba_bdd_2_text)
pca[,4:ncol(pca)]  <- lapply(pca[,4:ncol(pca)], as.numeric)
pca$no_columns <- pca %>% select(p_dawba_bdd_1_concerns_appearance, matches('p_dawba_bdd_2')) %>% ncol() %>% as.numeric()
pca$NA_count <- pca %>% select(p_dawba_bdd_1_concerns_appearance, matches('p_dawba_bdd_2')) %>% apply(., 1, count_na)
pca$diff <- c(pca$no_columns - pca$NA_count)
pca <- pca %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
pca$p_dawba_bdd_2_sum <- pca %>% select(matches("p_dawba_bdd_2")) %>% rowSums(na.rm=TRUE)
pca2 <- pca %>% filter(p_dawba_bdd_1_concerns_appearance=="2" | 
                         (p_dawba_bdd_1_concerns_appearance=="1" & 
                            (p_dawba_bdd_2a_skin_condition=="2" | p_dawba_bdd_2b_skin_colour=="2" | p_dawba_bdd_2c_hair_colour_or_condition=="2" | 
                               p_dawba_bdd_2d_muscle_bulk=="2" | p_dawba_bdd_2e_body_shape_or_size=="2" | 
                               p_dawba_bdd_2f_facial_features=="2" | p_dawba_bdd_2g_other_body_part=="2" | 
                               p_dawba_bdd_2h_asymmetry=="2" | p_dawba_bdd_2i_other_aspect_of_appearance=="2"))) %>% 
  mutate(p_dawba_bdd_criterion_a = 1) %>% select(DAWBA_ID, p_dawba_bdd_criterion_a)
pca <- left_join(pca, pca2, all=TRUE) %>% select(DAWBA_ID, PLUSID, Initials, p_dawba_bdd_2_sum, p_dawba_bdd_criterion_a)

# criterion b = repetitive behaviours

pcb <- dawba_w_names %>% select(DAWBA_ID, PLUSID, Initials, matches("p_dawba_bdd_4"))
pcb[,4:ncol(pcb)]  <- lapply(pcb[,4:ncol(pcb)], FUN = function(x) recode(x, `0`=0, `1`=0, `2`=1, .missing = NULL))
pcb$no_columns <- pcb %>% select(matches('p_dawba_bdd_4')) %>% ncol() %>% as.numeric()
pcb$NA_count <- pcb %>% select(matches('p_dawba_bdd_4')) %>% apply(., 1, count_na)
pcb$diff <- c(pcb$no_columns - pcb$NA_count)
pcb <- pcb %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
pcb$p_dawba_bdd_4_sum <- pcb %>% select(matches("p_dawba_bdd_4")) %>% rowSums(na.rm=TRUE)
pcb <- pcb %>% mutate(p_dawba_bdd_criterion_b = ifelse(p_dawba_bdd_4_sum>2, 1, NA)) %>% 
  select(DAWBA_ID, PLUSID, Initials, p_dawba_bdd_4_sum, p_dawba_bdd_criterion_b)

# criterion c = clinically significant distress 

pcc <- dawba_w_names %>% 
  select(DAWBA_ID, PLUSID, Initials, p_dawba_bdd_5a_time_spent_worrying_appearance,
         p_dawba_bdd_5b_time_spent_hiding_improving_appearance,
         p_dawba_bdd_8_distress, p_dawba_bdd_9a_impact_on_family_life,
         p_dawba_bdd_9b_impact_on_friendships, p_dawba_bdd_9c_impact_on_learning,
         p_dawba_bdd_9d_impact_on_leisure) 
pcc[,4:5]  <- lapply(pcc[,4:5], FUN = function(x) recode(x, `0`=0, `1`=0, `2`=0, `3`=3, `4`=3, .missing = NULL))
pcc <- pcc %>% filter(p_dawba_bdd_5a_time_spent_worrying_appearance=="3" | p_dawba_bdd_5b_time_spent_hiding_improving_appearance=="3" |
                        p_dawba_bdd_8_distress=="3" | p_dawba_bdd_9a_impact_on_family_life=="3" |
                        p_dawba_bdd_9b_impact_on_friendships=="3" | p_dawba_bdd_9b_impact_on_friendships=="3" |
                        p_dawba_bdd_9c_impact_on_learning=="3" | p_dawba_bdd_9d_impact_on_leisure=="3") %>% 
  mutate(p_dawba_bdd_criterion_c = 1) %>% 
  select(DAWBA_ID, PLUSID, Initials, p_dawba_bdd_criterion_c)

# recombining 

p_bdd_combined <- merge.default(pca, pcb, all=TRUE) %>% merge.default(., pcc, all=TRUE)

p_bdd_combined$p_dawba_bdd_criterion_a[is.na(p_bdd_combined$p_dawba_bdd_criterion_a)] <- 0
p_bdd_combined$p_dawba_bdd_criterion_b[is.na(p_bdd_combined$p_dawba_bdd_criterion_b)] <- 0
p_bdd_combined$p_dawba_bdd_criterion_c[is.na(p_bdd_combined$p_dawba_bdd_criterion_c)] <- 0

# determining whether a diagnosis is met 

p_bdd_combined <- p_bdd_combined %>% mutate(p_dawba_bdd_diag = (as.numeric(p_dawba_bdd_criterion_a) + as.numeric(p_dawba_bdd_criterion_b) + as.numeric(p_dawba_bdd_criterion_c)))

#####
# Child

sca <- dawba_w_names %>% select(DAWBA_ID, PLUSID, Initials, s_dawba_bdd_1_concerns_appearance, matches("s_dawba_bdd_2"), -s_dawba_bdd_2_text)
sca[,4:ncol(sca)]  <- lapply(sca[,4:ncol(sca)], as.numeric)
sca$no_columns <- sca %>% select(s_dawba_bdd_1_concerns_appearance, matches('s_dawba_bdd_2')) %>% ncol() %>% as.numeric()
sca$NA_count <- sca %>% select(s_dawba_bdd_1_concerns_appearance, matches('s_dawba_bdd_2')) %>% apply(., 1, count_na)
sca$diff <- c(sca$no_columns - sca$NA_count)
sca <- sca %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
sca$s_dawba_bdd_2_sum <- sca %>% select(matches("s_dawba_bdd_2")) %>% rowSums(na.rm=TRUE)
sca2 <- sca %>% filter(s_dawba_bdd_1_concerns_appearance=="2" | 
                         (s_dawba_bdd_1_concerns_appearance=="1" & 
                            (s_dawba_bdd_2a_skin_condition=="2" | s_dawba_bdd_2b_skin_colour=="2" | s_dawba_bdd_2c_hair_colour_or_condition=="2" | 
                               s_dawba_bdd_2d_muscle_bulk=="2" | s_dawba_bdd_2e_body_shape_or_size=="2" | 
                               s_dawba_bdd_2f_facial_features=="2" | s_dawba_bdd_2g_other_body_part=="2" | 
                               s_dawba_bdd_2h_asymmetry=="2" | s_dawba_bdd_2i_other_aspect_of_appearance=="2"))) %>% 
  mutate(s_dawba_bdd_criterion_a = 1) %>% select(DAWBA_ID, s_dawba_bdd_criterion_a)
sca <- left_join(sca, sca2, all=TRUE) %>% select(DAWBA_ID, PLUSID, Initials, s_dawba_bdd_2_sum, s_dawba_bdd_criterion_a)

# criterion b = repetitive behaviours

scb <- dawba_w_names %>% select(DAWBA_ID, PLUSID, Initials, matches("s_dawba_bdd_4"))
scb[,4:ncol(scb)]  <- lapply(scb[,4:ncol(scb)], FUN = function(x) recode(x, `0`=0, `1`=0, `2`=1, .missing = NULL))
scb$no_columns <- scb %>% select(matches('s_dawba_bdd_4')) %>% ncol() %>% as.numeric()
scb$NA_count <- scb %>% select(matches('s_dawba_bdd_4')) %>% apply(., 1, count_na)
scb$diff <- c(scb$no_columns - scb$NA_count)
scb <- scb %>% filter(diff>0) %>% select(-no_columns, -NA_count, -diff)
scb$s_dawba_bdd_4_sum <- scb %>% select(matches("s_dawba_bdd_4")) %>% rowSums(na.rm=TRUE)
scb <- scb %>% mutate(s_dawba_bdd_criterion_b = ifelse(s_dawba_bdd_4_sum>2, 1, NA)) %>% 
  select(DAWBA_ID, PLUSID, Initials, s_dawba_bdd_4_sum, s_dawba_bdd_criterion_b)

# criterion c = clinically significant distress 

scc <- dawba_w_names %>% 
  select(DAWBA_ID, PLUSID, Initials, s_dawba_bdd_5a_time_spent_worrying_appearance,
         s_dawba_bdd_5b_time_spent_hiding_improving_appearance,
         s_dawba_bdd_8_distress, s_dawba_bdd_9a_impact_on_family_life,
         s_dawba_bdd_9b_impact_on_friendships, s_dawba_bdd_9c_impact_on_learning,
         s_dawba_bdd_9d_impact_on_leisure) 
scc[,4:5]  <- lapply(scc[,4:5], FUN = function(x) recode(x, `0`=0, `1`=0, `2`=0, `3`=3, `4`=3, .missing = NULL))
scc <- scc %>% filter(s_dawba_bdd_5a_time_spent_worrying_appearance=="3" | s_dawba_bdd_5b_time_spent_hiding_improving_appearance=="3" |
                        s_dawba_bdd_8_distress=="3" | s_dawba_bdd_9a_impact_on_family_life=="3" |
                        s_dawba_bdd_9b_impact_on_friendships=="3" | s_dawba_bdd_9b_impact_on_friendships=="3" |
                        s_dawba_bdd_9c_impact_on_learning=="3" | s_dawba_bdd_9d_impact_on_leisure=="3") %>% 
  mutate(s_dawba_bdd_criterion_c = 1) %>% 
  select(DAWBA_ID, PLUSID, Initials, s_dawba_bdd_criterion_c)

# recombining 

s_bdd_combined <- merge.default(sca, scb, all=TRUE) %>% merge.default(., scc, all=TRUE)

s_bdd_combined$s_dawba_bdd_criterion_a[is.na(s_bdd_combined$s_dawba_bdd_criterion_a)] <- 0
s_bdd_combined$s_dawba_bdd_criterion_b[is.na(s_bdd_combined$s_dawba_bdd_criterion_b)] <- 0
s_bdd_combined$s_dawba_bdd_criterion_c[is.na(s_bdd_combined$s_dawba_bdd_criterion_c)] <- 0

# determining whether a diagnosis is met 

s_bdd_combined <- s_bdd_combined %>% mutate(s_dawba_bdd_diag = (as.numeric(s_dawba_bdd_criterion_a) + as.numeric(s_dawba_bdd_criterion_b) + as.numeric(s_dawba_bdd_criterion_c)))

#####
# Integrating the BDD probabilities into the DAWBA database

all_bdd_combined <- merge.default(s_bdd_combined, p_bdd_combined, all=TRUE)
fill_names <- all_bdd_combined %>% select(-DAWBA_ID) %>% colnames()
all_bdd_combined[fill_names] <- lapply(all_bdd_combined[fill_names], na_if, "")
all_bdd_combined <- all_bdd_combined %>% group_by(DAWBA_ID) %>% 
  fill(., names(fill_names), .direction = "down") %>%
  fill(., names(fill_names), .direction = "up") %>%
  distinct(., .keep_all = TRUE) %>% ungroup()

dawba_w_names <- merge.default(dawba_w_names, all_bdd_combined, all=TRUE) 
fill_names <- dawba_w_names %>% select(-DAWBA_ID, -matches("date"), -matches("Date"), -DOB) %>% colnames()
dawba_w_names[fill_names] <- lapply(dawba_w_names[fill_names], na_if, "")
fill_names <- dawba_w_names %>% select(-DAWBA_ID) %>% colnames()
dawba_w_names <- dawba_w_names %>% group_by(DAWBA_ID) %>% 
  fill(., names(fill_names), .direction = "down") %>%
  fill(., names(fill_names), .direction = "up") %>%
  arrange(DAWBA_ID, dawba_logins) %>% slice(1) %>% 
  ungroup()

# Exporting the database --------------------------------------------------

dawba_w_names %>% write_xlsx(paste0(dawba_pull, "old/dawba_archive_", todays_date_formatted, ".xlsx"))
dawba_w_names %>% write_xlsx(paste0(database_location, "MASTER_DATABASE_DAWBA.xlsx"))

# DAWBA deletions ---------------------------------------------------------

# creating lists of DAWBA IDs to delete 

dawba_removal <- DAWBA_Data_Download_raw %>% filter(!is.na(logins)) %>% 
  select(sid, firstcreated, logins, plogins, slogins, psdqdate, ssdqdate) %>% 
  rename(DAWBA_ID="sid")

dawba_removal$psdqdate <- as.Date(dawba_removal$psdqdate, "%d.%m.%y")
dawba_removal$ssdqdate <- as.Date(dawba_removal$ssdqdate, "%d.%m.%y")
dawba_removal$Overall_date <- coalesce(dawba_removal$ssdqdate, dawba_removal$psdqdate)
dawba_removal <- dawba_removal %>% filter(!is.na(Overall_date))

dawba_removal$since_sdq <- ((difftime(dawba_removal$Overall_date, todays_date_formatted, units = "weeks"))/4) %>% round(., digits = 2)

# SDQ completed > 4 months ago 

dawba_removal %>% filter(since_sdq < -5) %>% select(DAWBA_ID, Overall_date, since_sdq) %>% 
  write_xlsx(paste0(database_location, "dawba_deletions/", "dawba_greater_3m_", todays_date_formatted, ".xlsx"))

# Removing unnecessary variables ------------------------------------------

rm(pca, pca2, pcb, pcc, sca, sca2, scb, scc, split1, s_bdd_combined, p_bdd_combined, participant_identifiers_combined, all_bdd_combined, 
   master_IRTA_identifiers, master_current_screen_identifiers, master_old_screen_identifiers, DAWBA_Data_Download, 
   DAWBA_Data_Download_raw, DAWBA_Archive, dawba_combined, dawba_columns, dawba_date_variables, dawba_removal)
