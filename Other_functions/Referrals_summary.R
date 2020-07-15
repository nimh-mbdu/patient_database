
# clean up ----------------------------------------------------------------

##### current participants 

current_participants_identifiers <- master_IRTA_latest %>% select(Initials, DAWBA_ID, PLUSID, SDAN, IRTA_tracker, SEX, DOB, Participant_Type2, Protocol, 
                                                                  Clinical_Visit_Date, Screening_Start_Date, Referral_Date, Eligible, Eligibility_notes, Scheduling_status, Scheduling_status_notes, 
                                                                  Parent_e_consented, Child_e_assented, Parent_DAWBA_completed, Child_DAWBA_completed, DAWBA_completed) 
current_participants_identifiers$Overall_date <- coalesce(current_participants_identifiers$Referral_Date, current_participants_identifiers$Screening_Start_Date) 
current_participants_identifiers$Overall_date <- coalesce(current_participants_identifiers$Overall_date, current_participants_identifiers$Clinical_Visit_Date) 
current_participants_identifiers$temp <- coalesce(current_participants_identifiers$Clinical_Visit_Date, current_participants_identifiers$Overall_date) 

current_participants_identifiers <- current_participants_identifiers %>% filter(Participant_Type2=="MDD" | Participant_Type2=="HV") %>% 
  group_by(Initials) %>% fill(., DAWBA_ID:DAWBA_completed, .direction = c("down")) %>% fill(., DAWBA_ID:DAWBA_completed, .direction = c("up")) %>%
  arrange(Initials, temp) %>% slice(1) %>% ungroup() %>% filter(str_detect(Protocol, "0037") | is.na(Protocol)) %>% select(-Protocol, -temp) %>% 
  mutate(Source = "Current participants")

##### current screens 

current_screen_identifiers <- master_IRTA_screens_latest %>% select(Initials, DAWBA_ID, PLUSID, SDAN, IRTA_tracker, SEX, DOB, Participant_Type2, 
                                                                    Clinical_Visit_Date, Screening_Start_Date, Referral_Date, Eligible, Eligibility_notes, Scheduling_status, Scheduling_status_notes,  
                                                                    Parent_e_consented, Child_e_assented, Parent_DAWBA_completed, Child_DAWBA_completed, DAWBA_completed, Overall_date) 
current_screen_identifiers$Overall_date <- coalesce(current_screen_identifiers$Overall_date, current_screen_identifiers$Clinical_Visit_Date) 

current_screen_identifiers <- current_screen_identifiers %>% group_by(Initials) %>%
  fill(., DAWBA_ID:Overall_date, .direction = c("down")) %>% fill(., DAWBA_ID:Overall_date, .direction = c("up")) %>%
  arrange(Initials, Overall_date) %>% slice(1) %>% ungroup() %>% mutate(Source = "Current screens")

##### old screens

old_screen_identifiers <- master_IRTA_oldest_screens_latest %>% select(Initials, DAWBA_ID, PLUSID, SDAN, IRTA_tracker, SEX, DOB, Participant_Type2, 
                                                                       Clinical_Visit_Date, Screening_Start_Date, Referral_Date, Eligible, Eligibility_notes, Scheduling_status, Scheduling_status_notes, 
                                                                       Parent_e_consented, Child_e_assented, Parent_DAWBA_completed, Child_DAWBA_completed, DAWBA_completed, Overall_date) 
old_screen_identifiers$Overall_date <- coalesce(old_screen_identifiers$Overall_date, old_screen_identifiers$Clinical_Visit_Date) 

old_screen_identifiers <- old_screen_identifiers %>% group_by(Initials) %>%
  fill(., DAWBA_ID:Overall_date, .direction = c("down")) %>% fill(., DAWBA_ID:Overall_date, .direction = c("up")) %>%
  arrange(Initials, Overall_date) %>% slice(1) %>% ungroup() %>% mutate(Source = "OLD screens")

##### combining all screens 

participant_identifiers_combined <- merge.default(current_participants_identifiers, current_screen_identifiers, all=TRUE) %>%
  merge.default(., old_screen_identifiers, all=TRUE)
fill_names <- participant_identifiers_combined %>% select(-Initials) %>% colnames()
participant_identifiers_combined <- participant_identifiers_combined %>% group_by(Initials) %>%
  fill(., names(fill_names), .direction = c("down")) %>% fill(., names(fill_names), .direction = c("up")) %>%
  arrange(Initials, Source) %>% slice(1) %>% ungroup()

##### excluding those before BSC

referrals_post_BSC <- participant_identifiers_combined %>% filter(Overall_date>"2018-11-30") %>% filter(!is.na(Referral_Date) | !is.na(Screening_Start_Date))

# prep --------------------------------------------------------------------

today <- Sys.Date() %>% format("%m/%d/%y")
date_seq <- seq.dates("12/03/18", today, by = "weeks")
date_seq <- as.Date(date_seq, "%m/%d/%y")

temp <- tibble(Date=c(date_seq))

MDD_temp <- referrals_post_BSC %>% filter(Participant_Type2=="MDD") %>% merge.default(temp, ., all=TRUE) %>% select(Date, Overall_date, Participant_Type2)
MDD_temp$measurement_TDiff <- as.numeric(difftime(MDD_temp$Overall_date, MDD_temp$Date, tz="", units = "days"))
MDD_temp <- MDD_temp %>% filter(measurement_TDiff<7 & measurement_TDiff>=0)
MDD_temp <- merge.default(temp, MDD_temp, all=TRUE) %>% select(-measurement_TDiff)
MDD_temp$Participant_Type2 <- replace_na(MDD_temp$Participant_Type2, "MDD")

HV_temp <- referrals_post_BSC %>% filter(Participant_Type2=="HV") %>% merge.default(temp, ., all=TRUE) %>% select(Date, Overall_date, Participant_Type2)
HV_temp$measurement_TDiff <- as.numeric(difftime(HV_temp$Overall_date, HV_temp$Date, tz="", units = "days"))
HV_temp <- HV_temp %>% filter(measurement_TDiff<7 & measurement_TDiff>=0)
HV_temp <- merge.default(temp, HV_temp, all=TRUE) %>% select(-measurement_TDiff)
HV_temp$Participant_Type2 <- replace_na(HV_temp$Participant_Type2, "HV")

combined_temp <- merge.default(MDD_temp, HV_temp, all=TRUE)
combined_temp <- combined_temp %>% mutate(number = ifelse(!is.na(Overall_date), 1, 0))
referrals_final <- combined_temp %>% group_by(Date, Participant_Type2) %>% summarise(Freq = sum(number)) %>% ungroup()

# summaries ---------------------------------------------------------------

############### grouped by postcard mailout intervals 

referrals_final <- referrals_final %>% mutate(postcard_period_descrip = 
    ### MDD postcards
    ifelse((Participant_Type2 == "MDD" & Date < "2019-02-08"), "Between BSC cut off (2018-11-30) and 1st MDD mailout (2019-02-08)", # referral period from BSC tofirst MDD postcard mailout 
    ifelse((Participant_Type2 == "MDD" & Date >= "2019-02-08" & Date < "2019-09-20"), "Between 1st (2019-02-08) and 2nd (2019-09-20) MDD mailouts", # period between 1st and 2nd MDD postcard mailtouts 
    ifelse((Participant_Type2 == "MDD" & Date >= "2019-09-20" & Date < "2020-01-17"), "Between 2nd (2019-09-20) and 3rd (2020-01-17) MDD mailouts", # period between 2nd and 3rd MDD postcard mailtouts 
    ifelse((Participant_Type2 == "MDD" & Date >= "2020-01-17"), "Since 3rd MDD mailout (2020-01-17)", # period after 3rd MDD postcard mailout 
    ### HV postcards
    ifelse((Participant_Type2 == "HV" & Date < "2019-04-26"), "Between BSC cut off (2018-11-30) and 1st HV mailout (2019-04-26)", # referral period from BSC to first HV postcard mailout 
    ifelse((Participant_Type2 == "HV" & Date >= "2019-04-26" & Date < "2019-07-08"), "Between 1st (2019-04-26) and 2nd (2019-07-08) HV mailouts", # period between 1st and 2nd HV postcard mailtouts 
    ifelse((Participant_Type2 == "HV" & Date >= "2019-07-08"), "Since 2nd HV mailout (2019-07-08)", # period after 2nd HV postcard mailout     
    "NA"))))))))

referrals_final <- referrals_final %>% mutate(postcard_period_num = 
    ### MDD postcards
    ifelse((Participant_Type2 == "MDD" & Date < "2019-02-08"), 1, # referral period from BSC tofirst MDD postcard mailout 
    ifelse((Participant_Type2 == "MDD" & Date >= "2019-02-08" & Date < "2019-09-20"), 2, # period between 1st and 2nd MDD postcard mailtouts 
    ifelse((Participant_Type2 == "MDD" & Date >= "2019-09-20" & Date < "2020-01-17"), 3, # period between 2nd and 3rd MDD postcard mailtouts 
    ifelse((Participant_Type2 == "MDD" & Date >= "2020-01-17"), 4, # period after 3rd MDD postcard mailout 
    ### HV postcards
    ifelse((Participant_Type2 == "HV" & Date < "2019-04-26"), 1, # referral period from BSC to first HV postcard mailout 
    ifelse((Participant_Type2 == "HV" & Date >= "2019-04-26" & Date < "2019-07-08"), 2, # period between 1st and 2nd HV postcard mailtouts 
    ifelse((Participant_Type2 == "HV" & Date >= "2019-07-08"), 3, # period after 2nd HV postcard mailout     
    "NA"))))))))

summary_mail_periods <- referrals_final %>% group_by(postcard_period_num, Participant_Type2, postcard_period_descrip) %>% summarise(Freq = sum(Freq)) %>% 
  arrange(postcard_period_num) %>% ungroup() %>% select(-postcard_period_num)

############### grouped in 3 month intervals 

month_seq <- rep(1:(round((nrow(referrals_final)/26), 0) + 1), 26) %>% as.data.frame() %>% rename(quarter = ".") %>% arrange(quarter) %>% 
  slice(1:nrow(referrals_final))
referrals_final <- cbind(referrals_final, month_seq)

summary_3months <- referrals_final %>% group_by(quarter, Participant_Type2) %>% summarise(Freq = sum(Freq)) %>% ungroup()
dates_3months <- referrals_final %>% group_by(quarter) %>% arrange(Date) %>% slice(1, n()) %>% mutate(obs = 1:n()) %>% ungroup() %>% select(obs, Date, quarter)

quarter_labels <- pivot_wider(dates_3months, id_cols = "quarter", names_from = "obs",  values_from = "Date") %>% 
  rename(Starting = "1", Ending = "2")

summary_3months <- merge.default(summary_3months, quarter_labels, all=TRUE) %>% select(quarter, Starting, Ending, Participant_Type2, Freq)

# removing unnecessary variables ------------------------------------------

rm(current_participants_identifiers, current_screen_identifiers, old_screen_identifiers, fill_names, participant_identifiers_combined, referrals_post_BSC, today, 
   date_seq, temp, MDD_temp, HV_temp, combined_temp, month_seq, dates_3months, quarter_labels)
