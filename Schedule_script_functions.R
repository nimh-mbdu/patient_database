library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(tibble)


# Setup -------------------------------------------------------------------
#fix eligibility greater than 7

#Must match information in Schedule_script/R/schedule_script.R
# setwd("Z:/RA Instruction Manuals/Chris Camp/R/Schedule_script") #set location of script here
# data_location <- "Z:/Database/Master Participant Tracker/" #set filepath of data here
# filename <- "MASTER_IRTA_DATABASE.xlsx" #set database filename here
# 
# 
# 
#  # data_location <- "Z:/RA Instruction Manuals/Chris Camp/" #set filepath of data here
#  # filename <- "CC_Patient_List.xlsx" #set database filename here
#  # destination <- "Z:/RA Instruction Manuals/Chris Camp/CC_Patient_List_FU.xlsx" #set destination path and filename
# 
# database <- read.xlsx(paste0(data_location,filename))


# Functions ---------------------------------------------------------------

# is_last_v determines if any given longitudinal scan is the most recent of a given subject
# Parameters:
#         v <- Value of "Task1_Visit_Type" variable for given obs.
#         init <- Value of "Initials" variable for given obs.
# Returns:
#         Boolean TRUE if most recent scan
#         Boolean FALSE if not most recent scan
is_last_v <- function(v, init,database) {
  subj_data <- filter(database, Initials == init, grepl("v",Task1_Visit_Type))
  if (subj_data$Task1_Visit_Type[nrow(subj_data)] == v) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# has_scan determines if any given subject has ever scanned
# Parameters:
#         init <- Value of "Initials" variable for given obs.
# Returns:
#         Boolean TRUE if has scan
#         Boolean FALSE if never scanned
has_scan <- function(init,database) {
  subj_data <- filter(database, Initials == init, ("*_scan" %in% Task1_Name))
  if (nrow(subj_data) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# get_last_scan sets variable last_date to the most recent scan date
# Parameters:
#         init <- Value of "Initials" variable for given obs.
# Returns:
#         NA
get_last_scan <- function(init,database) {
  subj_data <- filter(database, Initials == init, ("*_scan %in% Task1_Name"), Task1_Date != "NA", Task1_Date != "")
  last_date <- as.Date(subj_data[nrow(subj_data),"Task1_Date"], origin = "1899-12-30")
}

# get_last_visit sets variable last_date to the most recent clinical visit date
# Parameters:
#         init <- Value of "Initials" variable for given obs.
# Returns:
#         NA
get_last_visit <- function(init,database) {
  subj_data <- filter(database, Initials == init, Clinical_Visit_Type != "")
  last_date <- as.Date(subj_data[nrow(subj_data),"Clinical_Visit_Date"], origin = "1899-12-30")
}

print_dates <- function(row,database) {
  eligibility <- database[row,"Eligible"]
  
  # Date generation for eligibility of 0 only.
  # Only prints notes if most recent longitudinal scan
  if (!is.na(database[row,"Eligible"]) & database[row, "Eligible"] == 0) {
    if (!is.na(database[row,"Task1_Name"])) {
      
      if (grepl("v",database[row, "Task1_Visit_Type"], fixed = TRUE)) { #Finds whether longitudinal scan in row
        
        if (!is.na(database[row,"Participant_Type2"]) & database[row,"Participant_Type2"] == "MDD" & 
            (database[row,"State"] != "MD" || database[row,"State"] != "VA" || database[row,"State"] != "DC")) {
          
          if(database[row,"Task1_Visit_Type"] %in% c("v1","v2","v3","v6","v9")) {
            database[row,"Next_FU_date"] <- as.Date(database[row,"Task1_Date"]+120,origin = "1899-12-30")
            assign('database',database,envir=.GlobalEnv)
          } else {
            database[row,"Next_FU_date"] <- as.Date(database[row,"Task1_Date"]+365,origin = "1899-12-30")
            assign('database',database,envir=.GlobalEnv)
          }
          
        } else if (!is.na(database[row,"Participant_Type2"]) & database[row,"Participant_Type2"] == "MDD") {
          
          database[row,"Next_FU_date"] <- as.Date(database[row,"Task1_Date"]+120,origin = "1899-12-30")
          assign('database',database,envir=.GlobalEnv)
          
        } else if (!is.na(database[row,"Participant_Type2"]) & database[row,"Participant_Type2"] == "HV"){
          
          database[row,"Next_FU_date"] <- as.Date(database[row,"Task1_Date"]+365,origin = "1899-12-30")
          assign('database',database,envir=.GlobalEnv)
          
        }
        
        if (is_last_v(database[row,"Task1_Visit_Type"], database[row,"Initials"],database)) { #Determines whether most recent longitudinal scan
          
          print_notes(row,database)
          
        }
        
      }
    }
  }
  assign('database',database,envir=.GlobalEnv)
}

# print_notes generates followup dates and notes
# Parameters:
#         eligibility <- value of "Eligibile" variable for given obs.
#         database <- dataframe containing all data
# Returns:
#         NA
print_notes <- function(row,database) {
  eligibility <- database[row,"Eligible"]
  
  #Case: if scan already scheduled
  if (!is.na(database[row,"Task1_Date"])) {
    #print("!is.na Task1")
    if (Sys.Date() - as.Date(database[row,"Task1_Date"], origin = "1899-12-30") < 0) {
      #print("scan already scheduled")
      database[row,"Next_FU_notes"] <- "Scan scheduled."
      assign('database',database,envir=.GlobalEnv)
    }
  }
  
  # Case: excluded
  if (!is.na(eligibility) & (eligibility == 4 | eligibility == 5 | eligibility == 6 | eligibility == 7))  {
    assign('database',database,envir=.GlobalEnv)
    
    # Case: eligibility = 1
    # Followup date is 6 months from last scan if prior scan exists,
    # or 6 months from last clinic visit if not.
  } else if (!is.na(eligibility) & (eligibility == 1)) {
    
    if (has_scan(database[row,"Initials"],database)) {
      
      database[row,"Next_FU_date"] <- get_last_scan(database[row,"Initials"],database) + 180
      assign('database',database,envir=.GlobalEnv)
      
    } else {
      if (!is.na(database[row,"Clinical_Visit_Date"])) {
        
        database[row,"Next_FU_date"] <- get_last_visit(database[row,"Initials"],database) + 180
        assign('database',database,envir=.GlobalEnv)
        
      } else {
        
        database[row,"Next_FU_notes"] <- "Clinic visit must occur before scan."
        assign('database',database,envir=.GlobalEnv)
        
      }
    }
    
    database[row,"Next_FU_notes"] <- "At last visit they were marked as 'cannot scan' - see eligibility and/or scheduling status notes to determine what follow up is necessary, if any."
    assign('database',database,envir=.GlobalEnv)
    
    # Case: Eligibility = 2 or 3
    # Same logic as Eligibility = 1, except notes are different
  } else if (!is.na(eligibility) & (eligibility == 2 | eligibility == 3)){
    #print("else if (!is.na(eligibility) & (eligibility == 2 | eligibility == 3))")
    if (has_scan(database[row,"Initials"],database)) {
      
      database[row,"Next_FU_date"] <- get_last_scan(database[row,"Initials"],database) + 180
      assign('database',database,envir=.GlobalEnv)
      
    } else {
      if (!is.na(database[row,"Clinical_Visit_Date"])) {
        
        #print("(!is.na(database[row,Clinical_Visit_Date]))")
        database[row,"Next_FU_date"] <- get_last_visit(database[row,"Initials"],database) + 180
        assign('database',database,envir=.GlobalEnv)
        
      } else {
        
        #print("elig 2 or 3 else")
        database[row,"Next_FU_notes"] <- "Clinic visit must occur before scan."
        assign('database',database,envir=.GlobalEnv)
        
      }
    }
    
    #print("At last visit they were marked as 'on hold'")
    database[row,"Next_FU_notes"] <- "At last visit they were marked as 'on hold' - see eligibility and/or scheduling status notes to determine what follow up is necessary, if any."
    assign('database',database,envir=.GlobalEnv)
    
    # Case: no eligibility
  } else if (is.na(eligibility)) {
    
    #print("eligibility missing")
    database[row,"Next_FU_notes"] <- "Eligibility missing."
    # Case: eligibilities 0 or 7 and greater
    # Determines situation from task number and followup date.
  } else {
    #print("else date diff")
    date_diff <- Sys.Date() - as.Date(database[row,"Next_FU_date"], origin = "1899-12-30")
    #print("else date diff")
    if ((!is.na(database[row,"Task1_Number"])) & 
        (database[row,"Task1_Number"] == 999 | 
         database[row,"Task1_Number"] == 777)) {
      
      database[row,"Next_FU_notes"] <- "Failed to scan at scheduled date. Reschedule ASAP."
      assign('database',database,envir=.GlobalEnv)
      
    } else if (!is.na(database[row,"Next_FU_date"]) & 
               date_diff >= 0) {
      
      if (date_diff >= 120) {
        database[row,"Next_FU_notes"] <- "Timepoint missed. Return for next scan or remove."
        assign('database',database,envir=.GlobalEnv)
      }
      
      else {
        
        database[row,"Next_FU_notes"] <- "Timepoint recently missed. Schedule ASAP."
        assign('database',database,envir=.GlobalEnv)
        
      }
      
    } else {
      #Eligibility greater than 7:
      if (!is.na(eligibility) & (eligibility > 7)) {
        #print("eli gre 7")
        if (is_last_v(database[row,"Task1_Visit_Type"],database[row,"Initials"],database)) {
          database[row,"Next_FU_notes"] <- "At last visit they finished treatment - see eligibility and/or scheduling status notes to determine what follow up is necessary, if any."
          assign('database',database,envir=.GlobalEnv)
        }
        
      } else if (database[row,"Task1_Visit_Type"] %in% c("v1","v2","v3","v6","v9")){ # If high priority scan
        
        database[row,"Next_FU_notes"] <- "Due for follow up scan; check eligibility and/or scheduling status notes."
        assign('database',database,envir=.GlobalEnv)
        
      } else { # Low priority scans
        
        database[row,"Next_FU_notes"] <- "Lower priority: if local = try schedule follow up scan; if out-of-towner = obtain SDQ+ measures only - parent & child. Check eligibility and/or scheduling status notes."
        assign('database',database,envir=.GlobalEnv)
      }
    }
  }
}