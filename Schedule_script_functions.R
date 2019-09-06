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
# filename <- "MASTER_IRTA_DATABASE.xlsx" #set master_IRTA_latest filename here
# 
# 
# 
#  # data_location <- "Z:/RA Instruction Manuals/Chris Camp/" #set filepath of data here
#  # filename <- "CC_Patient_List.xlsx" #set master_IRTA_latest filename here
#  # destination <- "Z:/RA Instruction Manuals/Chris Camp/CC_Patient_List_FU.xlsx" #set destination path and filename
# 
# master_IRTA_latest <- read.xlsx(paste0(data_location,filename))


# Functions ---------------------------------------------------------------

# is_last_v determines if any given longitudinal scan is the most recent of a given subject
# Parameters:
#         v <- Value of "Task1_Visit_Type" variable for given obs.
#         init <- Value of "Initials" variable for given obs.
# Returns:
#         Boolean TRUE if most recent scan
#         Boolean FALSE if not most recent scan
is_last_v <- function(v, init,master_IRTA_latest) {
  subj_data <- filter(master_IRTA_latest, Initials == init, grepl("v",Task1_Visit_Type))
  if (!is.na(v) & subj_data$Task1_Visit_Type[nrow(subj_data)] == v) {
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
has_scan <- function(init,master_IRTA_latest) {
  subj_data <- filter(master_IRTA_latest, Initials == init, ("*_scan" %in% Task1_Name))
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
get_last_scan <- function(init,master_IRTA_latest) {
  subj_data <- filter(master_IRTA_latest, Initials == init, ("*_scan %in% Task1_Name"), Task1_Date != "NA", Task1_Date != "")
  last_date <- subj_data[nrow(subj_data),"Task1_Date"]
}

# get_last_visit sets variable last_date to the most recent clinical visit date
# Parameters:
#         init <- Value of "Initials" variable for given obs.
# Returns:
#         NA
get_last_visit <- function(init,master_IRTA_latest) {
  subj_data <- filter(master_IRTA_latest, Initials == init, Clinical_Visit_Type != "")
  last_date <- as.Date(subj_data[nrow(subj_data),"Clinical_Visit_Date"], origin = "1899-12-30")
}

print_dates <- function(row,master_IRTA_latest) {
  eligibility <- master_IRTA_latest[row,"Eligible"]
  
  # Date generation for eligibility of 0 only.
  # Only prints notes if most recent longitudinal scan
  if (!is.na(master_IRTA_latest[row,"Eligible"]) & master_IRTA_latest[row, "Eligible"] == 0) {
    if (!is.na(master_IRTA_latest[row,"Task1_Name"])) {
      
      if (grepl("v",master_IRTA_latest[row, "Task1_Visit_Type"], fixed = TRUE)) { #Finds whether longitudinal scan in row
        
        if (!is.na(master_IRTA_latest[row,"Participant_Type2"]) & master_IRTA_latest[row,"Participant_Type2"] == "MDD") {
          if (!is.na(master_IRTA_latest[row,"State"]) & (master_IRTA_latest[row,"State"] != "MD" || master_IRTA_latest[row,"State"] != "VA" || master_IRTA_latest[row,"State"] != "DC")){
          
            if(master_IRTA_latest[row,"Task1_Visit_Type"] %in% c("v1","v2","v3","v6","v9")) {
              master_IRTA_latest[row,"Next_FU_date"] <- master_IRTA_latest[row,"Task1_Date"]+120
              assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
            } else {
              master_IRTA_latest[row,"Next_FU_date"] <- master_IRTA_latest[row,"Task1_Date"]+365
              assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
            }
          }
          else {
            master_IRTA_latest[row,"Next_FU_date"] <- master_IRTA_latest[row,"Task1_Date"]+120
            assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          }
        } else if (!is.na(master_IRTA_latest[row,"Participant_Type2"]) & master_IRTA_latest[row,"Participant_Type2"] == "HV"){
          
          master_IRTA_latest[row,"Next_FU_date"] <- master_IRTA_latest[row,"Task1_Date"]+365
          assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          
        }
      }
    }
  }
  assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
}

# print_notes generates followup dates and notes
# Parameters:
#         eligibility <- value of "Eligible" variable for given obs.
#         master_IRTA_latest <- dataframe containing all data
# Returns:
#         NA
print_notes <- function(row,master_IRTA_latest) {
  if (!is.na(master_IRTA_latest[row,"Next_FU_date"])) {
    eligibility <- master_IRTA_latest[row,"Eligible"]
    
    #Case: if scan already scheduled
    if (!is.na(master_IRTA_latest[row,"Task1_Date"])) {
      #print("!is.na Task1")
      if (Sys.Date() - master_IRTA_latest[row,"Task1_Date"] < 0) {
        #print("scan already scheduled")
        master_IRTA_latest[row,"Next_FU_notes"] <- "Scan scheduled."
        assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
      }
    }
    
    # Case: excluded
    if (!is.na(eligibility) & (eligibility == 4 | eligibility == 5 | eligibility == 6 | eligibility == 7))  {
      assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
      
      # Case: eligibility = 1
      # Followup date is 6 months from last scan if prior scan exists,
      # or 6 months from last clinic visit if not.
    } else if (!is.na(eligibility) & (eligibility == 1)) {
      
      if (has_scan(master_IRTA_latest[row,"Initials"],master_IRTA_latest)) {
        
        master_IRTA_latest[row,"Next_FU_date"] <- get_last_scan(master_IRTA_latest[row,"Initials"],master_IRTA_latest) + 180
        assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
        
      } else {
        if (!is.na(master_IRTA_latest[row,"Clinical_Visit_Date"])) {
          
          master_IRTA_latest[row,"Next_FU_date"] <- get_last_visit(master_IRTA_latest[row,"Initials"],master_IRTA_latest) + 180
          assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          
        } else {
          
          master_IRTA_latest[row,"Next_FU_notes"] <- "Clinic visit must occur before scan."
          assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          
        }
      }
      
      master_IRTA_latest[row,"Next_FU_notes"] <- "At last visit they were marked as 'cannot scan' - see eligibility and/or scheduling status notes to determine what follow up is necessary, if any."
      assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
      
      # Case: Eligibility = 2 or 3
      # Same logic as Eligibility = 1, except notes are different
    } else if (!is.na(eligibility) & (eligibility == 2 | eligibility == 3)){
      #print("else if (!is.na(eligibility) & (eligibility == 2 | eligibility == 3))")
      if (has_scan(master_IRTA_latest[row,"Initials"],master_IRTA_latest)) {
        
        master_IRTA_latest[row,"Next_FU_date"] <- get_last_scan(master_IRTA_latest[row,"Initials"],master_IRTA_latest) + 180
        assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
        
      } else {
        if (!is.na(master_IRTA_latest[row,"Clinical_Visit_Date"])) {
          
          #print("(!is.na(master_IRTA_latest[row,Clinical_Visit_Date]))")
          master_IRTA_latest[row,"Next_FU_date"] <- get_last_visit(master_IRTA_latest[row,"Initials"],master_IRTA_latest) + 180
          assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          
        } else {
          
          #print("elig 2 or 3 else")
          master_IRTA_latest[row,"Next_FU_notes"] <- "Clinic visit must occur before scan."
          assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          
        }
      }
      
      #print("At last visit they were marked as 'on hold'")
      master_IRTA_latest[row,"Next_FU_notes"] <- "At last visit they were marked as 'on hold' - see eligibility and/or scheduling status notes to determine what follow up is necessary, if any."
      assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
      
      # Case: no eligibility
    } else if (is.na(eligibility)) {
      
      #print("eligibility missing")
      master_IRTA_latest[row,"Next_FU_notes"] <- "Eligibility missing."
      # Case: eligibilities 0 or 7 and greater
      # Determines situation from task number and followup date.
    } else {
      if (is_last_v(master_IRTA_latest[row,"Task1_Visit_Type"],master_IRTA_latest[row,"Initials"],master_IRTA_latest)) {
        #print("else date diff")
        date_diff <- Sys.Date() - as.Date(master_IRTA_latest[row,"Next_FU_date"], origin = "1899-12-30")
        #print("else date diff")
        if ((!is.na(master_IRTA_latest[row,"Task1_Number"])) & 
            (master_IRTA_latest[row,"Task1_Number"] == 999 | 
             master_IRTA_latest[row,"Task1_Number"] == 777)) {
          
          master_IRTA_latest[row,"Next_FU_notes"] <- "Failed to scan at scheduled date. Reschedule ASAP."
          assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          
        } else if (!is.na(master_IRTA_latest[row,"Next_FU_date"]) & 
                   date_diff >= 0) {
          
          if (date_diff >= 120) {
            master_IRTA_latest[row,"Next_FU_notes"] <- "Timepoint missed. Return for next scan or remove."
            assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          }
          
          else {
            
            master_IRTA_latest[row,"Next_FU_notes"] <- "Timepoint recently missed. Schedule ASAP."
            assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
            
          }
          
        } else {
          #Eligibility greater than 7:
          if (!is.na(eligibility) & (eligibility > 7)) {
            #print("eli gre 7")
            if (is_last_v(master_IRTA_latest[row,"Task1_Visit_Type"],master_IRTA_latest[row,"Initials"],master_IRTA_latest)) {
              master_IRTA_latest[row,"Next_FU_notes"] <- "At last visit they finished treatment - see eligibility and/or scheduling status notes to determine what follow up is necessary, if any."
              assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
            }
            
          } else if (master_IRTA_latest[row,"Task1_Visit_Type"] %in% c("v1","v2","v3","v6","v9")){ # If high priority scan
            
            master_IRTA_latest[row,"Next_FU_notes"] <- "Due for follow up scan; check eligibility and/or scheduling status notes."
            assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
            
          } else { # Low priority scans
            
            master_IRTA_latest[row,"Next_FU_notes"] <- "Lower priority: if local = try schedule follow up scan; if out-of-towner = obtain SDQ+ measures only - parent & child. Check eligibility and/or scheduling status notes."
            assign('master_IRTA_latest',master_IRTA_latest,envir=.GlobalEnv)
          }
        }
      }
    }
  }
}