
# to change before running script -----------------------------------------

inpatient_initials <- c("KABR")

computer = 'pc' # set this to either 'mac' or 'pc' or 'other' 

todays_date_formatted <- as.Date("2020-03-09") 

# packages ----------------------------------------------------------------

suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr)) 

# directories -------------------------------------------------------------

if (computer=="pc") {
  string = 'W:/'
  sdan1 = 'Y:/'
} else if (computer=="mac") {
  string = '/Volumes/string-mbd/'
  sdan1 = '/Volumes/sdan1/'
} else { # if using a PC and your drives aren't mounted as specified above, enter what letter your drives are mounted under here... 
  string = 'Z:/'
  sdan1 = 'Y:/'
}

database_location = paste0(string, "Database/Master Psychometric Database/") # tasks database also located here 
inpatient_location = paste0(database_location, "Inpatient/") 
inpatient_summary_location = paste0(inpatient_location, "Reports/")

# loading data ------------------------------------------------------------

if (exists("MATCH_tracker")==FALSE) {
  inpatient_database_file <- list.files(path = paste0(inpatient_location), pattern = "^MASTER_DATABASE_Inpatient", all.files = FALSE,
                                        full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  inpatient_database_file_time <- file.mtime(paste0(inpatient_location, inpatient_database_file)) %>% as.Date()
  inpatient_database_combined <- tibble(File=c(inpatient_database_file), Date=c(inpatient_database_file_time)) %>% arrange(desc(Date)) %>% slice(1)
  MATCH_tracker <- read_excel(paste0(inpatient_location, inpatient_database_combined[1])) %>% mutate_all(as.character)
  MATCH_tracker$Clinical_Visit_Date <- as.Date(MATCH_tracker$Clinical_Visit_Date)
  rm(inpatient_database_file, inpatient_database_file_time, inpatient_database_combined)
}

MATCH_tracker$Clinical_Visit_Date2 <- format(MATCH_tracker$Clinical_Visit_Date, "%b-%d")
MATCH_tracker$Clinical_Visit_Date3 <- format(MATCH_tracker$Clinical_Visit_Date, "%B-%d-%Y")

temp_data <- MATCH_tracker %>% filter(Initials==inpatient_initials) %>% 
  select(Initials, Clinical_Visit_Date, Clinical_Visit_Date2, Clinical_Visit_Date3, Clinical_Visit_Type, Clinical_Visit_Number, 
         s_mfq1w_tot, s_ari1w_tot, s_scared_tot, s_shaps_tot, s_lsas_tot) %>% ungroup()
temp_data[,7:ncol(temp_data)] <- sapply(temp_data[,7:ncol(temp_data)], as.numeric)
temp_data[,7:ncol(temp_data)] <- sapply(temp_data[,7:ncol(temp_data)], round, 0)

out_file <- paste0(inpatient_summary_location, "end_of_treatment_graphs/", inpatient_initials)

if (file.exists(out_file)){
  print("file exists")
} else {
  print("doesn't exist, creating directory")
  dir.create(file.path(inpatient_summary_location, "end_of_treatment_graphs/", inpatient_initials))
}

setwd(out_file)

# MFQ

mfq_graph <- temp_data %>% ggplot(aes(x = Clinical_Visit_Date, y = s_mfq1w_tot)) +
  geom_point(size=2) + geom_line(size=0.25) +
  scale_x_date(date_break = "2 weeks", date_labels = "%m/%d") +
  geom_text(aes(label=s_mfq1w_tot), hjust=0, vjust=-1.5, size=4) + ylim(0,27) + theme_classic() +
  ggtitle("Depressive symptoms\n") + ylab("MFQ total\n") + xlab("\nDate (MM-DD)\n")

ggsave(filename = paste0("MFQ_graph_", inpatient_initials, ".png"), plot = mfq_graph, width=5, height=5)

# SCARED

scared_graph <- temp_data %>% ggplot(aes(x = Clinical_Visit_Date, y = s_scared_tot)) +
  geom_point(size=2) + geom_line(size=0.25) +
  scale_x_date(date_break = "2 weeks", date_labels = "%m/%d") +
  geom_text(aes(label=s_scared_tot), hjust=0, vjust=-1.5, size=4) + ylim(0,83) + theme_classic() +
  ggtitle("General anxiety symptoms\n") + ylab("SCARED total\n") + xlab("\nDate (MM-DD)\n")

ggsave(filename = paste0("SCARED_graph_", inpatient_initials, ".png"), plot = scared_graph, width=5, height=5)

# LSAS  

lsas_graph <- temp_data %>% ggplot(aes(x = Clinical_Visit_Date, y = s_lsas_tot)) +
  geom_point(size=2) + geom_line(size=0.25) +
  scale_x_date(date_break = "2 weeks", date_labels = "%m/%d") +
  geom_text(aes(label=s_lsas_tot), hjust=0, vjust=-1.5, size=4) + ylim(0,150) + theme_classic() +
  ggtitle("Social anxiety symptoms\n") + ylab("LSAS total\n") + xlab("\nDate (MM-DD)\n")

ggsave(filename = paste0("LSAS_graph_", inpatient_initials, ".png"), plot = lsas_graph, width=5, height=5)

