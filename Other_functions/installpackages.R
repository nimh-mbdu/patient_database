
packages <- c("readxl", "writexl", "tidyr", "dplyr", "summarytools", "rmarkdown", "eeptools", 
              "openxlsx", "data.table", "reshape2", "stringr","lubridate","ggplot2","rlang", 
              "purrr", "tidyverse","shiny","knitr","ggpubr","chron","kableExtra", "ggthemes", "ggrepel")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

