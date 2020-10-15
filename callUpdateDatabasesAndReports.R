args = commandArgs(trailingOnly=TRUE)
print(paste(args))
nargs=length(args)

if(nargs > 0) {
  modules2run=as.numeric(args[1]) # do not change this
}else{
  modules2run=0 #set this to the module you like to run, but should instead call updateDatabasesAndReports.R
  #and pass the modules2run as an argument to the function
}

msg = " Need to pass the number of the module you would like to run to the script
or, set the modules2run below \n
  Some common modules are:
    1: update master IRTA tracker & produce QC reports,
    2: update master database,
    6: runs 1 and 2
  Reports:
    4: Weekly numbers, 9: runs 1 and 4
    8: CBT report, 7: runs 1 and 8
    10: Clinician sheet, 11: runs 1 and 10
    12: Inpatient report, 13: runs 1, 2, and 12
    14: Supervision sheet, 15: runs 1,2, and 14
    for full list see 'to_change_before_running_master_script.xlsx' "

if(modules2run > 15 | modules2run < 0){
  stop(msg)
}

source("/string-mbd/Database/Database_Scripts_Github/updateDatabasesAndReports.R")
updateDatabasesAndReports(modules2run)
