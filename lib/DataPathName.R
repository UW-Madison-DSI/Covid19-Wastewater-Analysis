#Works on all processed data
ParseData <- function(FileName){
  DF <- read.csv(FileName)%>%
    mutate(Date=as.Date(Date,origin="1970-01-01 UTC"))
  return(DF)
}


HFGWastePath = function(BaseDir){
  PathName = paste0(BaseDir,"Data/processed/HFGWasteData011422.csv")
  return(PathName)
}

HFGCasePath = function(BaseDir){
  PathName = paste0(BaseDir,"Data/processed/HFGCaseData_2021-05-07.csv")
  return(PathName)
}

LIMSWastePath = function(BaseDir){
  PathName = paste0(BaseDir,"Data/processed/DHSWasteData-4_21_2022.csv")
  return(PathName)#workset4.csv
}#LIMSWasteData_02-09-22.csv  

LIMSCasePath = function(BaseDir){
  PathName = paste0(BaseDir,"Data/processed/DHSCaseData-4_21_2022.csv")
  return(PathName)
}#MMSD_Interceptor_Cases_2_7_22.csv