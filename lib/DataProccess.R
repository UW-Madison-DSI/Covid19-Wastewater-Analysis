#Needs
#tidyverse
#lubridate
#readxl

#Creates WastewaterData

tmpfn <- function(N1, N2) {
  AVG <- sqrt(N1 * N2)
  AVG <- ifelse(is.na(N1), N2, AVG)
  AVG <- ifelse(is.na(N2), N1, AVG)
}
#su
#
Wastewater <- function(filename){
  missing_codes <- c("","NA","0","Undetected","Not Detected",
                     "Field Parameters to be filled in", 
                     "Inhibited-to be re-ran", "#DIV/0!","-","In progress")
  sheets <- excel_sheets(Waterfilename)
  water_MMSD <- read_excel(Waterfilename,
                           na = missing_codes,
                           col_types = c("text", "text", rep("numeric", 10),rep("text", 4)),
                           sheet = 1)%>%
    #mutate(Date=mdy(Date))%>%
    select(-c(13:16))
  water_Interceptors <- read_excel(Waterfilename,
                                   na = missing_codes,
                                   col_types = c("text", "text", rep("numeric", 8), "text"),
                                   sheet = 2) %>%
    select(-"...11") %>%
    mutate(TSS = NA,
           Site = ifelse(Site == "MMSD P02", "MMSD P2", Site),
           Site = ifelse(Site == "MMSD P07", "MMSD P7", Site),
           Site = ifelse(Site == "MMSD P08", "MMSD P8", Site)
           #Date=mdy(Date)
           ) %>%
    select(1:5, TSS, everything())
  water_UW_Dorms <- read_excel(Waterfilename,
                               na = missing_codes,
                               col_types = c("text", "text", rep("numeric", 9), rep("text", 1)),
                               sheet = 3) %>%
    #mutate(Date=mdy(Date))%>%
    select(-"...12")
  water_UW_sellery <- read_excel(Waterfilename,
                                 na = missing_codes,
                                 col_types = c("text", "text", rep("numeric", 9), rep("text", 1)),
                                 sheet = 4) %>%
    #mutate(Date=mdy(Date))%>%
    select(-"...12")
  water <- 
    bind_rows(water_MMSD,
              water_Interceptors,
              water_UW_Dorms,
              water_UW_sellery) %>%
    rename(Date = "Collection Date",
           pH = "pH (SU)",
           Total_Flow = "Total Flow (MGD)",
           Conductivity = "Conductivity (uS/CM@25C)",
           N1 = "N1 (GC/L)",
           N2 = "N2 (GC/L)",
           PMMoV = "PMMoV (GC/L)",
           Pct_BCoV = "% Recovery (BCoV)"
           ) %>%
    filter(!is.na(Site)) %>%
    mutate(AVG = tmpfn(N1, N2),
           wt = 2 - is.na(N1) - is.na(N2),
           Site = ifelse(Site == "UW-D", "UW-LakeShore", Site),
           Site = ifelse(Site == "UW-S", "UW-Sellery", Site),
           TSS = ifelse(is.na(TSS), `TSS (mg/L)`, TSS),
           )%>%
    select(-`TSS (mg/L)`)%>%
    mutate(Date = coalesce(ddays(suppressWarnings(as.numeric(Date))-1)+ymd("1900-01-01"),
                           suppressWarnings(mdy(Date))))
}

#Creates  CovidNumberData

CovidData<- function(CovidFileName){
  lag=1
  covidData = read.csv(CovidFileName)%>%
    mutate(ServiceID = ifelse(ServiceID=="MMSD","Madison",paste("MMSD P",ServiceID,sep="")),Date = as.Date(Date))%>%
    rename(Site=ServiceID)%>%
    group_by(Site)%>%
    mutate(Cases=Cases - lag(Cases, n=lag,default = NA),Tests=Tests- lag(Tests, n=lag,default = NA))%>%
    mutate(roll=Cases/Tests)
  return(covidData)
}
CovidDataDorms <- function(File1,File2){
  CaseData1=read.csv(File1,sep = "",header = FALSE,col.names=c("Date","Site","negitive_tests","positive_tests"))
  CaseData2=read.csv(File2,sep = "",header = FALSE,col.names=c("Date","Site","negitive_tests","positive_tests"))
  returnedData=rbind(CaseData1,CaseData2)%>%
    mutate(Tests=negitive_tests+positive_tests)%>%
    mutate( Site = ifelse(Site == "UW_D", "UW-LakeShore", "UW-Sellery"))%>%
    mutate(Date=mdy(Date))%>%
    rename(Cases=positive_tests)
  return(returnedData)
}



N2Fixer <- function(data){
  N2Mod = lm(log(N2)~log(N1), data=data)
  data$temp=10^predict.lm(N2Mod,data,interval="none")
  data=data%>%
    mutate(N2=ifelse(is.na(N2),temp,N2))
  data=data%>%
    select(-temp)
  return(data)
}
N1Fixer <- function(data){
  N1Mod = lm(N1~N2, data=data)
  data$temp=predict.lm(N1Mod,data,interval="none")
  data=data%>%
    mutate(N2=ifelse(is.na(N2),temp,N2))
  data=data%>%
    select(-temp)
  return(data)
}
HFGInfo <- function(dataFN){
  missing_codes <- c("","NA","0","Undetected","Not Detected",
                     "Field Parameters to be filled in", 
                     "Inhibited-to be re-ran", "#DIV/0!",
                     "Undetermined", "LA")
  
  HFGInfo.PreHack <- suppressMessages(read_excel(dataFN,
                                na = missing_codes,
                                col_types = c("text","text", "date", rep("numeric", 4),"text",rep("numeric", 2),"text",rep("numeric", 3)),
                                sheet = 1))%>%
    rename(repName="qpCR Sample Name",
           Date="Collection Date",
           N1Ct="N1 CT",
           N1GC="N1 GC/L",
           N1LOD="N1 <LOD",
           N2Ct="N2 CT",
           N2GC="N2 GC/L",
           N2LOD="N2 <LOD",
           PMMOVCT="PMMOV CT",
           PMMOVGC="PMMOV GC/L",
           BCoV="BCoV% recovery",
           Filter = "Filter Rep",
           Well = "Well Rep")%>%
    mutate(N1LOD=N1LOD=="Y",
           N2LOD=N2LOD=="Y",
           Date=as.Date(Date))
  
  #hack fixing inconstancy
  HFGInfo = HFGInfo.PreHack#%>%
  #  mutate(Date=if_else(repName=="Platteville (T) rep 3"&Date==as.Date("2021-02-03"),as.Date("2021-02-02"),Date))%>%
  #  mutate(Date=if_else(repName=="Hudson (T) rep 1"&Date==as.Date("2021-02-01"),as.Date("2021-02-02"),Date))
  return(HFGInfo)
}