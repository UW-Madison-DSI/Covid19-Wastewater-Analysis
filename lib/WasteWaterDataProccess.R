#Depends on
#tidyverse
#lubridate
#readxl

#Creates WastewaterData

AVGGenFN <- function(N1, N2) {
  AVG <- sqrt(N1 * N2)
  AVG <- ifelse(is.na(N1), N2, AVG)
  AVG <- ifelse(is.na(N2), N1, AVG)
}

MissingCode <- function(){
  return(c("","NA","0","Undetected","Not Detected",
           "Field Parameters to be filled in", 
           "Inhibited-to be re-ran", "#DIV/0!","-","In progress"))
}


HFGWastePARSER = function(data){
  missing_codes <- MissingCode()
  
  HFGInfo.PreHack <- suppressMessages(read_excel(data,
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
           Date=as.Date(Date),
           AVG = AVGGenFN(N1GC, N2GC))
  
  #hack fixing inconstancy
  HFGInfo = HFGInfo.PreHack#%>%
  #  mutate(Date=if_else(repName=="Platteville (T) rep 3"&Date==as.Date("2021-02-03"),as.Date("2021-02-02"),Date))%>%
  #  mutate(Date=if_else(repName=="Hudson (T) rep 1"&Date==as.Date("2021-02-01"),as.Date("2021-02-02"),Date))
  return(HFGInfo)
}


LIMSDataPARSER <- function(LIMSFN){
  NumericVars=c("average_flow_rate","avg_sars_cov2_conc","bcov_rec_rate",
                "bcov_spike_conc","capacity_mgd","conductivity",
                "equiv_sewage_amt","n1_lod","n1_loq","n1_num_no_target_control",
                "n1_sars_cov2_conc","n1_sars_cov2_error","n1_sars_cov2_lod",
                "n2_lod","n2_loq","n2_num_no_target_control","n2_sars_cov2_conc",
                "n2_sars_cov2_error","ph","pmmov_conc","population_served",
                "temperature","tss")
  missing_codes <- MissingCode()
  
  LIMSDF <- read_excel(LIMSFN,na  =  MissingCode(),col_types = c(rep("text",61)))
  LIMSDF <- suppressWarnings(mutate(LIMSDF,
      across(all_of(NumericVars),as.numeric),
      test_result_date=as.POSIXlt(test_result_date,format="%m/%d/%Y %H:%M"),
      sample_collect_date=mdy(sample_collect_date)
    ))%>%
    rename(
      Site=wwtp_name,
      FlowRate=average_flow_rate,
      Cov1_below_lod=avg_sars_cov2_below_lod,
      cov2_conc=avg_sars_cov2_conc,
      BCoV=bcov_rec_rate,
      BCoVConc=bcov_spike_conc,
      county=county_names,
      Date=sample_collect_date,
      RepDate=test_result_date,
      N1=n1_sars_cov2_conc,
      N1Error=n1_sars_cov2_error,
      N2=n2_sars_cov2_conc,
      N2Error=n2_sars_cov2_error,
      PMMoV=pmmov_conc,
      Pop=population_served
    )%>%
    mutate(#Standardizing the Site names names
      Site=ifelse(Site=="Madison Metro","Madison",Site),
      Site=ifelse(Site=="Covid Sewage UW DORM","UW-LakeShore",Site),
      Site=ifelse(Site=="Covid Sewage UW Sell","UW-Sellery",Site),
      Site=ifelse(Site=="Madison-P2-Central","MMSD-P2",Site),
      Site=ifelse(Site=="Madison-P7-SE","MMSD-P7",Site),
      Site=ifelse(Site=="Madison-P8-West","MMSD-P8",Site),
      Site=ifelse(Site=="Madison-P11-SW","MMSD-P11",Site),
      Site=ifelse(Site=="Madison-P18-NE","MMSD-P18",Site),
      AVG = AVGGenFN(N1, N2),
      wt = 2 - is.na(N1) - is.na(N2),
      #Giving the different semesters unique names
      isDorm = ifelse(Site %in% c("UW-LakeShore","UW-Sellery"),TRUE,FALSE),
      Site = ifelse(isDorm&Date>=ymd("2021-01-11"),paste("Spring",Site),Site),
      Site = ifelse(isDorm&Date<=ymd("2020-12-25"),paste("Fall",Site),Site),
      Site = ifelse(isDorm&Date>ymd("2020-12-25")&
                      Date<ymd("2021-01-11"),paste("Break",Site),Site)
    )%>%
    select(-isDorm)%>%
    #converting -1 meaning missing data to NA
    mutate(N1=ifelse(N1==-1,NA,N1),
           N2=ifelse(N2==-1,NA,N2),
           PMMoV=ifelse(PMMoV==-1,NA,PMMoV),
           BCoV=ifelse(BCoV==-1,NA,BCoV))%>%
    mutate(AVG = AVGGenFN(N1, N2),
           wt = 2 - is.na(N1) - is.na(N2))
  
  return(LIMSDF)
}



N2Fixer = function(data){
  N2Mod = lm(log(N2)~log(N1), data=data)
  data$temp=10^predict.lm(N2Mod,data,interval="none")
  data=data%>%
    mutate(N2=ifelse(is.na(N2),temp,N2))
  data=data%>%
    select(-temp)
  return(data)
}

N1Fixer = function(data){
  N1Mod = lm(N1~N2, data=data)
  data$temp=predict.lm(N1Mod,data,interval="none")
  data=data%>%
    mutate(N2=ifelse(is.na(N2),temp,N2))
  data=data%>%
    select(-temp)
  return(data)
}



Wastewater <- function(Waterfilename){
  missing_codes <- MissingCode()
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
           Site = ifelse(Site == "MMSD P02", "MMSD-P2", Site),
           Site = ifelse(Site == "MMSD P07", "MMSD-P7", Site),
           Site = ifelse(Site == "MMSD P08", "MMSD-P8", Site),
           Site = ifelse(Site == "MMSD P18", "MMSD-P18", Site),
           Site = ifelse(Site == "MMSD P11", "MMSD-P11", Site),
           Site = ifelse(Site == "MMSD P2", "MMSD-P2", Site),
           Site = ifelse(Site == "MMSD P7", "MMSD-P7", Site),
           Site = ifelse(Site == "MMSD P8", "MMSD-P8", Site)
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
    mutate(AVG = AVGGenFN(N1, N2),
           wt = 2 - is.na(N1) - is.na(N2),
           Site = ifelse(Site == "UW-D", "UW-LakeShore", Site),
           Site = ifelse(Site == "UW-S", "UW-Sellery", Site),
           TSS = ifelse(is.na(TSS), `TSS (mg/L)`, TSS),
           isDorm = ifelse(Site %in% c( "UW-LakeShore","UW-Sellery"),TRUE,FALSE),
           Site = ifelse(isDorm&&Date>=ymd("2021-01-11"),paste("Spring",Site),Site),
           Site = ifelse(isDorm&&Date<=ymd("2020-12-25"),paste("Fall",Site),Site),
           Site = ifelse(isDorm&&Date>ymd("2020-12-25")&&Date<ymd("2021-01-11"),paste("Break",Site),Site)
    )%>%
    select(-`TSS (mg/L)`,-isDorm)%>%
    mutate(Date = coalesce(ddays(suppressWarnings(as.numeric(Date))-1)+ymd("1900-01-01"),
                           suppressWarnings(mdy(Date))))
}