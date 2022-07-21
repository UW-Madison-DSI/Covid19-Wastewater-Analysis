
RollPerPos = function(RollingDF,CaseName,TestName,Facet,n=7){
  TDF <- RollingDF%>%
    mutate(Facet=!!sym(Facet),
           Case=!!sym(CaseName),
           Test=!!sym(TestName)) #Names 7day variables something Consistent
  
  SiteBoundrys <- TDF%>%#Storing what range it makes sense to take the mean over
    group_by(Facet)%>%
    summarise(MaxDate=max(Date),
           MinDate=min(Date))
  
  FaucetOptions=unique(TDF$Facet) #Collect the names of all the locations
  FulldayRange=expand.grid(seq.Date(min(TDF$Date),max(TDF$Date), by = "day"),FaucetOptions)%>%
    rename(Date=Var1,Facet=Var2)#Get DF that has a row for every date in span
  FullDataFM <- full_join(TDF,FulldayRange,by=c("Date","Facet"))%>%#Make sure the data has a row for each day in span
    full_join(SiteBoundrys,by=c("Facet"))%>%
    filter(Date<=MaxDate&Date>=MinDate)%>%#Remove dates not in Site range
    arrange(Facet,Date)%>%
    group_by(Facet)%>%
    mutate(Per_pos=RollPerPosHelperFunc(Case,Test,n=n), 
           Case=RollAvgHelperFunc(Case,N=n,Method="AR"), #Take rolling mean
           Test=RollAvgHelperFunc(Test,N=n,Method="AR")
           )%>%
    ungroup()
  FullDataFM[[CaseName]]=FullDataFM$Case
  FullDataFM[[TestName]]=FullDataFM$Test
  FullDataFM[[Facet]]=FullDataFM$Facet
  
  FullDataFM <- FullDataFM%>%
    select(-Facet,-Test,-Case,-MaxDate,-MinDate)
  return(FullDataFM)
}


RollPerPosHelperFunc = function(vectorCases,vectorTests,n=7){
  stopifnot(length(vectorCases)==length(vectorTests))
  CurrNumCase = rep(NA,n)
  CurrNumTests = rep(NA,n)
  NoData=TRUE
  SlideMeanVec=vector(mode="double", length=length(vectorCases))
  for (i in 1:length(vectorCases)){
    if(!is.na(vectorCases[i])&&!is.na(vectorTests[i])){
      if(NoData){
        NoData=FALSE
      }
      CurrNumCase[(i-1)%%n+1]=vectorCases[i]
      CurrNumTests[(i-1)%%n+1]=vectorTests[i]
    }else{
      CurrNumCase[(i-1)%%n+1]=NA
      CurrNumTests[(i-1)%%n+1]=NA
    }
    if(NoData){
      SlideMeanVec[i]=-500
    }else{
      SlideMeanVec[i]=100*sum(CurrNumCase,na.rm=T)/sum(CurrNumTests,na.rm=T)
    }
  }
  return(SlideMeanVec)
}


RollAvg = function(RollingDF,FacetName="Site",n=21,method="Arth",var=c("ReportedCases","EpisodeCases","CollectedCases","ConfirmedCases")){
  TDF=RollingDF%>%
    mutate(Facet=!!sym(FacetName))
  FaucetOptions=unique(TDF$Facet)
  FulldayRange=expand.grid(seq.Date(min(TDF$Date),max(TDF$Date), by = "day"),FaucetOptions)%>%
    rename(Date=Var1,Facet=Var2)
  
  FullDataFM=full_join(TDF,FulldayRange,by=c("Date","Facet"))%>%
    arrange(Facet,Date)%>%
    group_by(Facet)%>%
    mutate(across(any_of(var),RollAvgHelperFunc,N=n,Method=method))%>%
    ungroup()
  FullDataFM[[FacetName]]=FullDataFM$Facet
  FullDataFM=FullDataFM%>%
    select(-Facet)
  return(FullDataFM)
}

RollAvgHelperFunc = function(vectorCases,N=14,Method="Geo"){
  CurrNumCase = rep(NA,N)
  SlideMeanVec=vector(mode="double", length=length(vectorCases))
  for (i in 1:length(vectorCases)){
    CurrNumCase[(i-1)%%N+1]=vectorCases[i]
    if(Method == "Geo"){
      slider = exp(mean(log(CurrNumCase),na.rm=T))
    } else{
      slider = mean(CurrNumCase,na.rm=T)
    }
    SlideMeanVec[i]=slider
    #exp(mean(log(CurrNumCase),na.rm=T))
    #mean(CurrNumCase,na.rm=T)
  }
  return(SlideMeanVec)
}

WeekendGen = function(DateVec){
  #Generating the weekends starts and end dates
  #TO DO:Capture the weekend if the data intersects with it
  DateRangeDF=data.frame(Date=seq(min(DateVec), max(DateVec), "days"))%>%
    mutate(Days=weekdays(Date))%>%
    filter(Days %in% c("Sunday","Monday"))
  if (DateRangeDF$Days[1]=="Monday"){
    DateRangeDF=DateRangeDF[-1,]}
  if (tail(DateRangeDF$Days, n=1)=="Sunday"){
    DateRangeDF=head(DateRangeDF, -1)}
  MRan=filter(DateRangeDF,Days=="Sunday")%>%
    rename(Left=Date)
  SRan=filter(DateRangeDF,Days=="Monday")%>%
    rename(Right=Date)
  DateRangeCDF=cbind(MRan,SRan)%>%
    select(Left,Right)
  return(DateRangeCDF)
}


DataPrep <- function(DF=NA,Site=NA,keep=c()){
  if(!is.na(Site)){
    FilteredVec1 <- filter(DF,Site==Site)
  }else{
    FilteredVec1 <- DF
  }
  
  FullDayDF <- data.frame(Date=seq(min(FilteredVec1$Date),
                                   max(FilteredVec1$Date),1))
  ReadyDF <- full_join(FullDayDF,FilteredVec1, by = c("Date"))%>%
    #fill(one_of(keep), .direction = "down")%>%
    mutate(Site=Site)%>%
    select(Date,Site,one_of(keep))
  return(ReadyDF)
}



DFLoessFNC <- function(Data,Var="N1",span=.3,Site="Madison"){#makes loess smoothing of data
  
  MainDF <- Data%>%
    filter(Site==Site)%>%
    tidyr::fill(all_of(Var), .direction = "down")#fills the df so the loess smoothing works
  loessModel=loessFit(y=log(MainDF[[Var]]),
                      x=MainDF$Date,
                      #weights=WeightList,
                      span=span,
                      #min.weight=minVal,
                      #max.weight=maxVal,
                      iterations=15)
  return(exp(loessModel$fitted))
}

SLDSmoothing <- function(CaseDF,CaseColumns,Weights=dgamma(1:21,scale =5.028338,shape =2.332779)){
  SmoothSize <- length(Weights)
  FullDayDF <- CaseDF%>%
    group_by(Site)%>%
    mutate(SLDCases = c(rep(NA,SmoothSize-1),
                        rollapply(!!sym(CaseColumns),width=SmoothSize,FUN=weighted.mean,
                                  w=Weights,
                                  na.rm = TRUE)))
  return(FullDayDF)
  
}


DFSmoothingFNC <- function(CaseDF,
                           Site,
                           Rolling="Cases",
                           PreRoll=FALSE,
                           Weights=dgamma(1:21,scale =5.028338,shape =2.332779)){
  #5.028338
  #2.332779
  
  FullDayDF <- DataPrep(CaseDF,Rolling,Site=Site)
  
  if(PreRoll){
    FullDayDF <- FullDayDF%>%
      group_by(Site)%>%
      mutate(SLDCases = c(rep(NA,6),
                        rollapply(!!sym(Rolling),width=7,
                                  FUN=mean,
                                  na.rm = TRUE)))
  }else{#refactor to work iwth diffretn SLD function. support optimization
    FullDayDF <- FullDayDF%>%
      mutate(SLDCases=!!sym(Rolling))
  }
  FullDayDF <- SLDSmoothing(FullDayDF,CaseColumns="SLDCases",Weights=Weights)
  return(FullDayDF)
}





ReplacementFilter <- function(n,Main,Rep){
  Noise <- abs((Main-Rep)/Rep)
  Noise[is.na(Noise)] <- FALSE
  NoiseFilter <- sort(Noise,TRUE)[n+1]
  #length(logN1VecFiltA[Noise>=NoiseFilter])
  Ret <- Main
  Ret[Noise>NoiseFilter] <- Rep[Noise>NoiseFilter]
  return(Ret)
}