
#Generate the dataframe structure used for rest of code
DFPrepAnalysis <- function(DF,
                           Weights,
                           StartDate,
                           DaySmoothing,
                           Lag,
                           CasesUsed="BinningThenSLDCases",
                           NSUsed="N1Mean",
                           DateStart=mdy("9/1/2020")
){
  MadData <- DF%>%
    filter(Date>DateStart)%>%
    mutate(MovedCases = data.table::shift(SLDCases,Lag),
           BinningCases = data.table::shift(Cases,Lag),
           Week=(as.numeric(Date)+StartDate)%/%DaySmoothing)%>%
    group_by(Week)%>%
    summarise(N1Median=median(N1,na.rm=TRUE),
              N1Mean=exp(mean(log(N1),na.rm=TRUE)),
              AVGMedian=median(sqrt(N1*N2),na.rm=TRUE),AVGMean=exp(mean(log(sqrt(N1*N2)),na.rm=TRUE)),
              BinningCases=mean(BinningCases,na.rm = TRUE),
              SLDThenBinningCases=mean(MovedCases,na.rm = TRUE))%>%
    mutate(BinningThenSLDCases=c(NA,NA,rollapply(BinningCases,width=3,FUN=weighted.mean,
                                                 w=Weights,
                                                 na.rm = TRUE)))%>%
    mutate(CasesMain=!!sym(CasesUsed),NSMain=!!sym(NSUsed))%>%
    filter(is.finite(CasesMain),is.finite(NSMain))
}


#Takes Case data and and N1 data and outputs results of binning and modeling the relationship
BinRelationGen <- function(DF,
                           Weights,
                           StartDate=0,
                           DaySmoothing=7,
                           Lag=0,
                           Show=FALSE,
                           Ret="LM",
                           CasesUsed="BinningThenSLDCases",
                           NSUsed="N1Mean",
                           DateStart=mdy("9/1/2020"),
                           LogModel=FALSE,
                           Intercept=TRUE,
                           Pop=FALSE){
  #Create the binning with all the function options
  MadData <- DFPrepAnalysis(DF=DF,
                            Weights=Weights,
                            StartDate=StartDate,
                            DaySmoothing=DaySmoothing,
                            Lag=Lag,
                            CasesUsed=CasesUsed,
                            NSUsed=NSUsed,
                            DateStart=DateStart)
  
  #has issues. need to be fixed. Some pop normalization
  if(FALSE){
    MadData$CasesMain <- MadData$CasesMain/mean(MadData$Pop)
  }
  
  #Changes the data to log log
  if(LogModel){
    MadData <- MadData%>%
      mutate(CasesMain=log(CasesMain),
             NSMain=log(NSMain))
  }
  
  #Controls weighter we assume there is an intercept or not
  if(Intercept){
    LMod <- lm(CasesMain~NSMain,data=MadData)
    PVal <- signif(summary(LMod)$coefficients[2,4],3)
    Slope=LMod[[1]][2]
    Inter=LMod[[1]][1]
  }else{
    LMod <- lm(CasesMain~NSMain-1,data=MadData)
    PVal <- signif(summary(LMod)$coefficients[1,4],3)
    Slope=LMod[[1]][1]
    Inter=0
  }
  
  COR <- signif(cor(MadData$CasesMain,MadData$NSMain,use="pairwise.complete.obs"),3)
  R2 <- signif(summary(LMod)$r.squared,3)
  
  if(Show){#Controls if we generate these plots. Only return them if Ret=="Plot"
    TSPlot <- MadData%>%#Ploting binned time function to see shape of TS
      ggplot(aes(x=Week))+
      geom_line(aes(y=CasesMain,color="CaseSignal"))+
      geom_line(aes(y=NSMain*Slope+Inter,color="Covid Signal"))
    
    #grabing edges of plot to place text
    MinX <- min(MadData$CasesMain,na.rm=TRUE)
    XRange <- max(MadData$CasesMain,na.rm=TRUE)-MinX
    MinY <- min(MadData$NSMain,na.rm=TRUE)
    YRange <- max(MadData$NSMain,na.rm=TRUE)-MinY
    
    CompPlot <- MadData%>%#Plot Case vs N1 to show correlation
      ggplot()+
      aes(x=CasesMain,y=NSMain)+
      geom_abline(aes(color="Line of best first",slope=1/Slope,intercept=-Inter/Slope),size=2)+
      geom_point(size=1.5)+#helpful stats and the chosen line are added
      labs(x=paste(DaySmoothing,"Day binning of SLD Cases"),
           y=paste(DaySmoothing,"Day binning of N1"))+
      annotate("text", x=.9*XRange+MinX, y=.2*YRange+MinY, label= paste("PVal:", PVal))+
      annotate("text", x=.9*XRange+MinX, y=.23*YRange+MinY,label= paste("S Factor:", signif(1/Slope,3)))+
      annotate("text", x=.9*XRange+MinX, y=.26*YRange+MinY,label= paste("R^2:", R2))+
      annotate("text", x=.9*XRange+MinX, y=.3*YRange+MinY, label= paste("Cor:", COR))
  }
  
  #Different return options
  if(Ret=="LM"){
    return(LMod)
  }else if(Ret=="COR"){
    return(COR)
  }else if(Ret=="R2"){
    return(R2)
  }else if(Ret=="PVal"){
    return(PVal)
  }else if(Ret=="Plot"){
    return(list(CompPlot,TSPlot))
  }else if(Ret=="All"){#StartDate,DaySmoothing,Lag
    return(paste(COR,R2,PVal,Slope))
  }
  
}

LocInput <- function(Mat,Loc,StartDate,DaySmoothing,Lag){
  #Works backward from list to figure out what part of the list
  #connects to what inputs
  SDL <- length(StartDate)
  LL <- length(Lag)
  iL <- StartDate[((Loc-1)%%SDL)+1]
  jL <- DaySmoothing[(Loc-1)%/%(SDL*LL)+1]
  kL <- Lag[((Loc-1)%%(SDL*LL)%/%SDL)+1]
  return(c(iL,jL,kL))
}

#Makes matrix exploring the parameter space
BinRelMatrix <- function(DF,
                         Weights,
                         StartDate=0:7,
                         DaySmoothing=c(7,14),
                         Lag=-2:2,
                         Show2=FALSE,
                         Mat=FALSE,
                         Ret="R2",
                         CasesUsed="BinningThenSLDCases",
                         NSUsed="N1Mean", 
                         DateStart=mdy("9/1/2020"),
                         LogModel=FALSE,
                         Intercept=FALSE,Pop=FALSE){
  
  #Get input options to get total length of list
  SDL <- length(StartDate)
  DSL <- length(DaySmoothing)
  LL <- length(Lag)
  #create list to operate on
  Matrix=vector(mode="numeric", length=SDL*DSL*LL)
  for (j in 1:DSL){
    for (k in 1:LL){
      for (i in 1:SDL){
        #create Binning and LM and store requested relationship
        Matrix[j*SDL*LL+k*SDL+i-SDL*LL-SDL]=BinRelationGen(DF=DF,
                                                           NSUsed=NSUsed,  
                                                           Weights=Weights,
                                                           StartDate=StartDate[i],
                                                           DaySmoothing=DaySmoothing[j],
                                                           Lag=Lag[k],
                                                           Ret=Ret,
                                                           CasesUsed=CasesUsed,
                                                           DateStart=DateStart,
                                                           LogModel=LogModel,
                                                           Intercept=Intercept,Pop=Pop)
      }
    }
  }
  if(Ret=="PVal"){#Want to min pval instead
    Loc <- which.min(Matrix)
    Target=min(Matrix)
  }else if(Ret=="All"){#no clear optimization for ret=="ALL" so just return mat
    return(Matrix)
  }else{#All other var want the max
    Loc <- which.max(Matrix)
    Target=max(Matrix)
  }
  
  ListInputs <- LocInput(Matrix,Loc,StartDate,DaySmoothing,Lag)
  #Make sure the max of list matches what it should be
  stopifnot(Target==BinRelationGen(DF=DF,NSUsed=NSUsed,Weights=Weights,
                                   StartDate=ListInputs[1],
                                   DaySmoothing=ListInputs[2],
                                   Lag=ListInputs[3],
                                   Ret=Ret,
                                   CasesUsed=CasesUsed,
                                   DateStart=DateStart,
                                   LogModel=LogModel,
                                   Intercept=Intercept,Pop=Pop))
  #Collect data for result report
  BestLM <- BinRelationGen(DF=DF,NSUsed=NSUsed,Weights=Weights,
                           StartDate=ListInputs[1],
                           DaySmoothing=ListInputs[2],
                           Lag=ListInputs[3],
                           Show=Show2,
                           Ret="LM",
                           CasesUsed=CasesUsed,
                           DateStart=DateStart,
                           LogModel=LogModel,
                           Intercept=Intercept,Pop=Pop)
  
  SlopePos=Intercept+1
  SlopeL <- signif(BestLM[[1]][SlopePos],3)
  DayOfWeekData <- weekdays(seq(as.Date("11/10/2020"), by=1, len=8))
  
  
  Ret <- paste("Best relationship at",DayOfWeekData[ListInputs[1]+1],ListInputs[2],ListInputs[3],"with", Ret ,"of",
               max(Matrix),"with F factor of",SlopeL)
  
  #Return options
  if(Mat){
    return(list(Matrix,Ret))
  }
  return(BestLM)
}


#makes plot of heatmap matrix
HeatMapMaker <- function(NVector,N,ColNames,RowNames,Main,ColorName,Site){
  ColorLegend <- brewer.pal(n = 3, name = ColorName)
  Color <- brewer.pal(n = 8, name = ColorName)
  Mat <- matrix(NVector, nrow=N)
  rownames(Mat) <- RowNames
  colnames(Mat) <- ColNames
  heatmap(Mat,Rowv=NA,Colv=NA,col = Color ,main=Main)
  legend(x="right",cex=.75,
         legend=c(signif(min(Mat),2),
                  signif(median(Mat),2),
                  signif(max(Mat),2)),
         fill=ColorLegend)
  title(xlab="time laged",ylab="Binning Start",sub=Site)
}


BestCorDFGen <- function(Site,DFCases,DFN1,Rolling="Cases",DateFilt=mdy("9/15/2020"),
                         keep=c("N1","N2")){
  SiteLimsDF <- DataPrep(DFN1,
                         keep=keep,
                         Site)
  
  SCPDF <- DFSmoothingFNC(DFCases,Site=Site,Rolling=Rolling)%>%
    mutate(Site=Site)
  
  SCPDF2 <- DFSmoothingFNC(DFCases,PreRoll=TRUE,Site=Site,Rolling=Rolling)%>%
    mutate(Site=Site)
  
  SCPDF3 <- inner_join(SCPDF,SCPDF2,by=c("Date","Site",!!sym(Rolling)),suffix=c("",".PreRolled"))
  
  MergedDF <- full_join(SCPDF3,SiteLimsDF, by=c("Date","Site"))
  
  
  MergedDF <- MergedDF%>%
    filter(Date>DateFilt)%>%
    select(Date,Site,Cases,SLDCases,SLDCases.PreRolled,N1,N2)
  
  return(MergedDF)
}

VecToDF <- function(DF,StartDate=0:7,
                    BinSize=c(7,14),Shift=-2:2){
  stopifnot(length(DF)==length(BinSize)*length(StartDate)*length(Shift))
  DayOfWeekData <- weekdays(as.Date(StartDate))
  xAxisPlot <- expand.grid(BinSize, Shift)
  xAxisPlot <- xAxisPlot[order(xAxisPlot$Var1),]
  
  AxisPattern<- apply(xAxisPlot, 1, paste, collapse=" ")
  Mat <- matrix(DF, nrow=length(StartDate))
  colnames(Mat) <- AxisPattern
  TransDF <- as.data.frame(Mat)
  TransDF$WeekStart <- DayOfWeekData
  TransDF <- pivot_longer(TransDF,`7 -2`:`14 2`,
                          names_to=c("Bin Size","Offset"),
                          names_sep=" ",values_to="All")%>%
    separate(All,c("COR","R2","PVal","Slope"),sep=" ")
  #TransDF <- TransDF[!duplicated(TransDF), ]
  
  return(TransDF)
}


TableDFGen <- function(DF,Cases,NSignal){
  
  DataTableVecLog <- BinRelMatrix(DF=DF,
                                  Weights=WeightVec,
                                  DaySmoothing=c(7,14),
                                  Ret="All",
                                  LogModel=TRUE,
                                  CasesUsed=Cases,
                                  NSUsed=NSignal,
                                  Intercept=FALSE)
  DFLog <- VecToDF(DataTableVecLog)%>%
    mutate(LogModel="TRUE",hasIntercept="FALSE")
  DataTableVecLogInt <- BinRelMatrix(DF=DF,
                                     Weights=WeightVec,
                                     DaySmoothing=c(7,14),
                                     Ret="All",
                                     LogModel=TRUE,
                                     CasesUsed=Cases,
                                     NSUsed=NSignal,
                                     Intercept=TRUE)
  DFLogInt <- VecToDF(DataTableVecLogInt)%>%
    mutate(LogModel="TRUE",hasIntercept="TRUE")
  
  DataTableVec <- BinRelMatrix(DF=DF,
                               Weights=WeightVec,
                               DaySmoothing=c(7,14),
                               Ret="All",
                               LogModel=FALSE,
                               CasesUsed=Cases,
                               NSUsed=NSignal,
                               Intercept=FALSE)
  DFBase <- VecToDF(DataTableVec)%>%
    mutate(LogModel="FALSE",hasIntercept="FALSE")
  DataTableVecInt <- BinRelMatrix(DF=DF,
                                  Weights=WeightVec,
                                  DaySmoothing=c(7,14),
                                  Ret="All",
                                  LogModel=FALSE,
                                  CasesUsed=Cases,
                                  NSUsed=NSignal,
                                  Intercept=TRUE)
  DFInt <- VecToDF(DataTableVecLogInt)%>%
    mutate(LogModel="FALSE",hasIntercept="TRUE")
  
  FullDF <- rbind(DFLog,DFInt,DFBase,DFLogInt)%>%
    mutate(CaseSignal=Cases,NSignal=NSignal)%>%
    select(CaseSignal,NSignal,LogModel,hasIntercept,WeekStart,
           `Bin Size`,Offset,COR,R2	,PVal)
}

TableDFGen2 <- function(DF,NSignal){
  CaseOptions <- c("BinningThenSLDCases","SLDThenBinningCases","BinningCases")
  do.call("rbind",lapply(CaseOptions,TableDFGen,DF=DF,NSignal=NSignal))
}

HeatMapCor <- function(DF,Weights,
                       StartDate=0:7,
                       DaySmoothing=c(7),
                       Lag=-2:2,
                       ShowPlots=FALSE,
                       CasesUsed="BinningThenSLDCases",
                       DateStart=mdy("10/1/2020"),
                       Pop=FALSE,
                       LogModel=FALSE){
  Site=unique(DF$Site)
  R2CF <- BinRelMatrix(DF=DF,Weights=Weights,DaySmoothing=DaySmoothing,
                       StartDate=StartDate,Lag=Lag,Show2=ShowPlots,
                       Mat=TRUE,Ret="R2",Pop=Pop,LogModel=LogModel,
                       CasesUsed=CasesUsed,DateStart=DateStart)
  PValCF <- BinRelMatrix(DF=DF,Weights=Weights,DaySmoothing=DaySmoothing,
                         StartDate=StartDate,LogModel=LogModel,
                         Lag=Lag,Show2=ShowPlots,Mat=TRUE,Ret="PVal",Pop=Pop,
                         CasesUsed=CasesUsed, DateStart=DateStart)
  CorCF <- BinRelMatrix(DF=DF,Weights=Weights,DaySmoothing=DaySmoothing,
                        StartDate=StartDate,LogModel=LogModel,
                        Lag=Lag,Show2=ShowPlots,Mat=TRUE,Ret="COR",Pop=Pop,
                        CasesUsed=CasesUsed,DateStart=DateStart)
  print(paste("R2:",R2CF[[2]]))
  print(paste("PVal:",PValCF[[2]]))
  print(paste("Cor:",CorCF[[2]]))
  
  DayOfWeekData <- weekdays(seq(DateStart, by=1, len=length(StartDate)))
  
  xAxisPlot <- expand.grid(DaySmoothing, Lag)
  xAxisPlot <- xAxisPlot[order(xAxisPlot$Var1),]
  
  AxisPattern<- apply(xAxisPlot, 1, paste, collapse=" ")
  HeatMapMaker(R2CF[[1]],length(StartDate),AxisPattern,
               DayOfWeekData,Site=Site, "R2 relationship",ColorName="YlOrRd")
  HeatMapMaker(PValCF[[1]],length(StartDate),AxisPattern,
               DayOfWeekData, Site=Site, "PVal relationship",ColorName="YlOrRd")
  HeatMapMaker(CorCF[[1]],length(StartDate),AxisPattern,
               DayOfWeekData, Site=Site, "Cor relationship",ColorName="YlOrRd")
}