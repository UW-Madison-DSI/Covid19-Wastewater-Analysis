Buildplot_gen = function(vari,MainDF,Standards,Loc=NA,ColorType=NA,spanN=NA,
                         LineDF=NA, MeanDF=NA ,DateLimits=NA,WeekDF=NA,
                         AxisPos="top",log_scale=F,RMOutliers=F, 
                         IgnoreLog = c("Pct_BCoV"), Xfreq="24 days",
                         LineColor="black",YLabel=NA,norm=NA, boxingDF=NA,
                         Colplot=F,nrow=1,scalesF="fixed",Start=NA,End=NA,
                         Bind=T){
  Leb=vari
  workDataFrameMain=MainDF%>%
    mutate(var=!!sym(vari))
  if(!is.na(norm)){
    workDataFrameMain=workDataFrameMain%>%
      mutate(var=var/!!sym(norm))
    Leb=paste(vari,"per 100 thousand people")
  }
  set.seed(Standards$myseed)
  GPlot=ggplot()
  if(Colplot&!is.na(MainDF)){
    GPlot=ColGen(GPlot,workDataFrameMain,Standards,ColorType)
  }
  else if(!is.na(MainDF)){
  GPlot=PointGen(GPlot,workDataFrameMain,Standards,ColorType)
  }
  if(is.data.frame(MeanDF)){
    workDataFrameMean=MeanDF%>%
      mutate(var=!!sym(vari))
    if(!is.na(norm)){
      workDataFrameMean=workDataFrameMean%>%
        mutate(var=var/!!sym(norm))
    }
    GPlot=PointGen(GPlot,workDataFrameMean,Standards,ColorType,Size=2.5,Bold=TRUE)
  }
  GPlot=GPlot+ylab(Leb)
  if(!is.na(spanN)){
    GPlot=GPlot+geom_smooth(data=workDataFrameMain,aes(y=var,x=Date),span=spanN,na.rm=TRUE)
  }
  if(is.data.frame(LineDF)){
    workDataFrameLine=LineDF%>%
      mutate(var=!!sym(vari))
    if(!is.na(norm)){
      workDataFrameLine=workDataFrameLine%>%
        mutate(var=var/!!sym(norm))
    }
    GPlot=GPlot+geom_line(data=workDataFrameLine,aes(y=var,x=Date),color=LineColor,size=1,na.rm=TRUE)
  }
  
  if(!is.na(boxingDF)){
    GPlot=GPlot+geom_rect(aes(ymin=lower,ymax=Upper,xmin=Date-.3,xmax=Date+.3),fill="red",alpha=.4,data=meanOfN1)
  }
  
  if(!is.na(Start)&!is.na(End)){
    GPlot=GPlot+geom_vline(xintercept = Start,linetype="dashed")
    GPlot=GPlot+geom_vline(xintercept = End,linetype="dashed")
  }
  

  rec_min=-Inf
  if(log_scale&&!(vari %in% IgnoreLog)){
    GPlot = GPlot + scale_y_log10()
    rec_min=0
  }
  if(is.data.frame(WeekDF)){
    GPlot=GPlot+geom_rect(data=WeekDF, 
                          aes(xmin=Left, xmax=Right, ymin=rec_min, ymax=Inf),
                          fill='pink', alpha=Standards$alphaWeek,na.rm=TRUE)
  }
  VarVec=workDataFrameMain%>%
    filter(Date %in% seq(DateLimits[1],DateLimits[2], by = "day"))%>%
    pull(var)
  ValLimMin=min(VarVec, na.rm = T)
  if (RMOutliers){
    if(log_scale){
      ValLimMax=quantile(VarVec,.995,na.rm=TRUE)[[1]]
    }
    else{
      ValLimMax=quantile(VarVec,.975,na.rm=TRUE)[[1]]
    }
  }
  else{
    ValLimMax=max(VarVec, na.rm = T)
  }
  if(!is.na(YLabel)){
    GPlot=GPlot+ylab(YLabel)
  }
  ValLimits=c(ValLimMin,ValLimMax)
  if(is.na(scalesF)){
    GPlot=GPlot+coord_cartesian(ylim=ValLimits)
  }
  GPlot=GPlot+coord_cartesian(xlim=DateLimits)
  GPlot=GPlot+scale_x_date(position=AxisPos,date_breaks=Xfreq,date_labels="%b %d")
  GPlot=GPlot+facet_wrap(as.formula(paste("~", Loc)), nrow = nrow,scales=scalesF)
  if(Bind){
    GPlot=GPlot+Middle_theme(Standards)
  }
  return(GPlot)
}

PointGen = function(Plot,DF,Standards,ColorType,Size=1,Bold=F){

  if(Bold){
    shape=23
    Alpha=1
  }else{
    shape=21
    Alpha=Standards$alphaPoint
  }
  RPlot=Plot
  if(!is.na(ColorType)){
    RPlot=RPlot+geom_jitter(data=DF,
                            aes(y=var,x=Date,
                                fill = !!sym(ColorType),
                                color= !!sym(ColorType)),
                            shape = shape,
                            alpha=Alpha,
                            height=0,width=.1,
                            size=Size*Standards$PointSize,na.rm=TRUE)
    
  }else{
    RPlot=RPlot+geom_jitter(data=DF,aes(y=var,x=Date),
                            shape = shape,
                            fill="Black",
                            alpha=Alpha,height=0,width=.1,
                            size=Size*Standards$PointSize,na.rm=TRUE)
  }
  if(Bold){
    #slightly jank
    RPlot=RPlot+scale_color_manual(values=rep("Black",3))
  }
  return(RPlot)
}

ColGen = function(Plot,DF,Standards,ColorType,Size=1,width=.4){
  RPlot=Plot
  if(!is.na(ColorType)){
    RPlot=RPlot+geom_rect(data=DF,aes(ymin=0,ymax=var,xmin=Date-width,
                            xmax=Date+width, fill=!!sym(ColorType)),
                            alpha=Standards$alphaPoint,
                            size=Size*Standards$PointSize,na.rm=TRUE)+ 
      scale_fill_manual(values=c("light blue","gray"))
    
  }else{
    RPlot=RPlot+geom_rect(data=DF,aes(ymin=0,ymax=var,xmin=Date-width,
                                      xmax=Date+width,),
                          fill= "gray", alpha=Standards$alphaPoint, 
                          size=Size*Standards$PointSize,na.rm=TRUE)
  }
  return(RPlot)
}



Middle_theme = function(ConfigOption){
  theme_grey() %+replace%    #Theme for the plots in the middle
    theme(
      plot.margin = unit(c(.5,0,0,0), "cm"),
      text = element_text(size=ConfigOption$GenFontSiz),
      axis.title.x = element_blank(),
      strip.text.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = ConfigOption$YAxisLabSiz, colour = "black"))
}
Header_theme = function(ConfigOption){
  Middle_theme(ConfigOption) %+replace%    #Theme for the top plots
    theme(plot.margin = unit(c(.5,0,0,0), "cm"),
          strip.text.x = element_text(size = ConfigOption$GenFontSiz, colour = "black"))
}
Second_theme = function(ConfigOption){
  Middle_theme(ConfigOption) %+replace%    #Theme for the plots underneither the first plots
    theme(plot.margin = unit(c(1,0,0,0), "cm"),
          axis.text.x = element_text(size = ConfigOption$XAxisLabSiz, colour = "black"))
}


SiteLagHeatMap = function(CaseDF,WasteDF,seqminmax,Loc,Dat,Forma){
  depenForm=formula(Forma)[3][[1]]
  indepForm=formula(Forma)[2][[1]]
  CaseFixDF=CaseDF%>%
    mutate(location=!!sym(Loc),Date=!!sym(Dat))
  WasteFixDF=WasteDF%>%
    mutate(location=!!sym(Loc),Date=!!sym(Dat))
  cor.df=NA
  for (i in seqminmax){
    if(i!=min(seqminmax)){
      
      cor.df=cor.df%>%
        select(-location)
    }
    TempCaseDF=CaseFixDF%>%
      mutate(Date=Date+i)
    MMSDDF=full_join(TempCaseDF,WasteFixDF,by=c("location","Date"))%>%
      filter(!is.na(location),!is.na(Date))
    TVec=MMSDDF%>%
      mutate( Val= !!depenForm,depVal=!!indepForm)%>%
      group_by(location)%>%
      summarize("{i}":= cor(x = Val, y = depVal, use = "pairwise.complete.obs"))
    cor.df=cbind(cor.df,TVec)
  }
  mincol <- toString(min(seqminmax))
  maxcol <- toString(max(seqminmax))
  cor.df=cor.df%>%
    select(-cor.df)%>%
    select(location,!location)%>%
    pivot_longer(mincol:maxcol,names_to="time lag",values_to = "Correlation")%>%
    filter(!is.na(Correlation))%>%
    group_by(location)%>%
    mutate(bestCase=max(Correlation))%>%
    mutate(SiteBest=ifelse(Correlation==bestCase,location,NA))
  cor.df$`time lag`=factor(cor.df$`time lag`, levels=c(mincol:maxcol))
  plotedGraph=cor.df%>%
    ggplot(aes(x=`time lag`)) + geom_tile(aes(y=location, fill= Correlation),na.rm=TRUE) + 
    geom_tile(aes(y=SiteBest),fill=NA,color="white",size=1,show.legend = FALSE,na.rm=TRUE)+
    theme(axis.text.x = element_text(angle = 90))+ 
    scale_fill_continuous(type = "viridis",limits=c(-1,1))+
    geom_text(aes(y=SiteBest,label = round(Correlation, 2)),na.rm=TRUE)
  return(plotedGraph)
}

BoxPlotProduction = function(wastewaterDF,Time,concentration,Loc,BinSiz=7,DateLimits=NA){
  BoxGraphic=wastewaterDF%>%
    mutate(Date=!!sym(Time),N1=!!sym(concentration),Site=!!sym(Loc))%>%
    mutate(arbataryBin = as.Date(BinSiz*((as.numeric(Date) %/% BinSiz) - (as.numeric(min(Date)) %/% BinSiz))+as.numeric(min(Date)),origin = as.Date("1970-01-01")))%>%
    mutate(logN1=log(N1))%>%
    group_by(arbataryBin,Site)%>%
    mutate(meanC=exp(mean(logN1,na.rm=TRUE)))%>%
    ggplot()+
    geom_boxplot(aes(y=N1,x=arbataryBin,group=arbataryBin),fill="light blue",na.rm=TRUE)+
    geom_point(aes(y=meanC,x=arbataryBin),shape = 4,color="red")+coord_cartesian(xlim=DateLimits)+scale_y_log10()+
    facet_wrap(~Site,nrow = 1)+scale_x_date(date_breaks="21 days",date_labels="%b %d")
  return(BoxGraphic)
}


LoessSmoothPlot <- function(SiteS="Madison",
                            Data=LIMSFullDF,
                            Independent="N1",Dependent="N1Error",
                            weights=c("Constant","N/NSE"),
                            Span=.3,min=1e-5,max=1e5,
                            BoxWidth=.5,
                            HasNo=F, Title=T,FullLim=F,SiteLab=T,NoLabel=F){
  
  LimitLIMSDF=Data%>%
    filter(Site==SiteS)%>%
    mutate(Indy=!!sym(Independent),
           Dep=!!sym(Dependent))%>%
    filter(!is.na(Dep),
           !is.na(Indy),
           Dep>0,
           Indy>0)
  
  
  LimitLIMSDF$Pred1 <- LoessGenerater(LimitLIMSDF,weights=weights[[1]],Span=Span,min = min,max = max)
  LimitLIMSDF$PredSE <- LoessGenerater(LimitLIMSDF,weights=weights[[2]],Span=Span,min = min,max = max)
  LoessPlot=LimitLIMSDF%>%
    mutate(ErrorMin=Indy-Dep,ErrorMax=Indy+Dep)%>%
    mutate(ErrorMin=ifelse(ErrorMin>0,ErrorMin,0))%>%
    ggplot()+
    aes(x=Date)+
    geom_rect(aes(ymin=ErrorMin,ymax=ErrorMax,xmin=Date-BoxWidth,xmax=Date+BoxWidth),color="black",alpha=.25)+
    geom_rect(aes(ymin=Indy,ymax=Indy,xmin=Date-BoxWidth,xmax=Date+BoxWidth),color="black",alpha=.25)+
    geom_line(aes(y=Pred1,color="1"),size=1.25)+
    geom_line(aes(y=PredSE,color=paste0(Independent,"/",Dependent)),size=1.25)
  if(SiteLab){
    LoessPlot=LoessPlot+facet_wrap(~Site)
  }
  LoessPlot=LoessPlot+
    labs(title="", y=paste0(Independent," (GC/L)"),
         color="Weights Used")+
    #Header_theme(ConfigOption)+
    theme(axis.text = element_text(size = ConfigOption$XAxisLabSiz, colour = "black"),
          axis.title = element_text(size = ConfigOption$GenFontSiz, colour = "black"),
          plot.margin = unit(c(0,0,0,0), "cm"))+ 
    theme(legend.position = "bottom")
  
  #Formatting code
  
  PlaceHolder=""
  if(HasNo){
    PlaceHolder="no"
  }
  
  if(Title){
    LoessPlot=LoessPlot+
      ggtitle(paste0("Weighted loess smoothing with ",PlaceHolder," truncation 
                     loessFit(",Independent, " ~ Date, span=",Span,")"))
  }
  if(FullLim){
    XVar=LIMSFullDF$Date
    YVar=pull(LIMSFullDF,Independent)
    XLim <- c(min(XVar),max(XVar))
    YLim <- c(min(YVar[YVar!=0]),max(YVar))
    LoessPlot <- LoessPlot+
      scale_x_date(limits = XLim)+
      scale_y_log10(limits = YLim)
  }else{
    LoessPlot <- LoessPlot+
      scale_y_log10()
  }
  if(NoLabel){
    LoessPlot <- LoessPlot + 
      labs(title="",x="",y="")
    # theme(axis.title.x=element_blank(),
    #     axis.title.y=element_blank())
  }
  return(LoessPlot)
}


TSPloting <- function(PlotingTS,SourceDF,DepName,IndName,FullPlot=TRUE,SubTitle=NA,FirstDif=FALSE){
  if(FirstDif){
    PlotingTS[[2]] <- diff(PlotingTS[[2]])
    PlotingTS[[1]] <- diff(PlotingTS[[1]])
    Lab <- paste("First Diffrence of", DepName, "and", IndName)
  }else{
    Lab <- paste("Visual relationship of", DepName, "and", IndName)
  }
  plot.new()
  par(mar = c(5, 4, 4, 4) + 0.3)           
  
  if(FullPlot){
    plot.ts(rollmean(exp(PlotingTS[[4]]), 7,align="right",fill = NA),
            col = "steelblue3", 
            axes = FALSE, 
            xlab = "", 
            ylab = "",
            xaxt = "n",
            log="y")
    
    axis(side = 2, col.axis="blue")
    
    par(new = TRUE)  
    plot.ts(rollmean(PlotingTS[[3]], 7,align="right",fill = NA),
            col = "hotpink",        
            axes = FALSE, xlab = "", ylab = "",xaxt = "n")
    
    par(new = TRUE)  
    plot.ts(exp(PlotingTS[[2]]),
            col = "blue",        
            axes = FALSE, 
            xlab = "", 
            ylab = "",
            log = 'y',
            xaxt = "n")
  }else{
    plot.ts(PlotingTS[[2]], 
            col = "blue", 
            axes = FALSE,
            main=Lab,
            ylab="",
            sub=SubTitle,
            log = '',
            xaxt = "n")
    axis(side = 2, col.axis="blue")
  }
  
  par(new = TRUE)                           
  plot.ts(PlotingTS[[1]], col = "red",        
          axes = FALSE, xlab = "", ylab = "",xaxt = "n")
  
  axis(side = 4, at = pretty(range(PlotingTS[[1]])),col.axis="red")
  mtext("Cases", side = 4, line = 3, col = "red")
  mtext("N1 (GC/L)", side = 2, line = 3, col = "blue")
  if(FullPlot){
    legendNames <- c("SLD Cases","7 MA Cases","Loess Smoothing","7 MA N1")
    legendColors <- c("red","hotpink","blue", "steelblue3")
  }else{
    legendNames<- c("SLD Cases","Loess Smoothing")
    legendColors<- c("red","blue")
  }
  legend("topright", legend=legendNames, col=legendColors, lty=1, cex=.75)
  axis(1,
       pretty(SourceDF$Date),labels =format(pretty(SourceDF$Date), "%Y-%m-%d"))
  #pretty(format(SourceDF$Date, "%Y-%m-%d")))
}

SLDGraphics <- function(SiteS,DepTSVec,IndTSVec,DepName,IndName,efficient=FALSE){
  print("Visual Relationship")
  TSPloting(list(DepTSVec,IndTSVec),MergedDF,DepName,IndName,
            FullPlot=FALSE,SubTitle=SiteS)
  TSPloting(list(DepTSVec,IndTSVec),MergedDF,DepName,IndName,
            FullPlot=FALSE,SubTitle=SiteS,FirstDif=TRUE)
  
  CCFVec <- ccf(IndTSVec,DepTSVec,
                main=paste("CC between",IndName,"and", DepName),
                sub=SiteS)
  OffSet <- which(CCFVec[[1]]==max(CCFVec[[1]]))-21 #Best offset of Straight ccf
  
  
  preWhiteFit <- auto.arima(IndTSVec, seasonal=FALSE,
                            stepwise=FALSE, approximation=FALSE) #underlying arima trend of Ind
  
  IndResid <- IndTSVec - fitted(Arima(IndTSVec, model = preWhiteFit))
  DepResid <- DepTSVec - fitted(Arima(DepTSVec, model = preWhiteFit))
  CCFVecPre <- ccf(IndResid, DepResid,lag.max=10,
                   main=paste("prewhiten CC between",IndName,"and", DepName),
                   sub=SiteS) #CC removing Arima relationship of Ind
  
  OffSetWhit <- which(CCFVecPre[[1]]==max(CCFVecPre[[1]]))-21#Best offset of PreWhite ccf
  
  if(TRUE){#max(CCFVecPre[[1]])<.15){ #If no prewhite corr is significant then use straight ccf
    print("Ignoring preWhite process")
    OffSetWhit <- OffSet
  }
  
  print(paste("using offset of", OffSetWhit))
  
  TSUnionDF <- ts.intersect(DepTSVec,
                            OGVec = stats::lag(IndTSVec,OffSetWhit))
  
  print("Visual Relationship with offset")
  TSPloting(list(TSUnionDF[,1],TSUnionDF[,2]),MergedDF,DepName,IndName,
            FullPlot=FALSE,SubTitle=SiteS)
  TSPloting(list(TSUnionDF[,1],TSUnionDF[,2]),MergedDF,DepName,IndName,
            FullPlot=FALSE,SubTitle=SiteS,FirstDif=TRUE)
  
  
  OLM <- lm(TSUnionDF[,1]~TSUnionDF[,2])
  print("Ordinary LM")
  print(summary(OLM))
  ggtsdisplay(residuals(OLM))
  FM <- auto.arima(TSUnionDF[,1],xreg=TSUnionDF[,2])
  #FM <- cochrane.orcutt(OLM,max.iter=1000)
  print("LM with Arima residuals")
  ggtsdisplay(residuals(FM))
  print(summary(FM))
  return(FM)
}



TSPloting2 <- function(PlotingTS,SourceDF,SubTitle,
                       SLD=TRUE,span=.125){
  if(SLD){
    Shade <- 0.5
    Thickness <- 1
  }else{
    Shade <-1
    Thickness <- 2
  }
  N1Axis=""
  CasesAxis=""
  RangeCases <- range(PlotingTS[[1]],na.rm=TRUE)
  RangeN1 <- range(PlotingTS[[2]],na.rm=TRUE)
  MaxN1 <- max(SourceDF$N1,na.rm=TRUE)
  MinN1 <- min(SourceDF$N1,na.rm=TRUE)
  MaxCases <- max(PlotingTS[[3]],na.rm=TRUE)
  N1Ratio <- log(MaxN1)/log(RangeN1[2])
  CasesRatio <- log(MaxCases)/(log(RangeCases[2]))
  Displace <- max(N1Ratio,CasesRatio)
  print(paste(RangeCases[[1]]))
  
  #RangeCases[2] <- RangeCases[2]*exp(Displace+1.5)
  #RangeN1[2] <- RangeN1[2]*exp(Displace)
  
  RangeCases[1] <- 0 #RangeCases[1]-4
  
  plot.new()
  par(mar = c(8, 4, 4, 4) + 0.1)
  plot(1, type="n", xlab=SubTitle, ylab="", axes = FALSE,
       xlim=range(SourceDF$Date), ylim=RangeN1,log=N1Axis)
  rect(SourceDF$Date-.5,
       SourceDF$N1-SourceDF$N1Error,
       SourceDF$Date+.5,
       SourceDF$N1+SourceDF$N1Error,
       col  = rgb(0, 0, 1, alpha=0.25),
       border  = NA,
       ylim = RangeN1,
       xlab = "",
       ylab = "",
       xaxt = "n")
  par(new = TRUE)
  barplot(as.numeric(PlotingTS[[3]]),
          col  = rgb(1, 0, 0, alpha=0.1),
          xlab = "",
          ylab = "",
          xaxt = "n",
          border = NA,
          log=CasesAxis,
          ylim = RangeCases,
          axes = FALSE)
  par(new = TRUE)
  
  plot.ts(rollmean(PlotingTS[[3]], 7,align="right",fill = NA),
          col = rgb(1, 0, 0, alpha=Shade),
          ylim = RangeCases,
          axes = FALSE,
          lwd=Thickness,
          log=CasesAxis,
          xlab = "",
          ylab = "",
          xaxt = "n")
  axis(4,col.axis = "red",cex.axis=.75)
  
  RangeDates <- range(SourceDF$Date)
  ticks <- seq(RangeDates[1],RangeDates[2], by = "month")
  axis(1,
       ticks,
       labels = FALSE,
       cex.axis=.5)
  text(cex=.75, x=ticks-2, y=-30, format(ticks,"%b %Y"),
       xpd=TRUE, srt=30)
  
  par(new = TRUE)
  plot.ts(PlotingTS[[2]],
          col = "blue",
          ylim = RangeN1,
          axes = FALSE,
          xlab = "",
          ylab = "",
          log=N1Axis,
          lwd=2,
          xaxt = "n")
  axis(2,col.axis = "blue",cex.axis=.75)
  
  legendNames <- c("7 day MA Cases",paste("loess smoothing with span=",span))
  legendColors <- c(rgb(1, 0, 0, alpha=Shade),"blue")
  if(SLD){
    legendNames <- c("Shedding lag distribution",legendNames)
    legendColors <- c("red",legendColors)
    
    par(new = TRUE)
    plot.ts(PlotingTS[[1]],
            ylim = RangeCases,
            col = "red",
            lwd=2,
            axes = FALSE,
            log=CasesAxis,
            xlab = "", ylab = "",xaxt = "n")
  }
  mtext("Cases", side = 4, line = 2, col = "red",cex=.75)
  mtext("N1 (GC/L)", side = 2, line = 2, col = "blue",cex=.75)
  
  
  legend("topright", legend=legendNames, col=legendColors, lty=1, cex=.5)
  
}
