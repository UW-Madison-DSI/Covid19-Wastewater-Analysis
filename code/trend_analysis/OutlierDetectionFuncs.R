



IQRFunc <- function(Vector,Gap=1.5,na.rm=TRUE,Ret=1){
  BaseQuimt <- quantile(Vector,c(.25,.75),na.rm=na.rm)
  IQRVal <- IQR(Vector,na.rm=na.rm)
  MedianVal <- median(Vector,na.rm=na.rm)
  ReturnVals <- list(MedianVal,BaseQuimt[[2]]+Gap*IQRVal,BaseQuimt[[1]]-Gap*IQRVal)
  return(ReturnVals[[Ret]])
}
MedianFunc <- function(Vector,Gap=4,na.rm=TRUE,Ret=1){
  BaseVal <- median(Vector,na.rm=na.rm)
  ReturnVals <- list(BaseVal,Gap*BaseVal,BaseVal/Gap)
  return(ReturnVals[[Ret]])
}
MeanSDFunc <- function(Vector,Gap=2.5,na.rm=TRUE,Ret=1){
  MeanVal <- mean(Vector,na.rm=na.rm)
  SD <- sd(Vector,na.rm=na.rm)
  ReturnVals <- list(MeanVal,MeanVal+Gap*SD,MeanVal-Gap*SD)
  return(ReturnVals[[Ret]])
}


RollingFunc <- function(Vector,Method,align,Gap=2,DaySmoothed=21){
  ReturnThresh <- rollapply(data = Vector, width = DaySmoothed, FUN = Method, 
                            Ret=1,fill=NA,Gap=Gap,align=align)
  HighThresh <- rollapply(data = Vector, width = DaySmoothed, FUN = Method, 
                          Ret=2,fill=NA,Gap=Gap,align=align)
  LowThresh <- rollapply(data = Vector, width = DaySmoothed, FUN = Method, 
                         Ret=3,fill=NA,Gap=Gap,align=align)
  
  return(list(ReturnThresh,LowThresh,HighThresh))
}

#method returns list of outlier best guess of data
OutlierDetectRobustFunc <- function(Vector,method,Gap,align,n = 5,Bin = 21,Lines=FALSE){
  SerLen <- length(Vector)
  BestVector <- c(rev(Vector[1:(1*Bin)]),Vector,rev(Vector[-(1*Bin):0+(SerLen+1)]))
  
  for(i in 1:n){#robustly remove outliers and recalc smooth line
    
    methReturn <- RollingFunc(BestVector, Gap=Gap,
                              method, DaySmoothed=Bin, align=align)
    
    BestVector <- ifelse(BestVector>methReturn[[3]] | BestVector<methReturn[[2]],
                         methReturn[[1]],BestVector)
  }
  BestVector <- ifelse(is.na(BestVector),methReturn[[1]],BestVector)[(1:SerLen)+(1*Bin)]
  if(Lines){#If you want to graph the final threshold
    return(methReturn)
  }
  return(BestVector)
}

IdentifyOutliers <- function(Vector, method="SD", align="center",
                             n = 5,Bin = 21, Action = "Flag", Gap=2.5){
  MethodOptions <- c("SD","IQR","Median")
  if(method %in% MethodOptions){
    OutputVec <- OutlierDetectRobustFunc(Vector, MeanSDFunc, Gap=Gap,
                                         align = align,n = n, Bin = Bin)
    if(method == "SD"){#Req default for Gap is 2.5
      method <- MeanSDFunc
    }else if(method == "IQR"){#Req default for Gap is 1.5
      method <- IQRFunc
    }else if(method == "Median"){#Req default for Gap is 4
      method <- MedianFunc
    }
    OutputVec <- OutlierDetectRobustFunc(Vector, MedianFunc, Gap=Gap,
                                         align = align,n = n, Bin = Bin)
  }
  OutputVec <- OutlierDetectRobustFunc(Vector, method, Gap=Gap,
                                        align = align,n = n, Bin = Bin)
  if(Action == "Replace"){
    return(OutputVec)
  }else if(Action == "Flag"){
    booleanReturn <- OutputVec != Vector
    booleanReturn[is.na(booleanReturn)] <- FALSE
    return(booleanReturn)
  }
}
  