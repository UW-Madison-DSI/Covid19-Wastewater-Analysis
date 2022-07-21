#returns vector of the min and max of function
MinMaxCollector <- function(Vec){
  if(all(is.na(Vec))){
    return(NA)
  }
  return(range(Vec, na.rm = TRUE))
}

#normalizes data to be between 0 and 1. 
#Not used significantly in newest build
MinMaxNormalization <- function(Vec,minMax=NA){#normalizes the data to range from 0 and 1
  if(all(!is.na(minMax))){
    normVec <- (Vec-minMax[1])/minMax[2]
  }else{
    normVec <- (Vec-min(Vec,na.rm=TRUE))/max(Vec,na.rm=TRUE)
  }
  return(normVec)
}

#Makes First Vector have the same min and max as the second one.
MinMaxFixing <- function(Vec, Bar, SubVec=NA){
  NormVec = MinMaxNormalization(Vec,MinMaxCollector(SubVec))
  BarRange = MinMaxCollector(Bar)
  RefitVec = NormVec*(BarRange[2]-BarRange[1])+BarRange[1]
  return(RefitVec)
}

NoNa <- function(DF,...){#Removes NA from the reverent columns
  ColumnNames <- c(...)
  NoNaDF <- DF%>%
    filter(
      across(
        .cols = ColumnNames,
        .fns = ~ !is.na(.x))
    )
  return(NoNaDF)
}

FillNA <- function(DF,...){#Fills NA with previous values
  ColumnNames <- c(...)
  NoNaDF <- DF%>%
    fill(ColumnNames)
  return(NoNaDF)
}

MedianMean <- function(Vec){# take the mean drooping the highest and lowest values
  RemoveBorderPoints <- replace(Vec, c(which.min(Vec), which.max(Vec)), NA)
  MeanValue <- mean(RemoveBorderPoints, na.rm = TRUE)
  return(MeanValue)
}