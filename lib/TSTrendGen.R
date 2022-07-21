#' Get a fitting parameter for the model
#'
#' @param DF The DF we are about to fit a smoothing curve on. should only
#' be from one TS
#' @param InVar The col name that the variable is about to smooth
#' 
#' @param Base The value we are scaling by the inverse of the number of
#' rows in the data
#' @param max The largest value returned
#'
#' @return a number between 0 and max
ParameterGuess <- function(DF,InVar, Base, max){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  span <- min(c(Base/temp$n, max))#More can be done here
  return(span)
}


#' LoessSmoothMod
#' Add a column of the smoothed values using Loess
#'
#' @param DF DF we are adding the loess smooth col to
#' @param InVar The column to be smoothed
#' @param OutVar The name of the new column
#' @param span The span fed into loess smoothing. if it equals "guess" then it
#' if found using ParameterGuess 
#' @param Filter Prefilter using the value of a Filter col
#'
#' @return A DF with an extra col with a loesss smoothed version of InVar
#' @export
#'
#' @examples
#' data(example_data, package = "DSIWastewater")
#' LoessSmoothMod(example_data)
LoessSmoothMod <- function(DF,InVar="sars_cov2_adj_load_log10",
                           OutVar="Loess", span="guess", Filter = NULL){
  if(span=="guess"){
    span <- ParameterGuess(DF,InVar, 17.8, .6)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!(!!sym(Filter)))
  }
  
  DF <- DF%>%
    arrange(date)
  
  DF[[OutVar]] <- loessFit(y=(DF[[InVar]]), 
                            x=DF$date, #create loess fit of the data
                            span=span, 
                            iterations=2)$fitted#2 iterations remove some bad patterns
  
  if(!is.null(Filter)){
    DF <- DF%>%
      bind_rows(OutDF)
  }
  
  return(DF)
}



#' ExpSmoothMod
#' Add a column of the smoothed values using exponential smoothing
#'
#' @param DF The DF we are to add a exponential smoothing column to
#' @param InVar The column to be smoothed
#' @param OutVar The name of the new column
#' @param alpha The alpha fed into robets exponential smoothing. if it equals "guess" then it
#' if found using ParameterGuess
#' @param beta  The beta fed into robets exponential smoothing. if it equals "guess" then it
#' if found using ParameterGuess
#' 
#' @param Filter Prefilter using the value of a Filter col
#'
#' @return A DF with an extra col with a exp smoothed version of InVar
ExpSmoothMod <- function(DF,InVar, OutVar,alpha="guess",beta="guess", Filter = NULL ){
  
  if(alpha=="guess"){
    alpha <- ParameterGuess(DF,InVar, 35.6, .4)
  }
  if(beta=="guess"){
    beta <- ParameterGuess(DF,InVar, 8.9, .4)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)) | is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!(!!sym(Filter))  & !is.na(!!sym(InVar)))
  }else{
    OutDF <- DF%>%
      filter(is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!is.na(!!sym(InVar)))
  }
  
  DF <- DF%>%
    arrange(date)
  
  DF[[OutVar]] <- robets::robets(y=DF[[InVar]],
                          model = "AAN",
                          beta  = beta,
                          alpha=alpha)$fitted#2 iterations remove some bad patterns
  DF <- DF%>%
    bind_rows(OutDF)
  
  return(DF)
}


#' NGuess for sgolaySmoothMod number of points per polynomial
#'
#' @param DF The DF to fit the sgolayfilt curve on. should only
#' be from one TS
#' @param InVar The col name that the variable is about to smooth
#' @param Base The value we are scaling by the of the number of
#' rows in the data
#' @param min The smallest value returned
#'
#' @return a number greater or equal to min
NGuess <- function(DF,InVar, Base, min){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  N <- max(c(floor(Base*temp$n), min))#More can be done here
  return(N + 1 - N%%2)
}


#' sgolaySmoothMod
#' Add a column of the smoothed values using sgolayfilt
#' @param DF dataframe containing the columns specified below
#' @param InVar The column to be smoothed
#' @param OutVar The name of the new column
#' @param poly The degree of the polynomial fit
#' @param n The number of points per polynomial fed into sgolayfilt. 
#' if it equals "guess" then it is found using ParameterGuess
#' @param Filter Prefilter using the value of a Filter col
#'
#' @return DF with an extra col with a sgolayfilt smoothed version of InVar
sgolaySmoothMod <- function(DF,InVar, OutVar,poly=5,n="guess", Filter = NULL){
  if(n=="guess"){
    n <- NGuess(DF,InVar,50/178, 7)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)) | is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    SmthDF <- DF%>%
      filter(!(!!sym(Filter)) & !is.na(!!sym(InVar)))
  }else{
    OutDF <- DF%>%
      filter(is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    SmthDF <- DF%>%
      filter(!is.na(!!sym(InVar)))
  }
  
  SmthDF <- SmthDF%>%
    arrange(date)

  SmthDF[[OutVar]] <- SmthDF%>%
    pull(!!InVar)%>%
    signal::sgolayfilt(p  = poly, n = n)
  
  RetDF <- SmthDF%>%
    bind_rows(OutDF)%>%
    arrange(date)
  
  return(RetDF)
}