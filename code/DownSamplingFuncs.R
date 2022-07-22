ParameterGuess <- function(DF,InVar, Base, max){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  span <- min(c(Base/temp$n, max))#More can be done here
  return(span)
}

ExpSmoothMod <- function(DF,InVar="sars_cov2_adj_load_log10",
                         OutVar="EXP", alpha="guess", beta="guess",
                         Filter = NULL ){
  
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
                                 alpha=alpha,
                                 k = 3)$fitted
  DF <- DF%>%
    bind_rows(OutDF)
  
  return(DF)
}


##################


DownSampleDF <-  function(DF, dayOfWeekVec){
  RetDF <- DF%>%
    filter(wday(date) %in% dayOfWeekVec)
  return(RetDF)
}

PrepDataSmoothings <- function(DF, filterVec = NA){
  if(typeof(filterVec) != "logical"){
    RetDF <- DF%>%
      DownSampleDF(filterVec)%>%
      mutate(data = length(filterVec),
             TrueName = paste0(filterVec, collapse = ""))
    Alpha = 0.07688985 * 6 / length(filterVec)
    Beta =  0.01922246 * 6 / length(filterVec)
  }else{
    RetDF <- DF
    Alpha = 0.07688985
    Beta =  0.01922246
  }
  RetDF <- RetDF%>%
    LoessSmoothMod()%>%
    ExpSmoothMod(alpha = Alpha,
                 beta = Beta)%>%
    arrange(date)%>%
    mutate(SevSmooth = rollmean(sars_cov2_adj_load_log10,
                                k=7, fill=NA, align = "right"))
}



prepDataForMessure <- function(DF, BreakOn = "WWTP", dataBase = 6){
  Cat.map <- c("major increase" = 2, "moderate increase" = 1, 
               "fluctuating" = 0, "no change" = 0,
               "moderate decrease" = -1, "major decrease" = -2)
  
  num_reg_estimates_data <- DF%>%
    select(!!sym(BreakOn), date, Method, Catagory)%>%
    mutate(Catagory = Cat.map[Catagory])
  
  RetDF <- num_reg_estimates_data%>%
    filter(Method == "Loess", data == dataBase)%>%
    select(date, Loess = Catagory)%>%
    full_join(num_reg_estimates_data)%>%
    select(!!sym(BreakOn),date,Method,Catagory,Loess)
  return(RetDF)
}