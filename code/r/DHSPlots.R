#' Create representative plot of the DHS analysis
#' 
#' createDHSMethod_Plot uses RegDF to create the top set of plots showing the 
#' predictions at each time period. 
#' It uses BaseDF to create the lower plot that shows what the regressed data
#' looks like
#'
#' @param RegDF data frame containing the regression analysis
#' @param BaseDF The data frame containing the raw data
#' @param FacGridFormula The formula we wish to facet the heat maps with
#' @param PointVal The point columns we want to plot
#' @param LineVal The Line columns we want to plot
#' @param ncol The number of plots in each row
#'
#' @return a ggplot of the heat map of each method and the underlying data
#' @export
#'
#' @examples
#' 
#' data(example_data, package = "DSIWastewater")
#' example_reg_table <- buildRegressionEstimateTable(example_data)
#' createDHSMethod_Plot(example_reg_table, example_data)
createDHSMethod_Plot <- function(RegDF,BaseDF, 
                             FacGridFormula = Method ~ WWTP,
                             PointVal = "sars_cov2_adj_load_log10", 
                             LineVal = NULL, 
                             ncol = 3
                             ){
  Xbreak <- as.character(FacGridFormula)[3]
  CatagoryColors <- c("major decrease" = "#0571b0", "moderate decrease" = "#92c5de",
                      "fluctuating" = "#979797", "no change" = "WHITE", 
                      "moderate increase" = "#f4a582", "major increase" = "#ca0020")
  BarGridSmoothRaw <- RegDF%>%
    
    split(.,.[[Xbreak]])%>%
    
    lapply(CreateHeatMaps_Plot, FacGridFormula, "Catagory", CatagoryColors,
           ToMerge=TRUE)
  
  
  
  
  Gplt <- BaseDF%>%
    
    mutate(Data = "Data")%>%
    
    filter(!!sym(Xbreak) %in% unique(RegDF[[Xbreak]]))%>%
    
    split(.,.[[Xbreak]])%>%
    
    lapply(createWasteGraph_Plot, 
           "date", 
           PointVal = PointVal, 
           LineVal = LineVal,
           ToMerge = TRUE)
  
  
  methodsUsed <- length(uniqueVal(as.character(FacGridFormula)[2], RegDF))
  if(length(Gplt)!=1){
    SavePlot <- orderAndZipListsOfPlots_Plot(BarGridSmoothRaw,Gplt,
                                             ratA = methodsUsed, ncol = ncol)
  }else{
    SavePlot <- BarGridSmoothRaw[[1]]/Gplt[[1]] + plot_layout(heights = c(methodsUsed, 1))
  }
  return(SavePlot)
}

#' Take two list of plots and combine them into one 3 col long plot
#'
#' @param top_plot_list Lists of plots that get added on top, generically the 
#' rectangle plot of method prediction. Needs to be the same length as bot_plot_list
#' @param bot_plot_list  List of plots to be combined on the bottom 
#' @param ratA The proportion the top plot should be.
#' @param ratB The proportion the bot plot should be.
#' @param ncol Where the plot should be faceted
#'
#' @return a ggplot
orderAndZipListsOfPlots_Plot <- function(top_plot_list, bot_plot_list, ratA=3,
                                         ratB=1, ncol = 3){
  stopifnot(length(top_plot_list) == length(bot_plot_list))
  RetList <- list()
  
  ele_list_length <- length(top_plot_list)
  for(i in 1:ele_list_length){
    a <- top_plot_list[[i]]
    b <- bot_plot_list[[i]]
    if(i %% ncol != 1){
      b <- b + theme(axis.title.y = element_blank())
    }
    if(i%%ncol != 0 && i != ele_list_length){
      a <- a + theme(strip.background.y = element_blank(),
                     strip.text.y = element_blank())
      b <- b + theme(strip.background.y = element_blank(),
                     strip.text.y = element_blank())
    }
    RetList[[i]] <- a/b + plot_layout(heights = c(ratA, ratB))
  }
  EffectiveNcol = min(ncol, length(top_plot_list))
  retPlot <- wrap_plots(RetList)+plot_layout(guide="collect", ncol = EffectiveNcol)
  return(retPlot)
}



#' CreateHeatMaps_Plot
#' 
#' Creates graphic of model prediction for each method
#'
#' @param DF The DF used to create the Heatmap
#' @param FacGridFormula how we wan to facet the heatmap
#' @param FillFac the name of the catagory method
#' @param CatagoryColors The color scheme used
#' @param ToMerge if true we remove the lower labels
#'
#' @return faceted ggplot heatmap
CreateHeatMaps_Plot <- function(DF, FacGridFormula, FillFac, CatagoryColors, ToMerge = FALSE){#, 
  BarGridSmoothRaw <- DF%>%
    ggplot()+
    geom_rect(aes(xmin=date-days_elapsed/2,xmax=date+days_elapsed/2,
                  ymin=0,
                  ymax = 10,fill = !!sym(FillFac)),
              na.rm=TRUE)+
    facet_grid(FacGridFormula)+
    scale_fill_manual(values = CatagoryColors)
  if(ToMerge){
    BarGridSmoothRaw <- BarGridSmoothRaw+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  return(BarGridSmoothRaw)
}

#' adds a ggplot component
#'
#' @param GGObj a ggplot object we are adding to
#' @param GGfunc what gg type object used
#' @param YcolorName name of the color, either a factor or a string
#' @param YVal name of the y variable used
#'
#' @return GGObj with the appended graphic
Abstract_PlotAdd <- function(GGObj, GGfunc, YVal, YcolorName = NULL){
  
  if(is.null(YcolorName)){
    YcolorName <- YVal
  }else{
    YcolorName <- sym(YcolorName)
  }
  
  
  RetObj <- GGObj+
    GGfunc(aes(y = !!sym(YVal), color = !!YcolorName), na.rm = TRUE)
  return(RetObj)
}


#' Wastewater graphic
#'
#' @param DF DF containing wastewater measurements specified in the remaining params
#' @param xVal name of x variable, normally close to "Date"
#' @param ToMerge remove facet info if true. be careful that the to plots have same ordering
#' @param PointVal the discrete measurements
#' @param LineVal the continuous measurements
#'
#' @return a ggplot object with points with lables for each PointVal and a lines for each LineVal
createWasteGraph_Plot <- function(DF, xVal, PointVal = NULL, LineVal = NULL, ToMerge = FALSE){
  RetPlot <- DF%>%
    ggplot( aes(x = !!sym(xVal)))
  
  if(!is.null(PointVal)){
    for (ele in PointVal) {
      RetPlot <- RetPlot%>%
        Abstract_PlotAdd(geom_point, ele)
    }
  }
  
  if(!is.null(LineVal)){
    for (ele in LineVal) {
      RetPlot <- RetPlot%>%
        Abstract_PlotAdd(geom_line, ele)
    }
  }
  
  RetPlot <- RetPlot+
    facet_grid(Data~WWTP)+
    scale_x_date(date_labels = "%b %y")
  
  if(ToMerge){
    RetPlot <- RetPlot+
      theme(
        strip.background.x = element_blank(),
        strip.text.x = element_blank()
      )
  }
  return(RetPlot)
}


#' createConfMatrix_Plot
#' 
#' creates a confusion matrix from data long format
#'
#' @param DF data frame containing results of DHS analysis
#' @param Cat The column with the values of the methods 
#' @param x The first method to compare
#' @param y The second method to compare
#' @return a ggplot object of the confusion matrix
createConfMatrix_Plot <- function(DF,x,y, Cat="Catagory"){
  RetPlt <- DF%>%
    filter(Method %in% c(x,y))%>%
    select(WWTP,date,Method,Catagory)%>%
    filter(WWTP != "Portage WWTF"  & WWTP != "Cedarburg WWTF")%>%
    pivot_wider(id_cols=c(WWTP, date),names_from = Method, values_from = !!sym(Cat))%>%
    group_by(!!sym(x),!!sym(y))%>%
    summarise(n = n())%>%
    filter(!is.na(!!sym(y)), !is.na(!!sym(x)))%>%
    ggplot(aes(x=!!sym(x),y=!!sym(y)))+
    geom_tile(aes(fill = n), na.rm=TRUE)+
    scale_fill_gradient(low="blue", high="red")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(RetPlt)
}



#' createMethodCompareBar_Plot: Compare Regression analysis methods
#'
#' @param DF data frame containing all the analysis for every method
#' @param Method factor column for each type of regression done
#' @param Cat Category column that the regression analysis is store
#'
#' @return ggplot object
createMethodCompareBar_Plot <- function(DF,Method = "Method",Cat="Catagory"){
  DF%>%
    group_by(!!sym(Method),!!sym(Cat))%>%
    summarise(n = n())%>%
    ggplot(aes(x=!!sym(Cat),y=n))+
    geom_col(aes(fill=!!sym(Method)),position = "dodge", na.rm=TRUE)+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
