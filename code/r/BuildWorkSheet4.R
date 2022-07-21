#' Convert wastewater_data data to workset4 shape
#' 
#' This takes the wastewater_data dataframe and rename variables, 
#' calculates sars_cov2_adj_load_log10 column, 
#' and filters rows where average_flow_rate is NA
#' 
#'
#' @param df data frame object from data/wastewater_data.rda 
#'
#' @return data frame
#' @export
#'
#' @examples
#' data(wastewater_data, package = "DSIWastewater")
#' buildWorkSheet4(wastewater_data)
buildWorkSheet4 <- function(df){
  ## format data as DHS code expects
  df <- df %>% 
    select(
      wwtp_name,sample_collect_date,population_served,  ## site data
      n1_sars_cov2_conc, n2_sars_cov2_conc,             ## N1, N2 measurement
      average_flow_rate                                 ## sample covariates
    ) %>% 
    rename(WWTP = wwtp_name, date = sample_collect_date) %>% 
    mutate(date = as.Date(date,format="%m/%d/%Y"))
  
  ### Note: Replacement small values with LOD/2 (as per 5/20/2022 discussion w/DHS)
  ##   in bin/cleanData.pl
  
  ## dependent regression variable: log of normalized average SARS-COV-2 level
  workset4 <- df %>% 
    filter(average_flow_rate != "NA") %>% 
    mutate (geoMean = sqrt(n1_sars_cov2_conc*n2_sars_cov2_conc)) %>% 
    mutate(sars_cov2_adj_load_log10 = log10(
      geoMean*average_flow_rate/population_served    
    )
    )
  
  
  ## filter out sites with too few measurements
  ##  and sort by date;
  workset4 <- workset4 %>% 
    group_by(WWTP) %>% 
    mutate(n = n()) %>% 
    arrange(date, .by_group = TRUE) %>% 
    ungroup()
  return(workset4)
}