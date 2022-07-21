#' compute first difference Jumps for N1 and N2
#'
#' @param df DataFrame. needs Column n1_sars_cov2_conc, n2_sars_cov2_conc, WWTP
#'
#' @return dataframe with 4 columns appended: delta(n1), delta(n2) from left and right
#' @export
#'
#' @examples
#' data(example_data, package = "DSIWastewater")
#' computeJumps(example_data)
computeJumps <- function(df) {
  df <- df %>% 
    group_by(WWTP) %>% 
    mutate(
      n1.before = lag(n1_sars_cov2_conc, order_by = WWTP),
      n1.after  = lead(n1_sars_cov2_conc, order_by = WWTP),
      n2.before = lag(n2_sars_cov2_conc, order_by = WWTP),
      n2.after  = lead(n2_sars_cov2_conc, order_by = WWTP)
    ) %>% 
    mutate(
      n1.jumpFromLeft  = n1_sars_cov2_conc - n1.before,
      n1.jumpFromRight = n1_sars_cov2_conc - n1.after,
      n2.jumpFromLeft  = n2_sars_cov2_conc - n2.before,
      n2.jumpFromRight = n2_sars_cov2_conc - n2.after
    ) %>% 
    select(-c(n1.before,n1.after,n2.before,n2.after))
  return(df)
}

#' rankJumps
#' 
#' Convert jumps from last step into a ordering
#'
#' @param df DataFrame. needs Column n1.jumpFromLeft, n1.jumpFromRight, 
#'           n2.jumpFromLeft, n2.jumpFromRight, WWTP
#'           
#' First 4 gen from computeJumps
#' 
#' @return dataframe with 4 columns appended: ranks of each of the 4 jumps;
#' @export
#'
#' @examples
#' data(example_data, package = "DSIWastewater")
#' df_data <- computeJumps(example_data)
#' rankJumps(df_data)
rankJumps <- function(df) {
  df <- df %>% 
    group_by(WWTP)   %>% 
    mutate(rank.n1.jumpFromLeft = rank(-n1.jumpFromLeft),
      rank.n1.jumpFromRight = rank(-n1.jumpFromRight),
      rank.n2.jumpFromLeft = rank(-n2.jumpFromLeft), 
      rank.n2.jumpFromRight = rank(-n2.jumpFromRight),
      MessureRank = pmin(rank.n1.jumpFromLeft, rank.n1.jumpFromRight, rank.n2.jumpFromLeft, rank.n2.jumpFromRight)
      ) %>% 

    ## sort by first jump ranks just to be definitive
    arrange(WWTP,rank.n1.jumpFromLeft) 
  return(df)
}

#' computeRankQuantiles
#' 
#' Convert jumps from last step into a ordering quintile 
#'
#' @param df dataframe. needs Column n1.jumpFromLeft, n1.jumpFromRight, 
#'           n2.jumpFromLeft, n2.jumpFromRight, WWTP
#'           
#' First 4 gen from computeJumps
#' 
#' @return dataframe with 4 columns appended: ranks of each of the 4 jumps;
#' @export
#'
#' @examples
#' data(example_data, package = "DSIWastewater")

#' df_data <- computeJumps(example_data)
#' ranked_data <- rankJumps(df_data)
#' computeRankQuantiles(ranked_data)
computeRankQuantiles <- function(df) {
  df <- df %>% 
    group_by(WWTP) %>% 
    mutate(numValues = n()) %>% 
    mutate(
      n1.jumpFromLeft.quantile  = rank.n1.jumpFromLeft/numValues,
      n1.jumpFromRight.quantile = rank.n1.jumpFromRight/numValues,

      n2.jumpFromLeft.quantile  = rank.n2.jumpFromLeft/numValues,
      n2.jumpFromRight.quantile = rank.n2.jumpFromRight/numValues,
      MessureRank.quantile = pmin(n1.jumpFromLeft.quantile, n1.jumpFromRight.quantile, n2.jumpFromLeft.quantile, n2.jumpFromRight.quantile)
    ) %>%
    select(-numValues) %>%
    
    ## sort by first jump ranks just to be definitive
    arrange(WWTP,n1.jumpFromLeft.quantile)   
}

#' Create column with Boolean based on a threashold
#'
#' @param DF Dataframe containing Column column
#' @param threshold a numeric used to flag if its an outlier
#' @param col column being flagged based on threshold 
#' @param outputColName name of flag column
#'
#' @return DF Dataframe with the extra column of if its flagged an outlier
#' @export
#'
#' @examples
#' data(example_data, package = "DSIWastewater")
#' df_data <- computeJumps(example_data)
#' ranked_data <- rankJumps(df_data)
#' ranked_quantile_data <- computeRankQuantiles(ranked_data)
#' flagOutliers(ranked_quantile_data, 9)
flagOutliers <- function(DF, threshold, col = MessureRank, outputColName = FlaggedOutlier){
  RetDF <- DF%>%
    mutate({{outputColName}} := {{col}} < threshold)
  return(RetDF)
}


#' Add column with NA values where the data was flagged
#'
#' @param DF DF containing the columns Measure and Filtcol
#' @param Messure The original measurement we want to keep inliers for
#' @param Filtcol the column containing the Boolean info needed to remove outliers
#' @param outputColName the name for the clean column
#'
#' @return DF with new column without the flagged values
#' @export
#'
#' @examples
#' data(example_data, package = "DSIWastewater")
#' df_data <- computeJumps(example_data)
#' ranked_data <- rankJumps(df_data)
#' ranked_quantile_data <- computeRankQuantiles(ranked_data)
#' classied_data <- flagOutliers(ranked_quantile_data, 9)
#' RemoveOutliers(classied_data)
RemoveOutliers <- function(DF, Messure = sars_cov2_adj_load_log10, Filtcol = FlaggedOutlier, outputColName = sars_adj_log10_Filtered){
  RetDF <- DF%>%
    mutate({{outputColName}} := ifelse({{Filtcol}}, NA, {{Messure}}))
  return(RetDF)
}

