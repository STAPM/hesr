
#' Split Oesophageal cancer cases into AC and SCC subtypes
#'
#' This code splits the diagnosis codes for Oesophageal cancer (C15) 
#' in the hospital episode statistics data in to SCC or AC. 
#' 
#' The percentage splits are informed by estimates given to us by Cancer Research UK, 
#' which are formatted and embedded in the package as [hesr::cruk_splits].
#' 
#' Make sure there are no missing values in the startage field before running this.
#' 
#' Run on the episode-level data after running [hesr::findEpisodes()].
#'
#' @param data Data table - the cleaned hospital episode statistics data.
#' @param splits Data table - the distribution of C15 cases by SCC and AC
#' @param col_name_vec Character vector - The name of the diagnosis code columns
#' scanned to identify target disease diagnosis codes.
#' @param prefix Character string - "C15" or "Oesophageal"
#'
#' @importFrom data.table := rbindlist
#'
#' @return Returns an updated version of the input hospital episode statistics data, 
#' with the diagnoses codes for 
#' oesophageal cancer (C15) split proportionally between Oesophageal SCC and Oesophageal AC.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' }
#'
cruk_split_oesophageal <- function(
  data,
  splits = hesr::cruk_splits,
  col_name_vec = c("diag_01"),
  prefix = "Oesophageal"
) {
  
  # Check startage variable for missing values
  
  testthat::expect_equal(
    nrow(data[is.na(startage)]), 0,
    info = "There are NAs in the startage variable"
  )
  
  # create age bands to match those used by cruk
  data[, cruk_ageband := c(
    "00-04",
    "05-09",
    "10-14",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75-79",
    "80-84",
    "85+")[findInterval(startage, c(0, seq(5, 85, 5)))]]
  
  
  # Merge with cruk splits
  data <- merge(data, splits, by = c("cruk_ageband", "sex"), all.x = T, all.y = F)
  
  # Make the splits
  
  # loop through diagnosis code columns
  
  for(cn in 1:length(col_name_vec)) {
    
    # cn <- 1
    
    # generate a random number column
    
    set.seed(cn)
    
    data[ , ran_temp := runif(nrow(data))]
    
    data[get(paste0(col_name_vec[cn], "_icdFlag")) == "Oesophageal" & ran_temp <= proportion_scc, (paste0(col_name_vec[cn], "_icdFlag")) := "Oesophageal_SCC"]
    data[get(paste0(col_name_vec[cn], "_icdFlag")) == "Oesophageal" & ran_temp > proportion_scc, (paste0(col_name_vec[cn], "_icdFlag")) := "Oesophageal_AC"]
    
  }
  
  data[ , ran_temp := NULL]
  data[ , cruk_ageband := NULL]
  data[ , proportion_scc := NULL]
  
  
  return(data)
}
