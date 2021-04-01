
#' CRUK Oesophageal split \lifecycle{stable}
#'
#' This code splits the diagnosis codes for Oesophageal cancer (C15) 
#' in the HES data in to SCC or AC using CRUK splits, which are formatted and kept
#' in the data-raw folder `prep_cruk_splits`. 
#' Make sure there are no NAs in the startage field before running this. I.e. clean HES data first. 
#'
#' @param hes Data.table - the cleaned HES data.
#' @param splits Data.table - the distribution of C15 cases by SCC and AC
#' @param var_name Character string - the name of the variable containing the C15 ICD-10 code to be split 
#' @param prefix Character string - "C15" or "Oesophageal"
#'
#' @importFrom data.table := rbindlist
#'
#' @return Returns an updated version of the HES data.table, with the diagnoses codes for 
#' oesophageal cancer (C15) split proportionally between Oesophageal SCC and Oesophageal AC.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' hes <- cruk_split_oesophageal(hes)
#'
#' }
#'
cruk_split_oesophageal <- function(
  hes,
  splits = hesr::cruk_splits,
  var_name = "epi_diag_selected",
  prefix = "Oesophageal"
) {
  
  # Check startage variable for NAs
  testthat::expect_identical(
    nrow(hes[is.na(startage)]), 0,
    info = "There are NAs in the startage variable"
  )
  
  # create age bands to match those used by cruk
  hes[, cruk_ageband := c(
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
  
  # Separate out oesophagus morb
  
  oesophagus_morb <- hes[get(var_name) == prefix]
  
  # Merge with cruk splits
  oesophagus_morb <- merge(oesophagus_morb, splits, by = c("cruk_ageband", "sex"), all.x = T, all.y = F)
  
  # Make the splits
  oesophagus_morb[ , (var_name) := as.character(
    sapply(proportion_scc, 
           
           function(x) {
             sample(paste0(prefix, c("_SCC", "_AC")), size = 1, prob = c(x, 1-x))
           }
           
    ))]
  
  oesophagus_morb[ , proportion_scc := NULL]
  
  # strip C15 out of hes data
  hes <- hes[get(var_name) != prefix]
  
  # Join on the split oesophagus morb to include C15s and C15a
  hes <- rbindlist(list(hes, oesophagus_morb), use.names = T)
  hes[ , cruk_ageband := NULL]
  
  # Test
  # hes[icd_code == "C15a"]
  
  
  return(hes[])
}
