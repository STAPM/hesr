
#' CRUK Oesophageal split \lifecycle{maturing}
#'
#' This code splits the diagnosis codes for Oesophageal cancer (C15) in the HES data in to SCC or AC using CRUK splits, which are formatted and kept
#' in the data-raw folder `prep_cruk_splits`. Make sure there are no NAs in the startage field before running this. I.e. clean HES data first. This function is used
#' within the function `narrow_method()`.
#'
#'
#'
#' @param hes is the cleaned HES data.
#'
#' @return Returns an updated version of the HES data, with the diagnoses codes for oesophageal cancer (C15) split proportionally between Oesophageal SCC and Oesophageal AC.
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
  hes
) {

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

  # separate out oesophagus morb

  oesophagus_morb <- hes[icd_code == "C15"]

  # merge with cruk splits
  oesophagus_morb <- merge(oesophagus_morb, hesr::cruk_splits, by = c("cruk_ageband", "sex"), all.x = T, all.y = F)

  oesophagus_morb[ , icd_code := as.character(sapply(proportion_scc, function(x) sample(c("C15s", "C15a"), size = 1, prob = c(x, 1-x))))]

  oesophagus_morb[ , proportion_scc := NULL]

  # strip C15 out of hes data
  hes <- hes[icd_code != "C15"]

  # join on the split oesophagus morb to include C15s and C15a
  hes <- rbindlist(list(hes, oesophagus_morb), use.names = T)
  hes[ , cruk_ageband := NULL]

  # Test
  # hes[icd_code == "C15a"]

  return(hes)

}
