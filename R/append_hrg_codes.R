
#' Append HRG codes \lifecycle{maturing}
#'
#' Append HRG codes to HES data for years 2002/03 to 2013/14
#'
#' For years 0203 to 1314, we need to read-in the HRG codes for costing. These codes were provided separately by NHS Digital using the same
#' encrypted HES ID as the original data. We need to append the HRG fields using the HES ID to match. Note: these fields were provided
#' separately was just a querk of how we obtained the data, normally they come along with the HES data as they do for our data after 1314.
#' This is the last step in the cleaning process of the HES data, therefore the function saves the cleaned HES data to the virtual machine
#' to be used, instead of having to rerun the cleaning process.
#'
#'
#' @param hes is the cleaned HES data.
#' @param k.year.ind is the year of data
#' 
#' @importFrom data.table fread setnames := 
#'
#' @return Returns an updated version of the HES data, adding in the HRG codes for year pre 2014
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' hes_2002 <- append_hrg_codes(hes, "0203")
#'
#' }
#'
append_hrg_codes <- function(
  hes,
  k.year.ind
) {

  if (k.year.ind %in% c(
    "0203",
    "0304",
    "0405",
    "0506",
    "0607",
    "0708",
    "0809",
    "0910",
    "1011",
    "1112",
    "1213",
    "1314"
  )) {
    cat("\tappending HRG codes\n")

  # Load in all data for a particular year
  hrg <- fread(paste0("D:/HES/HES_HRGcodes_toAppend/apc_", k.year.ind, ".txt"),
               header = TRUE, sep = "|", quote = "", showProgress = T, data.table = T, na.strings = c("NA", "N/A", "", "-", "null", "UZ01Z"))

  setnames(hrg, "EPIKEY", "epikey")

  # merge datasets
  hes <- merge(hes, hrg,
               by = c("encrypted_hesid", "epikey"),
               all.x = T, all.y = F, sort = F)

  rm(hrg)
  gc()
  }

  return(hes[])
}

