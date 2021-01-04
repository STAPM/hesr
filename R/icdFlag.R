
#' Title
#'
#' @param data 
#' @param lkups 
#' @param col_name 
#' @param nchars 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
icdFlag <- function(
  data,
  lkups,
  col_name,
  nchars
) {
  
  # Get the ICD-10 lookup codes for tobacco
  lkups_long <- tobalcepi::ExpandCodes(lkups)
  
  setnames(lkups_long, "icd10_lookups", "icd_code")
  
  lkups_long[ , condition := NULL]
  lkups_long[ , icd_flag := 1]
  
  # Get the reference ICD-10 codes from the HES data
  data[, icd_code := substr(get(col_name), 1, nchars)]
  
  # Merge the lookup data with the HES data
  data <- merge(data, lkups_long, by = "icd_code", all.x = T, all.y = F, sort = F)
  
  # store the icd10 codes that matched
  cn_store <- paste0(col_name, nchar, "icd")
  
  data[icd_flag == 1, (cn_store) := icd_code]
  
  data[ , icd_flag := NULL]
  
  
  return(data)
}