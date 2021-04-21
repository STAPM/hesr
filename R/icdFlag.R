
#' Scan for ICD-10 codes \lifecycle{stable}
#' 
#' Scans specified diagnosis code columns for 
#' ICD-10 codes in our tobacco and/or alcohol disease lists.
#' 
#' Uses the \code{tobalcepi::ExpandCodes()} function to convert the ICD-10 lookup tables 
#' within the tobalcepi package into long form. 
#' The scans the first 3 or 4 characters of the column name (specified by nchars) 
#' for the target ICD-10 code. If a match is found, then it is stored in a new variable.  
#'
#' @param data Data.table - the HES data.
#' @param lkups Data.table - the ICD-10 codes associated with tobacco and/or alcohol. 
#' These are stored as data within the tobalcepi package.
#' @param col_name Character string - The name of the diagnosis column that 
#' should be scanned to identify target ICD-10 codes.
#' @param nchars Integer - the number of characters of the ICD-10 code 
#' to scan for a match. This should either be 3 or 4 characters.  
#'
#' @return Returns the HES data.table with a column added 
#' containing the ICD-10 code found during the scan.
#' 
#' @importFrom data.table setnames :=
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' hes <- icdFlag(
#'   data = hes,
#'   lkups = tobalcepi::tob_icd10_lookups,
#'   col_name = "icd_code",
#'   nchars = 3
#' ) 
#' 
#' }
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
  
  #lkups_long[ , condition := NULL]
  lkups_long[ , icd_flag := 1]
  
  # Get the reference ICD-10 codes from the HES data
  data[, icd_code := substr(get(col_name), 1, nchars)]
  
  # Merge the lookup data with the HES data
  data <- merge(data, lkups_long, by = "icd_code", all.x = T, all.y = F, sort = F)
  
  # store the icd10 codes that matched
  cn_store <- paste0(col_name, nchars, "icd")
  
  data[icd_flag == 1, (cn_store) := condition]
  
  data[ , icd_flag := NULL]
  data[ , condition := NULL]
  
  
  return(data)
}