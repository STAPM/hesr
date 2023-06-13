
#' Scan episodes for matching International Classification of Disease codes
#' 
#' Scans specified diagnosis code columns for 
#' disease diagnosis codes that match the provided list of tobacco and/or alcohol related diseases.
#' 
#' The scans the first 3 or 4 characters of the column name
#' for the target disease diagnosis code. If a match is found, then it is stored in a new variable.  
#' 
#' This function is called by [hesr::findEpisodes()].  
#'
#' @param data Data table - the episode-level hospital episode statistics data.
#' @param lkups Data table - the disease diagnosis codes associated with tobacco and/or alcohol
#' related conditions.
#' @param col_name Character string - The name of the diagnosis column that 
#' should be scanned to identify target disease diagnosis codes.
#'
#' @return Returns the input episode-level hospital episode statistics data with a new column added 
#' containing the name of the tobacco and/or alcohol related condition found during the scan.
#' 
#' @importFrom data.table setnames :=
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' 
#' }
#' 
icdFlag <- function(
    data,
    lkups,
    col_name
) {
  
  # Create a new column name
  cn_store <- paste0(col_name, "_icdFlag")
  
  # Loop through the number of characters to scan
  
  nchars_vec <- c(3, 4)
  
  for(nchars in nchars_vec) {
    
    ##############################
    # Get the reference ICD codes from the episode-level data
    # This extracts the number of characters specified by nchars from the disease code
    data[, icd_code := substr(get(col_name), 1, nchars)]
    
    ##############################
    # Merge the lookup data with the HES data
    data <- merge(data, lkups, by = "icd_code", 
                  all.x = TRUE, all.y = FALSE, sort = FALSE)
    
    ##############################
    # Store the ICD codes that matched
    
    # Matching disease codes will have icd_flag = 1 rather than NA
    # where these matches occur, add the condition name (e.g. "Alcohol_poisoning") to the new column
    data[icd_flag == 1, (cn_store) := condition]
    
    # Delete the working columns added to the episode-level data for the purposes of matching
    data[ , icd_flag := NULL]
    data[ , icd_code := NULL]
    data[ , condition := NULL]
    
  }
  
  rm(cn_store, nchars_vec, nchars)
  
  return(data)
}