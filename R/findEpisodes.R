
#' Search for episodes with the target International Classification of Disease diagnoses
#' 
#' Look at specified diagnostic positions of episodes
#' and create a new column in the dataset that picks out the selected episodes.
#' 
#' Diagnosis codes are identified by the function [hesr::icdFlag()], 
#' which is called by this function. This function loops through the values of 
#' col_names_vec applying [hesr::icdFlag()].
#' 
#' Uses the function [tobalcepi::ExpandCodes()] to convert the disease code lookup tables 
#' within the tobalcepi package into long form, ready for use in the matching process.  
#'
#' @param data Data table - episode-level hospital episode statistics data.
#' @param lkups Data table - the International Classification of Disease 
#' codes associated with tobacco and/or alcohol related conditions.
#' @param col_name_vec Character vector - the column names to be scanned for matching disease diagnosis codes.
#' @param external_cause_names Character vector - the names of partially attributable acute conditions 
#' that will be used to identify whether the episode was assigned an external cause.
#'
#' @return Returns the input episode-level hospital episode statistics data 
#' with a new column added for each value of col_name_vec
#' containing the name of the condition found during the scan. 
#' 
#' @importFrom data.table :=
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' 
#' }
#' 
findEpisodes <- function(
    data,
    lkups,
    col_name_vec,
    external_cause_names
) {
  
  ####################################
  # Set up ICD code lookups
  
  lkups_long <- tobalcepi::ExpandCodes(lkups)
  
  setnames(lkups_long, "icd10_lookups", "icd_code")
  
  lkups_long[ , icd_flag := 1]
  
  ###################################
  # loop through inputs
  # adding a new column to the dataset each time
  
  for(cn in 1:length(col_name_vec)) {
    
    data <- hesr::icdFlag(
      data = data,
      lkups = copy(lkups_long),
      col_name = col_name_vec[cn])
    
  }
  
  rm(lkups_long, cn)
  
  ###################################
  # Create the external cause column
  
  # Note that for Scotland the data field "admission_reason" has options to specify assault and self harm as causes
  # but none of the episodes in the data supplied had these codes
  # and so the assumption is that all the necessary information is contained in the ICD diagnosis code columns
  
  data[ , external_cause := NA_character_]
  
  for(cn in 1:length(col_name_vec)) {
    
    data[is.na(external_cause) & get(paste0(col_name_vec[cn], "_icdFlag")) %in% external_cause_names, 
         external_cause := get(paste0(col_name_vec[cn], "_icdFlag"))]
    
  }
  
  return(data)
}
