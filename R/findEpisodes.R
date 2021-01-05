
#' Assign a diagnosis to each episode \lifecycle{maturing}
#' 
#' Look at the first diagnostic position of all episodes within a continuous inpatient spell
#'  and retain episodes that have a 
#' relevant diagnosis in the primary diagnostic position. 
#' Also look for whether a relevant external cause 
#' has been recorded within the list of diagnoses related to that episode.
#' 
#' Diagnosis codes are identified by the function \code{icdFlag()} and 
#' are then synthesised to produce a single diagnosis for each episode 
#' (external causes where two potential diagnoses are identified). The HES 
#' data is then filtered to retain only episodes for which a tobacco or alcohol related 
#' diagnosis has been identified. 
#'
#' @param data Data.table - the HES data.
#' @param lkups Data.table - the ICD-10 codes associated with tobacco and/or alcohol. 
#' These are stored as data within the tobalcepi package.
#'
#' @return Returns the HES data.table with a column added 
#' containing the ICD-10 code found during the scan.
#' 
#' @importFrom data.table :=
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' hes <- findEpisodes(
#'   data = hes,
#'   lkups = tobalcepi::tob_icd10_lookups
#' ) 
#' 
#' }
#' 
findEpisodes <- function(
  data,
  lkups
) {
  
  # Search for episodes with the target ICD-10 diagnoses
  
  # External causes - 3 character ICD-10 match
  data <- hesr::icdFlag(
    data = data,
    lkups = lkups,
    col_name = "cause",
    nchars = 3
  )
  
  # External causes - 4 character ICD-10 match
  data <- hesr::icdFlag(
    data = data,
    lkups = lkups,
    col_name = "cause",
    nchars = 4
  )
  
  # 1st diagnostic position - 3 chars
  
  # note that the tob ICD-10 lookups just have C15
  # so all Oesophageal cancers are being scanned for
  # and splitting into Oesophageal cancer subtypes will need to happen later
  
  data <- hesr::icdFlag(
    data = data,
    lkups = lkups,
    col_name = "diag_01",
    nchars = 3
  )
  
  # 1st diagnostic position - 4 chars
  data <- hesr::icdFlag(
    data = data,
    lkups = lkups,
    col_name = "diag_01",
    nchars = 4
  )
  
  # Look across the identified diagnoses and create 1 column 
  # with the diagnosis to be assigned to the episode
  data[!is.na(diag_014icd), epi_diag_selected := diag_014icd]
  data[!is.na(diag_013icd), epi_diag_selected := diag_013icd]
  data[!is.na(cause4icd), epi_diag_selected := cause4icd]
  data[!is.na(cause3icd), epi_diag_selected := cause3icd]
  
  # Retain only episodes that matched a diagnosis code
  data <- data[!is.na(epi_diag_selected)]
  
  # Remove columns not needed
  data[, cause3icd := NULL]
  data[, cause4icd := NULL]
  data[, diag_013icd := NULL]
  data[, diag_014icd := NULL]
  
  
  return(data)
}
