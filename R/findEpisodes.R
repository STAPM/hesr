
#' Title
#'
#' @param data 
#' @param lkups 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
#' 
#' 
findEpisodes <- function(
  data,
  lkups
) {
  
  # Search for episodes with the target ICD-10 diagnoses
  
  # External causes - 3 character ICD-10 match
  hes <- hesr::icdFlag(
    data = hes,
    lkups = tobalcepi::tob_icd10_lookups,
    col_name = "cause",
    nchars = 3
  )
  
  # External causes - 4 character ICD-10 match
  hes <- hesr::icdFlag(
    data = hes,
    lkups = tobalcepi::tob_icd10_lookups,
    col_name = "cause",
    nchars = 4
  )
  
  # 1st diagnostic position - 3 chars
  
  # note that the tob ICD-10 lookups just have C15
  # so all Oesophageal cancers are being scanned for
  # and splitting into Oesophageal cancer subtypes will need to happen later
  
  hes <- hesr::icdFlag(
    data = hes,
    lkups = tobalcepi::tob_icd10_lookups,
    col_name = "diag_01",
    nchars = 3
  )
  
  # 1st diagnostic position - 4 chars
  hes <- hesr::icdFlag(
    data = hes,
    lkups = tobalcepi::tob_icd10_lookups,
    col_name = "diag_01",
    nchars = 4
  )
  
  # Look across the identified diagnoses and create 1 column 
  # with the diagnosis to be assigned to the episode
  hes[!is.na(diag_014icd), epi_diag_selected := diag_014icd]
  hes[!is.na(diag_013icd), epi_diag_selected := diag_013icd]
  hes[!is.na(cause4icd), epi_diag_selected := cause4icd]
  hes[!is.na(cause3icd), epi_diag_selected := cause3icd]
  
  # Retain only episodes that matched a diagnosis code
  hes <- hes[!is.na(hes$epi_diag_selected)]
  
  # Remove columns not needed
  hes[, cause3icd := NULL]
  hes[, cause4icd := NULL]
  hes[, diag_013icd := NULL]
  hes[, diag_014icd := NULL]
  
  
  return(hes)
}
