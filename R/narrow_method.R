#' Narrow method for allocating risk \lifecycle{maturing}
#'
#'
#' This code assigns the appropriate age-sex-condition-specific AF to each risk-related diagnosis in the *primary* diagnostic position
#' (non-related diagnoses are left missing) across all episodes.
#'
#'
#' The narrow method selects episodes that have a risk-related ICD-10 code in the primary diagnostic position or
#' a risk-related external cause e.g. assault. This is in an effort to select the condition that is the cause of the episode.
#' Primarily, the external cause is assigned the cause of the episode, and if that is missing, the primary diagnostic position is assigned. This function is used
#' within th function `assign_risk()`.
#'
#' @param hes HES data
#' @param lkup cleaned and formatted attributable fraction lookup table.
#'
#'
#' @return Returns updated HES data, with only rows that have a primary risk-related diagnosis code
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' lkup <- format_afs("Tobacco")
#' narrow_method(hes, lkup)
#'
#' }
#'
#'
narrow_method <- function(
  hes,
  lkup
) {
  # create a new variable that has just the first 3 characters of the ICD-10 code in the primary diagnosis position
  hes[, icd_code := substr(diag_01, 1, 3)]

  hes <- cruk_split_oesophageal(hes)

  class(hes[, icd_code]) == class(lkup[, icd_code])
  class(hes[, sex]) == class(lkup[, sex])
  class(hes[, age_cat]) == class(lkup[, age_cat])
  class(hes[, imd_quintile]) == class(lkup[, imd_quintile])

  hes <- hes[!is.na(hes$imd_quintile), ]

  lkup[duplicated(lkup, by = c("age_cat", "sex", "imd_quintile", "icd_code"))]


  # merge the HES data with the data on attributable fractions (AFs in lkup)
  # the index variables for this match are 3 character ICD-10, sex, age-band and qimd
  hes <- merge(hes, lkup,
               by = c("icd_code", "sex", "age_cat", "imd_quintile"),
               all.x = T, all.y = F, sort = F)

  # Rename variables
  hes[, prm_3icd := icd_code]
  hes[, icd_code := NULL]

  hes[, prm_3af := AF]
  hes[, AF := NULL]

  hes[, prm_3desc := Description]
  hes[, Description := NULL]

  # repeat code above but now matching on 4 characters of the icd-10 code
  hes[, icd_code := substr(diag_01, 1, 4)]

  hes <- merge(hes, lkup,
               by = c("icd_code", "sex", "age_cat", "imd_quintile"),
               all.x = T, all.y = F, sort = F)

  hes[, prm_4icd := icd_code]
  hes[, icd_code := NULL]
  hes[, prm_4af := AF]
  hes[, AF := NULL]
  hes[, prm_4desc := Description]
  hes[, Description := NULL]

  # Take subsets
  narrow_af  <- hes[, c("prm_4af", "prm_3af")]
  narrow_icd  <- hes[, c("prm_4icd", "prm_3icd")]
  narrow_desc  <- hes[, c("prm_4desc", "prm_3desc")]

  # combine the scanning results between the 3 character and 4 character versions

  which.max1 <- function(x) {

    if (sum(!is.na(x)) == 0) {

      return(0)

    } else {

            return(which.max(x))
    }
  }

  # Create matrix which has one column that is the row numbers for the HES data,
  # and another column that says for each row which column (diagnostic position) to select as this is the maximum SAF.
  selectorMatrix <- cbind(1:nrow(hes), apply(narrow_af, 1, which.max1))

  #-----------------------------------------------------------------------------------------
  # 2)	Discard all episodes which do not include a risk-related diagnosis in the primary diagnostic position.
  # We now ignore all other diagnoses within that episode

  # Select only the rows i.e. episodes with an attributable disease in the primary diagnostic position
  sel <- which(selectorMatrix[, 2] > 0)

  # note sample size
  NROW(sel)
  # Tobacco 3,783,338
  # Alcohol 162,364


  hes <- hes[sel]

  # Add variables to HES data: the ICD-10 code and description of the highest SAF diagnosis.
  # Select the icd-10 code for the diagnosis that has the maximum SAF.
  # Column 1 in the above matrix gives the row coordinates.
  # Column 2 gives the column coordinates.
  # Which results in a new variable that contains the icd-10 code.

  # but this only works if they are data.frames
  narrow_af <- as.data.frame(narrow_af)
  narrow_icd <- as.data.frame(narrow_icd)
  narrow_desc <- as.data.frame(narrow_desc)

  hes[, max_icd := narrow_icd[selectorMatrix]]
  hes[, max_af := narrow_af[selectorMatrix]]
  hes[, max_desc := narrow_desc[selectorMatrix]]

  hes[, prm_3icd := NULL]
  hes[, prm_3af := NULL]
  hes[, prm_3desc := NULL]
  hes[, prm_4icd := NULL]
  hes[, prm_4af := NULL]
  hes[, prm_4desc := NULL]

  setdiff(unique(lkup$Description), unique(hes$max_desc))

  rm(lkup, narrow_icd, narrow_af, narrow_desc, selectorMatrix); gc()
  nrow(hes)

  setnames(hes, "max_desc", "Cause")

  #keep <- Hmisc::Cs(sex, age_cat, imd_quintile, admidate, admimeth, epistart, epiorder, episodeduration, Month, utla_name_london_groupings,
  #                  type, max_icd, max_af, Cause)

  #hes <- hes[ , ..keep]

  return(hes)

}
