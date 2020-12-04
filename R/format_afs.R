
#' Clean and format attributable fractions (AFs) \lifecycle{maturing}
#'
#' Reads in lookup table of attributable fractions for either alcohol or tobacco. Formats files to be the same as each other to use in the function `assign_risk()`.
#'
#' For now the option for 'Both' only works for the narrow method calculation. This does not include AFs, just matches the ICD-10 codes in HES to
#' the condition to keep the conditions for both alcohol and tobacco in order to cost them together.
#'
#' @param lkup attributable fraction lookup table
#' @param substance is the risk factor. E.g. Alcohol
#'
#' @return  returns a table by condition, sex, icd_code, age_cat and AF))
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' Alcohol_AAFs <- format_afs(substance = c("Alcohol"))
#'
#' }
#'

format_afs <- function(
  lkup,
  substance = c("Alcohol", "Tobacco", "Both")
) {

  if (substance %in% c("Tobacco", "Alcohol")) {

    # Smoking attributable fractions
    if (substance == "Tobacco") {

      #lkup <- read.csv("//tsclient/X/ScHARR/PR_Disease_Risk_TA/Code/Attributable_fractions/Smoking_attributable_fractions/Output/Archive/safs_long.csv")
      setDT(lkup)

      setnames(lkup,
              c("SAF", "ICD10_lookups", "disease_name", "Sex", "Age_cat"),
              c("AF", "icd_code", "Description", "sex", "age_cat"))

      lkup[, V1 := NULL]
      lkup[, sex := as.integer(plyr::revalue(sex, c("Male" = 1, "Female" = 2)))]
      lkup[, imd_quintile := as.character(imd_quintile)]

      lkup[Description == "Hip_fracture" & sex == "Male", AF := -1000]
      lkup[Description == "Breast" & sex == "Male", AF := -1000]
      lkup[Description == "Cervical" & sex == "Male", AF := -1000]
    }


    # Alcohol attributable fractions
    if (substance == "Alcohol") {

      #lkup <- read.csv("//tsclient/X/ScHARR/PR_Disease_Risk_TA/Code/Attributable_fractions/Alcohol_attributable_fractions/Output/Archive/aafs_long_2016.csv")
      setDT(lkup)

      setnames(lkup,
              c("Sex", "af", "ICD10_lookups", "condition"),
              c("sex", "AF", "icd_code", "Description"))

      lkup[, sex := as.integer(plyr::revalue(sex, c("Male" = 1, "Female" = 2)))]
      lkup[, imd_quintile := as.character(imd_quintile)]

    }

    lkup$age_cat <- as.character(lkup$age_cat)



    }

  if(substance == "Both"){

    lkup <- read.csv("//tsclient/X/ScHARR/PR_Disease_Risk_TA/Disease_Lists/ICD_10_lookup.csv")
    setDT(lkup)

    setnames(lkup,
             c("ICD10_lookups", "condition"),
             c("icd_code", "Description"))

    lkup <- unique(lkup)

  }

  lkup$icd_code <- as.character(lkup$icd_code)

  return(lkup)
}
