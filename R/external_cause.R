#' External cause method \lifecycle{maturing}
#'
#' For alcohol-related conditions, there are some that will only appear in the Cause field, rather than a diagnostic field.
#' These conditions will become the primary cause, over the primary diagnostic position. This would replace the function `narrow_method()`
#' which only looks at the primary diagnostic code.
#'
#'
#' @param hes is the data
#' @param lkup is the lookup table for condition and respective ICD-10 codes to match the hes data to conditions
#'
#' @return
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' external_cause(hes, hesr::lkup)
#'
#' }
#'
external_cause <- function(
  hes,
  lkup
) {

  # 3 characters
  hes[, icd_code := substr(cause, 1, 3)]

  lkup[duplicated(lkup, by = c("age_cat", "sex", "imd_quintile", "icd_code"))]

  hes <- merge(hes, lkup,
               by = c("icd_code", "sex", "age_cat", "imd_quintile"),
               all.x = T, all.y = F, sort = F)

  hes[, cause_3icd := icd_code]
  hes[, icd_code := NULL]

  hes[, cause_3af := AF]
  hes[, AF := NULL]

  hes[, cause_3desc := Description]
  hes[, Description := NULL]


  # 4 characters
  # repeat code above but now matching on 4 characters of the cause code
  hes[, icd_code := substr(cause, 1, 4)]

  hes <- merge(hes, lkup,
               by = c("icd_code", "sex", "age_cat", "imd_quintile"),
               all.x = T, all.y = F, sort = F)

  hes[, cause_4icd := icd_code]
  hes[, icd_code := NULL]

  hes[, cause_4af := AF]
  hes[, AF := NULL]

  hes[, cause_4desc := Description]
  hes[, Description := NULL]


  # create a new variable that has just the first 3 characters of the ICD-10 code in the primary diagnosis position
  hes[, icd_code := substr(diag_01, 1, 3)]

  hes <- cruk_split_oesophageal(hes)

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


  # Now want to create primary column based primarily on 4 code cause column -
  # if this is NA, then assign 3 code cause column -
  # if this is NA, assign 4 code diag 1 -
  # if this is NA, assign 3 code diag 1.

  hes <- hes[, Cause := ifelse(!is.na(cause_4desc), as.character(cause_4desc),
                          ifelse(!is.na(cause_3desc), as.character(cause_3desc),
                                 ifelse(!is.na(prm_4desc), as.character(prm_4desc), as.character(prm_3desc))))]

  hes <- hes[!is.na(hes$Cause),]

  hes <- hes[, max_icd := ifelse(!is.na(cause_4desc), as.character(cause_4icd),
                         ifelse(!is.na(cause_3desc), as.character(cause_3icd),
                                ifelse(!is.na(prm_4desc), as.character(prm_4icd),
                                       ifelse(!is.na(prm_3desc), as.character(prm_3icd), NA))))]

  hes <- hes[, max_af := ifelse(!is.na(cause_4desc), as.character(cause_4af),
                                 ifelse(!is.na(cause_3desc), as.character(cause_3af),
                                        ifelse(!is.na(prm_4desc), as.character(prm_4af),
                                               ifelse(!is.na(prm_3desc), as.character(prm_3af), NA))))]


  hes <- hes[, prm_3icd := NULL]
  hes <- hes[, prm_3desc := NULL]
  hes <- hes[, prm_4icd := NULL]
  hes <- hes[, prm_4desc := NULL]
  hes <- hes[, cause_3icd := NULL]
  hes <- hes[, cause_3desc := NULL]
  hes <- hes[, cause_4icd := NULL]
  hes <- hes[, cause_4desc := NULL]
  hes <- hes[, cause_3af := NULL]
  hes <- hes[, cause_4af := NULL]
  hes <- hes[, prm_3af := NULL]
  hes <- hes[, prm_4af := NULL]




  return(hes)

}
