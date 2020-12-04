#' Broad method for allocating risk \lifecycle{maturing}
#'
#'
#' The broad method selects the diagnosis code of an episode to be the diagnostic code with the highest PAF.
#' This is in an effort to select the condition that is most causally attributable to the risk factor as to be as inclusive as possible.
#'
#' @param hes HES data
#' @param lkup cleaned and formatted attributable fraction lookup table.
#'
#'
#' @return Returns a reduced HES data, with only rows that have the largest attributable risk-related diagnosis code within an episode.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' hes <- read_hes("1718")
#' lkup <- format_afs("Tobacco")
#' broad_method(hes, lkup)
#'
#' }
#'
broad_method <- function(
  hes,
  lkup
) {
  # For the diagnoses in this code we only need a subset of the variables in the cleaned HES data
  # so just select sex, age-band and the first 20 diagnosis codes.

  # The list of variables we want to keep.
  # Need to check whether we should include fewer diagnostic codes, as I think older years may have fewer (14).

  subset_diags <- c("sex","age_cat", "imd_quintile", "diag_01", "diag_02", "diag_03", "diag_04", "diag_05",
                    "diag_06", "diag_07", "diag_08", "diag_09", "diag_10", "diag_11", "diag_12",
                    "diag_13", "diag_14", "diag_15", "diag_16", "diag_17", "diag_18", "diag_19", "diag_20")

  # Create a temporary file with just the columns we want.
  temp <- hes[ , ..subset_diags]

  # Test that temp has right number of rows.
  #testthat::expect_identical(nrow(temp), k.rows)

  rm(subset_diags)
  gc()


  diagnoses <- c("diag_01", "diag_02", "diag_03", "diag_04", "diag_05", "diag_06",
                 "diag_07", "diag_08", "diag_09", "diag_10", "diag_11", "diag_12", "diag_13",
                 "diag_14", "diag_15", "diag_16", "diag_17", "diag_18", "diag_19", "diag_20")

  # A numerical index of those diagnosis codes.
  diagnosisPosition <- 1:length(diagnoses)


  # (1) Assign the appropriate age-sex-condition specific AF to every diagnosis--------------------------------

  # Loop through each diagnosis column and search for ICD-10 codes that match attributable diseases.
  # Then create new columns that store the corresponding attributable fractions (AFs).

  # Begin by going through that search procedure making matches based on just the first three characters of the ICD-10 code.
  # Many of the ICD-10 codes just have 3 characters e.g. X14 but some have 4 characters.
  # Matching on 4 characters will be done after this.

  # 3 character ICD-10 matching for each diagnosis code column.

  cat("\tscanning 3 chr codes\n")

  for(i in diagnosisPosition) {

    # Grab the name of that column.
    diagnosisHeader <- diagnoses[i]

    # Create a new column that has just the first 3 characters of the ICD-10 code.
    temp[ , icd_code := substr(get(diagnosisHeader), 1, 3)]

    # Merge the HES data (temp) with the data on attributable fractions (AFs in lkup).
    temp <- merge(temp, lkup, by = c("icd_code", "sex", "age_cat", "imd_quintile"), all.x = T, all.y = F, sort = F)

    # Test that temp has right number of rows.
    #expect_identical(nrow(temp), k.rows)

    # Create new variables that store the 3 character ICD-10 code and the corresponding AF for each diagnosis code column
    # i.e. any matches of ICD-10 and AF for diag_01 etc.
    temp[ , (paste0("d3_cause_", i)) := Description]
    temp[ , (paste0("d3_icd_", i)) := icd_code]
    temp[ , (paste0("d3_af_", i)) := AF]

    # Delete the working columns above ready for the next iteration of the loop.
    temp[ , Description := NULL]
    temp[ , icd_code := NULL]
    temp[ , AF := NULL]

  }

  # 4 character ICD-10 matching for each diagnosis code column.
  cat("\tscanning 4 chr codes\n")

  for(i in diagnosisPosition) {

    # Grab the name of that column.
    diagnosisHeader <- diagnoses[i]

    # Create a new column that has just the first 3 characters of the ICD-10 code.
    temp[ , icd_code := substr(get(diagnosisHeader), 1, 4)]

    # Merge the HES data (temp) with the data on attributable fractions (AFs in lkup).
    temp <- merge(temp, lkup, by = c("icd_code", "sex", "age_cat", "imd_quintile"), all.x = T, all.y = F, sort = F)

    # Test that temp has right number of rows.
    #expect_identical(nrow(temp), k.rows)

    # Create new variables that store the 3 character ICD-10 code and the corresponding AF for each diagnosis code column
    # i.e. any matches of ICD-10 and AAF for diag_01 etc.
    temp[ , (paste0("d4_cause_", i)) := Description]
    temp[ , (paste0("d4_icd_", i)) := icd_code]
    temp[ , (paste0("d4_af_", i)) := AF]

    # Delete the working columns above ready for the next iteration of the loop.
    temp[ , Description := NULL]
    temp[ , icd_code := NULL]
    temp[ , AF := NULL]

  }

  rm(diagnosisHeader, diagnosisPosition, diagnoses, i, lkup)
  gc()

  # 2.1 PREP the data----------------

  temp <- as.data.frame(temp)

  # Names of the variables where them AFs are stored.
  broad_af <- c("d4_af_1", "d3_af_1", "d4_af_2", "d3_af_2", "d4_af_3",
                "d3_af_3", "d4_af_4", "d3_af_4", "d4_af_5", "d3_af_5",
                "d4_af_6", "d3_af_6", "d4_af_7", "d3_af_7", "d4_af_8",
                "d3_af_8", "d4_af_9", "d3_af_9", "d4_af_10", "d3_af_10",
                "d4_af_11", "d3_af_11", "d4_af_12", "d3_af_12", "d4_af_13",
                "d3_af_13", "d4_af_14", "d3_af_14", "d4_af_15", "d3_af_15",
                "d4_af_16", "d3_af_16", "d4_af_17", "d3_af_17", "d4_af_18",
                "d3_af_18", "d4_af_19", "d3_af_19", "d4_af_20", "d3_af_20")

  # Names of the variables where the corresponding ICD-10 codes are stored.
  broad_icd <- c("d4_icd_1", "d3_icd_1", "d4_icd_2", "d3_icd_2", "d4_icd_3",
                 "d3_icd_3", "d4_icd_4", "d3_icd_4", "d4_icd_5", "d3_icd_5",
                 "d4_icd_6", "d3_icd_6", "d4_icd_7", "d3_icd_7", "d4_icd_8",
                 "d3_icd_8", "d4_icd_9", "d3_icd_9", "d4_icd_10", "d3_icd_10",
                 "d4_icd_11", "d3_icd_11", "d4_icd_12", "d3_icd_12", "d4_icd_13",
                 "d3_icd_13", "d4_icd_14", "d3_icd_14", "d4_icd_15", "d3_icd_15",
                 "d4_icd_16", "d3_icd_16", "d4_icd_17", "d3_icd_17", "d4_icd_18",
                 "d3_icd_18", "d4_icd_19", "d3_icd_19", "d4_icd_20", "d3_icd_20")

  # Names of the variables where the corresponding condition names are stored.
  broad_cause <- c("d4_cause_1", "d3_cause_1", "d4_cause_2", "d3_cause_2", "d4_cause_3",
                   "d3_cause_3", "d4_cause_4", "d3_cause_4", "d4_cause_5", "d3_cause_5",
                   "d4_cause_6", "d3_cause_6", "d4_cause_7", "d3_cause_7", "d4_cause_8",
                   "d3_cause_8", "d4_cause_9", "d3_cause_9", "d4_cause_10", "d3_cause_10",
                   "d4_cause_11", "d3_cause_11", "d4_cause_12", "d3_cause_12", "d4_cause_13",
                   "d3_cause_13", "d4_cause_14", "d3_cause_14", "d4_cause_15", "d3_cause_15",
                   "d4_cause_16", "d3_cause_16", "d4_cause_17", "d3_cause_17", "d4_cause_18",
                   "d3_cause_18", "d4_cause_19", "d3_cause_19", "d4_cause_20", "d3_cause_20")

  # Create subsets of the data that contain just the columns above.
  broad_af  <- temp[broad_af]
  broad_icd  <- temp[broad_icd]
  broad_cause  <- temp[broad_cause]

  # 2.2 Select max AFs and their ICD codes----------------

  cat("\tselect ICD10 code with largest AF\n")

  # For all episodes within the same admission, look across the alcohol/tobacco-related diagnoses and pick only the one with the highest AF.
  # Nb this may be negative (if alcohol is protective).
  # Each admission now has just one diagnosis.

  # Where 2 episodes within the same admission have the same highest AAF,
  # but these relate to different conditions, chose the one in the highest (leftmost) diagnostic position.


  which.max1 <- function(x) {

    if(sum(!is.na(x)) == 0) {

      # If an admission has no alcohol/tobacco-related diagnosis then code the position of the highest AF as 0.
      # Note this is NOT coding the value of the AF as 0.
      return(0)

    } else {

      # Returns the position with the maximum value for the AF.
      # Note that a value of 0.2 will be chosen over a larger protective effect of -0.3.
      # Note also that which.max() returns the first / leftmost diagnostic position.
      return(which.max(x))
    }
  }

  # Create matrix which
  # has one column that is the row numbers for the HES data,
  # and another column that says for each row which column (diagnostic position) to select as this is the maximum AF.
  selectorMatrix <- cbind(1:nrow(temp), apply(broad_af, 1, which.max1))

  # Select only the rows with an attributable disease.

  sel <- which(selectorMatrix[ , 2] > 0)

  temp <- temp[sel, ]
  broad_af <- broad_af[sel, ]
  selectorMatrix <- selectorMatrix[sel, ]

  ## Add variable to HES data: the ICD-10 code of the highest AF diagnosis.
  # Select the icd-10 code for the diagnosis that has the maximum AF.
  # Clever piece of code:
  # Column 1 in the above matrix gives the row coordinates.
  # Column 2 gives the column coordinates.
  # Which results in a new variable that contains the icd-10 code.

  temp$max_icd <- broad_icd[selectorMatrix]
  temp$max_cause <- broad_cause[selectorMatrix]

  # Add variable to HES data: the corresponding AF of the highest AF diagnosis
  # i.e. just the max of the AFs.
  temp$max_af <- apply(broad_af, 1, max, na.rm = TRUE)

  # Take required variables.
  requiredVars <- temp[c("max_cause", "max_icd", "max_af")]

  # Join data together.
  hes <- cbind(hes[sel, ], requiredVars)

  setnames(hes, c("max_cause", "max_icd"), c("Cause", "icd_code"))

  hes <- cruk_split_oesophageal(hes)
  hes <- hes[icd_code != "C15a",]

  rm(broad_icd, broad_cause, broad_af, temp, requiredVars, selectorMatrix, sel)
  gc()

  return(hes)

}
