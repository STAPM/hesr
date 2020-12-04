#' Cost episode-level HES data \lifecycle{maturing}
#'
#' Combine HES data with NHS reference costs, trimpoints, and OPCS costs.
#'
#' Matches the HES data with the NHS reference costs, trimpoints, and OPCS costs, using the HRG code and type of admission. Reference costs,
#' trimpoints, and OPCS costs are stored in the package.
#'
#'
#' @param hes
#' @param reference_costs
#' @param trimpoints Trimpoints that indicate the numbers of days in bed that are included within the standard costing. Days beyond the trimpoint need to be costed as extra.
#' @param opcs_lookup to add additional costs based on procedure code
#' @param k.year an indicator of the year that costs apply to
#'
#' @return Returns the hes data with a new column for the total episode cost (including excess trimpoints and OPCS costs), and a column for OPCS cost and trimpoints.
#' All inflated to 2016/17 prices.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' hes <- hes_cost(hes, hesr::unit_reference_costs, hesr::trimpoints, hesr::opcs_costs, k.year)
#'
#' }
#'
#'
hes_cost <-  function(

  hes,   # The hes data

  reference_costs,  # Reference costs

  trimpoints,    # Trimpoints that indicate the numbers of days in bed that are included within the standard costing.
  # Days beyond the trimpoint need to be costed as extra.

  opcs_lookup,   # to add additonal costs based on procedure code

  k.year      # an indicator of the year that costs apply to

) {

  k.nrow <- nrow(hes)

  costed_hes <- merge(
    hes,
    reference_costs,
    by = c("sushrg", "type"), all.x = TRUE, sort = F)


  # Test data has same number of rows as started with
  testthat::expect_equal(nrow(costed_hes), k.nrow)

  # Add procedure costs where needed----

  # There are some zero cost HRG codes that occur commonly
  # and respresent high-cost treatments
  # there are for chemotherapy, radiotherapy and kidney dialysis
  #   LA08E
  #   SB97Z
  #   SC97Z

  # Add in separate costs based on operation codes for these treatments

  # We only need a subset of the variables

  # The list of variables we want to keep
  subset_diags <- c(paste0("opertn_", stringr::str_pad(1:24, 2, "left", "0")), "type")

  # The names of the columns containing operation codes.
  diagnoses <- paste0("opertn_", stringr::str_pad(1:24, 2, "left", "0"))

  # A numerical index of those codes.
  diagnosisPosition <- 1:length(diagnoses)

  # Assign the appropriate cost to every operation

  # Loop through each operation column and search for OPCS codes that match costs.
  # Then create new columns that store the corresponding costs.

  # All of the OPCS codes have 4 characters e.g. X714

  need_opcs_cost <- costed_hes[stringr::str_sub(max_icd, 1, 1) == "C" | stringr::str_sub(max_icd, 1, 3) == "N18"]
  dont_need_opcs_cost <- costed_hes[stringr::str_sub(max_icd, 1, 1) != "C" & stringr::str_sub(max_icd, 1, 3) != "N18"]

  # Create a temporary file with just the columns we want.
  temp <- need_opcs_cost[, ..subset_diags]

  # 4 character ICD-10 matching for each operation code column.
  for (i in diagnosisPosition) {

    # Grab the name of that column.
    diagnosisHeader <- diagnoses[i]

    # Create a new column that has the first 4 characters of the OPCS code.
    temp[, OPCS := substr(temp[[diagnosisHeader]], 1, 4)]

    # Merge the HES data (temp) with the data on costs (cost in lkup).
    # The index variables for this match are 4 character OPCS, and type.
    temp <- merge(
      temp,
      opcs_lookup,
      by = c("OPCS", "type"), all.x = T, sort = F)

    # Create new variables that store the 4 character OPCS code and the corresponding cost for each operation code column
    # i.e. any matches of OPCS and cost for opertn_01 etc.
    #  temp[paste("d4_cause_", i, sep = "")] <- temp$Description
    temp[, paste("d4_opcs_", i, sep = "") := OPCS]
    temp[, paste("d4_cost_", i, sep = "") := cost]

    # Delete the working columns above ready for the next iteration of the loop.
    #  temp$Description <- NULL
    temp[, OPCS := NULL]
    temp[, cost := NULL]

  }

  d4_cost <- paste0("d4_cost_", 1:24)
  temp[, OPCS_cost := rowSums(temp[, ..d4_cost], na.rm = T)]

  need_opcs_cost[, OPCS_cost := temp[, OPCS_cost]]

  costs_with_opcs <- rbindlist(list(need_opcs_cost, dont_need_opcs_cost), use.names = T, fill = T)


  rm(diagnosisHeader, diagnosisPosition, diagnoses, i, need_opcs_cost, dont_need_opcs_cost)
  gc()


  # Calculate the cost of excess bed days----

  # First work out the number of excess bed days per episode/row.

  # If trimpoint > days, then the difference needs to be costed by multiplying the difference by data_h, otherwise = 0.

  hes <- merge(
    costs_with_opcs,
    trimpoints,
    by = c("sushrg"), all.x = TRUE, sort = F)

  hes[, Excess_costs := 0]

  # k.year = 2016

  # Some very long episodes > 1 year, causing very high costs when all bed days are costed.
  # Only cost bed days taken during the focal year (all episodes end in the focal year, but not all start in it).
  hes[ , epidur_year_start := episodeduration]
  k.year <- paste0(k.year, "-04-01")
  hes[epistart < as.Date(k.year, "%Y-%m-%d"), epidur_year_start := as.Date(epiend, "%Y-%m-%d") - as.Date(k.year, "%Y-%m-%d")]

  hes[ , epidur_year_start := as.integer(epidur_year_start)]

  hes[ , Episode.Trimpoint := as.integer(Episode.Trimpoint)]

  hes[epidur_year_start > Episode.Trimpoint, Excess_costs := (epidur_year_start - Episode.Trimpoint) * Excess_unit_cost]

  hes[is.na(cost), cost := 0]
  hes[is.na(Excess_costs), Excess_costs := 0]
  hes[is.na(OPCS_cost), OPCS_cost := 0]

  hes[ , Total_cost := cost + Excess_costs + OPCS_cost]

  # Need to inflate prices to 2015/16, by year.Using HCHS index
  hes[ , cost_2016 := Total_cost]
  hes[Year == 2010, cost_2016 := Total_cost * 1.092519]
  hes[Year == 2011, cost_2016 := Total_cost * 1.070088]
  hes[Year == 2012, cost_2016 := Total_cost * 1.05221]
  hes[Year == 2013, cost_2016 := Total_cost * 1.04062]
  hes[Year == 2014, cost_2016 := Total_cost * 1.031389]
  hes[Year == 2015, cost_2016 := Total_cost * 1.017845]

  # Report how many episodes have zero cost
  zero_cost_perc <- round(100 * nrow(hes[Total_cost == 0]) / nrow(hes), 2)

  zero_cost_codes <- table(hes[Total_cost == 0, sushrg], useNA = "ifany")


  cat(paste0("Percentage of zero cost episodes:\n\n", zero_cost_perc, "%\n\n", "Zero cost HRG codes:"))
  print(zero_cost_codes)

  # Test data has same number of rows as started with
  testthat::expect_equal(nrow(hes), k.nrow)

  #############################################################################
  write.csv(hes, "D:/HES/working_data_files/Admissions_rates/Admissions/tob_alc_unit_episode_costs_06082019.csv")
  #############################################################################

  return(hes)
}




