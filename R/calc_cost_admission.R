#' Calculate unit costs of risk-related admissions by sex, age, IMD and condition. \lifecycle{maturing}
#'
#' Calculating the cost of admissions in the latest year (2016) to assign to all years. We do this is an effort to be conservative, and cost only the episodes within
#' an admission that have been allocated to the primary condition. We retain only the rows in the HES data which have the same ICD10 code as the earliest risk-related
#' episode in the admission, and use the unit costs of episodes from the NHS Reference Costs to cost them. We sum this cost across the admission to calculate unit
#' admission costs.
#'
#' We use the 'hes_cost' function to match episodes with reference costs to calculate the average cost per episode for one year of data. We found when
#' calculating unit costs of admissions, we are left with some subgroups that have not been costed. In order to fill in these gaps we use the function
#' 'missing_costs' to create average unit costs of admissions by condition, sex, IMD quintile, and age category (age category depends on the size of the
#' data/number of admissions we have for each subgroup).
#'
#' @param k.year.ind is the year of HES data to read in.
#'
#' @return Returns an table of unit costs by condition, sex, age category and IMD quintile (where possible)
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' calc_cost_admission("1617")
#'
#' }
#'
#'

calc_cost_admission <- function(
  k.year.ind
) {

  k.year <- as.numeric(paste0("20", stringr::str_sub(k.year.ind, start = 1, end = 2)))

  hes <- read_hes(k.year.ind, test = TRUE)
  hes <- clean_hes(hes)
  hes <- define_spells(hes)
  hes <- local_authority(hes, k.year.ind)

  hes <- assign_risk(hes, k.year.ind, method = "Narrow", substance = "Alcohol", level = "Episode", summary = FALSE)

  # keep only rows we want to cost (all rows which have the ICD10 code from the earliest episode)
  hes <- hes[ , icd_choose := max_icd[1], by = "spell_id"]

  hes <- hes[max_icd == icd_choose, ]

  hes <- hes_cost(hes, reference_costs = hesr::unit_reference_costs, trimpoints = hesr::trimpoints, opcs_lookup = hesr::opcs_costs, k.year = "2016")

  unit_cost_admission <- missing_costs(hes, missing_cost_lookup = hesr::missing_cost_both_lookup)

  # Embed the data within the package
  usethis::use_data(unit_cost_admission, overwrite = TRUE)

  return(unit_cost_admission)
}

