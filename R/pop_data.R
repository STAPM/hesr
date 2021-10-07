
#' Add the population counts by age band, sex, IMD quintile
#'
#' This code reads in the population data for all years, received from the ONS.
#'
#' Collapses the population data by the grouping variables specified, usually sex, age group, and IMD quintile. But for the person-specific admissions, we want to
#' collapse by single age instead.
#'
#'
#' @param k.year is the year of data
#' @param age_start numeric. Youngest age required in HES data
#' @param age_categories Character vector of age categories. Default is c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89").
#' @param age_categories_start Numerical vector of start ages for age categories. Default is c(16, 18, 25, 35, 50, 65, 75).
#' @param subgroup is the subgroup you want to collapse the data by.
#'
#'
#' @return returns the population counts by subgroup
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' pop_data(k.year = "2016", subgroup = c("sex", "imd_quintile", "age_cat"))
#'
#' }
#'
#'
pop_data <- function(
  k.year,
  age_start,
  age_categories,
  age_categories_start,
  subgroup = c("sex", "imd_quintile", "age_cat")
) {

  # Read the population data for all years received from the ONS
  data.pop <- fread("//tsclient/X/ScHARR/PR_Mortality_data_TA/Code/Archive/10_master_data/Output/pop_counts_EW.txt",
                    select = c("pops", "year", "sex", "laua", "imd_quintile", "ageinyrs"))

  # remove wales
  data.pop <- data.pop[stringr::str_detect(laua, "E")]

  # process age band
  data.pop[ , ageinyrs := replace(ageinyrs, ageinyrs == "90+", "90")]
  data.pop[ , ageinyrs := as.vector(as.numeric(ageinyrs))]
  data.pop <- data.pop[ageinyrs >= age_start & ageinyrs <= 89]
  data.pop[, sex := as.integer(plyr::revalue(sex, c("Male" = 1, "Female" = 2)))]
  data.pop[, age_cat := age_categories[findInterval(ageinyrs, age_categories_start)]]




  # filter the pop data to retain only the focal year
  data.pop <- data.pop[year == k.year]

  # store population size for checking
  k.pop <- sum(data.pop$pops)

  #expect_equal(sum(data.pop$pops), k.pop)

  # collapse the pop numbers by the grouping variables
  data.pop <- data.pop[ , .(pop.size = sum(pops)), by = c("sex", "imd_quintile", "age_cat")]

  return(data.pop)

}
