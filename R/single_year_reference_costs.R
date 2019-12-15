#' Format and clean Reference Costs for single year
#'
#' Combining the reference costs for different types of admission. We only run this if we want one year of reference costs, such as 2016.
#' We do not use this because we find it leaves many HRG codes unmatched. But this function can be used as a test. Instead we prepare the unit reference costs for all years in the folder `data-raw` in
#'  `unit_reference_costs`. They are stored in the package in hesr::unit_reference_costs.
#'
#'
#' @param k.year.ind is the year of HES data to read in.
#'
#' @return Returns a table of episode unit costs by ICD10 code and admission type.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' single_year_reference_costs("1617")
#'
#' }
#'
#'

single_year_reference_costs <- function(
  k.year.ind
) {

  #Elective
  elective_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/Reference costs/Reference_costs_2016.xlsx", sheet = "EL", range = "A5:D2459") %>%
    mutate(type = "Elective")

  elective_excess_bed_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/Reference costs/Reference_costs_2016.xlsx", sheet = "EL_XS", range = "A5:D1887") %>%
    plyr::rename(c("National Average Unit Cost" = "Excess_cost", "Excess Bed days" = "Days"))

  elective_costs_2016 <- merge(elective_costs_2016, elective_excess_bed_costs_2016, by = c("Currency Code", "Currency Description")) %>%
    select(-Days) %>%
    plyr::rename(c("Number of FCEs\r\n" = "Number of FCE's"))

  #Non-elective Long stay
  non_elective_long_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/Reference costs/Reference_costs_2016.xlsx", sheet = "NEL", range = "A5:D2338") %>%
    mutate(type = "Non_Elective_LS")

  non_elective_excess_bed_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/Reference costs/Reference_costs_2016.xlsx", sheet = "NEL_XS", range = "A5:D2081") %>%
    plyr::rename(c("National Average Unit Cost" = "Excess_cost", "Excess Bed Days" = "Days"))

  non_elective_long_costs_2016 <- merge(non_elective_long_costs_2016, non_elective_excess_bed_costs_2016, by = c("Currency Code", "Currency Description")) %>%
    select(-Days)

  # Non-elective Short stay
  non_elective_short_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/Reference costs/Reference_costs_2016.xlsx", sheet = "NES", range = "A5:D2421") %>%
    mutate(type = "Non_Elective_SS",
           Excess_cost = as.numeric(0))

  # Day case
  day_case_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/Reference costs/Reference_costs_2016.xlsx", sheet = "DC", range = "A5:D2181") %>%
    mutate(type = "Day_case",
           Excess_cost = as.numeric(0))

  # Regular day or night
  DCRA_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/Reference costs/Reference_costs_2016.xlsx", sheet = "RP", range = "A5:D1121") %>%
    mutate(type = "DCRA",
           Excess_cost = as.numeric(0))


  # bind all data tables to make one data table of all admissions types
  reference_costs <-
    rbindlist(list(
      elective_costs_2016,
      non_elective_long_costs_2016,
      non_elective_short_costs_2016,
      day_case_costs_2016,
      DCRA_costs_2016), use.names = T) %>%
    plyr::rename(c("Currency Code" = "sushrg",
                   "Currency Description" = "description",
                   "National Average Unit Cost" = "cost",
                   "Number of FCE's" = "Episodes")) %>%
    select(-Episodes)

  n_rows <- nrow(reference_costs)
  cat(paste0('\t\t\t ', n_rows, ' rows of reference costs\n'))

 return(reference_costs)
}
