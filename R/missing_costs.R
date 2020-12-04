
#' Fill in missing costs \lifecycle{maturing}
#'
#' There will be missing unit costs where there have been zero records for a particular combination of variables.
#' If there are no admissions within a subgroup, we expand the age groups until there are enough admissions within a subgroup. If there are so few admissions
#' across all age groups, we can either: average across the condition, and allocate the same cost to everybody with that condition, or remove the condition
#' from the analysis. For example, for alcohol induced pseudo cushings syndrome, there was only 1 admission and therefore this is not enough to reliably
#' estimate a unit cost, and this condition should be removed from the analysis.
#' For now, We have removed the conditions that cannot be reliably costed by sex and IMD, We are currently in the process of updating this
#' to average across the condition (07/08/2019). This function is not finished yet.
#'
#'
#' @param hes is the hes data after it has been ran through hes_cost.R
#'
#' @param missing_cost_lookup is a lookup table to reallocate age category sizes, e.g. from 16-17 to 16-24.
#'
#' @return Returns a summary table of the unit admission costs by condition, age category, sex, and IMD quintile.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' hes <- hes_cost(hes, hesr::unit_reference_costs, hesr::trimpoints, hesr::opcs_costs, k.year = "2016")
#' hes <- missing_costs(hes, missing_cost_lookup = hesr::missing_cost_both_lookup)
#'
#' }
#'
missing_costs <- function(
  hes,
  missing_cost_lookup
) {

  # Create a new data sheet where all combinations are present.
  disease.names <- unique(hes$Cause)

  domain <- data.frame(expand.grid(
    age_cat = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89"),
    sex = c(1, 2),
    imd_quintile =  c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
    Cause = disease.names
  ))

  setDT(domain)

  costed_hes <- hes_cost[, c("sex", "age_cat", "imd_quintile", "spell_id", "Cause", "cost_2016")]


  # if running for the first time - to identify missing conditions and subgroups
  #admission_summary <- costed_hes[, .(
  #  n_episodes = .N,
  #  cost_of_admission = sum(cost_2016)),
  #  by = c("Cause", "spell_id", "age_cat", "imd_quintile", "sex")]

  #unit_cost_admission <- admission_summary[, .(
  #  unit_cost_admission = mean(cost_of_admission),
  #  sd = sd(cost_of_admission, na.rm = TRUE),
  #  se = sqrt(var(cost_of_admission, na.rm = TRUE) / .N),
  #  n_admissions = .N),
  #  by = c("Cause", "age_cat", "imd_quintile", "sex")]

  # Merge the data together, retaining all the nrows in domain.
  #summary <- merge(domain, unit_cost_admission,
  #                 by = c("age_cat", "sex", "Cause", "imd_quintile"), all.x = T, all.y = F)

  #setDT(summary)
  #missing <- unique(summary[is.na(unit_cost_admission), ])
  #unique(missing$Cause)

  #write.csv(missing, "data-raw/Costing/Missing_cost_sheet_alc_tob.csv")

  # No/too few admissions for: alcohol induced pseudo cushings syndrome (1 admission), Alcoholic_cardiomyopathy (183; 30/70 subgroups),
  # Alcoholic_myopathy (79; 13/70 subgroups), Excessive_Blood_Level_of_Alcohol (3), Alcoholic_polyneuropathy (34/70 subgroups),
  # Maternal_care_for_suspected_damage_to_foetus_from_alcohol (4), Other_intentional_injuries (134)

    #####################################################################
  # Expand age bands for ages that have NA admissions.
  # Merge the data together, retaining all the nrows in domain.
  summary <- merge(domain, costed_hes,
                   by = c("age_cat", "sex", "Cause", "imd_quintile"), all.x = T, all.y = F)

  setDT(summary)

  broad_age_cost <- merge(summary, missing_cost_lookup,
                             by = c("age_cat", "Cause"), all.x = T, all.y = F)

  broad_age_cost <- broad_age_cost[is.na(New_age_band), New_age_band := age_cat]

  admission_summary <- broad_age_cost[, .(
    n_episodes = .N,
    cost_of_admission = sum(cost_2016)),
    by = c("Cause", "spell_id", "New_age_band", "age_cat", "imd_quintile", "sex")]

  unit_cost_admission <- admission_summary[, .(
    unit_cost_admission = round(mean(cost_of_admission, na.rm = TRUE), 2),
    sd = round(sd(cost_of_admission, na.rm = TRUE), 2),
    se = round(sqrt(var(cost_of_admission, na.rm = TRUE) / .N), 2),
    n_admissions = .N),
    by = c("Cause", "New_age_band", "imd_quintile", "sex")]

  # We do not cost for male cervical and breast cancer.
  unit_cost_admission <- unit_cost_admission[Cause == "Cervical" & sex == 1, unit_cost_admission := 0]
  unit_cost_admission <- unit_cost_admission[Cause == "Cervical" & sex == 1, n_admissions := 0]
  unit_cost_admission <- unit_cost_admission[Cause == "Breast" & sex == 1, unit_cost_admission := 0]
  unit_cost_admission <- unit_cost_admission[Cause == "Breast" & sex == 1, n_admissions := 0]

  # Check there are no missing
  missing <- unique(unit_cost_admission[is.na(unit_cost_admission), ])
  missing_disease <- unique(missing$Cause)

  unit_cost_admission <- admission_summary[!Cause %in% missing_disease, .(
    unit_cost_admission = round(mean(cost_of_admission, na.rm = TRUE), 2),
    sd = round(sd(cost_of_admission, na.rm = TRUE), 2),
    se = round(sqrt(var(cost_of_admission, na.rm = TRUE) / .N), 2),
    n_admissions = .N),
    by = c("Cause", "New_age_band", "imd_quintile", "sex")]

  # Average across condition where there are too few admissions to reliably cost by subgroup
  #unit_cost_admission_missing <- admission_summary[Cause %in% missing_disease, .(
  #  unit_cost_admission = round(mean(cost_of_admission, na.rm = TRUE), 2),
  #  sd = round(sd(cost_of_admission, na.rm = TRUE), 2),
  #  se = round(sqrt(var(cost_of_admission, na.rm = TRUE) / .N), 2),
  #  n_admissions = .N),
  #  by = c("Cause")]

  #unit_cost_admission <- rbind(unit_cost_admission, unit_cost_admission_missing)


  unit_cost_admission <- unit_cost_admission[Cause != "Bulimia", ]
  unit_cost_admission <- unit_cost_admission[Cause != "Alcohol_induced_pseudoCushings_syndrome", ]
  unit_cost_admission <- unit_cost_admission[Cause != "Excessive_Blood_Level_of_Alcohol", ]
  unit_cost_admission <- unit_cost_admission[Cause != "Alcoholic_myopathy", ]
  unit_cost_admission <- unit_cost_admission[Cause != "Maternal_care_for_suspected_damage_to_foetus_from_alcohol", ]
  unit_cost_admission <- unit_cost_admission[Cause != "Alcoholic_cardiomyopathy", ]

  # Check missing subgroups
  #unit_cost_admission[Cause == "Alcoholic_cardiomyopathy", ]

  #◘

  return(unit_cost_admission)
}
