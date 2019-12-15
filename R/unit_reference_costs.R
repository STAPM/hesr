
#' Format and clean reference costs
#'
#' We use the NHS Digital Reference costs for years 2010-2016 to include as many HRG codes as possible, as when we used 2016 reference costs, we found ~70% remained unmatched.
#' Combining the reference costs for different types of admission.
#' Returns an table of episode unit costs by ICD10 code and admission type.
#'
#' @docType data
#'
#' @format A data table (sushrg, description, cost, Excess_cost, Year, type).
#'
#' @source
#'
#'
#'
"unit_reference_costs"
