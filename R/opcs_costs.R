
#' OPCS codes and costs
#'
#' The OPCS-4 (Office of Population, Censuses and Surveys: Classification of Interventions and Procedures, 4th revision) classification is used to record details
#' of any procedures or interventions performed. These are found in the operation diagnostic fields within HES. We use OPCS codes from 2016 only.
#' For chemotherapy, radiotherapy and renal dialysis, many episodes are assigned a zero-cost code for same day treatment. Therefore, for episodes with a primary diagnosis
#' of cancer, we added chemotherapy and/or radiotherapy costs, which are found through OPCS codes, as these are costed separately as high-cost treatments.
#'
#' @docType data
#'
#' @format A data table (type, cost, OPCS).
#'
#' @source
#'
#'
#'
"opcs_costs"
