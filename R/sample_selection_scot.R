
#' Reduce size of HES to sample required - Scotland
#'
#' Reads in cleaned Hospital episode statistics and reduces to include only sample we need, 
#' e.g. reduced ages, and assigns admission types.
#'
#' This code reduces the size of the HES data to only include applicable rows. 
#' See the \href{https://stapm.gitlab.io/r-packages/hesr/articles/cleaning_and_costing.html#sample-selection}{cleaning and costing vignette} for details.
#'
#' @param hes data.table - the cleaned HES data.
#' @param start_age Integer - the minimum age to consider for non-maternity admissions.
#' @param age_categories Character vector - the age category labels.
#' @param age_cat_start_age Integer vector - the ages that open each age category.
#' 
#' @importFrom data.table :=
#'
#' @return Returns a data.table of reduced HES data.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' sample_selection(hes,
#'                  start_age = 16,
#'                  age_categories = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89"),
#'                  age_cat_start_age = c(16, 18, 25, 35, 50, 65, 75)
#'                  )
#'
#' }
#'
#'
sample_selection_scot <- function(
  hes,
  start_age,
  age_categories,
  age_cat_start_age
) {

  # Cleaning ---------------------------------------------------------------

  hes <- hes[management_of_patient %in% c("1", "2", "A", "4", "3", "5", "6", "7")]

  # Select required ages
  hes <- hes[startage >= start_age & startage <= 89]

  # Create age bands
  #hes[, age_band := c("11-15", "16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89")[findInterval(startage, c(11, 16, 18, 25, 35, 50, 65, 75))]]

  # Create age categories
  hes[, age_cat := age_categories[findInterval(startage, age_cat_start_age)]]

  #hes[ , age_cat := as.character(cut(
  #  startage,
  #  c(-1, 17.5, 24.5, 34.5, 49.5, 64.5, 74.5, 121),
  #  labels = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89")
  #))]

  # Assign admission types----------------------------------------------------

  # Remove unknown admission methods
  hes <- hes[admimeth %in% c(10, 11, 12, 18, 19, 20, 21, 22, 30, 31, 32, 33, 34, 35, 36, 38, 39, 40)]
  
  #NROW(hes)

  hes[ , type := NA_character_]

  
  # Elective
  
  # England - admimeth
  # 11 = Waiting list 12 = Booked 13 = Planned Emergency Admission  
  # 81  = Transfer of any admitted patient from other Hospital Provider other than in an emergency
  
  # routine or urgent admissions
  hes[admimeth %in% c(10, 11, 12, 18, 19, 20, 21, 22, 40) & episodeduration > 0, type := "Elective"]

  # day cases
  hes[(admimeth %in% c(10, 11, 12, 18, 19, 20, 21, 22, 40) & episodeduration == 0) | management_of_patient %in% c("2", "4", "6", "A"), type := "Day_case"]
  
  # non-elective
  
  # England 
  # when admission is unpredictable and at short notice because of clinical need: 
  # 21 = Accident and emergency or dental casualty department of the Health Care Provider  
  # 22 = General Practitioner: after a request for immediate admission has been made direct to a Hospital Provider, i.e. not through a Bed bureau, by a General Practitioner: or deputy 
  # 23 = Bed bureau 
  # 24 = Consultant Clinic, of this or another Health Care Provider  
  # 25 = Admission via Mental Health Crisis Resolution Team 
  # 2A = Accident and Emergency Department of another provider where the PATIENT had not been admitted 
  # 2B = Transfer of an admitted patient from another Hospital Provider in an emergency  
  # 2C = Baby born at home as intended  
  # 2D = Other emergency admission 
  # 28 = Other means, examples are: - admitted from the Accident and Emergency Department of another provider where they had not been admitted- transfer of an admitted patient from another Hospital Provider in an emergency
  
  hes[admimeth %in% c(30, 31, 32, 33, 34, 35, 36, 38, 39) & episodeduration >= 2, type := "Non_Elective_LS"]
  hes[admimeth %in% c(30, 31, 32, 33, 34, 35, 36, 38, 39) & episodeduration < 2, type := "Non_Elective_SS"]

  # filters out 'other' admission types
  #hes[is.na(type)]
  hes <- hes[!is.na(type)]
  

  #k.rows <- nrow(hes)

  #hes <- transform(hes, Month = substr(admidate, 6, 7))
  #hes[ , Month := stringr::str_sub(admidate, 6, 7)]


  return(hes[])
}
