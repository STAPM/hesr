
#' Reduce size of HES to sample required
#'
#' Reads in cleaned Hospital episode statistics and reduces to include only sample we need, 
#' e.g. reduced ages, and assigns admission types.
#'
#' This code reduces the size of the HES data to only include applicable rows. 
#' See the \href{https://stapm.gitlab.io/r-packages/hesr/articles/cleaning_and_costing.html#sample-selection}{cleaning and costing vignette} for details.
#'
#' @param hes data.table - the cleaned HES data.
#' @param min_age Integer - the minimum age to consider for non-maternity admissions.
#' @param max_age Integer - the maximum age to consider. This is normally set at 89 years.
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
#'                  min_age = 16,
#'                  age_categories = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89"),
#'                  age_cat_start_age = c(16, 18, 25, 35, 50, 65, 75)
#'                  )
#'
#' }
#'
#'
sample_selection <- function(
  hes,
  min_age,
  max_age,
  age_categories,
  age_cat_start_age
) {

  # Cleaning ---------------------------------------------------------------

  # Filter out classpat = 5 (birth of baby)
  hes <- hes[classpat %in% c(1, 2, 3, 4)]

  # Select required ages
  hes <- hes[startage >= min_age & startage <= max_age]

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
  hes <- hes[!(admimeth %in% c("82", "83", "84", "89", "98", "99"))]
  
  #NROW(hes)

  hes[ , type := NA_character_]

  hes[admimeth %in% c("11", "12", "13", "81") & episodeduration > 0 & !(classpat %in% c("3", "4")) , type := "Elective" ]
  hes[is.element(admimeth, c("31", "32")) & episodeduration > 0 & sex == 2 & startage >= 10 & startage <= 55 & !(classpat %in% c("3", "4")) , type := "Elective"]

  hes[is.element(admimeth, c("11", "12", "13", "81")) & episodeduration == 0 & !(classpat %in% c("3", "4")), type := "Day_case"]
  hes[is.element(admimeth, c("31", "32")) & episodeduration == 0 & sex == 2 & startage >= 10 & startage <= 55 & !(classpat %in% c("3", "4")) , type := "Day_case"]
  hes[classpat %in% c("3", "4"), type := "DCRA"]

  hes[admimeth %in% c("21", "22", "23", "24", "25", "28", "2A", "2B", "2C", "2D") & episodeduration >= 2 & !(classpat %in% c("3", "4")) , type := "Non_Elective_LS"]
  hes[admimeth %in% c("21", "22", "23", "24", "25", "28", "2A", "2B", "2C", "2D") & episodeduration < 2 & !(classpat %in% c("3", "4")) , type := "Non_Elective_SS"]

  hes <- hes[!is.na(type)]

  #k.rows <- nrow(hes)

  #hes <- transform(hes, Month = substr(admidate, 6, 7))
  #hes[ , Month := stringr::str_sub(admidate, 6, 7)]


  return(hes[])
}
