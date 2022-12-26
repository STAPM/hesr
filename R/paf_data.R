
#' Example population attributable fractions
#' 
#' Lookup table of attributable fractions for conditions related to alcohol and tobacco. 
#' 
#' The PAF data embedded in the hesr package is meant to serve as an example 
#' of the structure that PAF estimates should be in to be used with the functions in the package
#' e.g. column headings. 
#' It is not necessary to update the PAFs embedded in the package when new PAFs are calculated
#' any new PAFs can just be prepared and input in the project code so long as they match the structure here
#' 
#' The example PAFs embedded in the package are 
#' Alcohol and Smoking Attributable Fractions for morbidity by sex, age-band and 
#' Index of Multiple Deprivation quintiles. 
#' For a pooled sample of the Scottish Health Survey from 2015 to 2019.
#' 
#' 
#' @docType data
#'
#' @format A data table (column names: "condition", "ageband", "sex", "imd_quintile", "PAF").
#'
#' @source The code that calculated the population attributable fractions in paf_data is stored in `X/ScHARR/PR_Disease_Risk_TA/Attrib_fractions/epi_pop_attrib_fractions`. 
#' They come from the file `PAF_summary_alcohol_and_tobacco_morb_Scotland_2022-12-18.xlsx`. 
#' They were calculated using the tobalcepi package version 1.6.0. 
#'
"paf_data"
