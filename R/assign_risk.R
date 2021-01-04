

#' Reducing the HES data to alcohol/tobacco-related episodes and admissions \lifecycle{maturing}
#'
#' This is the main function used to convert the cleaned data in to summary tables of either episodes, admissions or person-specific admissions.
#' It reads in the cleaned Hospital episode statistics for England, reducing it to the sample selection required. It assigns an attributable fraction
#' to each row/episode based on either the broad or narrow method, for the substance chosen (alcohol or tobacco). The code then goes
#' on to calculate a summary table of alcohol/tobacco attributable hospital episodes, admissions or person-specific admissions as chosen.
#'
#'
#' @param data Data.table - cleaned HES data.
#' @param year Character string - indicating the year of HES data - in the form "1718".
#' @param pop_data Data.table - mid-year population sizes. 
#' @param youngest_age numeric. Youngest age required in HES data
#' @param age_cat Character vector of age categories. Default is c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89").
#' @param age_cat_start Numerical vector of start ages for age categories. Default is c(16, 18, 25, 35, 50, 65, 75).
#' @param include_youth Logical - whether to include under 16s for youth drinking analysis. Defaults to FALSE. If TRUE, will include 11-15 as an age category,
#' but will give them the same attributable fraction as 16-17s. If FALSE, will not include 11-15s.
#' @param method method required to allocate episode to condition. "Broad" or "Narrow"
#' @param saf_lkup smoking attributable fraction long look up table in the format: "SAF", "ICD10_lookups", "disease_name", "Sex", "Age_cat". Default is for
#' age categories: c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89"), but if summarising the HES data by other age categories, can change the
#' attributable fraction lookup table here.
#' @param aaf_lkup alcohol attributable fraction long look up table in the format: "Sex", "af", "ICD10_lookups", "condition", "age_cat". Default is for
#' age categories: c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89"), but if summarising the HES data by other age categories, can change the
#' attributable fraction lookup table here.
#' @param substance risk. "Alcohol" or "Tobacco"
#' @param level the level of detail of summary table. "Episode", or "Admission".
#' @param summary Logical - whether to return a summary table or the full HES data (want this option for the costing).
#' Defaults to TRUE. If TRUE, will output a summary table of HES data. If FALSE, then will output the whole year of data.
#' @param dir The directory to save the output. Default is "D:/HES/working_data_files", but if summary = TRUE, can save this to the project repo.
#'
#' @return Returns a summary table of number of episodes/admissions/people, SAF, and attributable episodes/admissions/people,
#' by age category, sex, imd_quintile, and condition. If summary = FALSE, will return the whole data unsummarised (for use in costing at episode level)
#'
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' assign_risk(hes, "1617", method = "Narrow", substance = "Alcohol", level = "Episode", summary = TRUE)
#'
#' }
#'

assign_risk <- function(
  data,
  year,
  pop_data,
  youngest_age = 16,
  age_cat = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89"),
  age_cat_start = c(16, 18, 25, 35, 50, 65, 75),
  method = c("Broad", "Narrow"),
  saf_lkup = read.csv("//tsclient/X/ScHARR/PR_Disease_Risk_TA/Code/Attributable_fractions/Smoking_attributable_fractions/Output/Archive/safs_long.csv"),
  aaf_lkup = read.csv("//tsclient/X/ScHARR/PR_Disease_Risk_TA/Code/Attributable_fractions/Alcohol_attributable_fractions/Output/Archive/aafs_long_2016.csv"),
  substance = c("Alcohol", "Tobacco"),
  level = c("Episode", "Admission"),
  summary = TRUE,
  dir = "D:/HES/working_data_files"
) {

  
  ##########################################################
  
  # Set the year for filtering population data later
  k_year <- as.numeric(paste0("20", stringr::str_sub(year, start = 1, end = 2)))

  # Select the required sample
  cat("\tsample_selection\n")
  
  data <- sample_selection(data,
                          start_age = youngest_age,
                          age_categories = age_cat,
                          age_cat_start_age = age_cat_start
                          )

  
  cat("\tRead in the disease risk\n")

  #lkup <- format_afs(substance = substance)


  if (substance == "Tobacco") {
    lkup <- format_afs(lkup = saf_lkup,
                       substance = "Tobacco")
  }

  if (substance == "Alcohol") {
    lkup <- format_afs(lkup = aaf_lkup,
                       substance = "Alcohol")
  }

  # Both only works for narrow which does not need AFs at the moment. Used for costing.
  if (substance == "Both") {
    lkup <- format_afs(substance = "Both")
  }


  if(method == "Narrow"){
    cat("\tnarrow method\n")
    data <- external_cause(data, lkup)
  }


  if(method == "Broad"){
    cat("\tbroad method\n")
    data <- broad_method(data, lkup)
  }

  
  
  
  ##########################################################
  
  # Sum counts
  
  
  if(summary == TRUE){


    if(level == "Episode"){
    # one row per episode
    cat("\tfinal episode table\n")

      data <- data[ , .(
        n_episodes = .N,
        SAF = round(mean(max_af), 2),
        attrib_episodes = .N * max_af
     # ), by = c(stratvars, "Cause")]
        ), by = c("age_cat", "sex", "imd_quintile", "Cause")]

    }




    if(level == "Admission"){
    # one row per admission
    cat("\tfinal admission table\n")# Sort the columns so that the earlier episodes are first.


    setorderv(data,
              cols = c("encrypted_hesid", "spell_id", "epiorder"),
              order = c(1, 1, 1)
    )

    # To allocate an admission to a condition - assign admission to primary risk-related episode
    admissions <- unique(data, by = c("spell_id"))

    admissions <- admissions[ , .(
      n_admissions = .N,
      SAF = round(mean(max_af), 2)
    ), by = c("age_cat", "sex", "imd_quintile", "Cause")]

    domain <- data.frame(expand.grid(
      age_cat = age_cat,
      sex = 1:2,
      imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
      Cause = unique(aaf_lkup$condition)
    ))

    setDT(domain)

    admissions <- merge(domain, admissions,
                        by = c("age_cat", "sex", "imd_quintile", "Cause"), all = T)

    admissions[is.na(n_admissions), n_admissions := 0]

    admissions <- admissions[ , attrib_admissions := n_admissions * SAF]

    # Merge the admissions and population data
    k_year <- as.numeric(paste0("20", stringr::str_sub(year, start = 1, end = 2)))

    admissions <- merge(admissions, pop_data(k_year, age_start = youngest_age, age_categories = age_cat, age_categories_start = age_cat_start), all = T, by = c("sex", "imd_quintile", "age_cat"))

    # calculate the rate of admissions
    admissions[ , Rate := n_admissions / pop.size, by = c("sex", "imd_quintile", "age_cat")]

    admissions[ , sex := c("Male", "Female")[sex]]

    write.csv(admissions, paste0(dir, "Admission/", substance, "_admission_rates_", year,".csv"), row.names = FALSE)

    #}

    #if(level == "Person-specific"){

      # Allocate an admission to a condition - assign admission to primary risk-related episode
    data <- unique(data, by = c("encrypted_hesid", "spell_id"))

      # Remove duplicate admissions for each individual within the year
      # to leave a dataset consisting of a single alcohol-related diagnosis for each individual in the data.
      # Call this dataset person-specific single morbidity admissions.
      # Select only one row per individual taking the first occurring row.
    data_indiv <- unique(data, by = "encrypted_hesid")

      cat("\tselect only spells associated with main condition for each individual\n")

      # Create an index variable by pasting individual id and the ICD-10 code assigned to a spell.
      data_indiv[, index1 := paste0(encrypted_hesid, "_", Cause)]
      data[, index2 := paste0(encrypted_hesid, "_", Cause)]

      # Create a dataset that only has spells for an individual that are associated
      # with the ICD-10 code assigned to that individual for the year.
      data <- data[index2 %in% unique(hes_indiv$index1)]

      # Get multiplier (age cat)
      data_agecat <- data[ , .(Multiplier = .N), by = c("Cause", "encrypted_hesid", "age_cat", "sex", "imd_quintile")]

      data_agecat <- data_agecat[ , .(
        n_individuals = .N,
        av_multiplier = mean(Multiplier)
        ), by = c("age_cat", "sex", "imd_quintile", "Cause")]

      domain <- data.frame(expand.grid(
        age_cat = age_cat,
        sex = 1:2,
        imd_quintile = c("1_least_deprived", "2", "3", "4", "5_most_deprived"),
        Cause = unique(aaf_lkup$condition)
      ))

      setDT(domain)

      data_agecat <- merge(domain, data_agecat,
                       by = c("age_cat", "sex", "imd_quintile", "Cause"), all = T)

      data_agecat[is.na(n_individuals), n_individuals := 0]
      data_agecat[is.na(av_multiplier), av_multiplier := 0]

      # Merge the admissions and population data
      k_year <- as.numeric(paste0("20", stringr::str_sub(year, start = 1, end = 2)))

      data_agecat <- merge(data_agecat, pop_data(k_year, age_start = youngest_age, age_categories = age_cat, age_categories_start = age_cat_start), all = T, by = c("sex", "imd_quintile", "age_cat"))

      # calculate the rate of admissions
      data_agecat[ , Rate := n_individuals / pop.size]

      data_agecat[ , sex := c("Male", "Female")[sex]]


      write.csv(data_agecat, paste0(dir, "Person-specific/", substance, "_", method, "_person_specific_rates_age_cat_above_", youngest_age, "", year,".csv"), row.names = FALSE)

      # Get multiplier (age)
      #hes_age <- hes[ , .(Multiplier = .N), by = c("Cause", "encrypted_hesid", "startage", "sex", "imd_quintile")]

      #hes_age <- hes_age[ , .(
      #  n_individuals = .N,
      #  av_multiplier = mean(Multiplier)
      #), by = c("startage", "sex", "imd_quintile", "Cause")]

      #hes <- hes_age

      #write.csv(hes_age, paste0(dir, "Person-specific/", substance, "_", method, "_person_specific_rates_single_age_", youngest_age, "", k.year.ind,".csv"), row.names = FALSE)

      }


  }

  return(data)
}
