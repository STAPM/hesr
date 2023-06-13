
#' Clean Scottish Hospital Episode Statistics
#'
#' Basic cleaning operations on each year of Scottish HES data.  
#'
#' Cleaning the data to remove or consistently code 
#' duplicates, dummy dates, and missing data. 
#' Only records with data on the Index of Multiple Deprivation (IMD) decile 
#' in which the patient resides are retained. 
#' If an individual changes IMD quintile, age or sex within the year, 
#' then they are assigned the value for these characteristics that they had on the first admission of the year. 
#'
#' @param hes Data.table - is the raw Scottish HES data that is the output from \code{read_hes_scot()}..
#'
#' @return Returns a data.table that is a cleaned version of the HES data.
#'
#' @export
#'
#' @importFrom data.table := setorderv
#'
#' @examples
#'
#' \dontrun{
#'
#' hes <- read_hes_scot("2022")
#' cleaned_hes <- clean_hes_scot(hes)
#'
#' }
#'
clean_hes_scot <- function(
    hes
) {
  
  # Delete Duplicate Records---
  setorderv(hes,
            Hmisc::Cs(encrypted_hesid, admidate, epiorder, disdate, diag_01, epikey),
            c(1, 1, -1, 1, 1, 1)
  )
  
  # Variables by which to define duplicates
  dupvars <- Hmisc::Cs(encrypted_hesid, admidate, epiorder, disdate, diag_01, mainspef)
  
  # How many duplicate rows are there
  # nrow(hes) - uniqueN(hes, by = dupvars)
  
  # Retain only non-duplicate rows
  hes <- unique(hes, by = dupvars)
  
  rm(dupvars)
  
  # Replace other NA indicators----
  #hes[admimeth %in% c(98, 99), admimeth := NA]
  #hes[epiorder %in% c(98, 99), epiorder := NA]
  
  # Clean dates----
  
  cat("\tcleaning dates\n")
  
  # Dummy dates
  # Thaison replaced any dates < 1900-01-01 with NA for the same 4 fields.
  # Thaison replaced missing dob: "11800" with NA but we don't include this variable.
  # hes[mydob == "11800", mydob := NA]
  
  
  # Convert to date type
  hes[ , disdate := as.Date(paste0(substring(disdate, 1, 4), "-", substring(disdate, 5, 6), "-", substring(disdate, 7, 8)), "%Y-%m-%d")]
  hes[ , admidate := as.Date(paste0(substring(admidate, 1, 4), "-", substring(admidate, 5, 6), "-", substring(admidate, 7, 8)), "%Y-%m-%d")]
  hes[ , epiend := as.Date(paste0(substring(epiend, 1, 4), "-", substring(epiend, 5, 6), "-", substring(epiend, 7, 8)), "%Y-%m-%d")]
  hes[ , epistart := as.Date(paste0(substring(epistart, 1, 4), "-", substring(epistart, 5, 6), "-", substring(epistart, 7, 8)), "%Y-%m-%d")]
  
  #ref_date <- as.Date("1900-01-01", "%Y-%m-%d")
  
  #hes[disdate < ref_date, disdate := NA]
  #hes[admidate < ref_date, admidate := NA]
  #hes[epiend < ref_date, epiend := NA]
  #hes[epistart < ref_date, epistart := NA]
  
  #rm(ref_date)
  
  #dummydates <- as.Date(c("1800-01-01", "1801-01-01", "1600-01-01", "1582-01-01", "1582-10-15"), "%Y-%m-%d")
  
  #hes[disdate %in% dummydates, disdate := NA]
  #hes[admidate %in% dummydates, admidate := NA]
  #hes[epiend %in% dummydates, epiend := NA]
  #hes[epistart %in% dummydates, epistart := NA]
  
  #rm(dummydates)
  
  #hes[ , episodeduration := epiend - epistart]
  
  # Get number of missing dates NOTE
  # summary(hes$admidate)
  # summary(hes$disdate)
  # summary(hes$epistart)
  # summary(hes$epiend)
  
  # Back fill discharge dates----
  
  select_columns <- c("encrypted_hesid", "admidate", "epiorder", "disdate")
  dfm <- hes[, select_columns, with = F] # Take subset of necessary data
  
  nflag <- 1e9 # Define for repeat procedure
  counter <- 0
  
  n <- nrow(dfm)
  
  included <- dfm$encrypted_hesid == dfm$encrypted_hesid[c(2:n, 1)] &
    dfm$admidate == dfm$admidate[c(2:n, 1)] # Is row the same as row below
  
  cat("\t\tcleaning discharge dates\n")
  
  repeat { # Back fill missing discharge dates
    
    counter <- counter + 1
    print(counter)
    
    #system.time(
    flag <- which(
      included &
        !is.na(dfm$disdate) &
        is.na(dfm$disdate[c(2:n, 1)])
    )
    #)
    
    nflag <- length(flag)
    
    #print(nflag)
    
    if(nflag == 0) break
    
    dfm[flag + 1, disdate := dfm[flag, disdate]]
    
  }
  
  #summary(hes$disdate) # How many NAs before
  
  hes[ , disdate := dfm[ , disdate]] # Save backfilled dates in main dataset
  
  #summary(hes$disdate) # Check how many NAs after
  
  rm(dfm, select_columns, included, nflag, counter, flag)
  gc()
  
  # Back fill episode end dates----
  
  setorderv(hes,
            Hmisc::Cs(encrypted_hesid, admidate, epiorder, epiend),
            c(1, 1, -1, 1)
  )
  
  select_columns <- c("encrypted_hesid", "admidate", "epiorder", "epiend")
  dfm <- hes[, select_columns, with = F] # Take subset of necessary data
  
  n <- nrow(dfm)
  
  nflag <- 1e9 # Define for repeat procedure
  counter <- 0
  
  included <- dfm$encrypted_hesid == dfm$encrypted_hesid[c(2:n, 1)] &
    dfm$admidate == dfm$admidate[c(2:n, 1)] # Is row the same as row below
  
  cat("\t\tcleaning episode end dates\n")
  
  repeat { # Back fill missing episode end dates
    
    counter <- counter + 1
    print(counter)
    
    #system.time(
    flag <- which(
      included &
        !is.na(dfm$epiend) &
        is.na(dfm$epiend[c(2:n, 1)])
    )
    #)
    
    nflag <- length(flag)
    
    if(nflag == 0) break
    
    dfm[flag + 1, epiend := dfm[flag, epiend]]
    
  }
  
  #summary(hes$epiend) # How many NAs before
  
  hes[ , epiend := dfm[ , epiend]] # Save backfilled dates in main dataset
  
  #summary(hes$epiend) # Check how many NAs after
  
  rm(dfm, select_columns, included, nflag, counter, flag, n)
  gc()
  
  # Detect any observations admitted outside of year----
  
  # hes_3 <- hes_2_ordered[which(hes_2_ordered$admidate >= "2004-04-01" & hes_2_ordered$admidate <= "2005-03-31"),] # if any fall before this date
  # dim(hes_3) # Check same number
  # rm(hes_2_ordered)
  # gc()
  
  # hes_4 <- hes_3[which(hes_3$epiend >= "2004-04-01" & hes_3$epiend <= "2005-03-31"),] # if any fall before this date
  # dim(hes_4)
  # rm(hes_3)
  # gc()
  
  # Keep necessary data----
  
  #hes <- hes[epistat == 3] # Admission is finished episode
  # for the scottish data, assume that all episodes that have been provided are finished consultant episodes
  
  #hes <- hes[classpat == 1 | classpat == 2 | classpat == 3 | classpat == 4 | classpat == 5]
  # Admission is an ordinary admission or day case or regular day and night attenders e.g. for renal /chemo or maternity
  
  # Recode ages less than 0 (categorised by weeks using codes 7001+) as 0
  #hes[startage %in% 7001:7007, startage := 0]
  #hes[endage %in% 7001:7007, endage := 0]
  
  #table(hes$startage, useNA = "ifany")
  
  # Age falls in range 0-120
  hes <- hes[startage >= 0 & startage <= 120]
  
  # Valid sex
  hes <- hes[sex == 1 | sex == 2]
  
  # 1 = Male, 2 = Female
  hes[ , sex := as.character(sex)]
  hes[sex == "1", sex := "Male"]
  hes[sex == "2", sex := "Female"]
  
  # Valid admission date
  
  hes <- hes[!is.na(admidate)]
  
  hes <- hes[!is.na(simd2016_sc_quintile)]
  
  
  # Some final variable processing----
  
  # Create Index of Multiple Deprivation quintiles from the current deciles
  
  # double check this - Scotland tends to code IMD quintiles backwards
  
  hes[ , imd_quintile := dplyr::recode(as.character(simd2016_sc_quintile),
                                       "1" = "5_most_deprived",
                                       "2" = "4",
                                       "3" = "3",
                                       "4" = "2",
                                       "5" = "1_least_deprived")]
  
  hes <- hes[!is.na(hes$imd_quintile)]
  
  hes[, simd2016_sc_quintile := NULL]
  
  
  # FIXING BUGS ---
  # Individuals could change IMD quintile during the year.
  # Cleanup data by setting an individual's IMD quintile as the one they were
  # at the first time that they were observed in the year.
  
  hes[ , imd_quintile := imd_quintile[which(admidate == min(admidate, na.rm = T))[1]], by = encrypted_hesid]
  
  # Fix bug caused by individuals changing ages between admissions.
  # Set the age of an individual as the age that they were at their first admission in the year.
  hes[ , startage := startage[which(admidate == min(admidate, na.rm = T))[1]], by = encrypted_hesid]
  
  # Fix bug caused by individuals changing ages between episodes.
  # Set the sex of an individual as the sex that they were at their first episode in the year.
  hes[ , sex := sex[which(epistart == min(epistart, na.rm = T))[1]], by = encrypted_hesid]
  
  # Add months
  #hes <- transform(hes, Month = substr(admidate, 6, 7))
  
  # for continuous inpatient spells
  # use the gls_cis_marker provided with the scottish data
  # this is a count specific to each patient
  
  hes[ , spell_id := paste0(as.numeric(as.factor(encrypted_hesid)), "-", gls_cis_marker)]
  
  #################################
  # define dismeth
  # for use in the function define_spells()
  
  # Methods of discharge that dont count as a continuous inpatient spell = 1
  # methods that do = 9
  
  # Death
  # hes[discharge_transfer_to %in% c("00", "01") , dismeth := 1]
  # hes[discharge_type %in% c("40", "41", "42", "43") , dismeth := 1]
  # 
  # # Institution
  # hes[discharge_transfer_to %in% c("20", "24", "25", "26", "28", "29") , dismeth := 1]
  # 
  # # Other
  # hes[discharge_transfer_to %in% c("60", "61", "62", "68", "69", "70") , dismeth := 1]
  # 
  # # Private Residence
  # hes[discharge_transfer_to %in% c("10", "11", "12", "14", "18", "19") , dismeth := 1]
  # 
  # # Temporary
  # hes[discharge_transfer_to %in% c("30", "31", "32", "33", "34", "38", "39") , dismeth := 1]
  # 
  # # Irregular Discharge
  # hes[discharge_type %in% c("20", "21", "22", "23", "28", "29") , dismeth := 1]
  # 
  # # Regular Discharge
  # hes[discharge_type %in% c("10", "11", "18", "19", "70") , dismeth := 1]
  # 
  # # Transfer Within the Same Provider
  # hes[discharge_transfer_to %in% c("40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
  #                                  "4A", "4B", "4C", "4D", "4E", "4F", "4G", "4H") , dismeth := 9]
  # 
  # hes[discharge_transfer_to %in% c("50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
  #                                  "5A", "5B", "5C", "5D", "5E", "5F", "5G", "5H") , dismeth := 9]
  # 
  # # Regular Discharge
  # hes[discharge_type %in% c("12", "13") , dismeth := 9]
  
  
  hes[, discharge_transfer_to := NULL]
  hes[, discharge_type := NULL]
  hes[, procode3 := NULL]
  hes[, gls_cis_marker := NULL]
        
  
  
  return(hes[])
}

















