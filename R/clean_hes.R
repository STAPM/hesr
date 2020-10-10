
#' Clean HES
#'
#' This code does the basic cleaning operations on each year of HES data
#'
#' Cleaning the data to remove any duplicates, dummy dates, missing data, and data not required (e.g. admissions that have not ended in this year)
#'
#'
#'
#' @param hes is the raw HES data.
#'
#' @return Returns a cleaned version of the HES data.
#'
#' @export
#'
#' @importFrom data.table := transform
#'
#' @examples
#'
#' \dontrun{
#'
#' hes <- read_hes("0203")
#' cleaned_hes <- clean_hes(hes)
#'
#' }
#'
clean_hes <- function(
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
  hes[admimeth %in% c(98, 99), admimeth := NA]
  hes[epiorder %in% c(98, 99), epiorder := NA]

  # Clean dates----

  cat("\tcleaning dates\n")

  # Dummy dates
  # Thaison replaced any dates < 1900-01-01 with NA for the same 4 fields.
  # Thaison replaced missing dob: "11800" with NA but we don't include this variable.
  # hes[mydob == "11800", mydob := NA]


  dummydates <- c("1800-01-01", "1801-01-01", "1600-01-01", "1582-01-01", "1582-10-15")

  hes[disdate %in% dummydates, disdate := NA]
  hes[admidate %in% dummydates, admidate := NA]
  hes[epiend %in% dummydates, epiend := NA]
  hes[epistart %in% dummydates, epistart := NA]

  rm(dummydates)

  # Convert to date type
  hes[ , disdate := as.Date(disdate, "%Y-%m-%d")]
  hes[ , admidate := as.Date(admidate, "%Y-%m-%d")]
  hes[ , epiend := as.Date(epiend, "%Y-%m-%d")]
  hes[ , epistart := as.Date(epistart, "%Y-%m-%d")]

  hes[ , episodeduration := epiend - epistart]

  # Get number of missing dates NOTE
  # summary(hes$admidate)
  # summary(hes$disdate)
  # summary(hes$epistart)
  # summary(hes$epiend)

  # Back fill discharge dates----

  select_columns <- c("encrypted_hesid", "admidate", "epiorder", "disdate")
  dfm <- hes[, select_columns, with = F] # Take subset of neccessary data

  nflag <- 1e9 # Define for repeat procedure
  counter <- 0

  n <- nrow(dfm)

  included <- dfm$encrypted_hesid == dfm$encrypted_hesid[c(2:n, 1)] &
    dfm$admidate == dfm$admidate[c(2:n, 1)] # Is row the same as row below

  cat("\t\tcleaning discharge dates\n")

  repeat { # Back fill missing dicharge dates

    counter <- counter + 1
    print(counter)

    system.time(flag <- which(
      included &
        !is.na(dfm$disdate) &
        is.na(dfm$disdate[c(2:n, 1)])
    ))

    nflag <- length(flag)

    #print(nflag)

    if(nflag == 0) break

    dfm[flag + 1, disdate := dfm[flag, disdate]]

  }

  summary(hes$disdate) # How many NAs before

  hes[ , disdate := dfm[ , disdate]] # Save backfilled dates in main dataset

  summary(hes$disdate) # Check how many NAs after

  rm(dfm, select_columns, included, nflag, counter, flag)
  gc()

  # Back fill episode end dates----

  setorderv(hes,
            Hmisc::Cs(encrypted_hesid, admidate, epiorder, epiend),
            c(1, 1, -1, 1)
  )

  select_columns <- c("encrypted_hesid", "admidate", "epiorder", "epiend")
  dfm <- hes[, select_columns, with = F] # Take subset of neccessary data

  n <- nrow(dfm)

  nflag <- 1e9 # Define for repeat procedure
  counter <- 0

  included <- dfm$encrypted_hesid == dfm$encrypted_hesid[c(2:n, 1)] &
    dfm$admidate == dfm$admidate[c(2:n, 1)] # Is row the same as row below

  cat("\t\tcleaning episode end dates\n")

  repeat { # Back fill missing episode end dates

    counter <- counter + 1
    print(counter)

    system.time(flag <- which(
      included &
        !is.na(dfm$epiend) &
        is.na(dfm$epiend[c(2:n, 1)])
    ))

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

  # Keep neccessary data----

  hes <- hes[epistat == 3] # Admission is finished episode

  #hes <- hes[classpat == 1 | classpat == 2 | classpat == 3 | classpat == 4 | classpat == 5]
  # Admission is an ordinary admission or day case or regular day and night attenders e.g. for renal /chemo or maternity

  # Recode ages less than 0 (categorised by weeks using codes 7001+) as 0
  hes[startage %in% 7001:7007, startage := 0]
  hes[endage %in% 7001:7007, endage := 0]

  #table(hes$startage, useNA = "ifany")

  # Age falls in range 0-120
  hes <- hes[startage >= 0 & startage <= 120]

  # Valid sex
  hes <- hes[sex == 1 | sex == 2]

  # Only England regions---
  if (k.year.ind %in% c(
    "0203",
    "0304",
    "0405",
    "0506",
    "0607",
    "0708",
    "0809",
    "0910",
    "1011",
    "1112",
    "1213",
    "1314"
  )) {
    hes <- hes[resgor %in% Hmisc::Cs(A, B, C, D, E, F, G, H, J, K)]
  }

  if (k.year.ind %in% c(
    "1415",
    "1516",
    "1617",
    "1718")) {
    hes <- hes[stringr::str_detect(resladst_ons, "E")]
  }

  # Valid admission date

  # relevant for rcp tobacco report
  # this filtering will remove episodes associated with female infertility
  # as these episodes seem to have these data excluded
  hes <- hes[!is.na(admidate)]
  hes <- hes[!is.na(imd04_decile)]


  # Some final variable processing----

  # Create Index of Multiple Deprivation quintiles from the current deciles.
  hes[!is.na(imd04_decile), imd_quintile := dplyr::recode(imd04_decile,
                                                   "Most deprived 10%" = "5_most_deprived",
                                                   "More deprived 10-20%" = "5_most_deprived",
                                                   "More deprived 20-30%" = "4",
                                                   "More deprived 30-40%" = "4",
                                                   "More deprived 40-50%" = "3",
                                                   "Less deprived 40-50%" = "3",
                                                   "Less deprived 30-40%" = "2",
                                                   "Less deprived 20-30%" = "2",
                                                   "Less deprived 10-20%" = "1_least_deprived",
                                                   "Least deprived 10%" = "1_least_deprived")
      ]

  hes <- hes[!is.na(hes$imd_quintile), ]
  hes[, imd04_decile := NULL]


  # FIXING BUGS ---
  # Individuals could change IMD quintile or local authority during the year.
  # Cleanup data by setting an individual's IMD quintile or local authority as the one they were
  # at the first time that they were observed in the year.

  hes[ , imd_quintile := imd_quintile[which(admidate == min(admidate, na.rm = T))[1]], by = encrypted_hesid]

  # Fix bug caused by individuals changing ages between admissions.
  # Set the age of an individual as the age that they were at their first admission in the year.
  hes[ , startage := startage[which(admidate == min(admidate, na.rm = T))[1]], by = encrypted_hesid]

  # Fix bug caused by individuals changing ages between episodes.
  # Set the sex of an individual as the sex that they were at their first episode in the year.
  hes[ , sex := sex[which(epistart == min(epistart, na.rm = T))[1]], by = encrypted_hesid]

  # Add months
  hes <- transform(hes, Month = substr(admidate, 6, 7))


  return(hes[])
}
