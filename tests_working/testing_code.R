# Code for testing functions

# When creating a package
# depends means you don't need to call it.
#install.packages("usethis")
#library(usethis)
#usethis::use_package("data.table", type = "depends")
#usethis::use_package("bit64", type = "depends")
#usethis::use_package("Hmisc")
#usethis::use_package("readxl")
#usethis::use_package("stringr")
#usethis::use_package("dplyr")
#usethis::use_package("plyr")

install.packages("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr_0.1.0.zip", repos = NULL)
library(hesr)


hes_2002 <- read_hes("0203", test = TRUE)
hes_2017 <- read_hes("1718", test = TRUE)

# The years of HES data to use.
yrs <- c(
  #"0203",
  #"0304",
  #"0405",
  #"0506",
  #"0607",
  #"0708",
  #"0809",
  #"0910",
  #"1011",
  #"1112",
  #"1213",
  #"1314",
  #"1415",
  #"1516",
  "1617"#,
  #"1718"
)

# For each year:
for (k.year.ind in yrs) {

  cat(k.year.ind)

  hes <- hesr::read_hes(k.year.ind, test = FALSE)
  hes <- hesr::clean_hes(hes)
  hes <- hesr::define_spells(hes)
  hes <- hesr::local_authority(hes, k.year.ind)
  hes <- hesr::append_hrg_codes(hes, k.year.ind)
}


hes <- hesr::assign_risk(hes, k.year.ind, method = "Narrow", substance = "Alcohol", level = "Episode")

#############################
## Testing costing methods ##
#############################
# Test from cleaned HES data
k.year.ind <- "1617"
hes <- fread(paste0("D:/HES/working_data_files/cleaned_data/cleaned_",  k.year.ind, ".csv"))

hes <- hes[!is.na(startage), ]

hes <- hesr::assign_risk(hes, k.year.ind, method = "Narrow", substance = "Tobacco", level = "Episode", summary = FALSE)

#########################################
# Test admission level and assigning cost
#########################################
k.year.ind <- "1617"
hes <- fread(paste0("D:/HES/working_data_files/cleaned_data/cleaned_",  k.year.ind, ".csv"))

hes <- hesr::assign_risk(hes, k.year.ind, method = "Narrow", substance = "Tobacco", level = "Admission", summary = FALSE)

costed_hes <- merge(hes, hesr::unit_cost_admission, by = c("age_cat", "sex", "imd_quintile", "Cause"), all.x = T, all.y = F)


# Test using 'both' to get costs for ALL conditions we will use for tobacco and alcohol
k.year.ind <- "1617"
hes <- fread(paste0("D:/HES/working_data_files/cleaned_data/cleaned_",  k.year.ind, ".csv"))
hes <- hes[!is.na(startage) ,]

hes <- hesr::assign_risk(hes, k.year.ind, method = "Narrow", substance = "Both", level = "Admission", summary = FALSE)

