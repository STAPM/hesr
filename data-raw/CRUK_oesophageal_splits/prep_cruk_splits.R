# The aim of this code is to prepare the data on the proportion of oesophageal cases split by AC and SCC
#
# We use the CRUK splits for oesophageal cases
# The data are held in the disease risk folder PR_Disease_Risk_TA/General/CRUK work/Oesophageal AC and SCC data v2.xlsx
# The data is split by age groups: 1-4, 5-9, 19-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85+ and by sex.
#

# This code loads info from CRUK paper (they use 2014 data to inform the split)
library(readxl)
library(data.table)

cruk_splits <- read_excel("//tsclient/X/ScHARR/PR_Disease_Risk_TA/General/CRUK work/Oesophageal AC and SCC data v2.xlsx", sheet = "Sheet1", range = "D5:E23")

ages <- read_excel("//tsclient/X/ScHARR/PR_Disease_Risk_TA/General/CRUK work/Oesophageal AC and SCC data v2.xlsx", sheet = "Sheet1", range = "A5:A23")

cruk_splits <- cbind(ages, cruk_splits)

colnames(cruk_splits) <- c("cruk_ageband", "Male", "Female")

setDT(cruk_splits)

cruk_splits <- melt(cruk_splits, id.vars = "cruk_ageband", variable.name = "sex", value.name = "proportion_scc")

cruk_splits$sex <- as.integer(cruk_splits$sex)

# Convert numeric index of sex in CRUK splits data to "Male" / "Female"
cruk_splits[ , sex := as.character(sex)]
cruk_splits[sex == "1", sex := "Male"]
cruk_splits[sex == "2", sex := "Female"]

# Embed the data within the package
usethis::use_data(cruk_splits, overwrite = TRUE)

rm(cruk_splits)
gc()


