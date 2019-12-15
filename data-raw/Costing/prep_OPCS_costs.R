# Read in OPCS lookup table

opcs_costs <- fread("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/OPCS/OPCS_2016.csv")[ , c("type", "cost", "OPCS"), with = F]


# Embed the data within the package
usethis::use_data(opcs_costs, overwrite = TRUE)
