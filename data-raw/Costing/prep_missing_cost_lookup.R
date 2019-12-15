# Read in missing cost lookup sheet

missing_cost_lookup <- fread("data-raw/Costing/Missing_cost_lookup.csv")

# Embed the data within the package
usethis::use_data(missing_cost_lookup, overwrite = TRUE)


#######################################
missing_cost_both_lookup <- fread("data-raw/Costing/Missing_cost_alc_tob_lookup.csv")

# Embed the data within the package
usethis::use_data(missing_cost_both_lookup, overwrite = TRUE)

