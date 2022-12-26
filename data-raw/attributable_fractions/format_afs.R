
# Clean and format population attributable fractions

# Reads in lookup table of attributable fractions for either alcohol or tobacco. 
# Formats files into a standard layout.

# The PAF data embedded in the hesr package is meant to serve as an example 
# of the structure that PAF estimates should be in to be used with the functions in the package
# e.g. column headings
# it is not necessary to update the PAFs embedded in the package when new PAFs are calculated
# any new PAFs can just be prepared and input in the project code so long as they match the structure here

# The example PAFs embedded in the package are 
# Alcohol and Smoking Attributable Fractions for morbidity by sex, age-band and 
# Index of Multiple Deprivation quintiles. 
# For a pooled sample of the Scottish Health Survey from 2015 to 2019.

library(readxl)
library(data.table)

# Joint tobacco and alcohol attributable fractions

paf_data <- read_xlsx("//tsclient/X/ScHARR/PR_Disease_Risk_TA/Attrib_fractions/epi_pop_attrib_fractions/50_outputs/PAF_summary_alcohol_and_tobacco_morb_Scotland_2022-12-18.xlsx",
                      sheet = "5 year Sex Age IMD", range = "A2:F4252")

setDT(paf_data)

paf_data[ , year := NULL]

setnames(paf_data, "Population Attributable Fraction", "PAF")


# Embed the data within the package
usethis::use_data(paf_data, overwrite = TRUE)




