# hesr

> functions to read, clean and analyse English admitted patient care hospital episode statistics data.

## Authors
Developed by Laura Webster and Duncan Gillespie

## License
CCBY-4.0

## Usage

### Load and clean data
The following code will read and clean the data for 2002/03.  

```r
hes_data <- function() {

  data <- read_hes("0203")
  data <- clean_hes(data)
  data <- define_spells(data)
  data <- local_authority(data, "0203")
  data <- append_hrg_codes(data, "0203")
  
return(data)
}
```

### Summarise data
This is example code of summarising the cleaned HES data for the year 2016/17 into alcohol-related episodes using the narrow method. This will produce a summary table of number of episodes related to each condition by sex, age group and IMD quintile.  

```r
hes_data <- assign_risk(hes = hes_data, k.year.ind = "1617", 
  youngest_age = 16,
  age_cat = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89"),
  age_cat_start = c(16, 18, 25, 35, 50, 65, 75),
  method = "Narrow", 
  substance = "Alcohol",
  aaf_lkup = read.csv("//tsclient/X/ScHARR/PR_Disease_Risk_TA/Code/Attributable_fractions/Alcohol_attributable_fractions/Output/Archive/aafs_long_2016.csv"),
  level = "Episode", 
  summary = TRUE,  
  dir = "D:/HES/working_data_files")
```
### Calculate costs 
Calculating the cost of admissions in the latest year (2016) to assign to all years. Only need to run the function `calc_cost_admission()`, which will read in raw HES data, clean the HES data using both functions above, match to the reference costs, trimpoints, and OPCS costs to first calculate unit episode costs. It uses the function `missing costs()` to fill in missing costs and create average unit costs of admissions by condition, sex, IMD quintile, and age category. 

```r
costed_hes <- calc_cost_admission(k.year.ind = "1617")
```
### Assign costs to data
This is example code of assigning the unit admission costs to the summarised HES data. The first line summarises the data as above, but reduces the data further down into admission level. This will produce a summary table of the number of admissions related to alcohol in 2016/17 by condition, sex, age group, and IMD quintile. The second line merges this summary table with the table of unit admissions costs, which are also stratified by age group, sex, and IMD quintile. 

```r
hes_data <- assign_risk(hes_data, "1617", method = "Narrow", substance = "Alcohol", level = "Admission", summary = TRUE)

costed_hes <- merge(hes_data, hesr::unit_cost_admission, by = c("age_cat", "sex", "imd_quintile", "Cause"), all.x = T, all.y = F)
```
## Installation
You can install the package from github as follows:

```r
devtools::install_github("dosgillespie/hesr", build_vignettes = T)

vignette("cleaning_and_costing", "hesr")

```















