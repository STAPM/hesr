
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Process Hospital Episode Statistics data <img src="tools/hesr_hex.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

**DRAFT WORKING VERSION** - The package is usable but there are still
bugs and further developments that are being worked through i.e. some
code and documentation is still incomplete or in need of being refined.
The code and documentation are still undergoing internal review by the
analyst team.

## Motivation

`hesr` was created as part of a programme of work on the health
economics of tobacco and alcohol at the School of Health and Related
Research (ScHARR), The University of Sheffield. This programme is based
around the construction of the Sheffield Tobacco and Alcohol Policy
Model (STAPM), which aims to use comparable methodologies to evaluate
the impacts of tobacco and alcohol policies, and investigate the
consequences of clustering and interactions between tobacco and alcohol
consumption behaviours.

The original motivation for `hesr` was to standardised the way that the
Admitted Patient Care (APC) portion of the English Hospital Episode
Statistics (HES) were processed to produce inputs to our
decision-analytic models. The [HES
data](https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics)
is a data warehouse in which the APC portion contains details of all
admissions to hospital. The suite of functions within `hesr` reads the
data for each year since 2002/03, renames, organises and processes the
variables that we use for our analyses.

> Hospital Episode Statistics are accessed via a data sharing agreement
> with NHS Digital.

## Usage

`hesr` is a package for reading, cleaning and analysing the English
Admitted Patient Care Hospital Episode Statistics.

The **inputs** are the raw data files for each year and the list of
diseases attributable to tobacco and/or alcohol (defined by ICD-10
codes).

The **processes** applied by the functions in `hesr` give options to:

1.  Read each year of data.  
2.  Define spells.  
3.  Identify tobacco and alcohol related episodes and spells.  
4.  Calculate rates of hospital admission for tobacco and/or alcohol
    related causes.  
5.  Stratify rates by age, sex, socio-economic conditions (defined by
    Index of Multiple Deprivation) and geographical region.  
6.  Calculate the unit costs of hospital admissions.

The **output** of these processes is a set of rates that describe
admission to hospital, and a set of unit costs for use in health
economic modelling of the effects of changes to tobacco and/or alcohol
consumption.

## Installation

`hesr` is currently available only to members of the project team (but
please contact Duncan Gillespie <duncan.gillespie@sheffield.ac.uk> to
discuss). To access you need to [**sign-up for a GitLab
account**](https://gitlab.com/). You will then need to be added to the
STAPM project team to gain access.

Once that is sorted, you can **install the development version of
`hesr`** from GitLab with:

``` r
#install.packages("devtools")
#install.packages("getPass")

devtools::install_git(
  "https://gitlab.com/stapm/hesr.git", 
  credentials = git2r::cred_user_pass("uname", getPass::getPass()),
  ref = "x.x.x",
  build_vignettes = TRUE
)

# Where uname is your Gitlab user name.
# ref = "x.x.x" is the version to install - remove this to install the latest version
# this should make a box pop up where you enter your GitLab password
```

Then load the package, and some other packages that are useful. Note
that the code within `hesr` uses the `data.table::data.table()` syntax.

``` r
# Load the package
library(hesr)

# Other useful packages
library(dplyr) # for data manipulation and summary
library(magrittr) # for pipes
library(ggplot2) # for plotting
```

## Citation

Please cite the latest version of the package using:  
“Duncan Gillespie, Laura Webster, Colin Angus and Alan Brennan (2020).
hesr: Read, Clean and Analyse English Admitted Patient Care Hospital
Episode Statistics. R package version x.x.x.
<https://stapm.gitlab.io/hesr>.”

## Getting started

In ScHARR, all data is stored and processed according to the [ScHARR
Information Governance
Policy](https://www.sheffield.ac.uk/scharr/research/igov/policy00). The
HES data is stored on the University of Sheffield managed `heta_study`
virtual machine, which is accessible only to team members who are using
data. The working folder on the university networked X-drive is
`PR_HES_data_TA`. No individual-level data is included within this
package.

We have given a brief description of our use of the HES data in the
[privacy
notice](https://www.sheffield.ac.uk/scharr/sections/ph/research/alpol/research/geographic_and_socioeconomic_variation_in_alcohol_and_tobacco_related_hospital_admissions).

## Basic functionality

### Load and clean data

The following code will read and clean the data for 2002/03.

``` r
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

This is example code of summarising the cleaned HES data for the year
2016/17 into alcohol-related episodes using the narrow method. This will
produce a summary table of number of episodes related to each condition
by sex, age group and IMD quintile.

``` r
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

Calculating the cost of admissions in the latest year to assign to all
years. Only need to run the function `hesr::calc_cost_admission()`,
which will read in raw HES data, clean the HES data using both functions
above, match to the reference costs, trimpoints, and OPCS costs to first
calculate unit episode costs. It uses the function `hesr::missing
costs()` to fill in missing costs and create average unit costs of
admissions by condition, sex, IMD quintile, and age category.

``` r
costed_hes <- calc_cost_admission(k.year.ind = "1617")
```

### Assign costs to data

This is example code of assigning the unit admission costs to the
summarised HES data. The first line summarises the data as above, but
reduces the data further down into admission level. This will produce a
summary table of the number of admissions related to alcohol in 2016/17
by condition, sex, age group, and IMD quintile. The second line merges
this summary table with the table of unit admissions costs, which are
also stratified by age group, sex, and IMD quintile.

``` r
hes_data <- assign_risk(
  hes_data,
  "1617",
  method = "Narrow",
  substance = "Alcohol",
  level = "Admission",
  summary = TRUE
)

costed_hes <- merge(
  hes_data,
  hesr::unit_cost_admission,
  by = c("age_cat", "sex", "imd_quintile", "Cause"),
  all.x = T,
  all.y = F
)
```
