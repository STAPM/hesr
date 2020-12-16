
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Process Hospital Episode Statistics data <img src="logo.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Motivation

The motivation for `hesr` was to standardised the way that the Admitted
Patient Care (APC) portion of the English Hospital Episode Statistics
(HES) were processed to produce inputs to our decision-analytic models.
The [HES
data](https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics)
is a data warehouse in which the APC portion contains details of all
admissions to hospital. The suite of functions within `hesr` reads the
data for each year since 2002/03, renames, organises and processes the
variables that we use for our analyses.

`hesr` was created as part of a programme of work on the health
economics of tobacco and alcohol at the School of Health and Related
Research (ScHARR), The University of Sheffield. This programme is based
around the construction of the Sheffield Tobacco and Alcohol Policy
Model (STAPM), which aims to use comparable methodologies to evaluate
the impacts of tobacco and alcohol policies, and investigate the
consequences of clustering and interactions between tobacco and alcohol
consumption behaviours.

## Information governance

Hospital Episode Statistics for England are accessed via a data sharing
agreement with NHS Digital.

In ScHARR, all data is stored and processed according to the [ScHARR
Information Governance
Policy](https://www.sheffield.ac.uk/scharr/research/igov/policy00). The
HES data is stored on the University of Sheffield managed `heta_study`
virtual machine, which is accessible only to team members who are using
data. The working folder on the university networked X-drive is
`PR_HES_data_TA`. No individual-level data is included within this
package.

We have given a brief description of our use of the HES data in the
[privacy notice](https://stapm.gitlab.io/HES_privacy_notice.html).

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
  "https://gitlab.com/stapm/r-packages/hesr.git", 
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
library(data.table)

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
<https://stapm.gitlab.io/r-packages/hesr>.”
