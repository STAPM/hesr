
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Process Hospital Episode Statistics data <img src="logo.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

[![](https://img.shields.io/badge/doi-10.17605/OSF.IO/AD4VY-green.svg)](https://doi.org/10.17605/OSF.IO/AD4VY)
<!-- badges: end -->

## The Sheffield Tobacco and Alcohol Policy Modelling Platform

This R package was developed as part of the Sheffield Tobacco and
Alcohol Policy Modelling <https://stapm.gitlab.io/> by the [School of
Health and Related Research at the University of
Sheffield](https://www.sheffield.ac.uk/scharr).

The aim of the research programme is to identify and evaluate approaches
to reducing the harm from tobacco and alcohol, with the aim of improving
commissioning in a public health policy context, i.e. providing
knowledge to support benefits achieved by policymakers.

The two objectives of the research programme are:

- To evaluate the health and economic effects of past trends, policy
  changes or interventions that have affected alcohol consumption and/or
  tobacco smoking
- To appraise the health and economic outcomes of potential future
  trends, changes to alcohol and/or tobacco policy or new interventions

The STAPM modelling is not linked to the tobacco or alcohol industry and
is conducted without industry funding or influence.

## Purpose of making the code open source

The code has been made open source for the following two reasons:

- Transparency. Open science, allowing review and feedback to the
  project team on the code and methods used.
- Methodology sharing. For people to understand the code and methods
  used so they might use aspects of it in their own work, e.g., because
  they are doing something partially related that isn’t exactly the same
  job and might like to ‘dip into’ elements of this code for
  inspiration.

## Stage of testing and development

The code is actively being used in project work. It is being reviewed
and developed all the time; more tests and checks are being added.

The repository is not intended to be maintained by an open source
community wider than the development team.

## Disease lists

The list of diseases considered in the modelling is available here
<https://osf.io/v945r>

## Data checks

Data checks are visualisations that show the results of data processing
using the hesr package.

- [Tobcco and alcohol related hospital admissions in
  Scotland](https://stapm.gitlab.io/model-inputs/hosp_tobalc_scot_nat/scottish_hosp_rates_data_report.html)

## Code repositories

The code on Github (<https://github.com/STAPM/hesr>) is a mirror of the
code in a private Gitlab repository where the actual development takes
place (<https://gitlab.com/stapm/r-packages/hesr>). The code in the
Github repository is linked to a repository on the Open Science
Framework, which provides the doi for the package citation
(<https://osf.io/ad4vy/>).

## Citation

Gillespie D, Webster L, Angus C, Brennan A (\[YEAR\]). hesr: An R
Package for Processing Hospital Episode Statistics Data. R package
version \[x.x.x\]. University of Sheffield.
<https://stapm.gitlab.io/r-packages/hesr/>. doi:
<https://doi.org/10.17605/OSF.IO/AD4VY>

## Motivation

The motivation for `hesr` was to standardised the way that the Admitted
Patient Care (APC) portion of the English Hospital Episode Statistics
(HES) were processed to produce inputs to our decision-analytic models
(<https://stapm.gitlab.io/hes_data_england.html>). Functions have
subsequently been added to process equivalent Scottish Morbidity Record
(SMR01) data
(<https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=5>) for
the Scottish version of the STAPM modelling
(<https://stapm.gitlab.io/hes_data_scotland.html>).

`hesr` was created as part of a programme of work on the health
economics of tobacco and alcohol at the School of Health and Related
Research (ScHARR), The University of Sheffield. This programme is based
around the construction of the Sheffield Tobacco and Alcohol Policy
Model (STAPM), which aims to use comparable methodologies to evaluate
the impacts of tobacco and alcohol policies, and investigate the
consequences of clustering and interactions between tobacco and alcohol
consumption behaviours.

## Data

The data contains details of all admissions to hospital. Each HES record
contains a wide range of information about a patient including clinical
information about diagnoses and operations, patient information such as
age, sex, and socio-economic status, and administrative information such
as dates of arrival.

## Information governance

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

The suite of functions within `hesr` reads the data for each year,
renames, organises and processes the variables that we use for our
analyses.

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

`hesr` is publicly available via Github.

By default the user should install the latest tagged version of the
package. Otherwise, if you want to reproduce project work and know the
version of the package used, install that version.

If on a University of Sheffield managed computer, install the R, RStudio
and Rtools bundle from the Software Centre. Install Rtools - using the
[installr](https://cran.r-project.org/web/packages/installr/index.html)
package can make this easier. Then install the latest or a specified
version of `hesr` from Github with:

``` r
#install.packages("devtools")

devtools::install_git(
  "https://github.com/stapm/hesr.git", 
  ref = "x.x.x",
  build_vignettes = FALSE)

# ref = "x.x.x" is the version to install - change to the version you want e.g. "1.2.3"
```

Or clone the package repo locally and use the ‘install and restart’
button in the Build tab of RStudio. This option is more convenient when
testing development versions.

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
