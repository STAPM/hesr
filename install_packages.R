

# set local library location if needed
project_lib <- "C:/Users/cm1dog/Documents/R"

# Package names
packages <- c("data.table",
              "ggplot2",
              "cowplot",
              "readxl",
              "knitr",
              "stringr",
              "here",
              "magrittr",
              "RColorBrewer",
              "bookdown",
              "viridis",
              "rmarkdown",
              "readr",
              "dplyr",
              "writexl",
              "flextable",
              "plyr",
              "openxlsx",
              "Hmisc",
              "devtools",
              "tibble",
              "ggthemes", 
              "kableExtra")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], lib = project_lib)
}


