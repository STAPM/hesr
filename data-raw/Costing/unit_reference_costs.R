# Format and clean Reference Costs
# Combining the reference costs for different types of admission and years.

  # Elective #
  # 2016
    elective_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2016.xlsx", sheet = "EL", range = "A5:D2459") %>%
    mutate(type = "Elective", Year = "2016") %>%
      plyr::rename(c("Number of FCEs\r\n" = "FCEs", "National Average Unit Cost" = "Unit_cost"))

    elective_excess_bed_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2016.xlsx", sheet = "EL_XS", range = "A5:D1887") %>%
    plyr::rename(c("National Average Unit Cost" = "Excess_cost", "Excess Bed days" = "Days"))

    elective_costs_2016 <- merge(elective_costs_2016, elective_excess_bed_costs_2016, by = c("Currency Code", "Currency Description")) %>%
    select(-Days, -FCEs)
    rm(elective_excess_bed_costs_2016)

  # 2015
    elective_costs_2015 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2015.xlsx", sheet = "EL", range = "A5:D2459") %>%
    mutate(type = "Elective", Year = "2015") %>%
    plyr::rename(c("Number of FCEs\r\n" = "FCEs", "National Average Unit Cost" = "Unit_cost"))

    elective_excess_bed_costs_2015 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2015.xlsx", sheet = "EL_XS", range = "A5:D1875") %>%
    plyr::rename(c("National Average Unit Cost" = "Excess_cost", "Excess Bed days" = "Days"))

    elective_costs_2015 <- merge(elective_costs_2015, elective_excess_bed_costs_2015, by = c("Currency Code", "Currency Description")) %>%
    select(-Days, -FCEs)
    rm(elective_excess_bed_costs_2015)

  # 2014
  elective_costs_2014 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2014.xlsx", sheet = "EL", range = "A5:D2418")
  setDT(elective_costs_2014)
  elective_costs_2014 <- elective_costs_2014[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Elective", Year = "2014")]

  elective_excess_bed_costs_2014 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2014.xlsx", sheet = "EL_XS", range = "A4:D1858")
  setDT(elective_excess_bed_costs_2014)
  elective_excess_bed_costs_2014 <- elective_excess_bed_costs_2014[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  elective_costs_2014 <- merge(elective_costs_2014, elective_excess_bed_costs_2014, by = c("Currency Code", "Currency Description"))
  rm(elective_excess_bed_costs_2014)

  # 2013
  elective_costs_2013 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2013.xlsx", sheet = "EL", range = "A5:D2001")
  setDT(elective_costs_2013)
  elective_costs_2013 <- elective_costs_2013[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Elective", Year = "2013")]

  elective_excess_bed_costs_2013 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2013.xlsx", sheet = "EL_XS", range = "A5:D1626")
  setDT(elective_excess_bed_costs_2013)
  elective_excess_bed_costs_2013 <- elective_excess_bed_costs_2013[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  elective_costs_2013 <- merge(elective_costs_2013, elective_excess_bed_costs_2013, by = c("Currency Code", "Currency Description"))
  rm(elective_excess_bed_costs_2013)

  # 2012
  elective_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "A4:B2090")
  costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "G4:G2090") %>%
    plyr::rename(c("Unit cost" = "Unit_cost")) %>%
    mutate(type = "Elective", Year = "2012")
  elective_costs_2012 <- cbind(elective_costs_2012, costs_2012)

  elective_excess_bed_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "A4:B2090")
  bed_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "J4:J2090") %>%
   plyr::rename(c("Unit cost" = "Excess_cost"))
  elective_excess_bed_costs_2012 <- cbind(elective_excess_bed_costs_2012, bed_costs_2012)
  elective_costs_2012 <- merge(elective_costs_2012, elective_excess_bed_costs_2012, by = c("Currency Code", "Currency Description"))
  rm(elective_excess_bed_costs_2012, costs_2012, bed_costs_2012)

  # 2011
  elective_costs_2011 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2011.xlsx", sheet = "EI", range = "A4:D1372")
  setDT(elective_costs_2011)
  elective_costs_2011 <- elective_costs_2011[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Elective", Year = "2011")]

  elective_excess_bed_costs_2011 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2011.xlsx", sheet = "EI_XS", range = "A4:D1066")
  setDT(elective_excess_bed_costs_2011)
  elective_excess_bed_costs_2011 <- elective_excess_bed_costs_2011[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  elective_costs_2011 <- merge(elective_costs_2011, elective_excess_bed_costs_2011, by = c("Currency Code", "Currency Description"))
  rm(elective_excess_bed_costs_2011)

  # 2010
  elective_costs_2010 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2010.xlsx", sheet = "TEI", range = "A9:D1302")
  setDT(elective_costs_2010)
  elective_costs_2010 <- elective_costs_2010[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Elective", Year = "2010")]

  elective_excess_bed_costs_2010 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2010.xlsx", sheet = "TEIXS", range = "A9:D1007")
  setDT(elective_excess_bed_costs_2010)
  elective_excess_bed_costs_2010 <- elective_excess_bed_costs_2010[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  elective_costs_2010 <- merge(elective_costs_2010, elective_excess_bed_costs_2010, by = c("Currency Code", "Currency Description"))
  rm(elective_excess_bed_costs_2010)



  ##########################
  # Non-elective Long stay #
  ##########################
  # 2016
  non_elective_long_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2016.xlsx", sheet = "NEL", range = "A5:D2338")
  setDT(non_elective_long_costs_2016)
  non_elective_long_costs_2016 <- non_elective_long_costs_2016[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_LS", Year = "2016")]

  non_elective_excess_bed_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2016.xlsx", sheet = "NEL_XS", range = "A5:D2081")
  setDT(non_elective_excess_bed_costs_2016)
  non_elective_excess_bed_costs_2016 <- non_elective_excess_bed_costs_2016[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  non_elective_long_costs_2016 <- merge(non_elective_long_costs_2016, non_elective_excess_bed_costs_2016, by = c("Currency Code", "Currency Description"))
  rm(non_elective_excess_bed_costs_2016)

  # 2015
  non_elective_long_costs_2015 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2015.xlsx", sheet = "NEL", range = "A5:D2307")
  setDT(non_elective_long_costs_2015)
  non_elective_long_costs_2015 <- non_elective_long_costs_2015[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_LS", Year = "2015")]

  non_elective_excess_bed_costs_2015 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2015.xlsx", sheet = "NEL_XS", range = "A5:D2038")
  setDT(non_elective_excess_bed_costs_2015)
  non_elective_excess_bed_costs_2015 <- non_elective_excess_bed_costs_2015[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  non_elective_long_costs_2015 <- merge(non_elective_long_costs_2015, non_elective_excess_bed_costs_2015, by = c("Currency Code", "Currency Description"))
  rm(non_elective_excess_bed_costs_2015)

  # 2014
  non_elective_long_costs_2014 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2014.xlsx", sheet = "NEL", range = "A5:D2303")
  setDT(non_elective_long_costs_2014)
  non_elective_long_costs_2014 <- non_elective_long_costs_2014[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_LS", Year = "2014")]

  non_elective_excess_bed_costs_2014 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2014.xlsx", sheet = "NEL_XS", range = "A5:D2040")
  setDT(non_elective_excess_bed_costs_2014)
  non_elective_excess_bed_costs_2014 <- non_elective_excess_bed_costs_2014[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  non_elective_long_costs_2014 <- merge(non_elective_long_costs_2014, non_elective_excess_bed_costs_2014, by = c("Currency Code", "Currency Description"))
  rm(non_elective_excess_bed_costs_2014)

  # 2013
  non_elective_long_costs_2013 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2013.xlsx", sheet = "NEL", range = "A5:D1920")
  setDT(non_elective_long_costs_2013)
  non_elective_long_costs_2013 <- non_elective_long_costs_2013[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_LS", Year = "2013")]

  non_elective_excess_bed_costs_2013 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2013.xlsx", sheet = "NEL_XS", range = "A5:D1729")
  setDT(non_elective_excess_bed_costs_2013)
  non_elective_excess_bed_costs_2013 <- non_elective_excess_bed_costs_2013[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  non_elective_long_costs_2013 <- merge(non_elective_long_costs_2013, non_elective_excess_bed_costs_2013, by = c("Currency Code", "Currency Description"))
  rm(non_elective_excess_bed_costs_2013)

  # 2012
  non_elective_long_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "A4:B2090")
  costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "M4:M2090") %>%
    plyr::rename(c("Unit cost" = "Unit_cost")) %>%
    mutate(type = "Non_Elective_LS", Year = "2012")
  non_elective_long_costs_2012 <- cbind(non_elective_long_costs_2012, costs_2012)

  non_elective_excess_bed_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "A4:B2090")
  bed_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "P4:P2090") %>%
    plyr::rename(c("Unit cost" = "Excess_cost"))
  non_elective_excess_bed_costs_2012 <- cbind(non_elective_excess_bed_costs_2012, bed_costs_2012)
  non_elective_long_costs_2012 <- merge(non_elective_long_costs_2012, non_elective_excess_bed_costs_2012, by = c("Currency Code", "Currency Description"))
  rm(non_elective_excess_bed_costs_2012, costs_2012, bed_costs_2012)

  # 2011
  non_elective_long_costs_2011 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2011.xlsx", sheet = "NEI_L", range = "A4:D1344")
  setDT(non_elective_long_costs_2011)
  non_elective_long_costs_2011 <- non_elective_long_costs_2011[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_LS", Year = "2011")]

  non_elective_excess_bed_costs_2011 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2011.xlsx", sheet = "NEI_L_XS", range = "A4:D1119")
  setDT(non_elective_excess_bed_costs_2011)
  non_elective_excess_bed_costs_2011 <- non_elective_excess_bed_costs_2011[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  non_elective_long_costs_2011 <- merge(non_elective_long_costs_2011, non_elective_excess_bed_costs_2011, by = c("Currency Code", "Currency Description"))
  rm(non_elective_excess_bed_costs_2011)

  # 2010
  non_elective_long_costs_2010 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2010.xlsx", sheet = "TNEI_L", range = "A9:D1273")
  setDT(non_elective_long_costs_2010)
  non_elective_long_costs_2010 <- non_elective_long_costs_2010[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_LS", Year = "2010")]

  non_elective_excess_bed_costs_2010 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2010.xlsx", sheet = "TNEI_L_XS", range = "A9:D1091")
  setDT(non_elective_excess_bed_costs_2010)
  non_elective_excess_bed_costs_2010 <- non_elective_excess_bed_costs_2010[, .(`Currency Code`, `Currency Description`, Excess_cost = `National Average Unit Cost`)]

  non_elective_long_costs_2010 <- merge(non_elective_long_costs_2010, non_elective_excess_bed_costs_2010, by = c("Currency Code", "Currency Description"))
  rm(non_elective_excess_bed_costs_2010)

  ###########################
  # Non-elective Short stay #
  ###########################
  # 2016
  non_elective_short_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2016.xlsx", sheet = "NES", range = "A5:D2421")
  setDT(non_elective_short_costs_2016)
  non_elective_short_costs_2016 <- non_elective_short_costs_2016[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_SS", Year = "2016", Excess_cost = as.numeric(0))]

  # 2015
  non_elective_short_costs_2015 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2015.xlsx", sheet = "NES", range = "A5:D2370")
  setDT(non_elective_short_costs_2015)
  non_elective_short_costs_2015 <- non_elective_short_costs_2015[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_SS", Year = "2015", Excess_cost = as.numeric(0))]

  # 2014
  non_elective_short_costs_2014 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2014.xlsx", sheet = "NES", range = "A5:D2367")
  setDT(non_elective_short_costs_2014)
  non_elective_short_costs_2014 <- non_elective_short_costs_2014[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_SS", Year = "2014", Excess_cost = as.numeric(0))]

  # 2013
  non_elective_short_costs_2013 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2013.xlsx", sheet = "NES", range = "A5:D1967")
  setDT(non_elective_short_costs_2013)
  non_elective_short_costs_2013 <- non_elective_short_costs_2013[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_SS", Year = "2013", Excess_cost = as.numeric(0))]

  # 2012
  non_elective_short_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "A4:B2090")
  costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "S4:S2090") %>%
    plyr::rename(c("Unit cost" = "Unit_cost")) %>%
    mutate(type = "Non_Elective_SS", Year = "2012",
           Excess_cost = as.numeric(0))
  non_elective_short_costs_2012 <- cbind(non_elective_short_costs_2012, costs_2012)
  rm(costs_2012)

  # 2011
  non_elective_short_costs_2011 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2011.xlsx", sheet = "NEI_S", range = "A4:D1317")
  setDT(non_elective_short_costs_2011)
  non_elective_short_costs_2011 <- non_elective_short_costs_2011[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_SS", Year = "2011", Excess_cost = as.numeric(0))]

  # 2010
  non_elective_short_costs_2010 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2010.xlsx", sheet = "TNEI_S", range = "A9:D1262")
  setDT(non_elective_short_costs_2010)
  non_elective_short_costs_2010 <- non_elective_short_costs_2010[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Non_Elective_SS", Year = "2010", Excess_cost = as.numeric(0))]


  ############
  # Day case #
  ############
  # 2016
  day_case_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2016.xlsx", sheet = "DC", range = "A5:D2181")
  setDT(day_case_costs_2016)
  day_case_costs_2016 <- day_case_costs_2016[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Day_case", Year = "2016", Excess_cost = as.numeric(0))]


  # 2015
  day_case_costs_2015 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2015.xlsx", sheet = "DC", range = "A5:D2107")
  setDT(day_case_costs_2015)
  day_case_costs_2015 <- day_case_costs_2015[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Day_case", Year = "2015", Excess_cost = as.numeric(0))]

  #2014
  day_case_costs_2014 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2014.xlsx", sheet = "DC", range = "A4:D2062")
  setDT(day_case_costs_2014)
  day_case_costs_2014 <- day_case_costs_2014[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Day_case", Year = "2014", Excess_cost = as.numeric(0))]

  #2013
  day_case_costs_2013 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2013.xlsx", sheet = "DC", range = "A5:D1774")
  setDT(day_case_costs_2013)
  day_case_costs_2013 <- day_case_costs_2013[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Day_case", Year = "2013", Excess_cost = as.numeric(0))]


  #2012
  day_case_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "A4:B2090")
  costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "V4:V2090") %>%
    plyr::rename(c("Unit cost" = "Unit_cost")) %>%
    mutate(type = "Day_case", Year = "2012",
           Excess_cost = as.numeric(0))
  day_case_costs_2012 <- cbind(day_case_costs_2012, costs_2012)
  rm(costs_2012)

  #2011
  day_case_costs_2011 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2011.xlsx", sheet = "DC", range = "A4:D1229")
  setDT(day_case_costs_2011)
  day_case_costs_2011 <- day_case_costs_2011[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Day_case", Year = "2011", Excess_cost = as.numeric(0))]

  #2010
  day_case_costs_2010 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2010.xlsx", sheet = "TDC", range = "A9:D1203")
  setDT(day_case_costs_2010)
  day_case_costs_2010 <- day_case_costs_2010[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "Day_case", Year = "2010", Excess_cost = as.numeric(0))]


  ########################
  # Regular day or night #
  ########################
  # 2016
  DCRA_costs_2016 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2016.xlsx", sheet = "RP", range = "A5:D1121")
  setDT(DCRA_costs_2016)
  DCRA_costs_2016 <- DCRA_costs_2016[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "DCRA", Year = "2016", Excess_cost = as.numeric(0))]

  # 2015
  DCRA_costs_2015 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2015.xlsx", sheet = "RP", range = "A5:D1116")
  setDT(DCRA_costs_2015)
  DCRA_costs_2015 <- DCRA_costs_2015[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "DCRA", Year = "2015", Excess_cost = as.numeric(0))]

  # 2014
  DCRA_costs_2014 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2014.xlsx", sheet = "RP", range = "A5:D1105")
  setDT(DCRA_costs_2014)
  DCRA_costs_2014 <- DCRA_costs_2014[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "DCRA", Year = "2014", Excess_cost = as.numeric(0))]

  # 2013
  DCRA_costs_2013 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2013.xlsx", sheet = "RP", range = "A5:D923")
  setDT(DCRA_costs_2013)
  DCRA_costs_2013 <- DCRA_costs_2013[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "DCRA", Year = "2013", Excess_cost = as.numeric(0))]

  # 2012
  DCRA_costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "A4:B2090")
  costs_2012 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2012.xlsx", sheet = "Total - HRGs", range = "Y4:Y2090") %>%
    plyr::rename(c("Unit cost" = "Unit_cost")) %>%
    mutate(type = "DCRA", Year = "2012",
           Excess_cost = as.numeric(0))
  DCRA_costs_2012 <- cbind(DCRA_costs_2012, costs_2012)
  rm(costs_2012)

  # 2011
  DCRA_costs_2011 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2011.xlsx", sheet = "DCRA", range = "A4:D717")
  setDT(DCRA_costs_2011)
  DCRA_costs_2011 <- DCRA_costs_2011[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "DCRA", Year = "2011", Excess_cost = as.numeric(0))]

  # 2010
  DCRA_costs_2010 <- readxl::read_excel("//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/National_schedule_of_reference_costs_2010.xlsx", sheet = "TDCRA", range = "A9:D681")
  setDT(DCRA_costs_2010)
  DCRA_costs_2010 <- DCRA_costs_2010[, .(`Currency Code`, `Currency Description`, Unit_cost = `National Average Unit Cost`, type = "DCRA", Year = "2010", Excess_cost = as.numeric(0))]



  # bind all data tables to make one data table of all admissions types
  reference_costs <-
    rbindlist(list(
      #2016
      elective_costs_2016,
      non_elective_long_costs_2016,
      non_elective_short_costs_2016,
      day_case_costs_2016,
      DCRA_costs_2016,
      #2015
      elective_costs_2015,
      non_elective_long_costs_2015,
      non_elective_short_costs_2015,
      day_case_costs_2015,
      DCRA_costs_2015,
      #2014
      elective_costs_2014,
      non_elective_long_costs_2014,
      non_elective_short_costs_2014,
      day_case_costs_2014,
      DCRA_costs_2014,
      #2013
      elective_costs_2013,
      non_elective_long_costs_2013,
      non_elective_short_costs_2013,
      day_case_costs_2013,
      DCRA_costs_2013,
      #2012
      elective_costs_2012,
      non_elective_long_costs_2012,
      non_elective_short_costs_2012,
      day_case_costs_2012,
      DCRA_costs_2012,
      #2011
      elective_costs_2011,
      non_elective_long_costs_2011,
      non_elective_short_costs_2011,
      day_case_costs_2011,
      DCRA_costs_2011,
      #2010
      elective_costs_2010,
      non_elective_long_costs_2010,
      non_elective_short_costs_2010,
      day_case_costs_2010,
      DCRA_costs_2010
      ), use.names = T) %>%
    plyr::rename(c("Currency Code" = "sushrg",
                   "Currency Description" = "description",
                   "Unit_cost" = "cost",
                   "Excess_cost" = "Excess_unit_cost"))

  # Delete Duplicate Records
  setorder(reference_costs, sushrg, -Year)
  head(reference_costs)

  reference_costs <- unique(reference_costs[sushrg != ""], by = c("sushrg", "type"))


  n_rows <- nrow(reference_costs)
  cat(paste0('\t\t\t ', n_rows, ' rows of reference costs\n'))

  #write.csv(reference_costs, "//tsclient/X/ScHARR/PR_STAPM/Code/R_packages/hesr/data-raw/Costing/Reference_costs_2010-16.csv")

  # Embed the data within the package
  usethis::use_data(unit_reference_costs, overwrite = TRUE)
