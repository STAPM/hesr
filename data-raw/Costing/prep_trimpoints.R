# Format and clean Trimpoints for 2016

trimpoints <- readxl::read_excel("//tsclient/X/ScHARR/PR_HES_data_TA/Code/Costing/Trimpoints/hrg4__201718_reference_costs_grouper_trimpoints_v1.0.xlsx", sheet = "Trimpoints RC1718 (1617HES)", range = "A1:D2880") %>%
  select(-`Preset Trimpoint`, -`HRG4+ Code Description`, -Year) %>%
  plyr::rename(c("Episode Trimpoint" = "Episode.Trimpoint", "HRG4+ Code" = "sushrg"))


# Embed the data within the package
usethis::use_data(trimpoints, overwrite = TRUE)


