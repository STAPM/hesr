
# The years of HES data to use.
yrs <- c(
  #"0203",
  #"0304",
  #"0405",
  #"0506",
  #"0607",
  #"0708",
  #"0809",
  #"0910"#,
  #"1011",
  "1112",
  "1213",
  "1314",
  "1415",
  "1516",
  "1617",
  "1718"
)

# test


# For each year:
for (k.year.ind in yrs) {

  cat(k.year.ind)

  hes <- hesr::read_hes(k.year.ind, test = FALSE)
  hes <- hesr::clean_hes(hes)
  hes <- hesr::define_spells(hes)
  hes <- hesr::local_authority(hes, k.year.ind)
  hes <- hesr::append_hrg_codes(hes, k.year.ind)

}


yrs <- c(
  #"0203",
  #"0304",
  #"0405",
  #"0506",
  #"0607",
  #"0708",
  #"0809",
  #"0910",
  #"1011",
  #"1112",
  #"1213",
  #"1314",
  "1415"#,
  #"1516",
  #"1617"
)


# For each year:
for (k.year.ind in yrs) {

  cat(k.year.ind)

  year <- paste0("20", k.year.ind)

  hes <- fread(paste0("D:/HES/working_data_files/cleaned_data/",  year, "_cleaned.csv"))

  admissions <- assign_risk(hes,
                                  k.year.ind,
                                  youngest_age = 11,
                                  age_cat = c("11-15", "16-17", "18-24", "25-44", "45+"),
                                  age_cat_start = c(11, 16, 18, 25, 45),
                                  method = "Broad",
                                  substance = "Alcohol",
                                  level = "Admission")

}

