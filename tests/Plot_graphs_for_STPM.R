library(data.table)
library(ggplot2)

# By disease
HES_2016 <- fread("D:/HES/working_data_files/Admissions_rates/Person-specific_age_cat_rates/Tobacco_person_specific_age-cat_rates_1617.csv")

HES_2016 <- HES_2016[ , (N = sum(n_individuals)), by = c("Cause")]

Cancers <- c("Acute_myeloid_leukaemia", "Kidney", "Liver", "Colorectal", "Oral_cavity", "Pancreas", "Nasopharynx_sinonasal",
             "Stomach", "Bladder", "Oesophageal_AC", "Lung", "Oesophageal_SCC", "Larynx", "Lower_urinary_tract", "Cervical")

Respiratory <- c("Chronic_obstructive_pulmonary_disease", "Pneumonia", "Influenza_microbiologically_confirmed", "Asthma",
                 "Tuberculosis", "Obstructive_sleep_apnoea", "Idiopathic_pulmonary_fibrosis")

Cardiovascular <- c("Haemorrhagic_Stroke", "Ischaemic_Stroke", "Ischaemic_heart_disease", "Abdominal_aortic_aneurysm",
                 "Peripheral_arterial_disease", "Venous_thromboembolism")

`Mental health` <- c("Bulimia", "Depression", "Schizophrenia", "Psychosis", "Alzheimers_disease", "Vascular_dementia",
                   "All_cause_dementia")

Diabetes <- c("Diabetes")

Other <- c("Crohns_disease", "Hearing_loss", "Low_back_pain", "Multiple_sclerosis", "Hip_fracture",
           "Age_related_macular_degeneration", "Psoriasis", "Systematic_lupus_erythematosis", "Senile_cataract",
           "Rheumatoid_arthritis")

`Disorders less common` <- c("Ulcerative_colitis", "Parkinson")

`Kidney disease` <- c("End_stage_renal_disease", "Chronic_Kidney_disease")

HES_2016 <- HES_2016[ , Type := ifelse(Cause %in% Cancers, "Cancers",
                                       ifelse(Cause %in% Respiratory, "Respiratory",
                                              ifelse(Cause %in% Cardiovascular, "Cardiovascular",
                                                     ifelse(Cause %in% `Mental health`, "Mental Health",
                                                            ifelse(Cause %in% Diabetes, "Diabetes",
                                                                   ifelse(Cause %in% Other, "Other",
                                                                          ifelse(Cause %in% `Disorders less common`, "Disorders less common",
                                                                                 "Kidney disease")))))))]

ggplot(HES_2016) +
  geom_col(aes(x = Type, y = V1, fill = Cause)) +
  theme_minimal()





## by disease and agecat and sex
# By disease
HES_2016 <- fread("D:/HES/working_data_files/Admissions_rates/Person-specific_age_cat_rates/Tobacco_person_specific_age-cat_rates_1617.csv")

Cancers <- c("Acute_myeloid_leukaemia", "Kidney", "Liver", "Colorectal", "Oral_cavity", "Pancreas", "Nasopharynx_sinonasal",
             "Stomach", "Bladder", "Oesophageal_AC", "Lung", "Oesophageal_SCC", "Larynx", "Lower_urinary_tract", "Cervical")

Respiratory <- c("Chronic_obstructive_pulmonary_disease", "Pneumonia", "Influenza_microbiologically_confirmed", "Asthma",
                 "Tuberculosis", "Obstructive_sleep_apnoea", "Idiopathic_pulmonary_fibrosis")

Cardiovascular <- c("Haemorrhagic_Stroke", "Ischaemic_Stroke", "Ischaemic_heart_disease", "Abdominal_aortic_aneurysm",
                    "Peripheral_arterial_disease", "Venous_thromboembolism")

`Mental health` <- c("Bulimia", "Depression", "Schizophrenia", "Psychosis", "Alzheimers_disease", "Vascular_dementia",
                     "All_cause_dementia")

Diabetes <- c("Diabetes")

Other <- c("Crohns_disease", "Hearing_loss", "Low_back_pain", "Multiple_sclerosis", "Hip_fracture",
           "Age_related_macular_degeneration", "Psoriasis", "Systematic_lupus_erythematosis", "Senile_cataract",
           "Rheumatoid_arthritis")

`Disorders less common` <- c("Ulcerative_colitis", "Parkinson")

`Kidney disease` <- c("End_stage_renal_disease", "Chronic_Kidney_disease")

HES_2016 <- HES_2016[ , Type := ifelse(Cause %in% Cancers, "Cancers",
                                       ifelse(Cause %in% Respiratory, "Respiratory",
                                              ifelse(Cause %in% Cardiovascular, "Cardiovascular",
                                                     ifelse(Cause %in% `Mental health`, "Mental Health",
                                                            ifelse(Cause %in% Diabetes, "Diabetes",
                                                                   ifelse(Cause %in% Other, "Other",
                                                                          ifelse(Cause %in% `Disorders less common`, "Disorders less common",
                                                                                 "Kidney disease")))))))]

HES_2016 <- HES_2016[ , (N = sum(n_individuals)), by = c("Type", "sex", "age_cat")]


ggplot(HES_2016) +
  geom_col(aes(x = Type, y = V1, fill = age_cat), position = "dodge") +
  facet_wrap(~sex, ncol = 1) +
  theme_minimal() +
  ylab("Number of person-specific admissions") +
  theme_minimal() +
  xlab("Age category")



## by disease and IMD
# By disease
HES_2016 <- fread("D:/HES/working_data_files/Admissions_rates/Person-specific_age_cat_rates/Tobacco_person_specific_age-cat_rates_1617.csv")

Cancers <- c("Acute_myeloid_leukaemia", "Kidney", "Liver", "Colorectal", "Oral_cavity", "Pancreas", "Nasopharynx_sinonasal",
             "Stomach", "Bladder", "Oesophageal_AC", "Lung", "Oesophageal_SCC", "Larynx", "Lower_urinary_tract", "Cervical")

Respiratory <- c("Chronic_obstructive_pulmonary_disease", "Pneumonia", "Influenza_microbiologically_confirmed", "Asthma",
                 "Tuberculosis", "Obstructive_sleep_apnoea", "Idiopathic_pulmonary_fibrosis")

Cardiovascular <- c("Haemorrhagic_Stroke", "Ischaemic_Stroke", "Ischaemic_heart_disease", "Abdominal_aortic_aneurysm",
                    "Peripheral_arterial_disease", "Venous_thromboembolism")

`Mental health` <- c("Bulimia", "Depression", "Schizophrenia", "Psychosis", "Alzheimers_disease", "Vascular_dementia",
                     "All_cause_dementia")

Diabetes <- c("Diabetes")

Other <- c("Crohns_disease", "Hearing_loss", "Low_back_pain", "Multiple_sclerosis", "Hip_fracture",
           "Age_related_macular_degeneration", "Psoriasis", "Systematic_lupus_erythematosis", "Senile_cataract",
           "Rheumatoid_arthritis")

`Disorders less common` <- c("Ulcerative_colitis", "Parkinson")

`Kidney disease` <- c("End_stage_renal_disease", "Chronic_Kidney_disease")

HES_2016 <- HES_2016[ , Type := ifelse(Cause %in% Cancers, "Cancers",
                                       ifelse(Cause %in% Respiratory, "Respiratory",
                                              ifelse(Cause %in% Cardiovascular, "Cardiovascular",
                                                     ifelse(Cause %in% `Mental health`, "Mental Health",
                                                            ifelse(Cause %in% Diabetes, "Diabetes",
                                                                   ifelse(Cause %in% Other, "Other",
                                                                          ifelse(Cause %in% `Disorders less common`, "Disorders less common",
                                                                                 "Kidney disease")))))))]

HES_2016 <- HES_2016[ , (N = sum(n_individuals)), by = c("Type", "imd_quintile")]


ggplot(HES_2016) +
  geom_col(aes(x = Type, y = V1, fill = imd_quintile), position = "dodge") +
  theme_minimal() +
  ylab("Number of person-specific admissions") +
  theme_minimal() +
  xlab("Type of condition")





