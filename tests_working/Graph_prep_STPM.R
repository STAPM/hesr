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

write.csv(HES_2016, "D:/HES/working_data_files/Admissions_rates/Person-specific_age_cat_rates/Person-specific_admissions_2016.csv", row.names = FALSE)

HES_2016 <- HES_2016[ , (N = sum(n_individuals)), by = c("Type", "sex", "imd_quintile")]

write.csv(HES_2016, "D:/HES/working_data_files/Admissions_rates/Person-specific_age_cat_rates/Person-specific_admissions_2016_IMD.csv", row.names = FALSE)



# Total admissions
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


HES_2016 <- HES_2016[ , (N = sum((n_individuals * av_multiplier))), by = c("Type", "sex", "imd_quintile")]

write.csv(HES_2016, "D:/HES/working_data_files/Admissions_rates/Admission/Total_admissions_2016_IMD.csv", row.names = FALSE)






# Costs

costs <- fread("data-raw/Unit cost/Unit_cost_admissions.csv")

costs <- costs[ , Total_costs := unit_cost_admission * n_admissions]

costs <- costs[ , (Cost = sum(Total_costs)), by = c("Cause", "sex", "imd_quintile")]

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

costs <- costs[ , Type := ifelse(Cause %in% Cancers, "Cancers",
                                       ifelse(Cause %in% Respiratory, "Respiratory",
                                              ifelse(Cause %in% Cardiovascular, "Cardiovascular",
                                                     ifelse(Cause %in% `Mental health`, "Mental Health",
                                                            ifelse(Cause %in% Diabetes, "Diabetes",
                                                                   ifelse(Cause %in% Other, "Other",
                                                                          ifelse(Cause %in% `Disorders less common`, "Disorders less common",
                                                                                 "Kidney disease")))))))]

write.csv(costs, "data-raw/Unit cost/Total_costs_2016_sex_IMD.csv", row.names = FALSE)
