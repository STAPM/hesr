lkup <- read.csv("//tsclient/X/ScHARR/PR_Disease_Risk_TA/Code/Attributable_fractions/Smoking_attributable_fractions/Output/Archive/YCR_presentation_ICD10_codes.csv")
setDT(lkup)

setnames(lkup,
         c("ICD10_lookups", "condition"),
         c("icd_code", "Description"))


lkup$icd_code <- as.character(lkup$icd_code)

hes[, icd_code := substr(diag_01, 1, 3)]

#hes <- hesr::cruk_split_oesophageal(hes)

hes[ , age_cat := as.character(cut(
  startage,
  c(-1, 17.5, 24.5, 34.5, 49.5, 64.5, 74.5, 121),
  labels = c("16-17", "18-24", "25-34", "35-49", "50-64", "65-74", "75-89")
))]


class(hes[, icd_code]) == class(lkup[, icd_code])

hes <- hes[!is.na(hes$imd_quintile), ]

lkup[duplicated(lkup, by = c("icd_code"))]

# merge the HES data with the data on attributable fractions (AFs in lkup)
# the index variables for this match are 3 character ICD-10, sex, age-band and qimd
hes <- merge(hes, lkup,
             by = c("icd_code"),
             all.x = T, all.y = F, sort = F)


summary <- hes[utla_name_london_groupings %in% c("Sheffield", "Leeds", "Kingston upon Hull, City of") &
                 Description %in% c("Bladder", "Breast", "Colorectal", "Lung", "Kidney", "Prostate"), .(
  n_episodes = .N
), by = c("utla_name_london_groupings", "Description")]

summary$utla_name_london_groupings <- gsub("Kingston upon Hull, City of", "Hull", summary$utla_name_london_groupings)

library(ggplot2)

plot <- ggplot(summary) +
  geom_col(aes(x = Description, y = n_episodes, fill = Description)) +
  facet_wrap(~utla_name_london_groupings) +
  xlab("Cancer type") +
  ylab("Number of episodes") +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey"),
        strip.background = element_rect(color=NA, fill=NA, linetype="solid"),
        strip.text.x = element_text(size = 13),
        axis.text.x = element_text(angle = 45, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

