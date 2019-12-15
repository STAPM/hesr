
# The aim of this code is to prepare the data on local authority lookups



library(data.table)

# Read the local authority lookups 1
local_authority_lookups1 <- fread("X://ScHARR/PR_HES_data_TA/general/Geographical_lookups/ltla_utla.csv", colClasses = "character")

colnames(local_authority_lookups1) <- c("ltla", "utla", "utla_name", "utla_name_london_groupings")
local_authority_lookups1 <- dplyr::select(local_authority_lookups1, ltla, utla, utla_name_london_groupings)
#nrow(lkup1) # 326

#################################################################################################

# Read the local authority lookups 2
# the file from the code-history database that maps old to new ltla codes
local_authority_lookups2 <- fread("X://ScHARR/PR_HES_data_TA/general/Geographical_lookups/Equivalents.csv",
               na.strings = c("NA", ""), colClasses = "character")

# select only the new and old code version columns for ltla
cols <- Hmisc::Cs(GEOGCD, GEOGCDO)
local_authority_lookups2 <- local_authority_lookups2[ , ..cols]
rm(cols)

local_authority_lookups2 <- local_authority_lookups2[stringr::str_length(GEOGCDO) == 4 & stringr::str_detect(GEOGCD, "E")]

# name matching variable following lkup1
setnames(local_authority_lookups2, Hmisc::Cs(GEOGCD, GEOGCDO), Hmisc::Cs(ltla, resladst))

# select only the unique rows
local_authority_lookups2 <- unique(local_authority_lookups2, by = Hmisc::Cs(ltla, resladst))


#####################################################################################################


local_authority_lookups3 <- fread("X://ScHARR/PR_HES_data_TA/general/Geographical_lookups/Equivalents_extra.csv",
               na.strings = c("NA", ""), colClasses = "character")

setnames(local_authority_lookups3, "LA_code", "resladst")

######################################################################################################
# merge the lookup files
lkup <- merge(local_authority_lookups1, local_authority_lookups2, by = "ltla", all.x = T, all.y = F)

local_authority_lookups <- merge(lkup, local_authority_lookups3, by = "resladst", all = T)

local_authority_lookups[LA_name == "Mid Bedfordshire", utla_name_london_groupings := "Central Bedfordshire"]
local_authority_lookups[LA_name == "Bedford", utla_name_london_groupings := "Bedford"]
local_authority_lookups[LA_name == "South Bedfordshire", utla_name_london_groupings := "Central Bedfordshire"]
local_authority_lookups[LA_name == "Chester", utla_name_london_groupings := "Cheshire West and Chester"]
local_authority_lookups[LA_name == "Congleton", utla_name_london_groupings := "Cheshire West and Chester"]
local_authority_lookups[LA_name == "Crewe and Nantwich", utla_name_london_groupings := "Cheshire East"]
local_authority_lookups[LA_name == "Ellesmere Port & Neston", utla_name_london_groupings := "Cheshire West and Chester"]
local_authority_lookups[LA_name == "Macclesfield", utla_name_london_groupings := "Cheshire East"]
local_authority_lookups[LA_name == "Vale Royal", utla_name_london_groupings := "Cheshire West and Chester"]
local_authority_lookups[LA_name == "Caradon", utla_name_london_groupings := "Cornwall"]
local_authority_lookups[LA_name == "Carrick", utla_name_london_groupings := "Cornwall"]
local_authority_lookups[LA_name == "Kerrier", utla_name_london_groupings := "Cornwall"]
local_authority_lookups[LA_name == "North Cornwall", utla_name_london_groupings := "Cornwall"]
local_authority_lookups[LA_name == "Penwith", utla_name_london_groupings := "Cornwall"]
local_authority_lookups[LA_name == "Restormel", utla_name_london_groupings := "Cornwall"]
local_authority_lookups[LA_name == "Isles of Scilly", utla_name_london_groupings := "Cornwall"]
local_authority_lookups[LA_name == "Chester-le-Street", utla_name_london_groupings := "County Durham"]
local_authority_lookups[LA_name == "Derwentside", utla_name_london_groupings := "County Durham"]
local_authority_lookups[LA_name == "Durham", utla_name_london_groupings := "County Durham"]
local_authority_lookups[LA_name == "Easington", utla_name_london_groupings := "County Durham"]
local_authority_lookups[LA_name == "Sedgefield", utla_name_london_groupings := "County Durham"]
local_authority_lookups[LA_name == "Teesdale", utla_name_london_groupings := "County Durham"]
local_authority_lookups[LA_name == "Wear Valley", utla_name_london_groupings := "County Durham"]
local_authority_lookups[LA_name == "Alnwick", utla_name_london_groupings := "Northumberland"]
local_authority_lookups[LA_name == "Berwick-upon-Tweed", utla_name_london_groupings := "Northumberland"]
local_authority_lookups[LA_name == "Blyth Valley", utla_name_london_groupings := "Northumberland"]
local_authority_lookups[LA_name == "Castle Morpeth", utla_name_london_groupings := "Northumberland"]
local_authority_lookups[LA_name == "Tynedale", utla_name_london_groupings := "Northumberland"]
local_authority_lookups[LA_name == "Wansbeck", utla_name_london_groupings := "Northumberland"]
local_authority_lookups[LA_name == "Bridgnorth", utla_name_london_groupings := "Shropshire"]
local_authority_lookups[LA_name == "North Shropshire", utla_name_london_groupings := "Shropshire"]
local_authority_lookups[LA_name == "Oswestry", utla_name_london_groupings := "Shropshire"]
local_authority_lookups[LA_name == "Shrewsbury and Atcham", utla_name_london_groupings := "Shropshire"]
local_authority_lookups[LA_name == "South Shropshire", utla_name_london_groupings := "Shropshire"]
local_authority_lookups[LA_name == "Kennet", utla_name_london_groupings := "Wiltshire"]
local_authority_lookups[LA_name == "North Wiltshire", utla_name_london_groupings := "Wiltshire"]
local_authority_lookups[LA_name == "Salisbury" , utla_name_london_groupings := "Wiltshire"]
local_authority_lookups[LA_name == "West Wiltshire", utla_name_london_groupings := "Wiltshire"]

# check
utla_length <- length(unique(local_authority_lookups$utla_name_london_groupings)) # 120
cat(paste0('\t\t\t ', utla_length, ' utla groupings\n'))

local_authority_lookups[ , LA_name := NULL]
local_authority_lookups[ , ltla := NULL]
local_authority_lookups[ , utla := NULL]

# Embed the data within the package
usethis::use_data(local_authority_lookups, overwrite = TRUE)

rm(local_authority_lookups, local_authority_lookups1, local_authority_lookups2, local_authority_lookups3)
gc()


#########################################################################################################
# Read the local authority lookups 1
local_authority_lookups1 <- fread("X://ScHARR/PR_HES_data_TA/general/Geographical_lookups/ltla_utla.csv", colClasses = "character")

colnames(local_authority_lookups1) <- c("ltla", "utla", "utla_name", "utla_name_london_groupings")
local_authority_lookups1 <- dplyr::select(local_authority_lookups1, ltla, utla_name_london_groupings)

setnames(local_authority_lookups1, "ltla", "resladst_ons")

# Embed the data within the package
usethis::use_data(local_authority_lookups1, overwrite = TRUE)
rm(local_authority_lookups1)
gc()

