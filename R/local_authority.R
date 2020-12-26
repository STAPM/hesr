
#' Local authority lookup \lifecycle{maturing}
#'
#' Add the upper tier local authority identifiers, grouping inner and outer London.
#'
#' The code fixes the 
#' issue that in the data we were provided from
#' 2002/03 to 2013/14 lower tier local authority is coded using an old 
#' version of the ONS system (that was updated in 2011). From 2014/15
#' The codes in the `RESLADST` field represent the old-style ONS coding system, 
#' which is no longer supported by ONS.
#' Old-style codes are not available for new geographies, potentially resulting in a 
#' large number now being derived as ‘Y’.
#' Users are advised not to use the 'RESLADST' field, but to use the `RESLADST_ONS` field instead.
#' `RESLADST_ONS` also represents the local authority district of residence but uses the 
#' current ONS coding system.
#'
#' @param data data.table - the cleaned HES data.
#' @param year Character string - the year of data e.g. "0203".
#' @param LA_lookups_0203 data.table - LTLA to UTLA lookup table - old LTLA system.
#' @param LA_lookups_1415 data.table - LTLA to UTLA lookup table - new LTLA system.
#'
#' @return Returns an updated version of the HES data, adding in the upper tier local authority identifiers.
#' 
#' @importFrom data.table :=
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' hes_2002 <- local_authority(hes, "0203")
#'
#' }
#'
local_authority <- function(
  data,
  year,
  LA_lookups_0203 = hesr::local_authority_lookups,
  LA_lookups_1415 = hesr::local_authority_lookups1
) {

  cat("\tcleaning local authority info\n")

  # For years up to 1314, using old system
  if (year %in% c(
    "0203",
    "0304",
    "0405",
    "0506",
    "0607",
    "0708",
    "0809",
    "0910",
    "1011",
    "1112",
    "1213",
    "1314"
  )) {

    # Merge hes data with local authority lookups
    hes <- merge(data, LA_lookups_0203, by = "resladst", all.x = T, all.y = F, sort = F)

  }

  # For years after 1314, use updated ONS system
  if (year %in% c(
    "1415",
    "1516",
    "1617",
    "1718",
    "1819")) {

    data <- merge(data, LA_lookups_1415, by = "resladst_ons", all.x = T, all.y = F, sort = F)

    }

  ##############################
  # Remove records with missing LA data
  # and check number of records lost to missing LA data
  
  orig.rows <- nrow(data)

  data <- data[!is.na(utla_name_london_groupings)]

  after.rows <- nrow(data)

  cat(paste0('\t\t\tlost ', orig.rows - after.rows, ' due to missing LA info\n'))
  
  rm(orig.rows, after.rows)

  ##############################
  
  # If someone changes LA half way through the year, then assign them the LA that they 
  # had on their first admission of the year
  data[ , utla_name_london_groupings := utla_name_london_groupings[which(admidate == min(admidate, na.rm = T))[1]],
       by = encrypted_hesid]
  

  return(data[])
}
