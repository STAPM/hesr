
#' Local authority lookup
#'
#' This code adds in the up-to-date local authority identifiers.
#'
#' This code adds in the up-to-date local authority identifiers to fix the issue that in the data we were provided from
#' 2002/03 to 2013/14 lower tier local authority is coded using an old version of the ONS system (that was updated in 2011). From 2014/15
#' The codes in the `RESLADST` field represent the old-style ONS coding system, which is no longer supported by ONS.
#' Old-style codes are not available for new geographies, potentially resulting in a large number now being derived as ‘Y’.
#' Users are advised not to use the 'RESLADST' field, but to use the `RESLADST_ONS` field instead.
#' `RESLADST_ONS` also represents the Local authority district of residence but uses the current ONS coding system.
#'
#'
#'
#' @param hes is the cleaned HES data.
#'
#' @return Returns an updated version of the HES data, adding in the up-to-date local authority identifiers
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
  hes,
  k.year.ind
) {

  cat("\tcleaning local authority info\n")

  if (k.year.ind %in% c(
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
    hes <- merge(hes, hesr::local_authority_lookups, by = "resladst", all.x = T, all.y = F, sort = F)

  }

  if (k.year.ind %in% c(
    "1415",
    "1516",
    "1617",
    "1718")) {

    hes <- merge(hes, hesr::local_authority_lookups1, by = "resladst_ons", all.x = T, all.y = F, sort = F)

    }

  orig.rows <- nrow(hes)

  hes <- hes[!is.na(utla_name_london_groupings)]

  after.rows <- nrow(hes)

  cat(paste0('\t\t\tlost ', orig.rows - after.rows, ' due to missing LA info\n'))
  rm(orig.rows, after.rows)

  hes[ , utla_name_london_groupings := utla_name_london_groupings[which(admidate == min(admidate, na.rm = T))[1]],
       by = encrypted_hesid]

  return(hes[])
}
