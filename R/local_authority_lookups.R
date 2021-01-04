
#' Local authority lookup table - old ONS system
#'
#' File that contains the geographical lookup information  
#' that maps lower tier local authority codes to upper tier local authorities.
#' LTLA codes are classified by the old system, e.g. "00AA". 
#' For London, UTLAs are grouped into "inner" and "outer" London.
#'
#' @docType data
#'
#' @format A data table (resladst, utla_name_london_groupings).
#'
#' @source
#'
"local_authority_lookups"

#' Local authority lookup table - new ONS system
#'
#' File that contains the geographical lookup information  
#' that maps lower tier local authority codes to upper tier local authorities.
#' LTLA codes are classified by the new ONS system, e.g. "E07000004". 
#' For London, UTLAs are grouped into "inner" and "outer" London.
#'
#' @docType data
#'
#' @format A data table (resladst_ons, utla_name_london_groupings).
#'
#' @source
#'
"local_authority_lookups1"