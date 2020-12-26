
#' Define spells \lifecycle{maturing}
#'
#' This code defines unique admissions.
#'
#' Groups finished consultant episodes into continuous inpatient super-spells using 
#' the method developed by the Centre for Health Economics in York.
#' This groups episodes of care into admissions even if that admission contains episodes 
#' of care under different healthcare providers.
#' The method applies two rules:
#' \enumerate{
#'   \item Identify provider spells: Group episodes with the same pseudonymised id, 
#'   provider code and admission date.
#'   \item Identity continuous inpatient spells (super-spells): 
#'   Group provider spells if discharge method is unknown or indicates patient is still in
#' hospital and the episode at the new provider began on the same day as the episode 
#' at the previous provider ended.
#' }
#'
#'
#' @param hes is the cleaned HES data.
#'
#' @return Returns an updated version of the HES data, with each row now defined by an admission.
#' 
#' @importFrom data.table setorderv := shift .GRP rbindlist
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' cleaned_hes_2002 <- read_hes("0203")
#'
#' spell_defined_hes_2002 <- define_spells(cleaned_hes_2002)
#'
#' }
#'
define_spells <- function(
  hes
) {
  # first sort the data by individual and time
  setorderv(hes,
            c("encrypted_hesid", "admidate", "epistart"),
            c(1, 1, 1))
  
  # the method requires comparing the data in one row to the data in the preceding row
  # to make this easier, create new columns that lag the columns we need
  hes[, encrypted_hesid_lag := shift(encrypted_hesid, 1, type = "lag")]
  hes[, procode3_lag := shift(procode3, 1, type = "lag")]
  hes[, admidate_lag := shift(admidate, 1, type = "lag")]
  hes[, dismeth_lag := shift(dismeth, 1, type = "lag")]
  hes[, epistart_lag := shift(epistart, 1, type = "lag")]
  hes[, epiend_lag := shift(epiend, 1, type = "lag")]
  
  # filling in gap left in first row by lagging - with original value
  hes[1 , encrypted_hesid_lag := encrypted_hesid]
  hes[1 , procode3_lag := procode3]
  hes[1 , admidate_lag := admidate]
  hes[1 , dismeth_lag := dismeth]
  hes[1 , epistart_lag := epistart]
  hes[1 , epiend_lag := epiend]
  
  # apply the two rules in the CHE method
  cat("\t\tapply_rules\n")
  # rule 1 does most of the work - finding using combinations of individual, provider and admission date
  hes[, spell_id_rule1 := .GRP, by = list(encrypted_hesid, procode3, admidate)]
  
  # rule 2 flags episodes that rule 1 identified as the start of a new spell
  # but are actually part of the previous spell e.g. due to a transfer
  # note - I don't think this goes all the way to defining continuous inpatient spells (CIPs) - check
  hes[, spell_id_rule2 := .GRP, by = list(encrypted_hesid == encrypted_hesid_lag &
                                            (
                                              admidate != admidate_lag &
                                                dismeth_lag > 5 &
                                                (epistart == epiend_lag | epistart == epistart_lag)
                                            ))]
  
  # create a new spell id variable - starting with the spells identified by rule 1
  hes[, spell_id := spell_id_rule1]
  
  # identify the individuals for whom rule 2 found that 2 spells should be combined
  check <- hes[, list(n2 = length(unique(spell_id_rule2))) , by = list(encrypted_hesid)]
  check <- check[n2 > 1]
  
  #nrow(check)
  # 836 individuals
  
  # make the job of dealing with rule 2 more efficient
  # by splitting the data into individuals that do and don't have spells that need joining up
  spell_ok <- hes[!(encrypted_hesid %in% check[, encrypted_hesid])]
  spell_needs_attention <- hes[(encrypted_hesid %in% check[, encrypted_hesid])]
  
  # in the 'needs attention' data, first create a new spell variable
  spell_needs_attention[, spell_id_new := spell_id]
  
  # then loop through each episode (row) in these data
  for (i in 2:nrow(spell_needs_attention)) {
    
    spell_needs_attention[i, spell_id_new := ifelse(
      
      # if same individual
      spell_needs_attention[i, encrypted_hesid] == spell_needs_attention[i - 1, encrypted_hesid] &
        
        # and rule 1 identified that episode as belonging to the same spell as the previous episode
        (spell_needs_attention[i, spell_id_rule1] == spell_needs_attention[i - 1, spell_id_rule1] |
           
           # or rule 2 identified that episode as part of the previous spell (where rule 1 did not)
           spell_needs_attention[i, spell_id_rule2] == 2),
      
      # then assign the current episode to the same spell as the previous one
      spell_needs_attention[i - 1, spell_id_new],
      
      # otherwise,
      ifelse(
        # if same individual
        spell_needs_attention[i, encrypted_hesid] == spell_needs_attention[i - 1, encrypted_hesid],
        
        # assign as belonging to a new spell
        spell_needs_attention[i - 1, spell_id_new] + 1,
        
        # if different individual, use the original spell id from rule 1
        spell_needs_attention[i, spell_id]
      )
    )]
    
  }
  
  #spell_needs_attention[encrypted_hesid == sample(check[ , encrypted_hesid], 1), c("encrypted_hesid", "admidate", "procode3", "dismeth", "epistart", "epiend", "spell_id_rule1", "spell_id_rule2", "spell_id", "spell_id_new"), with = T]
  #spell_needs_attention[encrypted_hesid == "84E47042C4D4E16D6F0D525CB7B0A5B9", c("encrypted_hesid", "admidate", "procode3", "dismeth", "epistart", "epiend", "spell_id_rule1", "spell_id_rule2", "spell_id", "spell_id_new"), with = T]
  
  # replace original with new spell id variable
  spell_needs_attention[, spell_id := spell_id_new]
  spell_needs_attention[, spell_id_new := NULL]
  
  # and rejoin the split datasets
  hes <- rbindlist(list(spell_ok, spell_needs_attention), use.names = T)
  
  #length(unique(hes$spell_id_rule1))
  # 11847741
  
  #length(unique(hes$spell_id))
  # 11846880
  
  #length(unique(spell_ok$spell_id)) + length(unique(spell_needs_attention$spell_id))
  # 11846880
  
  #length(unique(hes$spell_id_rule1)) - length(unique(hes$spell_id))
  # 861
  
  hes[, encrypted_hesid_lag := NULL]
  hes[, procode3_lag := NULL]
  hes[, admidate_lag := NULL]
  hes[, dismeth_lag := NULL]
  hes[, epistart_lag := NULL]
  hes[, epiend_lag := NULL]
  hes[, spell_id_rule1 := NULL]
  hes[, spell_id_rule2 := NULL]
  
  rm(check, spell_needs_attention, spell_ok, i)
  gc()
  
  
  # Since spells have now been joined into super-spells
  # make admidate equal the admidate of the first spell in the superspell
  # for all episodes in the superspell
  hes[ , admidate := admidate[which(epistart == min(epistart, na.rm = T))[1]], by = spell_id]
  
  
  
  return(hes[])
}
