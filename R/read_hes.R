
#' Read in HES
#'
#' Reads in Hospital episode statistics for England.
#'
#' The variable names change between 13/14 and 14/15, and again between 16/17 and 17/18.
#' This code reads in the HES data for one year whilst selecting only the variables we need and formatting to be the same for each year.
#'
#'
#' Variables included:
#'
#' \itemize{
#' \item startage: age at start and end of episode. This is a derived field, calculated from episode start date (epistart) and date of birth (dob). Contains the patient's age in whole years.
#' (Ages from 1 to 115 (1990-91 to 1994-95) and from 1 to 120 (1995-96 onwards)).
#' \itemize{
#' For patients under 1 year old, special codes in the range 7001 to 7007 apply:
#' \itemize{
#' \item 7001 = Less than 1 day
#' \item 7002 = 1 to 6 days
#' \item 7003 = 7 to 28 days
#' \item 7004 = 29 to 90 days (under 3 months)
#' \item 7005 = 91 to 181 days (approximately 3 months to under 6 months)
#' \item 7006 = 182 to 272 days (approximately 6 months to under 9 months)
#' \item 7007 = 273 to 364 days (approximately 9 months to under 1 year)
#' \item null = Not applicable (other maternity event or not known)
#' }
#' }
#' \item mydob: data of birth (month and year)
#' \item dob_cfl:  date of birth check flag - patient
#' \item ethnos: Ethnic category
#' \item encrypted_hesid: 1415 pseudo_hesid
#' \item admidate: contains the date the patient was admitted to hospital at the start of a hospital spell. Admidate is recorded on all episodes within a spell.
#' \item 2012/13 onwards:
#' \itemize{
#' \item 01/01/1800 - null date submitted
#' \item 01/01/1801 - invalid date submitted
#' }
#' \item 1989/90 to 2011/12:
#' \itemize{
#' \item 01/01/1600 ? null date submitted
#' \item 15/10/1582 ? invalid date submitted
#' }
#' \item admimeth: contains a code which identifies how the patient was admitted to hospital. Admimeth is recorded on the first and also all subsequent episodes within the spell (ie where the spell is made up of more than one episode).
#' \itemize{
#' \item Elective Admission, when the decision to admit could be separated in time from the actual admission:
#' \itemize{
#' \item 11 = Waiting list
#' \item 12 = Booked
#' \item 13 = Planned Emergency Admission,
#' }
#' \item Non-elective (emergency admission): when admission is unpredictable and at short notice because of clinical need:
#' \itemize{
#' \item 21 = Accident and emergency or dental casualty department of the Health Care Provider
#' \item 22 = General Practitioner: after a request for immediate admission has been made direct to a Hospital Provider, i.e. not through a Bed bureau, by a General Practitioner: or deputy
#' \item 23 = Bed bureau
#' \item 24 = Consultant Clinic, of this or another Health Care Provider
#' \item 25 = Admission via Mental Health Crisis Resolution Team
#' \item 2A = Accident and Emergency Department of another provider where the PATIENT had not been admitted
#' \item 2B = Transfer of an admitted patient from another Hospital Provider in an emergency
#' \item 2C = Baby born at home as intended
#' \item 2D = Other emergency admission
#' \item 28 = Other means, examples are:
#' \itemize{
#' \item admitted from the Accident and Emergency Department of another provider where they had not been admitted
#' \item transfer of an admitted patient from another Hospital Provider in an emergency
#' \item baby born at home as intended Maternity Admission,
#' }
#' }
#' \item Maternity admission, of a pregnant or recently pregnant woman to a maternity ward (including delivery facilities) except when the intention is to terminate the pregnancy:
#' \itemize{
#' \item 31 = Admitted ante-partum
#' \item 32 = Admitted post-partum
#' }
#' Other admission not specified above:
#' \item 82 = The birth of a baby in this Health Care Provider
#' \item 83 = Baby born outside the Health Care Provider except when born at home as intended.
#' \item 81  = Transfer of any admitted patient from other Hospital Provider other than in an emergency
#' \item 98 = Not applicable
#' \item 99 = Not known: a validation error
#' }
#' \item disdate: contains the date on which the patient was discharged from hospital. It is only present in the record for the last episode of a spell.
#' \item spelbgin: This is a derived field that contains a code that defines whether the episode is the first of a spell and whether the spell started in the current or previous year.
#' \itemize{
#' \item 0 = Not first episode of spell;
#' \item 1 = First episode of spell that started in previous year;
#' \item 2 = First episode of spell that started in current year;
#' \item null = Not applicable
#' }
#' \item spelend: a code which defines whether the episode is the last of a spell. It is set for finished episodes (episode status - epistat - is 3) for general,
#' delivery or birth episodes (episode type - epitype - is 1, 2 or 3) provided the discharge method (dismeth) confirms that the spell has finished.
#' Y = Last episode of spell ; N = Not last episode of spell
#' \item speldur: This is a derived field that contains the difference in days between the admission date (admidate) and the discharge date (epiend)
#' provided the discharge method (dismeth) confirms that the spell has finished.
#' If the episode has not finished it is calculated from the end of the year and admidate.
#' \itemize{
#' \item 5n = Duration of spell in days from 0 to 29,200;
#' \item null = Not applicable: patient not discharged (dismeth not in range 1-5), other maternity event (epitype is 5 or 6) or not valid
#' }
#' \item epiend: contains the date on which a patient left the care of a particular consultant, for one of the following reasons: Discharged from hospital (includes transfers) or moved to the care of another consultant.
#' A null entry either indicates that the episode was unfinished at the end of the data year, or the date was unknown.
#' \itemize{
#' \item 2012/13 onwards:
#' \itemize{
#' \item 01/01/1800 - null date submitted
#' \item 01/01/1801 - invalid date submitted
#' }
#' \item 1989/90 to 2011/12:
#' \itemize{
#' \item 01/01/1600
#' }
#' }
#' \item epistart: contains the date on which a patient was under the care of a particular consultant. If a patient has more than one episode in a spell, for each new episode there is a new value of epistart.
#' However, the admission date which is copied to each new episode in a spell will remain unchanged and will be equal to the episode start date of the first episode in hospital.
#' \itemize{
#' \item  2012/13 onwards:
#' \itemize{
#' \item 01/01/1800 - null date submitted
#' \item 01/01/1801 - invalid date submitted
#' }
#' \item 1989/90 to 2011/12:
#' \itemize{
#' \item 01/01/1600 ? null date submitted
#' \item 15/10/1582 ? invalid date submitted
#' }
#' }
#' \item epiorder: contains the number of the episode within the current spell. All spells start with an episode where epiorder is 01.
#' Many spells finish with this episode, but if the patient moves to the care of another consultant, a new episode begins.
#' Episode numbers increase by 1 for each new episode until the patient is discharged (this includes transfers to another NHS trust or primary care trust -
#' ie the first episode in the new trust will have epiorder 01). If the same patient returns for a different spell in hospital, epiorder is again set to 01.
#' Admissions are calculated by counting the number of times epiorder is 01. When studying long term care, remember that it is not unusual to transfer psychiatric patients from one hospital to another.
#' \itemize{
#' \item 2n = The number of the episode in the sequence of episodes from 01-87;
#' \item 98 = Not applicable;
#' \item 99 = Not known;
#' \item null = Not applicable: other maternity event
#' }
#' \item epistat: tells you whether the episode had finished before the end of the HES data-year (ie whether the episode was still 'live' at midnight on 31 March).
#' For example, if a patient was admitted on 25 March 2005 and was not discharged (or transferred to the care of another consultant) until 4 April 2005,
#' there will be a record describing the unfinished episode (episode status = 1) in the 2004-05 data, and a separate record describing the finished episode (episode status = 3) in the 2005-06 data.
#' Because hospital providers are advised not to include clinical data (diagnosis and operation codes) in unfinished records, these are normally excluded from analyses.
#' Also, if unfinished episodes are included in time series analyses - where data for more than one year is involved - there is a danger of counting the same episode twice.
#' \item classpat: identifies day cases, ordinary admissions, regular day and regular night attenders, and the special case of mothers and babies using only delivery facilities.
#' Data about regular day and regular night attenders are not available for analysis prior to 2002-03.
#' Since the introduction of the NHS wide clearing service in April 1996, this field has been derived from related items in the Commissioning Data Set (eg intended management).
#' \itemize{
#' \item 1 = Ordinary admission
#' \item 2 = Day case admission
#' \item 3 = Regular day attender
#' \item 4 = Regular night attender
#' \item 5 = Mothers and babies using only delivery facilities
#' \item 8 = Not applicable (other maternity event)
#' \item 9 = Not known
#' }
#' \item mainspef: contains a code that defines the specialty under which the consultant is contracted. It can be compared with tretspef, the specialty under which the consultant worked.
#' \item tretspef: speciality of consultant
#' \item diag_xx: ICD10 coded diagnosis codes. 1-20
#' \item cause: copy of the first diagnosis code that represents an external cause
#' \item resgor: government office region of residence
#' \item resladst: local authority district of residence (old version of ONS coding system)
#' \item imd04_decile: uses IMD version 2004 on activity up to and including 2006-07; IMD version 2007 on activity between 2007-08 and 2009-10;
#' IMD version 2010 on activity from 2010-11 to present.
#' \item epikey: record identifier
#' \item opertn_xx: Procedure codes - before april 2007 there are 12 operate codes. after there are 24.
#' \item posopdur: Number of days between the main operation and the end of the episode from 0-365. null = Not applicable: no operation or episode unfinished
#' }
#' Additional variables added for use in HRG code grouper:
#' \itemize{
#' \item procodet
#' \item admisorc
#' \item disdest
#' \item dismeth
#' \item epidur
#' \item PROVSPNOPS
#' \item procode3
#' }
#'
#' @param k.year.ind is the year of HES data. In the format "yy/yy".
#' @param test Logical - whether to run a small sample of HES data
#' Defaults to FALSE. If FALSE, will output 1,000 rows of HES data. If TRUE, then will output the whole year of data.
#'
#' @return Returns a data table. Note that:
#' \itemize{
#' \item All variable names are converted to lower case.
#' }
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' hes_2002 <- read_hes("0203", test = TRUE)
#'
#' }
#'
read_hes <- function(
  k.year.ind,
  test = FALSE
) {

  # Select neccessary variables to keep----
  keepvars <- Hmisc::Cs(

    startage,
    endage,
    ethnos,
    encrypted_hesid,
    sex,
    admidate,
    admimeth,
    disdate,
    spelbgin,
    spelend,
    speldur,
    epiend,
    epistart,
    epiorder,
    epistat,
    classpat,
    mainspef,
    tretspef,
    diag_01,
    diag_02, diag_03, diag_04, diag_05, diag_06, diag_07, diag_08,
    diag_09, diag_10, diag_11, diag_12, diag_13, diag_14, diag_15, diag_16,
    diag_17, diag_18, diag_19, diag_20,
    cause,
    resgor,
    resladst,
    imd04_decile,
    #postdist, # postcode district of patient
    #LSOA01, MSOA01, # lower and middle super output areas under the 2001 definition
    epikey,
    procodet, admisorc, disdest, dismeth, epidur, PROVSPNOPS, procode3,
    opertn_01, opertn_02, opertn_03, opertn_04, opertn_05, opertn_06, opertn_07, opertn_08, opertn_09,
    opertn_10, opertn_11, opertn_12,
    posopdur
  )

  keepvars_new <- Hmisc::Cs(
    STARTAGE,
    ENDAGE,
    ETHNOS,
    ENCRYPTED_HESID,
    SEX,
    ADMIDATE,
    ADMIMETH,
    DISDATE,
    SPELBGIN,
    SPELEND,
    SPELDUR,
    EPIEND,
    EPISTART,
    EPIORDER,
    EPISTAT,
    CLASSPAT,
    MAINSPEF,
    TRETSPEF,
    DIAG_01,
    DIAG_02, DIAG_03, DIAG_04, DIAG_05, DIAG_06, DIAG_07, DIAG_08,
    DIAG_09, DIAG_10, DIAG_11, DIAG_12, DIAG_13, DIAG_14, DIAG_15, DIAG_16,
    DIAG_17, DIAG_18, DIAG_19, DIAG_20,
    CAUSE,
    RESGOR_ONS,
    RESLADST_ONS,
    IMD04_DECILE,
    EPIKEY,
    PROCODET, ADMISORC, DISDEST, DISMETH, EPIDUR, PROVSPNOPS, PROCODE3,
    OPERTN_01, OPERTN_02, OPERTN_03, OPERTN_04, OPERTN_05, OPERTN_06, OPERTN_07, OPERTN_08, OPERTN_09,
    OPERTN_10, OPERTN_11, OPERTN_12, OPERTN_13, OPERTN_14, OPERTN_15, OPERTN_16, OPERTN_17, OPERTN_18, OPERTN_19, OPERTN_20, OPERTN_21,
    OPERTN_22, OPERTN_23, OPERTN_24,
    POSOPDUR,

    SUSHRG#,
    #SUSHRGVERS#,
    #The SUS PbR derived healthcare resource group (HRG) code (HRG4 from 2009-10) at episode level.
    #5an = SUS generated HRG
    #Null = Records that have been excluded from PbR in SUS as the activity is outside the scope of PbR

    #ALCDIAG, ALCDIAG_4, ALCFRAC # new alcohol variables
  )

  keepvars_17 <- Hmisc::Cs(
    STARTAGE,
    ENDAGE,
    ETHNOS,
    ENCRYPTED_HESID,
    SEX,
    ADMIDATE,
    ADMIMETH,
    DISDATE,
    SPELBGIN,
    SPELEND,
    SPELDUR,
    EPIEND,
    EPISTART,
    EPIORDER,
    EPISTAT,
    CLASSPAT,
    MAINSPEF,
    TRETSPEF,
    DIAG_01,
    DIAG_02, DIAG_03, DIAG_04, DIAG_05, DIAG_06, DIAG_07, DIAG_08,
    DIAG_09, DIAG_10, DIAG_11, DIAG_12, DIAG_13, DIAG_14, DIAG_15, DIAG_16,
    DIAG_17, DIAG_18, DIAG_19, DIAG_20,
    CAUSE,
    RESGOR_ONS,
    RESLADST_ONS,
    IMD04_DECILE,
    EPIKEY,
    PROCODET, ADMISORC, DISDEST, DISMETH, EPIDUR, PROVSPNOPS, PROCODE3,
    OPERTN_4_01, OPERTN_4_02, OPERTN_4_03, OPERTN_4_04, OPERTN_4_05, OPERTN_4_06, OPERTN_4_07,
    OPERTN_4_08, OPERTN_4_09, OPERTN_4_10, OPERTN_4_11, OPERTN_4_12, OPERTN_4_13, OPERTN_4_14,
    OPERTN_4_15, OPERTN_4_16, OPERTN_4_17, OPERTN_4_18, OPERTN_4_19, OPERTN_4_20, OPERTN_4_21,
    OPERTN_4_22, OPERTN_4_23, OPERTN_4_24,
    POSOPDUR,

    SUSHRG
  )




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

  # Load in all data for a particular year
    if (test == TRUE) {
    cat("\t\ttest data\n")

    data <- fread(paste0("D:/HES/APC_",  k.year.ind, ".txt"),
        header = TRUE, sep = "|", quote = "", nrows = 1000, verbose = F, showProgress = T, data.table = T,
        na.strings = c("NA", "N/A", "", "-", "null", "UZ01Z"))
    data <- data[ , ..keepvars]
    }

    if (test == FALSE){
    cat("\t\tfull data\n")

    data <- fread(paste0("D:/HES/APC_",  k.year.ind, ".txt"))
    data <- data[ , ..keepvars]
    }

  }


  if (k.year.ind %in% c(
    "1415",
    "1516",
    "1617")) {


    if (test == TRUE) {

      cat("\t\ttest data\n")

      data <- fread(paste0("D:/HES/APC_",  k.year.ind, ".txt"),
                    header = TRUE, sep = "|", quote = "", nrows = 5000, verbose = F, showProgress = T, data.table = T,
                    na.strings = c("NA", "N/A", "", "-", "null", "UZ01Z"))
      data <- data[ , ..keepvars_new]

      setnames(data, names(data), tolower(names(data)))
    }

    if (test == FALSE) {

      cat("\t\tfull data\n")

      data <- fread(paste0("D:/HES/APC_",  k.year.ind, ".txt"))
      data <- data[ , ..keepvars_new]

      setnames(data, names(data), tolower(names(data)))
    }
  }


  if (k.year.ind %in% c(
    "1718")) {

    if (test == TRUE) {
      cat("\t\ttest data\n")

      data <- fread(paste0("D:/HES/APC_",  k.year.ind, ".txt"),
                    header = TRUE, sep = "|", quote = "", nrows = 1000, verbose = F, showProgress = T, data.table = T,
                    na.strings = c("NA", "N/A", "", "-", "null", "UZ01Z"))
      data <- data[ , ..keepvars_17]

      setnames(data, names(data), tolower(names(data)))

      sub("_4", "", data[, colnames(data)])
    }

    if (test == FALSE) {
      cat("\t\tfull data\n")

      data <- fread(paste0("D:/HES/APC_",  k.year.ind, ".txt"))
      data <- data[ , ..keepvars_17]

      setnames(data, names(data), tolower(names(data)))

      sub("_4", "", data[, colnames(data)])

    }
  }


  return(data)
}





