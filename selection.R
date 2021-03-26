perform <- tibble(read.xlsx("diamondsexports/BIVA_ED_OCT20_2020.xlsx", detectDates = T))

diamonds <- read_delim("https://www.euclids-perform-diamonds-h2020.eu/media/e2.txt", delim = "\t")

RNAseqList <- read.xlsx("Batch_F_and_F_extension_noDELPHIC.xlsx", startRow = 2)


#### Remove 0 from episode so all are in E1,E2 format ####

remove0 <- function(x){
 gsub("-E0","-E", x)
  
}

perform$UNIQUE_PATIENT_ID <- remove0(perform$UNIQUE_PATIENT_ID)
diamonds$UNIQUE_PATIENT_ID <- remove0(diamonds$UNIQUE_PATIENT_ID)
RNAseqList$Patient.ID <- remove0(RNAseqList$Patient.ID)
RNAseqList$On.Jethro.clinical.diamonds.database.06_07_20 <- remove0(RNAseqList$On.Jethro.clinical.diamonds.database.06_07_20)


#### Define Limits for Abnormal Tests ####

fibrinogen <- 5
CRP <- 20
Ddimer <- 1000
Ferritin <- 400
Albumin <- 25
Lymphocytes <- 1.5
tropT <- 14
bnp <- 500
PT <- 15.8
INR <- 1.5
PCT <- 0.15
neutrophils <- 6

#### Define Functions ####
#Find which tests were positive
covidPositiveTests <- function(TEST_TYPE, RESULT, ...){
  
  if(!grepl("POSITIVE", RESULT)){NA_character_
    } else {
  
  whichArePositive <- which(str_split(RESULT,";", simplify = T) %in% "POSITIVE")
  
  paste(str_split(TEST_TYPE, ";", simplify = T)[,whichArePositive], collapse=";")
  
  
  }
}

#Find which tests were positive
covidNegativeTests <- function(TEST_TYPE, RESULT, ...){
  
  if(!grepl("NEGATIVE", RESULT)){NA_character_
  } else {
    
    whichArePositive <- which(str_split(RESULT,";", simplify = T) %in% "NEGATIVE")
    
    paste(str_split(TEST_TYPE, ";", simplify = T)[,whichArePositive], collapse=";")
    
    
  }
}

fullPIMSCriteria <- function(FEVER, 
                             DATE_FEVER_ONSET,
                             `TIME_POINT_8=FIBRINOGEN`,
                             `TIME_POINT_16=CRP`,
                             `TIME_POINT_20=D_DIMER`,
                             `TIME_POINT_18=FERRITIN`,
                             `TIME_POINT_14=ALBUMIN`,
                             `TIME_POINT_5=LYMPHOCYTES`,
                             SHOCK,
                             `TIME_POINT_12=INOTROPES`,
                             INOTROPES,
                             RESPIRATORY,
                             CARDIAC,
                             GASTROINTESTINAL,
                             SENSORY,
                             HEADACHE,
                             OTHER_NEURO,
                             VIRUS_DETECTED,
                             BACTERIA_SPECIFY,
                             PATHOGEN_SPECIFY,
                             age,
                             ...){
  if(
    ((!is.na(age)) & age <=18) & 
    (FEVER=="YES" | (!DATE_FEVER_ONSET %in% c(NA,NULL,"NA","NULL"))) &
     (SHOCK=="YES" | grepl("YES",`TIME_POINT_12=INOTROPES`) | grepl("YES",INOTROPES) | 
      CARDIAC == "YES" | RESPIRATORY == "YES" | GASTROINTESTINAL == "YES" | 
      SENSORY=="YES" | HEADACHE == "YES" | OTHER_NEURO == "YES") &
     max(parse_number(str_split(`TIME_POINT_8=FIBRINOGEN`,";",simplify = T)), na.rm = T) > fibrinogen  &
     max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP  &
     max(parse_number(str_split(`TIME_POINT_20=D_DIMER`,";",simplify = T)), na.rm = T) > Ddimer  &
     max(parse_number(str_split(`TIME_POINT_18=FERRITIN`,";",simplify = T)), na.rm = T) > Ferritin  &
     max(parse_number(str_split(`TIME_POINT_14=ALBUMIN`,";",simplify = T)), na.rm = T) < Albumin  &
     max(parse_number(str_split(`TIME_POINT_5=LYMPHOCYTES`,";",simplify = T)), na.rm = T) < Lymphocytes  &
     BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL) &
     VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2") & 
     PATHOGEN_SPECIFY %in% c(NA,"NA","NULL",NULL)
     ){
    TRUE
  } else {
    FALSE
  }
  
}

fullPIMSminusAppendix <- function(FEVER, 
                             DATE_FEVER_ONSET,
                             `TIME_POINT_8=FIBRINOGEN`,
                             `TIME_POINT_16=CRP`,
                             `TIME_POINT_20=D_DIMER`,
                             `TIME_POINT_18=FERRITIN`,
                             `TIME_POINT_14=ALBUMIN`,
                             `TIME_POINT_5=LYMPHOCYTES`,
                             `TIME_POINT_4=NEUTROPHILS`,
                             SHOCK,
                             `TIME_POINT_12=INOTROPES`,
                             INOTROPES,
                             RESPIRATORY,
                             CARDIAC,
                             GASTROINTESTINAL,
                             SENSORY,
                             HEADACHE,
                             OTHER_NEURO,
                             VIRUS_DETECTED,
                             BACTERIA_SPECIFY,
                             PATHOGEN_SPECIFY,
                             age,
                             ...){
  if(
    ((!is.na(age)) & age <=18) & 
    (FEVER=="YES" | (!DATE_FEVER_ONSET %in% c(NA,NULL,"NA","NULL"))) &
    (SHOCK=="YES" | grepl("YES",`TIME_POINT_12=INOTROPES`) | grepl("YES",INOTROPES) | 
     CARDIAC == "YES" | RESPIRATORY == "YES" | GASTROINTESTINAL == "YES" | 
     SENSORY=="YES" | HEADACHE == "YES" | OTHER_NEURO == "YES") &
    max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP  &
    max(parse_number(str_split(`TIME_POINT_4=NEUTROPHILS`,";",simplify = T)), na.rm = T) > neutrophils  &
    max(parse_number(str_split(`TIME_POINT_5=LYMPHOCYTES`,";",simplify = T)), na.rm = T) < Lymphocytes  &
    BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL) &
    VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2") & 
    PATHOGEN_SPECIFY %in% c(NA,"NA","NULL",NULL)
  ){
    TRUE
  } else {
    FALSE
  }
  
}

fullMisCriteria <- function(FEVER,
                           DATE_FEVER_ONSET,
                           DATETIME_FIRST_HOSPITAL,
                           RASH,
                           CONJUNCTIVITIS,
                           MUCOSITIS,
                           INFLAMMED_EXTREMITIES,
                           SHOCK,
                           `TIME_POINT_12=INOTROPES`,
                           INOTROPES,
                           FIRST_ECHO_NORMAL,
                           FIRST_PERICARDITIS,
                           FIRST_VALVULAR_REGURGITATION,
                           WORST_ECHO_NORMAL,
                           WORST_PERICARDITIS,
                           WORST_VALVULAR_REGURGITATION,
                           FIRST_ECHO_LOPEZ_Z_SCORE_1,
                           FIRST_ECHO_LOPEZ_Z_SCORE_2,
                           WORST_ECHO_LOPEZ_Z_SCORE_1,
                           WORST_ECHO_LOPEZ_Z_SCORE_2,
                           `TIME_POINT_19=TROP_T`,
                           `TIME_POINT_22=BNP_NT`,
                           `TIME_POINT_21=INR`,
                           `TIME_POINT_9=PT`,
                           `TIME_POINT_20=D_DIMER`,
                           GASTROINTESTINAL,
                           VIRUS_DETECTED,
                           BACTERIA_SPECIFY,
                           PATHOGEN_SPECIFY,
                           `TIME_POINT_16=CRP`,
                           `TIME_POINT_17=PCT`,
                           positiveCovidTests,
                           age,
                           ...){
  
  score = 0
  if ("YES" %in% c(RASH,CONJUNCTIVITIS,MUCOSITIS,INFLAMMED_EXTREMITIES)){score <- score +1}
  if (SHOCK=="YES" | grepl("YES",INOTROPES) | grepl("YES",`TIME_POINT_12=INOTROPES`)) {score <- score + 1}
  if (FIRST_ECHO_NORMAL=="NO" | WORST_ECHO_NORMAL=="NO" | 
      "YES" %in% c(FIRST_PERICARDITIS,FIRST_VALVULAR_REGURGITATION,WORST_PERICARDITIS,WORST_VALVULAR_REGURGITATION) |
      max(c(parse_number(FIRST_ECHO_LOPEZ_Z_SCORE_1),parse_number(FIRST_ECHO_LOPEZ_Z_SCORE_2),parse_number(WORST_ECHO_LOPEZ_Z_SCORE_1),parse_number(WORST_ECHO_LOPEZ_Z_SCORE_2)), na.rm = T)>2.5 |
      max(parse_number(str_split(`TIME_POINT_19=TROP_T`,";",simplify = T)), na.rm = T) > tropT  |
      max(parse_number(str_split(`TIME_POINT_22=BNP_NT`,";",simplify = T)), na.rm = T) > bnp  
  ) { score <- score+1}
  if (max(parse_number(str_split(`TIME_POINT_9=PT`,";",simplify = T)), na.rm = T) > PT |
      max(parse_number(str_split(`TIME_POINT_21=INR`,";",simplify = T)), na.rm = T) > INR) {score <- score +1}
  if(GASTROINTESTINAL=="YES"){score <- score+1}
  
  if(DATE_FEVER_ONSET %in% c("NA","NULL",NA,NULL) | DATETIME_FIRST_HOSPITAL %in% c("NA","NULL",NA,NULL)){
    if(FEVER=="YES"){feverCriteria<- T} else {feverCriteria <- F}
  } else {
    if(as.numeric(as.Date(DATETIME_FIRST_HOSPITAL) - as.Date(DATE_FEVER_ONSET))>=3){
      feverCriteria <- T
    } else {feverCriteria <- F}
  }
  
  
  if(
  
    ((!is.na(age)) | age <=18) &
    #fever >able to calculate =3 days or any fever if not able to calculate length of fever
    feverCriteria &
  score >=2 &
  (max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP | 
   max(parse_number(str_split(`TIME_POINT_17=PCT`,";",simplify = T)), na.rm = T) > PCT) &
  BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL,"") &
  VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2","") & 
  PATHOGEN_SPECIFY %in% c(NA,"NA","NULL",NULL,"") &
  (grepl("PCR|IGG|IGM|ANTIGEN",positiveCovidTests) | grepl("SARS-CoV-2",VIRUS_DETECTED))
    ){
    TRUE
  } else{
    FALSE
  }
  
}

PIMSplusBactVir <- function(FEVER, 
                             DATE_FEVER_ONSET,
                             `TIME_POINT_8=FIBRINOGEN`,
                             `TIME_POINT_16=CRP`,
                             `TIME_POINT_20=D_DIMER`,
                             `TIME_POINT_18=FERRITIN`,
                             `TIME_POINT_14=ALBUMIN`,
                             `TIME_POINT_5=LYMPHOCYTES`,
                             SHOCK,
                             `TIME_POINT_12=INOTROPES`,
                             INOTROPES,
                             RESPIRATORY,
                             CARDIAC,
                             GASTROINTESTINAL,
                             SENSORY,
                             HEADACHE,
                             OTHER_NEURO,
                             VIRUS_DETECTED,
                             BACTERIA_SPECIFY,
                             PATHOGEN_SPECIFY,
                             age,
                             ...){
  if(
    ((!is.na(age)) & age <=18) & 
    (FEVER=="YES" | (!DATE_FEVER_ONSET %in% c(NA,NULL,"NA","NULL"))) &
    (SHOCK=="YES" | grepl("YES",`TIME_POINT_12=INOTROPES`) | grepl("YES",INOTROPES) | 
     CARDIAC == "YES" | RESPIRATORY == "YES" | GASTROINTESTINAL == "YES" | 
     SENSORY=="YES" | HEADACHE == "YES" | OTHER_NEURO == "YES") &
    max(parse_number(str_split(`TIME_POINT_8=FIBRINOGEN`,";",simplify = T)), na.rm = T) > fibrinogen  &
    max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP  &
    max(parse_number(str_split(`TIME_POINT_20=D_DIMER`,";",simplify = T)), na.rm = T) > Ddimer  &
    max(parse_number(str_split(`TIME_POINT_18=FERRITIN`,";",simplify = T)), na.rm = T) > Ferritin  &
    max(parse_number(str_split(`TIME_POINT_14=ALBUMIN`,";",simplify = T)), na.rm = T) < Albumin  &
    max(parse_number(str_split(`TIME_POINT_5=LYMPHOCYTES`,";",simplify = T)), na.rm = T) < Lymphocytes  &
    ((!BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL)) |
    (!VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2")))
  ){
    TRUE
  } else {
    FALSE
  }
  
}

fullPIMSminusAppendixplusBactVir <- function(FEVER, 
                                  DATE_FEVER_ONSET,
                                  `TIME_POINT_8=FIBRINOGEN`,
                                  `TIME_POINT_16=CRP`,
                                  `TIME_POINT_20=D_DIMER`,
                                  `TIME_POINT_18=FERRITIN`,
                                  `TIME_POINT_14=ALBUMIN`,
                                  `TIME_POINT_5=LYMPHOCYTES`,
                                  `TIME_POINT_4=NEUTROPHILS`,
                                  SHOCK,
                                  `TIME_POINT_12=INOTROPES`,
                                  INOTROPES,
                                  RESPIRATORY,
                                  CARDIAC,
                                  GASTROINTESTINAL,
                                  SENSORY,
                                  HEADACHE,
                                  OTHER_NEURO,
                                  VIRUS_DETECTED,
                                  BACTERIA_SPECIFY,
                                  PATHOGEN_SPECIFY,
                                  age,
                                  ...){
  if(
    ((!is.na(age)) & age <=18) & 
    (FEVER=="YES" | (!DATE_FEVER_ONSET %in% c(NA,NULL,"NA","NULL"))) &
    (SHOCK=="YES" | grepl("YES",`TIME_POINT_12=INOTROPES`) | grepl("YES",INOTROPES) | 
     CARDIAC == "YES" | RESPIRATORY == "YES" | GASTROINTESTINAL == "YES" | 
     SENSORY=="YES" | HEADACHE == "YES" | OTHER_NEURO == "YES") &
    max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP  &
    max(parse_number(str_split(`TIME_POINT_4=NEUTROPHILS`,";",simplify = T)), na.rm = T) > neutrophils  &
    max(parse_number(str_split(`TIME_POINT_5=LYMPHOCYTES`,";",simplify = T)), na.rm = T) < Lymphocytes  &
    ((!BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL)) &
    (!VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2")))
  ){
    TRUE
  } else {
    FALSE
  }
  
}



MisCplusBactVir <- function(FEVER,
                            DATE_FEVER_ONSET,
                            DATETIME_FIRST_HOSPITAL,
                            RASH,
                            CONJUNCTIVITIS,
                            MUCOSITIS,
                            INFLAMMED_EXTREMITIES,
                            SHOCK,
                            `TIME_POINT_12=INOTROPES`,
                            INOTROPES,
                            FIRST_ECHO_NORMAL,
                            FIRST_PERICARDITIS,
                            FIRST_VALVULAR_REGURGITATION,
                            WORST_ECHO_NORMAL,
                            WORST_PERICARDITIS,
                            WORST_VALVULAR_REGURGITATION,
                            FIRST_ECHO_LOPEZ_Z_SCORE_1,
                            FIRST_ECHO_LOPEZ_Z_SCORE_2,
                            WORST_ECHO_LOPEZ_Z_SCORE_1,
                            WORST_ECHO_LOPEZ_Z_SCORE_2,
                            `TIME_POINT_19=TROP_T`,
                            `TIME_POINT_22=BNP_NT`,
                            `TIME_POINT_21=INR`,
                            `TIME_POINT_9=PT`,
                            `TIME_POINT_20=D_DIMER`,
                            GASTROINTESTINAL,
                            VIRUS_DETECTED,
                            BACTERIA_SPECIFY,
                            PATHOGEN_SPECIFY,
                            `TIME_POINT_16=CRP`,
                            `TIME_POINT_17=PCT`,
                            positiveCovidTests,
                            age,
                            ...){
  
  score = 0
  if ("YES" %in% c(RASH,CONJUNCTIVITIS,MUCOSITIS,INFLAMMED_EXTREMITIES)){score <- score +1}
  if (SHOCK=="YES" | grepl("YES",INOTROPES) | grepl("YES",`TIME_POINT_12=INOTROPES`)) {score <- score + 1}
  if (FIRST_ECHO_NORMAL=="NO" | WORST_ECHO_NORMAL=="NO" | 
      "YES" %in% c(FIRST_PERICARDITIS,FIRST_VALVULAR_REGURGITATION,WORST_PERICARDITIS,WORST_VALVULAR_REGURGITATION) |
      max(c(parse_number(FIRST_ECHO_LOPEZ_Z_SCORE_1),parse_number(FIRST_ECHO_LOPEZ_Z_SCORE_2),parse_number(WORST_ECHO_LOPEZ_Z_SCORE_1),parse_number(WORST_ECHO_LOPEZ_Z_SCORE_2)), na.rm = T)>2.5 |
      max(parse_number(str_split(`TIME_POINT_19=TROP_T`,";",simplify = T)), na.rm = T) > tropT  |
      max(parse_number(str_split(`TIME_POINT_22=BNP_NT`,";",simplify = T)), na.rm = T) > bnp  
  ) { score <- score+1}
  if (max(parse_number(str_split(`TIME_POINT_9=PT`,";",simplify = T)), na.rm = T) > PT |
      max(parse_number(str_split(`TIME_POINT_21=INR`,";",simplify = T)), na.rm = T) > INR) {score <- score +1}
  if(GASTROINTESTINAL=="YES"){score <- score+1}
  
  if(DATE_FEVER_ONSET %in% c("NA","NULL",NA,NULL) | DATETIME_FIRST_HOSPITAL %in% c("NA","NULL",NA,NULL)){
    if(FEVER=="YES"){feverCriteria<- T} else {feverCriteria <- F}
  } else {
    if(as.numeric(as.Date(DATETIME_FIRST_HOSPITAL) - as.Date(DATE_FEVER_ONSET))>=3){
      feverCriteria <- T
    } else {feverCriteria <- F}
  }
  
  
  if(
    
    ((!is.na(age)) | age <=18) &
    #fever >able to calculate =3 days or any fever if not able to calculate length of fever
    feverCriteria &
    score >=2 &
    (max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP | 
     max(parse_number(str_split(`TIME_POINT_17=PCT`,";",simplify = T)), na.rm = T) > PCT) &
    ((!BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL,"")) |
    (!VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2",""))) &
    (grepl("PCR|IGG|IGM|ANTIGEN",positiveCovidTests) | grepl("SARS-CoV-2",VIRUS_DETECTED))
  ){
    TRUE
  } else{
    FALSE
  }
  
}


uncomplicatedCovid <- function(age,VIRUS_DETECTED,
                               BACTERIA_SPECIFY,
                               PATHOGEN_SPECIFY,
                               VIRUS_ACCOUNT_ALL_FEATURES,
                               VIRUS_CONSISTENT_WITH_SYMPTOMS,
                               PATHOGEN_SYNDROMES,
                               FINAL_PHENOTYPE_DIAGNOSIS,
                               SECONDARY_PHENOTYPE,
                               INFLAMMATORY,
                               positiveCovidTests,
                               ...){
  
  VIRUS_DETECTED <- str_split(VIRUS_DETECTED,";",simplify = T)
  VIRUS_ACCOUNT_ALL_FEATURES <- str_split(VIRUS_ACCOUNT_ALL_FEATURES,";",simplify = T)
  VIRUS_CONSISTENT_WITH_SYMPTOMS <- str_split(VIRUS_CONSISTENT_WITH_SYMPTOMS,";",simplify = T)
  
  if("SARS-CoV-2" %in% VIRUS_DETECTED){
  if(
    (!is.na(age) & age<=18) &
    grepl("PCR|ANTIGEN",positiveCovidTests) & 
    (VIRUS_CONSISTENT_WITH_SYMPTOMS[which(VIRUS_DETECTED=="SARS-CoV-2")] == "YES" |
     VIRUS_ACCOUNT_ALL_FEATURES[which(VIRUS_DETECTED=="SARS-CoV-2")] == "YES") &
    PATHOGEN_SYNDROMES == "COIVD19" &
    INFLAMMATORY != "COVID-RELATED INFLAMMATION" &
    FINAL_PHENOTYPE_DIAGNOSIS %in% c("DEFINITE VIRAL","VIRAL SYNDROME") &
    SECONDARY_PHENOTYPE != "Inflammatory" &
    BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL,"") &
    all(VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2","")) & 
    PATHOGEN_SPECIFY %in% c(NA,"NA","NULL",NULL,"")
  ){
    TRUE
  } else {
    FALSE
  }
  } else {FALSE}
}

atypicalCovid <- function(age,VIRUS_DETECTED,
                          BACTERIA_SPECIFY,
                          PATHOGEN_SPECIFY,
                          VIRUS_ACCOUNT_ALL_FEATURES,
                          VIRUS_CONSISTENT_WITH_SYMPTOMS,
                          PATHOGEN_SYNDROMES,
                          FINAL_PHENOTYPE_DIAGNOSIS,
                          SECONDARY_PHENOTYPE,
                          INFLAMMATORY,
                          positiveCovidTests,
                          ...){
  
  VIRUS_DETECTED <- str_split(VIRUS_DETECTED,";",simplify = T)
  VIRUS_ACCOUNT_ALL_FEATURES <- str_split(VIRUS_ACCOUNT_ALL_FEATURES,";",simplify = T)
  VIRUS_CONSISTENT_WITH_SYMPTOMS <- str_split(VIRUS_CONSISTENT_WITH_SYMPTOMS,";",simplify = T)
  
  if("SARS-CoV-2" %in% VIRUS_DETECTED){
  if(
    (!is.na(age) & age<=18) &
    grepl("PCR|ANTIGEN",positiveCovidTests) &
    (VIRUS_CONSISTENT_WITH_SYMPTOMS[which(VIRUS_DETECTED=="SARS-CoV-2")] == "YES" |
     VIRUS_ACCOUNT_ALL_FEATURES[which(VIRUS_DETECTED=="SARS-CoV-2")] == "YES") &
    INFLAMMATORY != "COVID-RELATED INFLAMMATION" &
    ((!FINAL_PHENOTYPE_DIAGNOSIS %in% c("DEFINITE VIRAL","VIRAL SYNDROME")) | 
     PATHOGEN_SYNDROMES != "COIVD19") &
    FINAL_PHENOTYPE_DIAGNOSIS != "INFLAMATORY SYNDROM" &
    SECONDARY_PHENOTYPE != "Inflammatory" &
    BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL,"") &
    all(VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2","")) & 
    PATHOGEN_SPECIFY %in% c(NA,"NA","NULL",NULL,"")
  ){
    TRUE
  } else {
    FALSE
  }
  } else{FALSE}
}

coinfectCovid <- function(age,VIRUS_DETECTED,
                          BACTERIA_SPECIFY,
                          PATHOGEN_SPECIFY,
                          VIRUS_ACCOUNT_ALL_FEATURES,
                          VIRUS_CONSISTENT_WITH_SYMPTOMS,
                          PATHOGEN_SYNDROMES,
                          FINAL_PHENOTYPE_DIAGNOSIS,
                          SECONDARY_PHENOTYPE,
                          INFLAMMATORY,
                          positiveCovidTests,
                          ...){
  
  VIRUS_DETECTED <- str_split(VIRUS_DETECTED,";",simplify = T)
  VIRUS_ACCOUNT_ALL_FEATURES <- str_split(VIRUS_ACCOUNT_ALL_FEATURES,";",simplify = T)
  VIRUS_CONSISTENT_WITH_SYMPTOMS <- str_split(VIRUS_CONSISTENT_WITH_SYMPTOMS,";",simplify = T)
  
  if("SARS-CoV-2" %in% VIRUS_DETECTED){
  if(
    (!is.na(age) & age<=18) &
    grepl("PCR|ANTIGEN",positiveCovidTests) &
    (VIRUS_CONSISTENT_WITH_SYMPTOMS[which(VIRUS_DETECTED=="SARS-CoV-2")] == "YES" |
     VIRUS_ACCOUNT_ALL_FEATURES[which(VIRUS_DETECTED=="SARS-CoV-2")] == "YES") &
    INFLAMMATORY != "COVID-RELATED INFLAMMATION" &
    FINAL_PHENOTYPE_DIAGNOSIS != "INFLAMATORY SYNDROM" &
    SECONDARY_PHENOTYPE != "Inflammatory" &
     ((!BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL,"")) |
      any(!VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2","")))
  ){
    TRUE
  } else {
    FALSE
  }
  } else {FALSE}
}


incidentalCovid <- function(age,VIRUS_DETECTED,
         BACTERIA_SPECIFY,
         PATHOGEN_SPECIFY,
         VIRUS_ACCOUNT_ALL_FEATURES,
         VIRUS_CONSISTENT_WITH_SYMPTOMS,
         PATHOGEN_SYNDROMES,
         FINAL_PHENOTYPE_DIAGNOSIS,
         SECONDARY_PHENOTYPE,
         INFLAMMATORY,
         positiveCovidTests,
         ...){
  
  VIRUS_DETECTED <- str_split(VIRUS_DETECTED,";",simplify = T)
  VIRUS_ACCOUNT_ALL_FEATURES <- str_split(VIRUS_ACCOUNT_ALL_FEATURES,";",simplify = T)
  VIRUS_CONSISTENT_WITH_SYMPTOMS <- str_split(VIRUS_CONSISTENT_WITH_SYMPTOMS,";",simplify = T)
  
  if("SARS-CoV-2" %in% VIRUS_DETECTED){
  if(
    (!is.na(age) & age<=18) &
    grepl("PCR|ANTIGEN",positiveCovidTests) &
    (VIRUS_CONSISTENT_WITH_SYMPTOMS[which(VIRUS_DETECTED=="SARS-CoV-2")] == "NO" |
     VIRUS_ACCOUNT_ALL_FEATURES[which(VIRUS_DETECTED=="SARS-CoV-2")] == "NO") &
    INFLAMMATORY != "COVID-RELATED INFLAMMATION" &
    PATHOGEN_SYNDROMES != "COIVD19" &
    FINAL_PHENOTYPE_DIAGNOSIS != "INFLAMATORY SYNDROM" &
    SECONDARY_PHENOTYPE != "Inflammatory" &
    BACTERIA_SPECIFY %in% c(NA,"NA","NULL",NULL,"") &
    all(VIRUS_DETECTED %in% c(NA,NULL,"NA","NULL","SARS-CoV-2",""))
  ){
    TRUE
  } else {
    FALSE
  }
  } else{FALSE}
}


#### Add fields to DF ####
selectionsDF <- diamonds %>% mutate(
    #See which diamonds samples are on the RNA seq list above
    hadRNASeq =if_else(
        diamonds$UNIQUE_PATIENT_ID %in% c(RNAseqList$Patient.ID,RNAseqList$On.Jethro.clinical.diamonds.database.06_07_20),
        TRUE,
        FALSE
      ),
    
    #calcuate age ata symptom onset, hospital admission or base on todays date
    age = case_when(
      is.na(as.Date(DATE_OF_BIRTH)) ~ NA_real_,
      !is.na(as.Date(DATE_SYMPTOM_ONSET)) ~ as.numeric(as.Date(DATE_SYMPTOM_ONSET) - as.Date(DATE_OF_BIRTH))/365,
      !is.na(as.Date(DATETIME_FIRST_HOSPITAL)) ~ as.numeric(as.Date(DATETIME_FIRST_HOSPITAL) - as.Date(DATE_OF_BIRTH))/365,
      TRUE ~ as.numeric(Sys.Date() - as.Date(DATE_OF_BIRTH))/365
    ),
    
    #Apply positive and negative covid functions above
    positiveCovidTests = pmap_chr(., covidPositiveTests),
    negativeCovidTests = pmap_chr(., covidNegativeTests),
)
    
    

selectionsDF %<>% mutate(
  fullMisC = pmap_lgl(.,fullMisCriteria),
  fullPims = pmap_lgl(.,fullPIMSCriteria),
  pimsWoutAppendix = pmap_lgl(.,fullPIMSminusAppendix),
  MisCplusBactVir = pmap_lgl(.,MisCplusBactVir),
  PIMSplusBactVir = pmap_lgl(.,PIMSplusBactVir),
  pimsWoutAppendixBactVir = pmap_lgl(.,fullPIMSminusAppendixplusBactVir),

  uncomplicatedCovid = pmap_lgl(., uncomplicatedCovid),
  atypicalCovid = pmap_lgl(., atypicalCovid),
  coinfectCovid = pmap_lgl(., coinfectCovid),
  incidentalCovid = pmap_lgl(.,incidentalCovid)
)

selectionsDF %<>% mutate(
    selection = case_when(
      fullMisC ~ "Full Mis-C Criteria",
      #fullPims ~ "Full PIMS-TS criteria",
      pimsWoutAppendix ~ "Full PimsTS Wout Appendix",
      MisCplusBactVir ~ "MisC plus Bact/Viral Pathogen",
      #PIMSplusBactVir ~ "PIMS plus Bact/Viral Pathogen",
      pimsWoutAppendixBactVir ~ "PIMSwoutAppx plus Bact/Viral Pathogen",

      #other Covid Inflammation
        (!is.na(age) & age<=18) &
        (grepl("PCR|IGG|IGM|ANTIGEN",positiveCovidTests) |
         grepl("SARS-CoV-2",VIRUS_DETECTED)) &
        (FINAL_PHENOTYPE_DIAGNOSIS=="INFLAMATORY SYNDROM" |
           SECONDARY_PHENOTYPE=="Inflammatory" |
           INFLAMMATORY=="COVID-RELATED INFLAMMATION") ~ "Other Covid-Related Inflammation",

      #Acute Covid
      uncomplicatedCovid ~ "Uncomplicated Covid",
      atypicalCovid ~ "Atypical Covid",
      coinfectCovid ~ "Co-infection with Covid",
      incidentalCovid ~ "Incidental Covid",
      
      TRUE ~ NA_character_
    )
    
)


View(selectionsDF %>% filter(selection=="Uncomplicated Covid") %>%
       select(age,positiveCovidTests,FINAL_PHENOTYPE_DIAGNOSIS,SECONDARY_PHENOTYPE,
              PATHOGEN_SYNDROMES,VIRUS_DETECTED,VIRUS_CONSISTENT_WITH_SYMPTOMS,VIRUS_ACCOUNT_ALL_FEATURES,
              BACTERIA_SPECIFY,fullMisC,pimsWoutAppendix,MisCplusBactVir,pimsWoutAppendixBactVir))


table(selectionsDF %>% filter(!hadRNASeq) %>% select(selection))

View(selectionsDF %>% filter(selection=="Uncomplicated Covid") %>%
           select(age,positiveCovidTests,FINAL_PHENOTYPE_DIAGNOSIS,SECONDARY_PHENOTYPE,
           PATHOGEN_SYNDROMES,VIRUS_DETECTED,VIRUS_CONSISTENT_WITH_SYMPTOMS,VIRUS_ACCOUNT_ALL_FEATURES,
           BACTERIA_SPECIFY,fullMisC,pimsWoutAppendix,MisCplusBactVir,pimsWoutAppendixBactVir))
View(selectionsDF %>% filter(selection=="Incidental Covid") %>%
         select(age,positiveCovidTests,FINAL_PHENOTYPE_DIAGNOSIS,SECONDARY_PHENOTYPE,
         PATHOGEN_SYNDROMES,VIRUS_DETECTED,VIRUS_CONSISTENT_WITH_SYMPTOMS,VIRUS_ACCOUNT_ALL_FEATURES,
         BACTERIA_SPECIFY,fullMisC,pimsWoutAppendix,MisCplusBactVir,pimsWoutAppendixBactVir))

View(selectionsDF %>% filter(selection=="Atypical Covid") %>%
       select(age,positiveCovidTests,FINAL_PHENOTYPE_DIAGNOSIS,SECONDARY_PHENOTYPE,
              PATHOGEN_SYNDROMES,VIRUS_DETECTED,VIRUS_CONSISTENT_WITH_SYMPTOMS,VIRUS_ACCOUNT_ALL_FEATURES,
              BACTERIA_SPECIFY,fullMisC,pimsWoutAppendix,MisCplusBactVir,pimsWoutAppendixBactVir))


nestedSelections <- selectionsDF %>% group_by(selection) %>% nest()
nestedSelections %>% mutate(
n = map_int(data, ~{nrow(.x)}),
DB = map_int(data, ~{nrow(.x %>% filter(FINAL_PHENOTYPE_DIAGNOSIS=="DEFINITE BACTERIAL"))}),
DV = map_int(data, ~{nrow(.x %>% filter(FINAL_PHENOTYPE_DIAGNOSIS=="DEFINITE VIRAL"))}),

)
  
##########


covidNest <- covidPims %>% mutate(
  site = str_sub(ID, 5,8)
) %>%
  group_by(site) %>%
  nest()

covidNest %<>% mutate(
  noConsent = map_int(data, ~{nrow(.x %>% filter(hasConsent=="NO"& ageYears<=18))}),
  cantCalcAge = map_int(data, ~{nrow(.x %>% filter(is.na(ageYears) & hasConsent!="NO"& ageYears<=18))}),
  noHadRNASeq  =map_int(data, ~{nrow(.x %>% filter(hadRNASeq=="YES"& ageYears<=18))}),
  numberCOVIDAny = map_int(data, ~{nrow(.x %>% filter(!grepl(c("NEGATIVE","NULL"),covid19Status)))}),
  COVIDPCR = map_int(data, ~{nrow(.x %>% filter(grepl("PCR",covid19Status)))}),
  numberMisC = map_int(data, ~{nrow(.x %>% filter(misC=="YES" & hadRNASeq=="NO"& ageYears<=18))}),
  otherCovidRelatedInflam = map_int(data, ~{nrow(.x %>% filter(otherCovidInflam=="YES" & hadRNASeq=="NO"& ageYears<=18))}),
  incidental = map_int(data, ~{nrow(.x %>% filter(incidentalCovid=="YES" & hadRNASeq=="NO"& ageYears<=18))}),
  coinfect = map_int(data, ~{nrow(.x %>% filter(ageYears<=18 & hadRNASeq=="NO" & coinfectCovid=="YES"))}),
  atypical = map_int(data, ~{nrow(.x %>% filter(atypicalCovid=="YES" & hadRNASeq=="NO"& ageYears<=18))}),
  uncomplicated = map_int(data, ~{nrow(.x %>% filter(uncomplicatedCovid=="YES" & hadRNASeq=="NO" & ageYears<=18))})
)

View(covidNest)


covidNest %>% filter(uncomplicated>0) %>%
  unnest(cols = c(data)) %>% filter(uncomplicatedCovid=="YES")


