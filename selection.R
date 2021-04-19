library(tidyverse)
library(readxl)
library(httr)
library(stringr)
library(magrittr)


perform <- read_xlsx("diamondsexports/BIVA_ED_OCT20_2020.xlsx")

#because readxl cant read in from url yet
GET(url="https://www.euclids-perform-diamonds-h2020.eu/media/DIAMONDS_DAILY_EXPORT.xlsx",
    write_disk(tf <- tempfile(fileext = ".xlsx")))
diamonds <- read_xlsx(tf)
dateOfDiamondsDL <- Sys.Date()


RNAseqList <- read_xlsx("Batch_F_and_F_extension_noDELPHIC.xlsx",skip = 1)


#### Remove 0 from episode so all are in E1,E2 format ####

remove0 <- function(x){
 gsub("-E0","-E", x)
  
}

perform$UNIQUE_PATIENT_ID <- remove0(perform$UNIQUE_PATIENT_ID)
diamonds$UNIQUE_PATIENT_ID <- remove0(diamonds$UNIQUE_PATIENT_ID)
diamonds$ALTERNATE_STUDY_NO <- remove0(diamonds$ALTERNATE_STUDY_NO)
RNAseqList$Patient.ID <- remove0(RNAseqList$`Patient ID`)
RNAseqList$On.Jethro.clinical.diamonds.database.06_07_20 <- remove0(RNAseqList$`On Jethro clinical diamonds database 06_07_20`)


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

aetiologyNegative <- c("NO=NULL","NO=NEGATIVE","NULL=NULL","Unk=NULL","Unk=NEGATIVE","NULL=NEGATIVE",
                       "YES=SARS-CoV-2", "NO=SARS-CoV-2", "NULL=SARS-CoV-2", "Unk=SARS-CoV-2")
# 
# #### Old Functions ####
# #Find which tests were positive
# covidPositiveTests <- function(`TEST_TYPE=RESULT`, ...){
#   
#   if(is.na(`TEST_TYPE=RESULT`) | !grepl("POSITIVE", `TEST_TYPE=RESULT`)){NA_character_
#     } else {
#   
#   whichArePositive <- which(grepl("POSITIVE",str_split(`TEST_TYPE=RESULT`,";", simplify = T)))
#   
#   paste(gsub("=.*","",str_split(`TEST_TYPE=RESULT`,";", simplify = T)[whichArePositive]), collapse = ";")
#     }
# }
# 
# #Find which tests were positive
# covidNegativeTests <- function(`TEST_TYPE=RESULT`, ...){
#   
#   if(is.na(`TEST_TYPE=RESULT`) | !grepl("NEGATIVE", `TEST_TYPE=RESULT`)){NA_character_
#   } else {
#     
#     whichAreNegative <- which(grepl("NEGATIVE",str_split(`TEST_TYPE=RESULT`,";", simplify = T)))
#     
#     paste(gsub("=.*","",str_split(`TEST_TYPE=RESULT`,";", simplify = T)[whichAreNegative]), collapse = ";")
#   }
# }
# fullPIMSCriteria <- function(FEVER, 
#                              DATE_FEVER_ONSET,
#                              `TIME_POINT_8=FIBRINOGEN`,
#                              `TIME_POINT_16=CRP`,
#                              `TIME_POINT_20=D_DIMER`,
#                              `TIME_POINT_18=FERRITIN`,
#                              `TIME_POINT_14=ALBUMIN`,
#                              `TIME_POINT_5=LYMPHOCYTES`,
#                              SHOCK,
#                              `TIME_POINT_12=INOTROPES`,
#                              INOTROPES,
#                              RESPIRATORY,
#                              CARDIAC,
#                              GASTROINTESTINAL,
#                              SENSORY,
#                              HEADACHE,
#                              OTHER_NEURO,
#                              `VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                              `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                              PATHOGEN_SPECIFY,
#                              age,
#                              ...){
#   if(
#     ((!is.na(age)) & age <=18) & 
#     (FEVER%in%"YES" | (!DATE_FEVER_ONSET %in% c(NA,NULL,"NA","NULL"))) &
#      (SHOCK %in% "YES" | any(grepl("YES",`TIME_POINT_12=INOTROPES`)) | any(grepl("YES",INOTROPES)) | 
#       CARDIAC %in% "YES" | RESPIRATORY %in% "YES" | GASTROINTESTINAL %in% "YES" | 
#       SENSORY %in% "YES" | HEADACHE %in% "YES" | OTHER_NEURO %in% "YES") &
#      max(parse_number(str_split(`TIME_POINT_8=FIBRINOGEN`,";",simplify = T)), na.rm = T) > fibrinogen  &
#      max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP  &
#      max(parse_number(str_split(`TIME_POINT_20=D_DIMER`,";",simplify = T)), na.rm = T) > Ddimer  &
#      max(parse_number(str_split(`TIME_POINT_18=FERRITIN`,";",simplify = T)), na.rm = T) > Ferritin  &
#      max(parse_number(str_split(`TIME_POINT_14=ALBUMIN`,";",simplify = T)), na.rm = T) < Albumin  &
#      max(parse_number(str_split(`TIME_POINT_5=LYMPHOCYTES`,";",simplify = T)), na.rm = T) < Lymphocytes  &
#      `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in%aetiologyNegative &
#      `VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative & 
#      PATHOGEN_SPECIFY %in%aetiologyNegative
#      ){
#     TRUE
#   } else {
#     FALSE
#   }
#   
# }
# 
# fullPIMSminusAppendix <- function(FEVER, 
#                              DATE_FEVER_ONSET,
#                              `TIME_POINT_8=FIBRINOGEN`,
#                              `TIME_POINT_16=CRP`,
#                              `TIME_POINT_20=D_DIMER`,
#                              `TIME_POINT_18=FERRITIN`,
#                              `TIME_POINT_14=ALBUMIN`,
#                              `TIME_POINT_5=LYMPHOCYTES`,
#                              `TIME_POINT_4=NEUTROPHILS`,
#                              SHOCK,
#                              `TIME_POINT_12=INOTROPES`,
#                              INOTROPES,
#                              RESPIRATORY,
#                              CARDIAC,
#                              GASTROINTESTINAL,
#                              SENSORY,
#                              HEADACHE,
#                              OTHER_NEURO,
#                              `VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                              `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                              PATHOGEN_SPECIFY,
#                              age,
#                              ...){
#   if(
#     ((!is.na(age)) & age <=18) & 
#     (FEVER %in% "YES" | (!DATE_FEVER_ONSET %in% c(NA,NULL,"NA","NULL"))) &
#     (SHOCK %in% "YES" | any(grepl("YES",`TIME_POINT_12=INOTROPES`)) | any(grepl("YES",INOTROPES)) | 
#      CARDIAC %in% "YES" | RESPIRATORY %in% "YES" | GASTROINTESTINAL %in% "YES" | 
#      SENSORY %in% "YES" | HEADACHE %in% "YES" | OTHER_NEURO %in% "YES") &
#     max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP  &
#     max(parse_number(str_split(`TIME_POINT_4=NEUTROPHILS`,";",simplify = T)), na.rm = T) > neutrophils  &
#     max(parse_number(str_split(`TIME_POINT_5=LYMPHOCYTES`,";",simplify = T)), na.rm = T) < Lymphocytes  &
#     `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in% aetiologyNegative &
#    `VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative & 
#     PATHOGEN_SPECIFY %in%aetiologyNegative
#   ){
#     TRUE
#   } else {
#     FALSE
#   }
#   
# }
# 
# fullMisCriteria <- function(FEVER,
#                            DATE_FEVER_ONSET,
#                            DATETIME_FIRST_HOSPITAL,
#                            RASH,
#                            CONJUNCTIVITIS,
#                            MUCOSITIS,
#                            INFLAMMED_EXTREMITIES,
#                            SHOCK,
#                            `TIME_POINT_12=INOTROPES`,
#                            INOTROPES,
#                            FIRST_ECHO_NORMAL,
#                            FIRST_PERICARDITIS,
#                            FIRST_VALVULAR_REGURGITATION,
#                            WORST_ECHO_NORMAL,
#                            WORST_PERICARDITIS,
#                            WORST_VALVULAR_REGURGITATION,
#                            FIRST_ECHO_LOPEZ_Z_SCORE_1,
#                            FIRST_ECHO_LOPEZ_Z_SCORE_2,
#                            WORST_ECHO_LOPEZ_Z_SCORE_1,
#                            WORST_ECHO_LOPEZ_Z_SCORE_2,
#                            `TIME_POINT_19=TROP_T`,
#                            `TIME_POINT_22=BNP_NT`,
#                            `TIME_POINT_21=INR`,
#                            `TIME_POINT_9=PT`,
#                            `TIME_POINT_20=D_DIMER`,
#                            GASTROINTESTINAL,
#                            `VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                            `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                            PATHOGEN_SPECIFY,
#                            `TIME_POINT_16=CRP`,
#                            `TIME_POINT_17=PCT`,
#                            positiveCovidTests,
#                            age,
#                            ...){
#   
#   score = 0
#   if ("YES" %in% c(RASH,CONJUNCTIVITIS,MUCOSITIS,INFLAMMED_EXTREMITIES)){score <- score +1}
#   if (SHOCK %in% "YES" | any(grepl("YES",INOTROPES)) | any(grepl("YES",`TIME_POINT_12=INOTROPES`))) {score <- score + 1}
#   if (FIRST_ECHO_NORMAL %in% "NO" | WORST_ECHO_NORMAL %in% "NO" | 
#       "YES" %in% c(FIRST_PERICARDITIS,FIRST_VALVULAR_REGURGITATION,WORST_PERICARDITIS,WORST_VALVULAR_REGURGITATION) |
#       max(c(parse_number(FIRST_ECHO_LOPEZ_Z_SCORE_1),parse_number(FIRST_ECHO_LOPEZ_Z_SCORE_2),parse_number(WORST_ECHO_LOPEZ_Z_SCORE_1),parse_number(WORST_ECHO_LOPEZ_Z_SCORE_2)), na.rm = T)>2.5 |
#       max(parse_number(str_split(`TIME_POINT_19=TROP_T`,";",simplify = T)), na.rm = T) > tropT  |
#       max(parse_number(str_split(`TIME_POINT_22=BNP_NT`,";",simplify = T)), na.rm = T) > bnp  
#   ) { score <- score+1}
#   if (max(parse_number(str_split(`TIME_POINT_9=PT`,";",simplify = T)), na.rm = T) > PT |
#       max(parse_number(str_split(`TIME_POINT_21=INR`,";",simplify = T)), na.rm = T) > INR) {score <- score +1}
#   if(GASTROINTESTINAL %in% "YES"){score <- score+1}
#   
#   if(DATE_FEVER_ONSET %in% c("NA","NULL",NA,NULL) | DATETIME_FIRST_HOSPITAL %in% c("NA","NULL",NA,NULL)){
#     if(FEVER %in% "YES"){feverCriteria<- T} else {feverCriteria <- F}
#   } else {
#     if(as.numeric(as.Date(DATETIME_FIRST_HOSPITAL) - as.Date(DATE_FEVER_ONSET))>=3){
#       feverCriteria <- T
#     } else {feverCriteria <- F}
#   }
#   
#   
#   if(
#   
#     ((!is.na(age)) | age <=18) &
#     #fever >able to calculate =3 days or any fever if not able to calculate length of fever
#     feverCriteria &
#   score >=2 &
#   (max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP | 
#    max(parse_number(str_split(`TIME_POINT_17=PCT`,";",simplify = T)), na.rm = T) > PCT) &
#   `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in% c(NA,"NA","NULL",NULL,"") &
#   `VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% c(NA,NULL,"NA","NULL","SARS-CoV-2","") & 
#   PATHOGEN_SPECIFY %in% c(NA,"NA","NULL",NULL,"") &
#   (any(grepl("PCR|IGG|IGM|ANTIGEN",positiveCovidTests)) | any(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`)))
#     ){
#     TRUE
#   } else{
#     FALSE
#   }
#   
# }
# 
# PIMSplusBactVir <- function(FEVER, 
#                              DATE_FEVER_ONSET,
#                              `TIME_POINT_8=FIBRINOGEN`,
#                              `TIME_POINT_16=CRP`,
#                              `TIME_POINT_20=D_DIMER`,
#                              `TIME_POINT_18=FERRITIN`,
#                              `TIME_POINT_14=ALBUMIN`,
#                              `TIME_POINT_5=LYMPHOCYTES`,
#                              SHOCK,
#                              `TIME_POINT_12=INOTROPES`,
#                              INOTROPES,
#                              RESPIRATORY,
#                              CARDIAC,
#                              GASTROINTESTINAL,
#                              SENSORY,
#                              HEADACHE,
#                              OTHER_NEURO,
#                              `VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                              `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                              PATHOGEN_SPECIFY,
#                              age,
#                              ...){
#   if(
#     ((!is.na(age)) & age <=18) & 
#     (FEVER %in% "YES" | (!DATE_FEVER_ONSET %in% c(NA,NULL,"NA","NULL"))) &
#     (SHOCK %in% "YES" | any(grepl("YES",`TIME_POINT_12=INOTROPES`)) | any(grepl("YES",INOTROPES)) | 
#      CARDIAC %in% "YES" | RESPIRATORY %in% "YES" | GASTROINTESTINAL %in% "YES" | 
#      SENSORY %in% "YES" | HEADACHE %in% "YES" | OTHER_NEURO %in% "YES") &
#     max(parse_number(str_split(`TIME_POINT_8=FIBRINOGEN`,";",simplify = T)), na.rm = T) > fibrinogen  &
#     max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP  &
#     max(parse_number(str_split(`TIME_POINT_20=D_DIMER`,";",simplify = T)), na.rm = T) > Ddimer  &
#     max(parse_number(str_split(`TIME_POINT_18=FERRITIN`,";",simplify = T)), na.rm = T) > Ferritin  &
#     max(parse_number(str_split(`TIME_POINT_14=ALBUMIN`,";",simplify = T)), na.rm = T) < Albumin  &
#     max(parse_number(str_split(`TIME_POINT_5=LYMPHOCYTES`,";",simplify = T)), na.rm = T) < Lymphocytes  &
#     ((!`BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in%aetiologyNegative) |
#     (!`VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative))
#   ){
#     TRUE
#   } else {
#     FALSE
#   }
#   
# }
# 
# fullPIMSminusAppendixplusBactVir <- function(FEVER, 
#                                   DATE_FEVER_ONSET,
#                                   `TIME_POINT_8=FIBRINOGEN`,
#                                   `TIME_POINT_16=CRP`,
#                                   `TIME_POINT_20=D_DIMER`,
#                                   `TIME_POINT_18=FERRITIN`,
#                                   `TIME_POINT_14=ALBUMIN`,
#                                   `TIME_POINT_5=LYMPHOCYTES`,
#                                   `TIME_POINT_4=NEUTROPHILS`,
#                                   SHOCK,
#                                   `TIME_POINT_12=INOTROPES`,
#                                   INOTROPES,
#                                   RESPIRATORY,
#                                   CARDIAC,
#                                   GASTROINTESTINAL,
#                                   SENSORY,
#                                   HEADACHE,
#                                   OTHER_NEURO,
#                                   `VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                                   `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                                   PATHOGEN_SPECIFY,
#                                   age,
#                                   ...){
#   if(
#     ((!is.na(age)) & age <=18) & 
#     (FEVER %in% "YES" | (!DATE_FEVER_ONSET %in% c(NA,NULL,"NA","NULL"))) &
#     (SHOCK %in% "YES" | any(grepl("YES",`TIME_POINT_12=INOTROPES`)) | any(grepl("YES",INOTROPES)) | 
#      CARDIAC %in% "YES" | RESPIRATORY %in% "YES" | GASTROINTESTINAL %in% "YES" | 
#      SENSORY %in% "YES" | HEADACHE %in% "YES" | OTHER_NEURO %in% "YES") &
#     max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP  &
#     max(parse_number(str_split(`TIME_POINT_4=NEUTROPHILS`,";",simplify = T)), na.rm = T) > neutrophils  &
#     max(parse_number(str_split(`TIME_POINT_5=LYMPHOCYTES`,";",simplify = T)), na.rm = T) < Lymphocytes  &
#     ((!`BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in%aetiologyNegative) &
#     (!`VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative))
#   ){
#     TRUE
#   } else {
#     FALSE
#   }
#   
# }
# 
# 
# 
# MisCplusBactVir <- function(FEVER,
#                             DATE_FEVER_ONSET,
#                             DATETIME_FIRST_HOSPITAL,
#                             RASH,
#                             CONJUNCTIVITIS,
#                             MUCOSITIS,
#                             INFLAMMED_EXTREMITIES,
#                             SHOCK,
#                             `TIME_POINT_12=INOTROPES`,
#                             INOTROPES,
#                             FIRST_ECHO_NORMAL,
#                             FIRST_PERICARDITIS,
#                             FIRST_VALVULAR_REGURGITATION,
#                             WORST_ECHO_NORMAL,
#                             WORST_PERICARDITIS,
#                             WORST_VALVULAR_REGURGITATION,
#                             FIRST_ECHO_LOPEZ_Z_SCORE_1,
#                             FIRST_ECHO_LOPEZ_Z_SCORE_2,
#                             WORST_ECHO_LOPEZ_Z_SCORE_1,
#                             WORST_ECHO_LOPEZ_Z_SCORE_2,
#                             `TIME_POINT_19=TROP_T`,
#                             `TIME_POINT_22=BNP_NT`,
#                             `TIME_POINT_21=INR`,
#                             `TIME_POINT_9=PT`,
#                             `TIME_POINT_20=D_DIMER`,
#                             GASTROINTESTINAL,
#                             `VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                             `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                             PATHOGEN_SPECIFY,
#                             `TIME_POINT_16=CRP`,
#                             `TIME_POINT_17=PCT`,
#                             positiveCovidTests,
#                             age,
#                             ...){
#   
#   score = 0
#   if ("YES" %in% c(RASH,CONJUNCTIVITIS,MUCOSITIS,INFLAMMED_EXTREMITIES)){score <- score +1}
#   if (SHOCK %in% "YES" | any(grepl("YES",INOTROPES)) | any(grepl("YES",`TIME_POINT_12=INOTROPES`))) {score <- score + 1}
#   if (FIRST_ECHO_NORMAL %in% "NO" | WORST_ECHO_NORMAL %in% "NO" | 
#       "YES" %in% c(FIRST_PERICARDITIS,FIRST_VALVULAR_REGURGITATION,WORST_PERICARDITIS,WORST_VALVULAR_REGURGITATION) |
#       max(c(parse_number(FIRST_ECHO_LOPEZ_Z_SCORE_1),parse_number(FIRST_ECHO_LOPEZ_Z_SCORE_2),parse_number(WORST_ECHO_LOPEZ_Z_SCORE_1),parse_number(WORST_ECHO_LOPEZ_Z_SCORE_2)), na.rm = T)>2.5 |
#       max(parse_number(str_split(`TIME_POINT_19=TROP_T`,";",simplify = T)), na.rm = T) > tropT  |
#       max(parse_number(str_split(`TIME_POINT_22=BNP_NT`,";",simplify = T)), na.rm = T) > bnp  
#   ) { score <- score+1}
#   if (max(parse_number(str_split(`TIME_POINT_9=PT`,";",simplify = T)), na.rm = T) > PT |
#       max(parse_number(str_split(`TIME_POINT_21=INR`,";",simplify = T)), na.rm = T) > INR) {score <- score +1}
#   if(GASTROINTESTINAL %in% "YES"){score <- score+1}
#   
#   if(DATE_FEVER_ONSET %in% c("NA","NULL",NA,NULL) | DATETIME_FIRST_HOSPITAL %in% c("NA","NULL",NA,NULL)){
#     if(FEVER %in% "YES"){feverCriteria<- T} else {feverCriteria <- F}
#   } else {
#     if(as.numeric(as.Date(DATETIME_FIRST_HOSPITAL) - as.Date(DATE_FEVER_ONSET))>=3){
#       feverCriteria <- T
#     } else {feverCriteria <- F}
#   }
#   
#   
#   if(
#     
#     ((!is.na(age)) | age <=18) &
#     #fever >able to calculate =3 days or any fever if not able to calculate length of fever
#     feverCriteria &
#     score >=2 &
#     (max(parse_number(str_split(`TIME_POINT_16=CRP`,";",simplify = T)), na.rm = T) > CRP | 
#      max(parse_number(str_split(`TIME_POINT_17=PCT`,";",simplify = T)), na.rm = T) > PCT) &
#     ((!`BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in% aetiologyNegative) |
#     (!`VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative)) &
#     (grepl("PCR|IGG|IGM|ANTIGEN",positiveCovidTests) | grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))
#   ){
#     TRUE
#   } else{
#     FALSE
#   }
#   
# }
# 
# 
# uncomplicatedCovid <- function(age,`VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                                `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                                PATHOGEN_SPECIFY,
#                                `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,
#                                `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,
#                                PATHOGEN_SYNDROMES,
#                                FINAL_PHENOTYPE_DIAGNOSIS,
#                                SECONDARY_PHENOTYPE,
#                                INFLAMMATORY,
#                                positiveCovidTests,
#                                ...){
#   
#   `VIRUS_AETIOLOGY=VIRUS_DETECTED` <- str_split(`VIRUS_AETIOLOGY=VIRUS_DETECTED`,";",simplify = T)
#   `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES` <- str_split(`VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,";",simplify = T)
#   `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS` <- str_split(`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,";",simplify = T)
#   
#   if("SARS-CoV-2" %in% `VIRUS_AETIOLOGY=VIRUS_DETECTED`){
#   if(
#     (!is.na(age) & age<=18) &
#     grepl("PCR|ANTIGEN",positiveCovidTests) & 
#     (gsub(".*=","",`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`[which(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))]) %in% "YES" |
#      gsub(".*=","",`VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`[which(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))]) %in%  "YES") &
#     PATHOGEN_SYNDROMES %in% "COIVD19" &
#     (!INFLAMMATORY %in% "COVID-RELATED INFLAMMATION") &
#     FINAL_PHENOTYPE_DIAGNOSIS %in% c("DEFINITE VIRAL","VIRAL SYNDROME") &
#     (!SECONDARY_PHENOTYPE %in% "Inflammatory") &
#     `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in% aetiologyNegative &
#     all(`VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative) & 
#     PATHOGEN_SPECIFY %in% aetiologyNegative
#   ){
#     TRUE
#   } else {
#     FALSE
#   }
#   } else {FALSE}
# }
# 
# atypicalCovid <- function(age,`VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                           `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                           PATHOGEN_SPECIFY,
#                           `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,
#                           `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,
#                           PATHOGEN_SYNDROMES,
#                           FINAL_PHENOTYPE_DIAGNOSIS,
#                           SECONDARY_PHENOTYPE,
#                           INFLAMMATORY,
#                           positiveCovidTests,
#                           ...){
#   
#   `VIRUS_AETIOLOGY=VIRUS_DETECTED` <- str_split(`VIRUS_AETIOLOGY=VIRUS_DETECTED`,";",simplify = T)
#   `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES` <- str_split(`VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,";",simplify = T)
#   `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS` <- str_split(`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,";",simplify = T)
#   
#   if("SARS-CoV-2" %in% `VIRUS_AETIOLOGY=VIRUS_DETECTED`){
#   if(
#     (!is.na(age) & age<=18) &
#     grepl("PCR|ANTIGEN",positiveCovidTests) &
#     (gsub(".*=","",`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`[which(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))]) %in% "YES" |
#      gsub(".*=","",`VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`[which(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))]) %in% "YES") &
#     (!INFLAMMATORY %in% "COVID-RELATED INFLAMMATION") &
#     ((!FINAL_PHENOTYPE_DIAGNOSIS %in% c("DEFINITE VIRAL","VIRAL SYNDROME")) | 
#      (!PATHOGEN_SYNDROMES %in% "COIVD19")) &
#     (!FINAL_PHENOTYPE_DIAGNOSIS %in% "INFLAMATORY SYNDROM") &
#     (!SECONDARY_PHENOTYPE %in% "Inflammatory") &
#     `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in% aetiologyNegative &
#     all(`VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative) & 
#     PATHOGEN_SPECIFY %in% aetiologyNegative
#   ){
#     TRUE
#   } else {
#     FALSE
#   }
#   } else{FALSE}
# }
# 
# coinfectCovid <- function(age,`VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#                           `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#                           PATHOGEN_SPECIFY,
#                           `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,
#                           `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,
#                           PATHOGEN_SYNDROMES,
#                           FINAL_PHENOTYPE_DIAGNOSIS,
#                           SECONDARY_PHENOTYPE,
#                           INFLAMMATORY,
#                           positiveCovidTests,
#                           ...){
#   
#   `VIRUS_AETIOLOGY=VIRUS_DETECTED` <- str_split(`VIRUS_AETIOLOGY=VIRUS_DETECTED`,";",simplify = T)
#   `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES` <- str_split(`VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,";",simplify = T)
#   `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS` <- str_split(`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,";",simplify = T)
#   
#   if("SARS-CoV-2" %in% `VIRUS_AETIOLOGY=VIRUS_DETECTED`){
#   if(
#     (!is.na(age) & age<=18) &
#     grepl("PCR|ANTIGEN",positiveCovidTests) &
#     (gsub(".*=","",`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`[which(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))]) %in% "YES" |
#      gsub(".*=","",`VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`[which(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))]) %in% "YES") &
#     (!INFLAMMATORY %in% "COVID-RELATED INFLAMMATION") &
#     (!FINAL_PHENOTYPE_DIAGNOSIS %in% "INFLAMATORY SYNDROM") &
#     (!SECONDARY_PHENOTYPE %in% "Inflammatory") &
#      ((!`BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in% aetiologyNegative) |
#       any(!`VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative))
#   ){
#     TRUE
#   } else {
#     FALSE
#   }
#   } else {FALSE}
# }
# 
# 
# incidentalCovid <- function(age,`VIRUS_AETIOLOGY=VIRUS_DETECTED`,
#          `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
#          PATHOGEN_SPECIFY,
#          `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,
#          `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,
#          PATHOGEN_SYNDROMES,
#          FINAL_PHENOTYPE_DIAGNOSIS,
#          SECONDARY_PHENOTYPE,
#          INFLAMMATORY,
#          positiveCovidTests,
#          ...){
#   
#   `VIRUS_AETIOLOGY=VIRUS_DETECTED` <- str_split(`VIRUS_AETIOLOGY=VIRUS_DETECTED`,";",simplify = T)
#   `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES` <- str_split(`VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,";",simplify = T)
#   `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS` <- str_split(`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,";",simplify = T)
#   
#   if("SARS-CoV-2" %in% `VIRUS_AETIOLOGY=VIRUS_DETECTED`){
#   if(
#     (!is.na(age) & age<=18) &
#     grepl("PCR|ANTIGEN",positiveCovidTests) &
#     (gsub(".*=","",`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`[which(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))]) %in% c("NO",NA,NULL,"NULL","Unk") |
#      gsub(".*=","",`VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`[which(grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`))]) %in% c("NO",NA,NULL,"NULL","Unk")) &
#     (!INFLAMMATORY %in% "COVID-RELATED INFLAMMATION") &
#     (!PATHOGEN_SYNDROMES %in% "COIVD19") &
#     (!FINAL_PHENOTYPE_DIAGNOSIS %in% "INFLAMATORY SYNDROM") &
#     (!SECONDARY_PHENOTYPE %in% "Inflammatory") &
#     `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY` %in% aetiologyNegative &
#     all(`VIRUS_AETIOLOGY=VIRUS_DETECTED` %in% aetiologyNegative)
#   ){
#     TRUE
#   } else {
#     FALSE
#   }
#   } else{FALSE}
# }
# 
# 





#### New Functions ####

positiveCovidTests <- function(`TEST_TYPE=RESULT`, ...){
  
  if(any(grepl("POSITIVE",`TEST_TYPE=RESULT`))){
    
    whichArePositive <- which(grepl("POSITIVE",str_split(`TEST_TYPE=RESULT`,";",simplify = T)))
    
    paste(gsub("=.*","",str_split(`TEST_TYPE=RESULT`,";",simplify = T)[,whichArePositive]),collapse = ";")
  } else {
    NA_character_
  }
  
}


negativeCovidTests <- function(`TEST_TYPE=RESULT`, ...){
  
  if(any(grepl("NEGATIVE",`TEST_TYPE=RESULT`))){
    
    whichAreNegative <- which(grepl("NEGATIVE",str_split(`TEST_TYPE=RESULT`,";",simplify = T)))
    
    paste(gsub("=.*","",str_split(`TEST_TYPE=RESULT`,";",simplify = T)[,whichAreNegative]),collapse = ";")
  } else {
    NA_character_
  }
  
}


feverLength <- function(DATE_FEVER_ONSET,
                        DATE_FIRST_HOSPITAL,
                        DATETIME_FRB, 
                        FEVER,
                        ...){
  if(is.na(DATE_FEVER_ONSET)){
    if(!FEVER %in% "YES"){
      NA_integer_
    } else{100}
  } else if(!is.na(DATETIME_FRB)) {
    as.integer(as.Date(DATETIME_FRB) - as.Date(DATE_FEVER_ONSET))
  } else if(!is.na(DATE_FIRST_HOSPITAL)){
    as.integer(as.Date(DATE_FIRST_HOSPITAL) - as.Date(DATE_FEVER_ONSET))
  } else {100}
}

misCFeatures <- function(RASH,
                         CONJUNCTIVITIS,
                         INFLAMMED_EXTREMITIES,
                         MUCOSITIS,
                         SHOCK,
                         `TIME_POINT_12=INOTROPES`,
                         INOTROPES,
                         FIRST_ECHO_NORMAL,
                         FIRST_MYOCARDITIS,
                         FIRST_PERICARDITIS,
                         FIRST_VALVULAR_REGURGITATION,
                         WORST_ECHO_NORMAL,
                         WORST_MYOCARDITIS,
                         WORST_PERICARDITIS,
                         WORST_VALVULAR_REGURGITATION,
                         `TIME_POINT_19=TROP_T`,
                         `TIME_POINT_22=BNP_NT`,
                         `TIME_POINT_21=INR`,
                         `TIME_POINT_9=PT`,
                         GASTROINTESTINAL,
                         ...){
  
  score <- 0
  
  if ("YES" %in% c(RASH,CONJUNCTIVITIS,MUCOSITIS,INFLAMMED_EXTREMITIES)) {score <- score + 1}
  
  if ("YES" %in% c(SHOCK,INOTROPES) | grepl("YES",`TIME_POINT_12=INOTROPES`)) {score <- score + 1}
  
  if (
    "NO" %in% c(FIRST_ECHO_NORMAL,WORST_ECHO_NORMAL) |
      "YES" %in% c(FIRST_MYOCARDITIS,FIRST_PERICARDITIS,FIRST_VALVULAR_REGURGITATION,
                   WORST_MYOCARDITIS,WORST_PERICARDITIS,WORST_VALVULAR_REGURGITATION) |
        any(as.numeric(sub(".*=","", str_split(`TIME_POINT_19=TROP_T`,";",simplify = T)))>tropT, na.rm = T) |
        any(as.numeric(sub(".*=","",str_split(`TIME_POINT_22=BNP_NT`,";",simplify = T)))>bnp, na.rm = T)
  ) {score <- score + 1}
  
  if(
     any(as.numeric(sub(".*=","",str_split(`TIME_POINT_9=PT`,";",simplify = T)))>PT, na.rm = T) |
     any(as.numeric(sub(".*=","",str_split(`TIME_POINT_21=INR`,";",simplify = T)))>INR, na.rm = T)
  ) {score <- score + 1}
  
  if ("YES" %in% GASTROINTESTINAL) {score <- score + 1}
  
  score
}

KDcriteria <- function(RASH,
                       MUCOSITIS,
                       CONJUNCTIVITIS,
                       INFLAMMED_EXTREMITIES,
                       LYMPHADENOPATHY,
                       ...){
  score <- 0
  if(RASH %in% "YES") {score <- score + 1}
  if(MUCOSITIS %in% "YES") {score <- score + 1}
  if(CONJUNCTIVITIS %in% "YES") {score <- score + 1}
  if(INFLAMMED_EXTREMITIES %in% "YES") {score <- score + 1}
  if(LYMPHADENOPATHY %in% "YES") {score <- score + 1}
  
  score
}

maxZscore <- function(FIRST_ECHO_LOPEZ_Z_SCORE_1,
                      FIRST_ECHO_LOPEZ_Z_SCORE_2,
                      WORST_ECHO_LOPEZ_Z_SCORE_1,
                      WORST_ECHO_LOPEZ_Z_SCORE_2,
                      ...){
  
  if (all(is.na(parse_number(as.character(c(FIRST_ECHO_LOPEZ_Z_SCORE_1,
                                            FIRST_ECHO_LOPEZ_Z_SCORE_2,
                                            WORST_ECHO_LOPEZ_Z_SCORE_1,
                                            WORST_ECHO_LOPEZ_Z_SCORE_2)))))){
    NA_integer_
  } else {
    
    max(parse_number(as.character(c(FIRST_ECHO_LOPEZ_Z_SCORE_1,
                                    FIRST_ECHO_LOPEZ_Z_SCORE_2,
                                    WORST_ECHO_LOPEZ_Z_SCORE_1,
                                    WORST_ECHO_LOPEZ_Z_SCORE_2))), na.rm = T)
  }
  
}


anyFeatures <- function(`VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS`,
                        `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`,
                        `VIRUS_AETIOLOGY=VIRUS_DETECTED`,
                        ...) {
  
  if (grepl("SARS-CoV-2",`VIRUS_AETIOLOGY=VIRUS_DETECTED`)){
    if ("SARS-CoV-2=YES" %in% `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS` |
        "SARS-CoV-2=YES" %in% `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`) {TRUE
      } else if (
        "SARS-CoV-2=NO" %in% `VIRUS_DETECTED_1=VIRUS_CONSISTENT_WITH_SYMPTOMS` &
        "SARS-CoV-2=NO" %in% `VIRUS_DETECTED_2=VIRUS_ACCOUNT_ALL_FEATURES`
      ) {FALSE
        } else {NA}
    
  } else {NA}
  
  
}

otherOrgs <- function(`VIRUS_AETIOLOGY=VIRUS_DETECTED`,
                      `BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
                      ...){
  
  if (
    all(sub(".*=","",str_split(`VIRUS_AETIOLOGY=VIRUS_DETECTED`,";",simplify = T)) %in% c(NA,NULL,"NA","NULL","SARS-CoV-2","Unk","NO"), na.rm = T) &
    all(sub(".*=","",str_split(`BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,";",simplify = T)) %in% c(NA,NULL,"NA","NULL","SARS-CoV-2","Unk","NO"), na.rm = T)
  ){
    FALSE
    } else {TRUE}
  
}

processTime <- function(x){
  
  if (!is.na(x)){
    as.double(str_split(x, ";",simplify = T)[1])
  } else {
    NA
  }
  
}


forOxford <- function(selection,
                      hasConsent,
                      hadRNASeq,
                      samplesAvailable,
                      PAX_TUBE_TP2,
                      PAXProcessTime,
                      ...){
  
  if(!hasConsent){
    "No Consent"
  } else if(hadRNASeq){
    "Already Sequenced"
  } else if(PAX_TUBE_TP2 %in% "YES"){
    "Has TP2 sample"
  } else{
    
    if(selection %in% "possible MisC"){
      "?"
    } else if (selection %in% c("Full Mis-C Criteria","MisC with Co-Infection","Uncomplicated Covid","Atypical Covid")){
      if(is.na(samplesAvailable)){
        "No Sample Data"
      } else if(samplesAvailable %in% "All") {
        "1"
      } else if(samplesAvailable %in% c("No EDTA","No Serum")){
        "2"
      } else if(samplesAvailable %in% "Only PAX"){
        "3"
      } else {"No PAX"}
      
    } else{""}
    
  }
  
}



#### Add fields to DF ####
selectionsDF <- diamonds %>% mutate(
    #See which diamonds samples are on the RNA seq list above
    hadRNASeq =if_else(
        diamonds$UNIQUE_PATIENT_ID %in% c(RNAseqList$Patient.ID,RNAseqList$On.Jethro.clinical.diamonds.database.06_07_20),
        TRUE,
        FALSE
      ),
    
    hasConsent = if_else(
      diamonds$CONSENT_USE_RESEARCH_SAMPLE %in% "YES",
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
    positiveCovidTests = pmap_chr(.,positiveCovidTests),
    negativeCovidTests = pmap_chr(.,negativeCovidTests),

  
  InflammatoryPatient = if_else(
    FINAL_PHENOTYPE_DIAGNOSIS %in% "INFLAMATORY SYNDROM" | SECONDARY_PHENOTYPE %in% "Inflammatory" | !is.na(INFLAMMATORY),
    TRUE,
    FALSE
  ),
  
  anyCovidPos = if_else(
    PREVIOUS_HISTORY_COVID19 %in% "Yes" | PREVIOUS_ADMISSION_COVID19 %in% "Yes" | !is.na(positiveCovidTests),
    TRUE,
    FALSE
  ),
  
  covidPCRPos = if_else(
    grepl("PCR|ANTIGEN",positiveCovidTests),
    TRUE,
    FALSE
  ),
  
  anyfeaturesofIllness = pmap_lgl(.,anyFeatures),
  
  paediatric = if_else(
    age<19,
    TRUE,
    FALSE
  ),
  
  feverLength = pmap_dbl(.,feverLength),
  
  raisedInflam = if_else(
    MAXIMUM_CRP > CRP | any(as.numeric(gsub(".*=","",str_split(diamonds$`TIME_POINT_17=PCT`[942],";", simplify = T))) > PCT),
    TRUE,
    FALSE
  ),
  
  otherOrgs = pmap_lgl(., otherOrgs),
  
  misCFeatures = pmap_dbl(.,misCFeatures),
  
  KDFeatures = pmap_dbl(., KDcriteria),
  
  maxZscore = pmap_dbl(., maxZscore),
  
  samplesAvailable = case_when(
    !is.na(PAX_DATE_TIME) & !is.na(SERUM_DATE_TIME) & !is.na(EDTA_DATE_TIME) ~ "All",
    !is.na(PAX_DATE_TIME) & !is.na(SERUM_DATE_TIME) & is.na(EDTA_DATE_TIME) ~ "No EDTA",
    !is.na(PAX_DATE_TIME) & is.na(SERUM_DATE_TIME) & !is.na(EDTA_DATE_TIME) ~ "No Serum",
    is.na(PAX_DATE_TIME) & !is.na(SERUM_DATE_TIME) & !is.na(EDTA_DATE_TIME) ~ "No PAX",
    !is.na(PAX_DATE_TIME) & is.na(SERUM_DATE_TIME) & is.na(EDTA_DATE_TIME) ~ "Only PAX",
    is.na(PAX_DATE_TIME) & !is.na(SERUM_DATE_TIME) & is.na(EDTA_DATE_TIME) ~ "Only Serum",
    is.na(PAX_DATE_TIME) & is.na(SERUM_DATE_TIME) & !is.na(EDTA_DATE_TIME) ~ "Only EDTA",
    TRUE ~ NA_character_
  ),
  
  PAXProcessTime = map_dbl(PAX_TIME_TO_PROCESS,processTime),
  SerumProcessTime = map_dbl(SERUM_TIME_TO_PROCESSS,processTime),
  EDTAProcesTime = map_dbl(EDTA_TIME_TO_PROCESS,processTime),
  
  admissiontoPax =  as.integer(as.Date(str_split(PAX_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(DATETIME_FIRST_HOSPITAL,";",simplify = T)[,1])),
  admissiontoSerum = as.integer(as.Date(str_split(SERUM_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(DATETIME_FIRST_HOSPITAL,";",simplify = T)[,1])),
  admissiontoEDTA =  as.integer(as.Date(str_split(EDTA_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(DATETIME_FIRST_HOSPITAL,";",simplify = T)[,1])),
  
  firstIxtoPax =  as.integer(as.Date(str_split(PAX_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(DATE_TIME_INVESTIGATIONS,";",simplify = T)[,1])),
  firstIxtoSerum =  as.integer(as.Date(str_split(SERUM_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(DATE_TIME_INVESTIGATIONS,";",simplify = T)[,1])),
  firstIxtoEDTA =  as.integer(as.Date(str_split(EDTA_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(DATE_TIME_INVESTIGATIONS,";",simplify = T)[,1])),
  
  cultureToPax = as.integer(as.Date(str_split(PAX_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(CULTURE_DATE,";",simplify = T)[,1])),
  cultureToSerum = as.integer(as.Date(str_split(SERUM_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(CULTURE_DATE,";",simplify = T)[,1])),
  cultureToEDTA = as.integer(as.Date(str_split(EDTA_DATE_TIME,";",simplify=T)[,1]) - as.Date(str_split(CULTURE_DATE,";",simplify = T)[,1])),
  
  
  
) %>% unite ("syndromesConcat",c(CARDIOVASCULAR,LOWER_RESPIRATORY_TRACT,URTI_EAR_NOSE_THROAT,
                                      MUSCULOSKELETAL.1,NEUROLOGICAL,SURGICAL,
                                      SOFT_TISSUE,URINARY_TRACT,PATHOGEN_SYNDROMES,
                                      SEPSIS_SYNDROMES, UNDIFFERENTIATED_FEVER,
                                      OTHER_INFECTIONS,INFLAMMATORY,GIT,HAEMATOLOGICAL,
                                      OTHER_SYNDROMES_ICD10),
              sep = "; ", na.rm = T, remove = F) %>%

 mutate(
  
  fullMisC = if_else(
    InflammatoryPatient & (anyCovidPos|INFLAMMATORY %in% "COVID-RELATED INFLAMMATION") & paediatric & feverLength>=3 & raisedInflam & (!otherOrgs) & misCFeatures>=2,
    TRUE,
    FALSE
  ),
  
  MiscCwithCoInfection = if_else(
    InflammatoryPatient & (anyCovidPos|INFLAMMATORY %in% "COVID-RELATED INFLAMMATION") & paediatric & feverLength>=3 & raisedInflam & otherOrgs & misCFeatures>=2,
    TRUE,
    FALSE
  ),
  
  OtherCovidInflam = if_else(
    InflammatoryPatient & (anyCovidPos|INFLAMMATORY %in% "COVID-RELATED INFLAMMATION") & paediatric & !(feverLength>=3 & raisedInflam & otherOrgs & misCFeatures>=2),
    TRUE,
    FALSE
  ),
  
  NonCovidInflammation = if_else(
    InflammatoryPatient & (!INFLAMMATORY %in% "COVID-RELATED INFLAMMATION") & paediatric & !anyCovidPos,
    TRUE,
    FALSE
  ),
  
  incidentalCovid = if_else(
    covidPCRPos & !anyfeaturesofIllness & paediatric,
    TRUE,
    FALSE
  ),
  
  covidCoinfection = if_else(
    covidPCRPos & anyfeaturesofIllness & otherOrgs & paediatric,
    TRUE,
    FALSE
  ),
  
  uncomplicatedCovid = if_else(
    covidPCRPos & anyfeaturesofIllness & paediatric & (!otherOrgs) & (!InflammatoryPatient) & grepl("COIVD19",PATHOGEN_SYNDROMES) & FINAL_PHENOTYPE_DIAGNOSIS %in% c("VIRAL SYNDROME","DEFINITE VIRAL"),
    TRUE,
    FALSE
  ),
  
  atypicalCovid = if_else(
    covidPCRPos & paediatric & anyfeaturesofIllness & (!otherOrgs) & (!InflammatoryPatient) & !(grepl("COIVD19",PATHOGEN_SYNDROMES) & FINAL_PHENOTYPE_DIAGNOSIS %in% c("VIRAL SYNDROME","DEFINITE VIRAL")),
    TRUE,
    FALSE
  ),
  
  
  classicalKD = if_else(
    feverLength >=4 & KDFeatures >=4,
    TRUE,
    FALSE
  ),
  
  atypicalKD = if_else(t 5999)
  
) %>%

 mutate(
    selection = case_when(
      fullMisC ~ "Full Mis-C Criteria",
      MiscCwithCoInfection ~ "MisC with Co-Infection",
      OtherCovidInflam & (is.na(RASH) &
                            is.na(CONJUNCTIVITIS) &
                            is.na(INFLAMMED_EXTREMITIES) &
                            is.na(MUCOSITIS) &
                            is.na(SHOCK) &
                            is.na(INOTROPES)) ~ "possible MisC",
      OtherCovidInflam ~ "Other Covid Inflammatory Disease",
      NonCovidInflammation ~ "Non-Covid Related Inflammatory Disease",


      #Acute Covid
      incidentalCovid ~ "Incidental Covid",
      covidCoinfection ~ "Co-infection with Covid",
      atypicalCovid ~ "Atypical Covid",
      uncomplicatedCovid ~ "Uncomplicated Covid",
      
      TRUE ~ NA_character_
    ),
    ForOxford = ""
    
) %>% mutate( OxfordFirstPass = pmap_chr(.,forOxford))


write_excel_csv(selectionsDF %>% select(UNIQUE_PATIENT_ID,selection, OxfordFirstPass, ForOxford,
                        age,GENDER,ETHNICITY_REPORTED,
                        FINAL_PHENOTYPE_DIAGNOSIS,SECONDARY_PHENOTYPE,syndromesConcat,
                        otherOrgs,`VIRUS_AETIOLOGY=VIRUS_DETECTED`,`BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,PATHOGEN_DETECTED,
                        `SAMPLE_TYPE_1=CULTURE_RESULT`, 
                        hasConsent,hadRNASeq,
                        classicalKD,atypicalKD,
                        samplesAvailable,
                        PAXProcessTime,SerumProcessTime,EDTAProcesTime,
                        contains("admissionto"),contains("firstIx"), contains("cultureTo"),
                        EDTA_ALLIQUOT,SMART_TUBE,STORAGE_LOCATION,
                        PAX_TUBE_TP1,PAX_TUBE_TP2,PAXgene_TYPE,COMMENTS_4,
                        SERUM_100,SERUM_250,COMMENTS_8
                        ), file = "ForSelection.csv")


nestedSelections <- selectionsDF %>% filter(!is.na(selection)) %>%
  select(UNIQUE_PATIENT_ID, SITE_CODE,
         age, GENDER, ETHNICITY_REPORTED,
         DATETIME_FIRST_HOSPITAL,hasConsent,hadRNASeq,
         `VIRUS_AETIOLOGY=VIRUS_DETECTED`,`BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
         syndromesConcat,FINAL_PHENOTYPE_DIAGNOSIS,SECONDARY_PHENOTYPE,
         PAX_TIME_TO_PROCESS,SERUM_TIME_TO_PROCESSS,EDTA_TIME_TO_PROCESS,THROAT_SWAB,
         otherOrgs,
         selection,classicalKD,atypicalKD) %>%
  group_by(selection) %>% 
  nest() %>% 
  mutate(
n = map_int(data, ~{nrow(.x)}),
hasConsentandNotSequenced = map_int(data, ~{nrow(.x %>% filter(hasConsent & !hadRNASeq))}),

PCRpos = map_int(data, ~{nrow(.x %>% filter(covidPCRPos))}),
otherPos = map_int(data, ~{nrow(.x %>% filter(anyCovidPos & !covidPCRPos))}),

DB = map_int(data, ~{nrow(.x %>% filter(FINAL_PHENOTYPE_DIAGNOSIS=="DEFINITE BACTERIAL"))}),
DV = map_int(data, ~{nrow(.x %>% filter(FINAL_PHENOTYPE_DIAGNOSIS=="DEFINITE VIRAL"))}),

classicalKD = map_int(data, ~{nrow(.x %>% filter(classicalKD))}),
atypicalKD = map_int(data, ~{nrow(.x %>% filter(atypicalKD))}),



syndromeCRI = map_int(data, ~{nrow(.x %>% filter(grepl("COVID-RELATED",syndromesConcat)))}),
syndromeCRICoi = map_int(data, ~{nrow(.x %>% filter(grepl("COVID-RELATED",syndromesConcat) & otherOrgs))})
)
  

View(nestedSelections)
write_excel_csv(selectionsDF %>% filter(!is.na(selection)) %>%
  select(UNIQUE_PATIENT_ID, SITE_CODE, selection, age, GENDER, ETHNICITY_REPORTED,
         DATETIME_FIRST_HOSPITAL,hasConsent,hadRNASeq,
         `VIRUS_AETIOLOGY=VIRUS_DETECTED`,`BACTERIA_AETIOLOGY=BACTERIA_SPECIFY`,
         syndromesConcat,FINAL_PHENOTYPE_DIAGNOSIS,SECONDARY_PHENOTYPE,
         PAXgene_TYPE,PAX_TIME_TO_PROCESS,
         SERUM_TIME_TO_PROCESSS,
         EDTA_TIME_TO_PROCESS,
         THROAT_SWAB,
         classicalKD,atypicalKD), file="DiamondsSelection.csv")



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


