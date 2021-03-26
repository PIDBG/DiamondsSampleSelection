
#### library ####
library(DiagrammeR)
library(openxlsx)
library(stringi)
library(stringr)
library(readr)
library(tidyr)
library(dplyr)

#diamondsFilename <- "diamondsexports/DIAMONDS_EXPORT_MARCH3.xlsx"



#### import and intial qc  +  filtering  ####
#import RNAseqList
RNAseqList <- read.xlsx("Batch_F_and_F_extension_noDELPHIC.xlsx", startRow = 2)

#add a 0 when format of id is -E1  so all have format -E01
stri_sub(RNAseqList$Patient.ID[stri_sub(RNAseqList$Patient.ID,15,16)!="E0"],16,15) <- 0

#import diamonds + perform export
#diamondsExport <- read.xlsx(diamondsFilename, detectDates = T)
diamondsExport <- read_delim("https://www.euclids-perform-diamonds-h2020.eu/media/e2.txt", delim = "\t")
diamondsExport <- as.data.frame(diamondsExport)

downloadDate <- format(Sys.Date(), "%d/%m/%Y")
#### create variables for covidPims #####
#new dataFrame for pims/covid recording
covidPims <- data.frame(ID = diamondsExport$UNIQUE_PATIENT_ID, 
                        comments="",
                        ageYears = 0, feverLength = 0, rash = "", conj="", mucositis="",extremities="", lymphadenitis = "", 
                        shock = "", myocardialDysfunction = "",maxZscore=NA, coagulopathy = "",
                        GI = "", maxCRP=NA, maxESR = NA, maxProcalc = NA, bacteriaSpecify = "", covid19Status = "", Phenotype1 = "",
                        Phenotype2 = "", pathogenSyndrome = "", inflamSyndrome = "")



  for (i in 1:nrow(covidPims)) {
    if (covidPims$ID[i] %in% diamondsExport$UNIQUE_PATIENT_ID){ #if patient in diamonds db
      
      #calculate age at symptom onset in years####
      if (!(diamondsExport$DATE_SYMPTOM_ONSET[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL") ||
            diamondsExport$DATE_OF_BIRTH[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL"))){
      covidPims$ageYears[i] <-
        as.numeric((as.Date(diamondsExport$DATE_SYMPTOM_ONSET[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]) - 
                      as.Date(diamondsExport$DATE_OF_BIRTH[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]))/365)
      } else if (!diamondsExport$DATE_OF_BIRTH[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL")){
        covidPims$ageYears[i] <- as.numeric((Sys.Date() - as.Date(diamondsExport$DATE_OF_BIRTH[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])))/365
        
      }
      
      
      #fever length####
      if (!(diamondsExport$DATE_FEVER_ONSET[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL") ||
            diamondsExport$DATETIME_FRB[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL"))){
        covidPims$feverLength[i] <- 
          as.Date(diamondsExport$DATETIME_FRB[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]) - 
          as.Date(diamondsExport$DATE_FEVER_ONSET[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])
      }
      
      
      #rash/conj/muco-cutaneous/lymph####
      covidPims$rash[i] <-
        diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                               "RASH"]
      
      covidPims$conj[i] <-
        diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                         "CONJUNCTIVITIS"]
      
      covidPims$mucositis[i] <-
        diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                         "MUCOSITIS"]
      
      covidPims$extremities[i] <-
        diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                          "INFLAMMED_EXTREMITIES"]
      
      covidPims$lymphadenitis[i] <-
        diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                         "LYMPHADENOPATHY"]
      
      ###hypotension/shock####
      covidPims$shock[i] <- 
        paste("shock = ",diamondsExport$SHOCK[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],
              "; Inotropes = ", diamondsExport$INOTROPES[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])
      
      #myocardial dysfunction####
      if(!all(is.na(diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                                     c("FIRST_ECHO_NORMAL","WORST_ECHO_NORMAL",
                                       "FIRST_MYOCARDITIS","FIRST_PERICARDITIS",
                                       "FIRST_VALVULAR_REGURGITATION","FIRST_FRACTIONAL_SHORTENING",
                                       "WORST_MYOCARDITIS","WORST_PERICARDITIS","WORST_VALVULAR_REGURGITATION",
                                       "WORST_VALVULAR_REGURGITATION")]=="YES"))){
      if (any(any(diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                               c("FIRST_ECHO_NORMAL","WORST_ECHO_NORMAL",
                                 "FIRST_MYOCARDITIS","FIRST_PERICARDITIS",
                                 "FIRST_VALVULAR_REGURGITATION","FIRST_FRACTIONAL_SHORTENING",
                                 "WORST_MYOCARDITIS","WORST_PERICARDITIS","WORST_VALVULAR_REGURGITATION",
                                 "WORST_VALVULAR_REGURGITATION")]=="YES") 
 )) {
        
        covidPims$myocardialDysfunction[i] <- "YES"
        
      }
      }
      
      if(!all(is.na(as.numeric(diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                                                c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
                                                  "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")])))) {
        covidPims$maxZscore[i] <- max(as.numeric(diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                                        c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
                                          "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")]), na.rm = T)
      }
      
      if(!is.na(as.numeric(sub(".*=","",stri_split(diamondsExport$`TIME_POINT_19=TROP_T`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])))){
        covidPims$Trop[i] <- max(as.numeric(sub(".*=","",stri_split(diamondsExport$`TIME_POINT_19=TROP_T`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])),na.rm = T)
      }else{covidPims$Trop[i] <- NA}      
      
      if(!is.na(as.numeric(sub(".*=","",stri_split(diamondsExport$`TIME_POINT_22=BNP_NT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])))){
        covidPims$BNP[i] <- max(as.numeric(sub(".*=","",stri_split(diamondsExport$`TIME_POINT_22=BNP_NT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])),na.rm = T)
      }else{covidPims$BNP[i] <- NA}
      
      
      #caogulopathy (pt/ptt/ddimer)#### 
      
      #if the max PT is > 15.8
      if(all(!is.na(parse_number((stri_split(diamondsExport$`TIME_POINT_9=PT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])))){
        if(max(parse_number((stri_split(diamondsExport$`TIME_POINT_9=PT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)>15.8) {
            covidPims$coagulopathy[i] <- "YES"    
        } 
      }
      
      #if max D-dimer > 850
      if(all(!is.na(parse_number((stri_split(diamondsExport$`TIME_POINT_20=D_DIMER`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])))){
        if(max(parse_number((stri_split(diamondsExport$`TIME_POINT_20=D_DIMER`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)>850) {
          covidPims$coagulopathy[i] <- "YES"    
        } 
      }
      
      if(!all(is.na(as.numeric(sub(".*=","",stri_split(diamondsExport$`TIME_POINT_9=PT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]]))))){
      covidPims$PT[i] <- max(as.numeric(sub(".*=","",stri_split(diamondsExport$`TIME_POINT_9=PT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])),na.rm = T)
      }else{covidPims$PT[i] <- NA}
      if(!all(is.na(as.numeric(sub(".*=","",stri_split(diamondsExport$`TIME_POINT_20=D_DIMER`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]]))))){
        covidPims$Ddimer[i] <- max(as.numeric(sub(".*=","",stri_split(diamondsExport$`TIME_POINT_20=D_DIMER`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])),na.rm = T)
      }else{covidPims$Ddimer[i] <- NA}      
      
      ## No APTT - ? need to add INR
      
      #GI####
      
      covidPims$GI[i] <- diamondsExport$GASTROINTESTINAL[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      
      
      #imflam markers####
      
      #CRP
      covidPims$maxCRP[i] <- as.numeric(diamondsExport$MAXIMUM_CRP[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])
      
      #PCT
      if (!all(is.na(parse_number(stri_split(diamondsExport$`TIME_POINT_17=PCT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";")[[1]])))) {
      covidPims$maxProcalc[i] <- max(parse_number(stri_split(diamondsExport$`TIME_POINT_17=PCT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";")[[1]]), na.rm = T)
      }
      #other microbial cause of inflammation####
      covidPims$bacteriaSpecify[i] <- diamondsExport$BACTERIA_SPECIFY[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      covidPims$virusSpecify[i] <- diamondsExport$VIRUS_DETECTED[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      covidPims$virusSomeFeatures[i] <- diamondsExport$VIRUS_CONSISTENT_WITH_SYMPTOMS[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      covidPims$virusAllFeatures[i] <- diamondsExport$VIRUS_ACCOUNT_ALL_FEATURES[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      
      covidPims$otherOrg[i] <- diamondsExport$PATHOGEN_SPECIFY[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      
      #evidence of covid19####
      if(length(stri_split(diamondsExport$TEST_TYPE[10],regex = ";")[[1]])>1){
      covidPims$covid19Status[i] <- paste(
        stri_split(diamondsExport$TEST_TYPE[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]][ #find the test type of the positive results
        grep("POSITIVE",stri_split(diamondsExport$RESULT[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])], #find which are positive
        collapse = "; ")
      } else if(grepl("POSITIVE",diamondsExport$RESULT[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])) {
        covidPims$covid19Status[i] <- paste(stri_split(diamondsExport$TEST_TYPE[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])
      }
      
      
      covidPims$prevCovid[i] <- diamondsExport$PREVIOUS_HISTORY_COVID19[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      
      #treatment
      covidPims$treatments[i] <- 
        gsub("NULL|NULL;","",
             paste(unique(sub(".*=","",
                              stri_split(
                                diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],"TREATMENT_NAME"], 
                                regex = ";")[[1]])), 
                   collapse=";"))
      
      #phenotype and syndrome
      covidPims$Phenotype1[i] <- diamondsExport$FINAL_PHENOTYPE_DIAGNOSIS[diamondsExport$UNIQUE_PATIENT_ID == covidPims$ID[i]]
      covidPims$Phenotype2[i] <- diamondsExport$SECONDARY_PHENOTYPE[diamondsExport$UNIQUE_PATIENT_ID == covidPims$ID[i]]
      
      if(diamondsExport$PATHOGEN_SYNDROMES[diamondsExport$UNIQUE_PATIENT_ID == covidPims$ID[i]] == "COIVD19"){
        covidPims$pathogenSyndrome[i] <- "COVID19"
      }
      
      covidPims$inflamSyndrome[i] <- diamondsExport$INFLAMMATORY[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]
         
      
    } 

  }

# if unable to calculated fever length or age

covidPims[is.na(covidPims$ageYears) | covidPims$ageYears<0, "ageYears"] <-  NA
covidPims[is.na(covidPims$feverLength) | covidPims$feverLength<0, "feverLength"] <- NA



#### define inflam / mis-c / kd  #####




covidPims$misC <- "NO"
covidPims$otherCovidInflam <- "NO"
covidPims$otherInflam <- "NO"
covidPims$possibleMisC <- "NO"

covidPims$classicalKD <- "NO"
covidPims$atypicalKD <- "NO"
covidPims$misCFeatures <- 0
covidPims$kdFeatures <- 0

for (i in 1:nrow(covidPims)){
  
  
  if (any(covidPims[i,c("rash","conj","mucositis","extremities")]=="YES")) {
    covidPims$misCFeatures[i] <- covidPims$misCFeatures[i] + 1
  }
  if (grepl("YES",covidPims$shock[i])){
    covidPims$misCFeatures[i] <- covidPims$misCFeatures[i] + 1
  }
  if(covidPims$myocardialDysfunction[i]=="YES" |
     ( any((diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                      c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
                        "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")]) >2.5) &
         !all(diamondsExport[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i],
                                   c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
                                      "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")]%in% c(NA,NULL,"NULL"))) |
     !(max(parse_number((stri_split(diamondsExport$`TIME_POINT_19=TROP_T`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)<15 |
       all(is.na(parse_number((stri_split(diamondsExport$`TIME_POINT_19=TROP_T`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])))
       )|
     !(max(parse_number((stri_split(diamondsExport$`TIME_POINT_22=BNP_NT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)<500 |
       all(is.na(parse_number((stri_split(diamondsExport$`TIME_POINT_22=BNP_NT`[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])) )))
    {
    covidPims$misCFeatures[i] <- covidPims$misCFeatures[i] + 1
  }
  if(covidPims$coagulopathy[i]=="YES"){
    covidPims$misCFeatures[i] <- covidPims$misCFeatures[i] + 1
  }
  if(covidPims$GI[i]=="YES"){
    covidPims$misCFeatures[i] <-  covidPims$misCFeatures[i] + 1
  }
  
  covidPims$kdFeatures[i] <- sum(covidPims[i,c("rash","conj","mucositis","extremities","lymphadenitis")]=="YES")
  
  
  #mis-c classification
  if ((!is.na(covidPims$ageYears[i])) &
    covidPims$ageYears[i]<19){
    if((!is.na(covidPims$feverLength[i])) &
      covidPims$feverLength[i]>=3){
      if(((!is.na(covidPims$maxCRP[i])) &
         covidPims$maxCRP[i] > 10) |
         ((!is.na(covidPims$maxProcalc[i])) &
            covidPims$maxProcalc[i]>2)
         ){
        if(covidPims$bacteriaSpecify[i]%in%c(NULL,"NULL","NEGATIVE")){
          if((!covidPims$covid19Status[i] %in% c("NULL",NULL,""))|
             (covidPims$ID[i] %in% diamondsExport$UNIQUE_PATIENT_ID &
              diamondsExport$PREVIOUS_HISTORY_COVID19[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="Yes")){
            if(covidPims$misCFeatures[i]>=2){
              
              covidPims$misC[i] <- "YES"
              
            }
          }
        }
      }
    }  
  }
  
  #otherCovidInflam
  
  if(((!covidPims$covid19Status[i] %in% c(NULL,"NULL","")) |
     (covidPims$ID[i] %in% diamondsExport$UNIQUE_PATIENT_ID &
       diamondsExport$PREVIOUS_HISTORY_COVID19[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="Yes")) &
     covidPims$misC[i] == "NO" &
     (covidPims$Phenotype1[i] == "INFLAMATORY SYNDROM" |
      covidPims$Phenotype2[i] == "Inflammatory" |
      (!covidPims$inflamSyndrome[i] %in% c("NULL",NULL)))
     ){
    covidPims$otherCovidInflam[i] <- "YES"
  }

  #otherinflam
  
  if(covidPims$covid19Status[i] %in% c("NULL",NULL,"") &
     covidPims$misC[i] == "NO" &
     covidPims$otherCovidInflam[i] == "NO" &
     (covidPims$Phenotype1[i] == "INFLAMATORY SYNDROM" |
      covidPims$Phenotype2[i] == "Inflammatory" |
      (!covidPims$inflamSyndrome[i] %in% c("NULL",NULL)))
  ){
    covidPims$otherInflam[i] <- "YES"
  }
  
  
  #if could be pims but missing info
  
  if(covidPims$misC[i] == "NO" &
     ((is.na(covidPims$ageYears[i])) |
         covidPims$ageYears[i]<19) &
       ((is.na(covidPims$feverLength[i])) |
          covidPims$feverLength[i]>=3) &
         (((is.na(covidPims$maxCRP[i])) |
             covidPims$maxCRP[i] > 10) |
            ((is.na(covidPims$maxProcalc[i])) |
             covidPims$maxProcalc[i]>2)) &
           (covidPims$bacteriaSpecify[i]%in%c(NULL,"NULL","NEGATIVE")) &
             ((covidPims$covid19Status[i] %in% c("NULL",NULL,""))|
                (covidPims$ID[i] %in% diamondsExport$UNIQUE_PATIENT_ID &
                 diamondsExport$PREVIOUS_HISTORY_COVID19[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="Yes"))&
               (covidPims$misCFeatures[i]>=2) |
     any(is.na(covidPims[i,c("rash","conj","mucositis","extremities")]))){
                 
                 covidPims$possibleMisC[i] <- "YES"
                 
               
             }
           
  

#classical kd classificaiton
if((!is.na(covidPims$feverLength[i])) &
  covidPims$feverLength[i]>=4 &
   covidPims$kdFeatures[i]>=4){
  covidPims$classicalKD[i] <- "YES"
}
  
  #Atypical KD
  if((!is.na(covidPims$maxZscore[i])) &
     covidPims$classicalKD[i] == "NO" &
    covidPims$maxZscore[i]>2.5){
    covidPims$atypicalKD[i] <- "YES"
  }
}






####  role of covid  #####

covidPims$incidentalCovid <- "NO"
covidPims$coinfectCovid <- "NO"
covidPims$atypicalCovid <- "NO"
covidPims$uncomplicatedCovid <- "NO"
coinfect <- 0
uncomplicated <- 0
otherorgs <- 0
inflam <- 0
coviddvvs <- 0
otherpheno <- 0
atypical <- 0
incidental <- 0


for (i in 1:nrow(covidPims)){
  if(covidPims$ID[i] %in% diamondsExport$UNIQUE_PATIENT_ID){
  #if sars-cov-2 listed on viruses detected or if covid test result was postive
  if(grepl("SARS-CoV-2",diamondsExport$VIRUS_DETECTED[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])){
   
    #if more than 1 virus, find position of sars-cov-2
  position <- which(stri_split(diamondsExport$VIRUS_DETECTED[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]]=="SARS-CoV-2")    
  
  
  #if covid listed, but doesnt account for all features or some symptoms, mark as incidental
  if(
    stri_split(diamondsExport$VIRUS_ACCOUNT_ALL_FEATURES[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]][position]%in% c("NO","NULL",NULL) &
    stri_split(diamondsExport$VIRUS_CONSISTENT_WITH_SYMPTOMS[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]][position] %in% c("NO","NULL",NULL)
  ) {
    covidPims$incidentalCovid[i] <- "YES"
    incidental <- incidental + 1
    
  }
  #if another org identified -> mark as covid co-infection
  
  if(length(stri_split(diamondsExport$VIRUS_DETECTED[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])>1){
     
           if(
             (!stri_split(diamondsExport$VIRUS_DETECTED[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]][
             which(!stri_split(diamondsExport$VIRUS_DETECTED[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]]%in%"SARS-CoV-2")
             ] %in%  "NULL")
  ){

    if (covidPims$incidentalCovid[i] == "NO"){
      covidPims$coinfectCovid[i] <- "YES"
      otherorgs <- otherorgs + 1}
  }
  }
  
  if((!diamondsExport$BACTERIA_SPECIFY[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL,"NEGATIVE")) |
         (!diamondsExport$PATHOGEN_SPECIFY[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL,"NEGATIVE"))
  ){
    covidPims$coinfectCovid[i] <- "YES"
    
    if (covidPims$incidentalCovid[i] == "NO"){
      if(covidPims$coinfectCovid[i]=="NO"){ otherorgs <- otherorgs + 1}
      covidPims$coinfectCovid[i] <- "YES"
     }
  }
  
  #if not incidental and not co-infect and  syndrome listed as covid19 and either dv or vs and not pims -> mark as uncomplicated covid.
  if(covidPims$incidentalCovid[i] == "NO" &
     covidPims$coinfectCovid[i] == "NO" &
     diamondsExport$PATHOGEN_SYNDROMES[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% "COIVD19" &
     covidPims$Phenotype1[i] %in% c("DEFINITE VIRAL","VIRAL SYNDROME") &
     (diamondsExport$INFLAMMATORY[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL)) &
     (!covidPims$Phenotype2[i] %in% c("Inflammatory"))
  ){
    covidPims$uncomplicatedCovid[i] <- "YES"
    
    uncomplicated <- uncomplicated + 1
    
  
    } else if (#if all of the uncomplicated covid19 but different phenotype -> mark as atypical covid
    
    covidPims$incidentalCovid[i] == "NO" &
    covidPims$coinfectCovid[i] == "NO" &
    (diamondsExport$INFLAMMATORY[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL)) &
    (!diamondsExport$SECONDARY_PHENOTYPE[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("Inflammatory"))
    
    ){
      covidPims$atypicalCovid[i] <- "YES"
      
     atypical <- atypical + 1
      
    } else if((!diamondsExport$INFLAMMATORY[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL)) |
              covidPims$Phenotype2[i] %in% c("Inflammatory")){
      inflam <- inflam + 1
    } 
  
  
  
  
  
  }
  
  
  }
}



##### Which patients have consent & samples ####
covidPims$hasConsent <- "NO"
covidPims$PAX <- "NO"
covidPims$SMART <- "NO"
covidPims$EDTA <- "NO"
covidPims$THROAT <- "NO"


for (i in 1:nrow(covidPims)) {
  
  #consent
  if(diamondsExport$CONSENT_USE_RESEARCH_SAMPLE[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] == "YES"){
    covidPims$hasConsent[i] <- "YES"
  } else if(diamondsExport$CONSENT_USE_RESEARCH_SAMPLE[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]] == "NULL"){
    covidPims$hasConsent[i] <- "EMPTY"
  }
  
  
  #PAX TP1
  if((!is.na(diamondsExport$PAX_TUBE_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])) &
    diamondsExport$PAX_TUBE_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="YES"){
    covidPims$PAX[i] <- "YES"
  }
  
  #SMART TP1
  if((!is.na(diamondsExport$SMART_TUBE_1_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])) &
    diamondsExport$SMART_TUBE_1_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="YES"){
    covidPims$SMART[i] <- "YES"
  }
  
  #EDTA TP1
  if((!is.na(diamondsExport$BLOOD_EDTA_2HRS_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])) &
    diamondsExport$BLOOD_EDTA_2HRS_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="YES"){
    covidPims$EDTA[i] <- "2HRS"
  } else if((!is.na(diamondsExport$BLOOD_EDTA_6HRS_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])) &
    diamondsExport$BLOOD_EDTA_6HRS_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="YES"){
    covidPims$EDTA[i] <- "6HRS"
  }
  
  #THROAT TP1
  if((!is.na(diamondsExport$THROAT_SWAB_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]])) &
    diamondsExport$THROAT_SWAB_TP1[diamondsExport$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="YES"){
    covidPims$THROAT[i] <- "YES"
  }
  
}


### Already had RNASeq ####

covidPims$hadRNASeq <- "NO"
covidPims[covidPims$ID %in% RNAseqList$Patient.ID |
            covidPims$ID %in% RNAseqList$On.Jethro.clinical.diamonds.database.06_07_20,
          "hadRNASeq"] <- "YES"



######  export #####

covidPims$downloadDate <- downloadDate


write.csv(covidPims,file="finalCovidPims.csv")

#for Shiny
write.csv(covidPims,file="DiamondsSearch/data/finalCovidPims.csv")


write.csv(covidPims %>% filter(hadRNASeq=="YES"), file = "covidPimsHadRNASeq.csv")



table(covidPims %>% filter(ageYears<19) %>% select(atypicalCovid))



table(diamonds %>% select(DATE_OF_BIRTH))


table((covidPims %>% filter(misC=="YES") %>% select(ID) %>% stri_sub(from = 5, to = 8)))


table(covidPims %>% filter(misC=="YES" & hadRNASeq=="NO") %>% mutate(
  site = str_sub(ID,5,8)
) %>% select(site))
