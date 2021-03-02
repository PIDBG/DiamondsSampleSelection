setwd("/Users/priyenshah/OneDrive - Imperial College London/Pri-Jethro shared/DiamondsSelection")

#### library ####
library(DiagrammeR)
library(openxlsx)
library(stringi)
library(stringr)
library(readr)
library(tidyr)
library(dplyr)

diamondsFilename <- "DIAMONDS_CONSORTIUM_EXTERNAL_SAMPLES_USE_FEB26.xlsx"

#### import and intial qc  +  filtering  ####
#import RNAseqList
RNAseqList <- read.xlsx("/Users/priyenshah/OneDrive - Imperial College London/Pri-Jethro shared/RNAseq exp Feb 2021/Batch_F_and_F_extension_noDELPHIC.xlsx", startRow = 2)

#add a 0 when format of id is -E1  so all have format -E01
stri_sub(RNAseqList$Patient.ID[stri_sub(RNAseqList$Patient.ID,15,16)!="E0"],16,15) <- 0

#import diamonds + perform export
diamondsExport <- read.xlsx(diamondsFilename, detectDates = T)
#performExport <- read.xlsx("BIVA_ED_OCT20_2020.xlsx", detectDates = T)


#add a 0 when format of id is -E1  so all have format -E01
#stri_sub(performExport$UNIQUE_PATIENT_ID[stri_sub(performExport$UNIQUE_PATIENT_ID,15,16)!="E0"],16,15) <- 0


#bring in patients in wit DIS ids in jethros column
#RNAseqList$Patient.ID2 <- RNAseqList$Patient.ID
#RNAseqList[stri_sub(RNAseqList$Patient.ID,1,3)%in%"BIV" &
#             stri_sub(RNAseqList$On.Jethro.clinical.diamonds.database.06_07_20,1,3)%in%"DIS","Patient.ID2"] <- 
#                                                                                        RNAseqList[stri_sub(RNAseqList$Patient.ID,1,3)%in%"BIV" &
#                                                                                                   stri_sub(RNAseqList$On.Jethro.clinical.diamonds.database.06_07_20,1,3)%in%"DIS","On.Jethro.clinical.diamonds.database.06_07_20"]
#                                                                                      

#filter diamonds export for those in RNAseqList

#to use whole diamonds db - remove the filter here
diamondsInRNAseq <-diamondsExport#[diamondsExport$UNIQUE_PATIENT_ID %in% RNAseqList$Patient.ID2,]

#performInRNAseq <- performExport[performExport$UNIQUE_PATIENT_ID %in% RNAseqList$Patient.ID,]



#### create variables for covidPims #####
#new dataFrame for pims/covid recording
covidPims <- data.frame(ID = c(diamondsInRNAseq$UNIQUE_PATIENT_ID#,
                               #performInRNAseq$UNIQUE_PATIENT_ID
                               ), 
                        comments="",
                        ageYears = NA, feverLength = NA, rash = "", conj="", mucositis="",extremities="", lymphadenitis = "", 
                        shock = "", myocardialDysfunction = "",maxZscore=NA, coagulopathy = "",
                        GI = "", maxCRP=NA, maxESR = NA, maxProcalc = NA, bacteriaSpecify = "", covid19Status = "", Phenotype1 = "",
                        Phenotype2 = "", pathogenSyndrome = "", inflamSyndrome = "")



  for (i in 1:nrow(covidPims)) {
    if (covidPims$ID[i] %in% diamondsInRNAseq$UNIQUE_PATIENT_ID){ #if patient in diamonds db
      
      #calculate age at symptom onset in years####
      if (!(diamondsInRNAseq$DATE_SYMPTOM_ONSET[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL") ||
            diamondsInRNAseq$DATE_OF_BIRTH[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL"))){
      covidPims$ageYears[i] <-
        as.numeric((as.Date(diamondsInRNAseq$DATE_SYMPTOM_ONSET[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]) - 
                      as.Date(diamondsInRNAseq$DATE_OF_BIRTH[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]))/365)
      }
      
      #fever length####
      if (!(diamondsInRNAseq$DATE_FEVER_ONSET[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL") ||
            diamondsInRNAseq$DATETIME_FRB[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL"))){
        covidPims$feverLength[i] <- 
          as.Date(diamondsInRNAseq$DATETIME_FRB[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]) - 
          as.Date(diamondsInRNAseq$DATE_FEVER_ONSET[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]])
      }
      
      
      #rash/conj/muco-cutaneous/lymph####
      covidPims$rash[i] <-
        diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                               "RASH"]
      
      covidPims$conj[i] <-
        diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                         "CONJUNCTIVITIS"]
      
      covidPims$mucositis[i] <-
        diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                         "MUCOSITIS"]
      
      covidPims$extremities[i] <-
        diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                          "INFLAMMED_EXTREMITIES"]
      
      covidPims$lymphadenitis[i] <-
        diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                         "LYMPHADENOPATHY"]
      
      ###hypotension/shock####
      covidPims$shock[i] <- 
        paste("shock = ",diamondsInRNAseq$SHOCK[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],
              "; Inotropes = ", diamondsInRNAseq$INOTROPES_130[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]])
      
      #myocardial dysfunction####
      if(!all(is.na(diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                                     c("FIRST_ECHO_NORMAL","WORST_ECHO_NORMAL",
                                       "FIRST_MYOCARDITIS","FIRST_PERICARDITIS",
                                       "FIRST_VALVULAR_REGURGITATION","FIRST_FRACTIONAL_SHORTENING",
                                       "WORST_MYOCARDITIS","WORST_PERICARDITIS","WORST_VALVULAR_REGURGITATION",
                                       "WORST_VALVULAR_REGURGITATION")]=="YES"))){
      if (any(any(diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                               c("FIRST_ECHO_NORMAL","WORST_ECHO_NORMAL",
                                 "FIRST_MYOCARDITIS","FIRST_PERICARDITIS",
                                 "FIRST_VALVULAR_REGURGITATION","FIRST_FRACTIONAL_SHORTENING",
                                 "WORST_MYOCARDITIS","WORST_PERICARDITIS","WORST_VALVULAR_REGURGITATION",
                                 "WORST_VALVULAR_REGURGITATION")]=="YES") #|
          # any((diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
          #                  c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
          #                    "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")]) >2.5 &
          #     !(diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
          #                               c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
          #                                  "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")])=="NULL") #|
        # !(max(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_40=TROP_T`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)<15 | 
        #   all(is.na(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_40=TROP_T`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])))
        #   )|
        # !(max(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_43=BNP_NT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)<500 | 
        #   all(is.na(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_43=BNP_NT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])) ))
      )) {
        
        covidPims$myocardialDysfunction[i] <- "YES"
        
      }
      }
      
      if(!all(is.na(as.numeric(diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                                                c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
                                                  "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")])))) {
        covidPims$maxZscore[i] <- max(as.numeric(diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                                        c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
                                          "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")]), na.rm = T)
      }
      
      if(!is.na(as.numeric(sub(".*=","",stri_split(diamondsInRNAseq$`TIME_POINT_40=TROP_T`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])))){
        covidPims$Trop[i] <- max(as.numeric(sub(".*=","",stri_split(diamondsInRNAseq$`TIME_POINT_40=TROP_T`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])),na.rm = T)
      }else{covidPims$Trop[i] <- NA}      
      
      if(!is.na(as.numeric(sub(".*=","",stri_split(diamondsInRNAseq$`TIME_POINT_43=BNP_NT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])))){
        covidPims$BNP[i] <- max(as.numeric(sub(".*=","",stri_split(diamondsInRNAseq$`TIME_POINT_43=BNP_NT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])),na.rm = T)
      }else{covidPims$BNP[i] <- NA}
      
      
      #caogulopathy (pt/ptt/ddimer)#### 
      
      #if the max PT is > 15.8
      if(all(!is.na(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_30=PT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])))){
        if(max(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_30=PT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)>15.8) {
            covidPims$coagulopathy[i] <- "YES"    
        } 
      }
      
      #if max D-dimer > 850
      if(all(!is.na(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_41=D_DIMER`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])))){
        if(max(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_41=D_DIMER`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)>850) {
          covidPims$coagulopathy[i] <- "YES"    
        } 
      }
      
      if(!all(is.na(as.numeric(sub(".*=","",stri_split(diamondsInRNAseq$`TIME_POINT_30=PT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]]))))){
      covidPims$PT[i] <- max(as.numeric(sub(".*=","",stri_split(diamondsInRNAseq$`TIME_POINT_30=PT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])),na.rm = T)
      }else{covidPims$PT[i] <- NA}
      if(!all(is.na(as.numeric(sub(".*=","",stri_split(diamondsInRNAseq$`TIME_POINT_41=D_DIMER`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]]))))){
        covidPims$Ddimer[i] <- max(as.numeric(sub(".*=","",stri_split(diamondsInRNAseq$`TIME_POINT_41=D_DIMER`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])),na.rm = T)
      }else{covidPims$Ddimer[i] <- NA}      
      
      ## No APTT - ? need to add INR
      
      #GI####
      
      covidPims$GI[i] <- diamondsInRNAseq$GASTROINTESTINAL[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      
      
      #imflam markers####
      
      #CRP
      covidPims$maxCRP[i] <- as.numeric(diamondsInRNAseq$MAXIMUM_CRP[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]])
      
      #PCT
      if (!all(is.na(parse_number(stri_split(diamondsInRNAseq$`TIME_POINT_38=PCT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";")[[1]])))) {
      covidPims$maxProcalc[i] <- max(parse_number(stri_split(diamondsInRNAseq$`TIME_POINT_38=PCT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";")[[1]]), na.rm = T)
      }
      #other microbial cause of inflammation####
      covidPims$bacteriaSpecify[i] <- diamondsInRNAseq$BACTERIA_SPECIFY[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      covidPims$virusSpecify[i] <- diamondsInRNAseq$VIRUS_DETECTED[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      covidPims$virusSomeFeatures[i] <- diamondsInRNAseq$VIRUS_CONSISTENT_WITH_SYMPTOMS[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      covidPims$virusAllFeatures[i] <- diamondsInRNAseq$VIRUS_ACCOUNT_ALL_FEATURES[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      
      covidPims$otherOrg[i] <- diamondsInRNAseq$PATHOGEN_SPECIFY[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      
      #evidence of covid19####
      if(length(stri_split(diamondsInRNAseq$TEST_TYPE[10],regex = ";")[[1]])>1){
      covidPims$covid19Status[i] <- paste(
        stri_split(diamondsInRNAseq$TEST_TYPE[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]][ #find the test type of the positive results
        grep("POSITIVE",stri_split(diamondsInRNAseq$RESULT[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])], #find which are positive
        collapse = "; ")
      } else if(grepl("POSITIVE",diamondsInRNAseq$RESULT[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]])) {
        covidPims$covid19Status[i] <- paste(stri_split(diamondsInRNAseq$TEST_TYPE[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])
      }
      
      
      covidPims$prevCovid[i] <- diamondsInRNAseq$PREVIOUS_HISTORY_COVID19[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
      
      #treatment
      covidPims$treatments[i] <- 
        gsub("NULL|NULL;","",
             paste(unique(sub(".*=","",
                              stri_split(
                                diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],"TIME_POINT_82=TREATMENT_NAME"], 
                                regex = ";")[[1]])), 
                   collapse=";"))
      
      #phenotype and syndrome
      covidPims$Phenotype1[i] <- diamondsInRNAseq$FINAL_PHENOTYPE_DIAGNOSIS[diamondsInRNAseq$UNIQUE_PATIENT_ID == covidPims$ID[i]]
      covidPims$Phenotype2[i] <- diamondsInRNAseq$SECONDARY_PHENOTYPE[diamondsInRNAseq$UNIQUE_PATIENT_ID == covidPims$ID[i]]
      
      if(diamondsInRNAseq$PATHOGEN_SYNDROMES[diamondsInRNAseq$UNIQUE_PATIENT_ID == covidPims$ID[i]] == "COIVD19"){
        covidPims$pathogenSyndrome[i] <- "COVID19"
      }
      
      covidPims$inflamSyndrome[i] <- diamondsInRNAseq$INFLAMMATORY[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
         
      
    } 
    #else if (covidPims$ID[i] %in% performInRNAseq$UNIQUE_PATIENT_ID) { #if patient in perform db
  #     
  #     #for perform samples in 2020#####
  #     
  #     #if also in diamonds db
  #     if(length(stri_sub(RNAseqList[RNAseqList$Patient.ID %in% performInRNAseq$UNIQUE_PATIENT_ID[performInRNAseq$UNIQUE_PATIENT_ID == covidPims$ID[i] &
  #                                                                                                as.Date(performInRNAseq$DATE_OF_SYMPTOM_ONSET)>as.Date("2020-01-01")], 
  #                                   "Patient.ID2"],1,3))>0){
  #             if (stri_sub(RNAseqList[RNAseqList$Patient.ID %in% performInRNAseq$UNIQUE_PATIENT_ID[performInRNAseq$UNIQUE_PATIENT_ID == covidPims$ID[i] &
  #                                                                                         as.Date(performInRNAseq$DATE_OF_SYMPTOM_ONSET)>as.Date("2020-01-01")], 
  #                                                                                         "Patient.ID2"],1,3)=="DIS") {
  #                    covidPims$comments[i] <- paste("also in Diamonds DB, see: ",
  #                                                   RNAseqList$Patient.ID2[RNAseqList$Patient.ID==covidPims$ID[i]])
  #              } else if (stri_sub(RNAseqList[RNAseqList$Patient.ID %in% performInRNAseq$UNIQUE_PATIENT_ID[performInRNAseq$UNIQUE_PATIENT_ID == covidPims$ID[i] &
  #                                                                                                 as.Date(performInRNAseq$DATE_OF_SYMPTOM_ONSET)>as.Date("2020-01-01")], 
  #                                    "Patient.ID2"],1,3)=="BIV") {
  #                     covidPims$comments[i] <- "Perform patient after 2020, not on Diamonds DB"
  #             }
  #     }
  #     #calculate age at symptom onset in years
  #     if(!(performInRNAseq$DATE_OF_SYMPTOM_ONSET[performInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL") ||
  #          performInRNAseq$DATE_OF_BIRTH[performInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c(NA,NULL,"NA","NULL"))){
  #     covidPims$ageYears[i] <-
  #       as.numeric((as.Date(performInRNAseq$DATE_OF_SYMPTOM_ONSET[performInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]) - 
  #                     as.Date(performInRNAseq$DATE_OF_BIRTH[performInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]))/365)
  #     }
  #     
  #     covidPims$Phenotype1[i] <- performInRNAseq$FINAL_PHENOTYPE_DIAGNOSIS[performInRNAseq$UNIQUE_PATIENT_ID == covidPims$ID[i]]
  #     
  #     covidPims$inflamSyndrome[i] <- performInRNAseq$INFLAMMATORY[performInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]
  #     
  #   }
  #   
  }


covidPims[covidPims$ageYears<0, "ageYears"] <- NA
covidPims[covidPims$feverLength<0, "ageYears"] <- NA



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
     ( any((diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                      c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
                        "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")]) >2.5) &
         !all(diamondsInRNAseq[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i],
                                   c("FIRST_ECHO_LOPEZ_Z_SCORE_1","FIRST_ECHO_LOPEZ_Z_SCORE_2",
                                      "WORST_ECHO_LOPEZ_Z_SCORE_1","WORST_ECHO_LOPEZ_Z_SCORE_2")]%in% c(NA,NULL,"NULL"))) |
     !(max(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_40=TROP_T`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)<15 |
       all(is.na(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_40=TROP_T`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])))
       )|
     !(max(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_43=BNP_NT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]]), na.rm = T)<500 |
       all(is.na(parse_number((stri_split(diamondsInRNAseq$`TIME_POINT_43=BNP_NT`[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]], regex = ";"))[[1]])) )))
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
             (covidPims$ID[i] %in% diamondsInRNAseq$UNIQUE_PATIENT_ID &
              diamondsInRNAseq$PREVIOUS_HISTORY_COVID19[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="Yes")){
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
     (covidPims$ID[i] %in% diamondsInRNAseq$UNIQUE_PATIENT_ID &
       diamondsInRNAseq$PREVIOUS_HISTORY_COVID19[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="Yes")) &
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
                (covidPims$ID[i] %in% diamondsInRNAseq$UNIQUE_PATIENT_ID &
                 diamondsInRNAseq$PREVIOUS_HISTORY_COVID19[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]]=="Yes"))&
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

write.csv(covidPims, file="covidPimsInflam.csv")




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
  if(covidPims$ID[i] %in% diamondsInRNAseq$UNIQUE_PATIENT_ID){
  #if sars-cov-2 listed on viruses detected or if covid test result was postive
  if(grepl("SARS-CoV-2",diamondsInRNAseq$VIRUS_DETECTED[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]])){
   
    #if more than 1 virus, find position of sars-cov-2
  position <- which(stri_split(diamondsInRNAseq$VIRUS_DETECTED[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]]=="SARS-CoV-2")    
  
  
  #if covid listed, but doesnt account for all features or some symptoms, mark as incidental
  if(
    stri_split(diamondsInRNAseq$VIRUS_ACCOUNT_ALL_FEATURES[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]][position]%in% c("NO","NULL",NULL) &
    stri_split(diamondsInRNAseq$VIRUS_CONSISTENT_WITH_SYMPTOMS[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]][position] %in% c("NO","NULL",NULL)
  ) {
    covidPims$incidentalCovid[i] <- "YES"
    incidental <- incidental + 1
    
  }
  #if another org identified -> mark as covid co-infection
  
  if(length(stri_split(diamondsInRNAseq$VIRUS_DETECTED[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]])>1){
     
           if(
             (!stri_split(diamondsInRNAseq$VIRUS_DETECTED[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]][
             which(!stri_split(diamondsInRNAseq$VIRUS_DETECTED[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]],regex = ";")[[1]]%in%"SARS-CoV-2")
             ] %in%  "NULL") |
    (!diamondsInRNAseq$BACTERIA_SPECIFY[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL,"NEGATIVE")) |
    (!diamondsInRNAseq$PATHOGEN_SPECIFY[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL,"NEGATIVE"))
  ){
    covidPims$coinfectCovid[i] <- "YES"
    if (covidPims$incidentalCovid[i] == "NO"){otherorgs <- otherorgs + 1}
  }
  }
  #if not incidental and not co-infect and  syndrome listed as covid19 and either dv or vs and not pims -> mark as uncomplicated covid.
  if(covidPims$incidentalCovid[i] == "NO" &
     covidPims$coinfectCovid[i] == "NO" &
     diamondsInRNAseq$PATHOGEN_SYNDROMES[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% "COIVD19" &
     covidPims$Phenotype1[i] %in% c("DEFINITE VIRAL","VIRAL SYNDROME") &
     (diamondsInRNAseq$INFLAMMATORY[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL)) &
     (!covidPims$Phenotype2[i] %in% c("Inflammatory"))
  ){
    covidPims$uncomplicatedCovid[i] <- "YES"
    
    uncomplicated <- uncomplicated + 1
    
  
    } else if (#if all of the uncomplicated covid19 but different phenotype -> mark as atypical covid
    
    covidPims$incidentalCovid[i] == "NO" &
    covidPims$coinfectCovid[i] == "NO" &
    (diamondsInRNAseq$INFLAMMATORY[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL)) &
    (!diamondsInRNAseq$SECONDARY_PHENOTYPE[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("Inflammatory"))
    
    ){
      covidPims$atypicalCovid[i] <- "YES"
      
     atypical <- atypical + 1
      
    } else if((!diamondsInRNAseq$INFLAMMATORY[diamondsInRNAseq$UNIQUE_PATIENT_ID==covidPims$ID[i]] %in% c("NULL",NULL)) |
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


write.csv(covidPims,file="finalCovidPims.csv")

#for Shiny
write.csv(covidPims,file="DiamondsSearch/data/finalCovidPims.csv")



