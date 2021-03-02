#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DiagrammeR)
library(shiny)
library(dplyr)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  
  
    #listener
  toListen <- reactive({
    list(input$ageIncludeNA, input$feverIncludeNA,input$age,input$feverDays,input$KDfeatures,input$MisCfeatures,input$raisedInflamMarkers,input$othercause,input$covid19,
         input$inflam,input$phenotypes,input$treatments,input$cardiac,
         input$CRPLimit,input$PCTLimit,input$tropLimit,input$BNPLimit,input$ddimerLimit,input$ptLimit,input$zScoreLimit)
  })
  
  # filtering function
  
    filteredDF <- read.csv("data/finalCovidPims.csv")
    observeEvent(toListen(),{
      

      if(input$ageIncludeNA){
        filteredDF <- filteredDF %>% filter(is.na(ageYears) | 
                                              (ageYears>=input$age[1] & ageYears<=input$age[2])
                                            )
      } else {
        filteredDF <- filteredDF %>% filter((ageYears>=input$age[1] & ageYears<=input$age[2])
        )
      }
      
      if(input$feverIncludeNA){
        filteredDF <- filteredDF %>% filter(is.na(feverLength) |
                                            feverLength>=input$feverDays)
      
      } else {
        filteredDF <- filteredDF %>% filter(feverLength>=input$feverDays)
      }
      
      if(input$MisCfeatures=="yes"){
        filteredDF <- filteredDF %>% filter(((rash=="YES" | conj == "YES" | mucositis == "YES" | extremities == "YES") &
                                              grepl("YES",shock)) |
                                              ((rash=="YES" | conj == "YES" | mucositis == "YES" | extremities == "YES") &
                                                 (myocardialDysfunction=="YES" | maxZscore>=input$zScoreLimit| Trop>input$tropLimit | BNP > input$BNPLimit)) |
                                              ((rash=="YES" | conj == "YES" | mucositis == "YES" | extremities == "YES") &
                                                 (PT > input$ptLimit | Ddimer > input$ddimerLimit)) |
                                              ((rash=="YES" | conj == "YES" | mucositis == "YES" | extremities == "YES") &
                                              GI=="YES") |
                                              (grepl("YES",shock) &
                                                (PT > input$ptLimit | Ddimer > input$ddimerLimit)) |
                                              (grepl("YES",shock) &
                                                 (myocardialDysfunction=="YES" | maxZscore>=input$zScoreLimit | Trop>input$tropLimit | BNP > input$BNPLimit)) |
                                              (grepl("YES",shock) &
                                                 (PT > input$ptLimit | Ddimer > input$ddimerLimit)) |
                                              (grepl("YES",shock) &
                                                 GI=="YES") |
                                              ((myocardialDysfunction=="YES" | maxZscore>=input$zScoreLimit| Trop>input$tropLimit | BNP > input$BNPLimit) &
                                                 (PT > input$ptLimit | Ddimer > input$ddimerLimit)) |
                                              ((myocardialDysfunction=="YES" | maxZscore>=input$zScoreLimit| Trop>input$tropLimit | BNP > input$BNPLimit) &
                                                 GI=="YES") |
                                              ( (PT > input$ptLimit | Ddimer > input$ddimerLimit) &
                                                  GI=="YES")
                                          )
      } else if(input$MisCfeatures=="no"){
        filteredDF <- filteredDF %>% filter(!(((rash=="YES" | conj == "YES" | mucositis == "YES" | extremities == "YES") &
                                               grepl("YES",shock)) |
                                              ((rash=="YES" | conj == "YES" | mucositis == "YES" | extremities == "YES") &
                                                 (myocardialDysfunction=="YES" | maxZscore>=input$zScoreLimit| Trop>input$tropLimit | BNP > input$BNPLimit)) |
                                              ((rash=="YES" | conj == "YES" | mucositis == "YES" | extremities == "YES") &
                                                 (PT > input$ptLimit | Ddimer > input$ddimerLimit)) |
                                              ((rash=="YES" | conj == "YES" | mucositis == "YES" | extremities == "YES") &
                                                 GI=="YES") |
                                              (grepl("YES",shock) &
                                                 (PT > input$ptLimit | Ddimer > input$ddimerLimit)) |
                                              (grepl("YES",shock) &
                                                 (myocardialDysfunction=="YES" | maxZscore>=input$zScoreLimit | Trop>input$tropLimit | BNP > input$BNPLimit)) |
                                              (grepl("YES",shock) &
                                                 (PT > input$ptLimit | Ddimer > input$ddimerLimit)) |
                                              (grepl("YES",shock) &
                                                 GI=="YES") |
                                              ((myocardialDysfunction=="YES" | maxZscore>=input$zScoreLimit| Trop>input$tropLimit | BNP > input$BNPLimit) &
                                                 (PT > input$ptLimit | Ddimer > input$ddimerLimit)) |
                                              ((myocardialDysfunction=="YES" | maxZscore>=input$zScoreLimit| Trop>input$tropLimit | BNP > input$BNPLimit) &
                                                 GI=="YES") |
                                              ( (PT > input$ptLimit | Ddimer > input$ddimerLimit) &
                                                  GI=="YES")
        ))
      }
        
        
        if(input$KDfeatures=="yes"){
       filteredDF <-  filteredDF %>% filter(rowSums(filteredDF[,c("rash","conj","mucositis","extremities","lymphadenitis")]=="YES")>=4)
        } else if(input$KDfeatures=="no"){
          filteredDF <-  filteredDF %>% filter(!rowSums(filteredDF[,c("rash","conj","mucositis","extremities","lymphadenitis")]=="YES")>=4)
        }
      
      if(input$raisedInflamMarkers) {
        filteredDF <- filteredDF %>% filter((as.numeric(maxCRP)>input$CRPLimit &  (!is.na(as.numeric(maxCRP))) )| 
                                              (maxProcalc>input$PCTLimit & (!is.na(maxProcalc)) & (!is.null(maxProcalc)) ))
      }
      
     if(input$cardiac) {
        filteredDF <- filteredDF %>% filter(maxZscore > input$zScoreLimit)
      }
      
      if(all(c("Viral","Bacterial","Other") %in% input$othercause)){
        filteredDF <- filteredDF %>% filter(gsub("SARS-CoV-2","NULL",virusSpecify) %in% c("NULL","",NULL) &
                                              bacteriaSpecify == "NULL" &
                                              otherOrg == "NULL")
                                   
      } else if(all(c("Viral","Bacterial") %in% input$othercause)){
        filteredDF <- filteredDF %>% filter(gsub("SARS-CoV-2","NULL",virusSpecify) %in% c("NULL","",NULL) &
                                              bacteriaSpecify == "NULL")
        
      } else if (all(c("Viral","Other") %in% input$othercause)){
      filteredDF <- filteredDF %>% filter(gsub("SARS-CoV-2","NULL",virusSpecify) %in% c("NULL","",NULL) &
                                            otherOrg == "NULL")
      
    } else if (all(c("Bacterial","Other") %in% input$othercause)){
      filteredDF <- filteredDF %>% filter(bacteriaSpecify == "NULL" &
                                            otherOrg == "NULL")
      
    } else if ("Viral" %in% input$othercause){
      filteredDF <- filteredDF %>% filter(gsub("SARS-CoV-2","NULL",virusSpecify) %in% c("NULL","",NULL))
    } else if("Bacterial" %in% input$othercause){
      filteredDF <- filteredDF %>% filter(bacteriaSpecify == "NULL")
    } else if ("Other" %in% input$othercause){
      filteredDF <- filteredDF %>% filter(otherOrg == "NULL")
    }
      
      if(any(c("PCR","IGG","IGM") %in% input$covid19) &  
             all(c("Previous History","COVID19 Listed in Microbiology Section") %in% input$covid19)){
        filteredDF <- filteredDF %>% filter(covid19Status %in% input$covid19 |
                                              prevCovid == "Yes" |
                                              virusSpecify == "SARS-CoV-2")
      } else if (any(c("PCR","IGG","IGM") %in% input$covid19) &  
                 ("Previous History" %in% input$covid19)) {
        filteredDF <- filteredDF %>% filter(covid19Status %in% input$covid19 |
                                              prevCovid == "Yes" )
      } else if(any(c("PCR","IGG","IGM") %in% input$covid19) &  
                ("COVID19 Listed in Microbiology Section" %in% input$covid19)){
        filteredDF <- filteredDF %>% filter(covid19Status %in% input$covid19 |
                                              virusSpecify == "SARS-CoV-2")
      } else if(any(c("PCR","IGG","IGM") %in% input$covid19)){
        filteredDF <- filteredDF %>% filter(covid19Status %in% input$covid19)
      } else if("Previous History" %in% input$covid19){
        filteredDF <- filteredDF %>% filter(prevCovid == "Yes")
      } else if("COVID19 Listed in Microbiology Section" %in% input$covid19){
        filteredDF <- filteredDF %>% filter(virusSpecify == "SARS-CoV-2")
      }
      
      if(all(c("Inflammatory Phenotype","Any Inflammatory Syndrome","Recieved IVIG") %in% input$inflam)){
        filteredDF <- filteredDF %>% filter(Phenotype1=="INFLAMATORY SYNDROM" | Phenotype2 == "Inflammatory" |
                                              inflamSyndrome != "NULL" |
                                              grepl("IMMUNOGLOBULIN",treatments))
      } else if(all(c("Inflammatory Phenotype","Covid-Related Inflammation","Recieved IVIG") %in% input$inflam)){
        filteredDF <- filteredDF %>% filter(Phenotype1=="INFLAMATORY SYNDROM" | Phenotype2 == "Inflammatory" |
                                              inflamSyndrome == "COVID-RELATED INFLAMMATION"|
                                              grepl("IMMUNOGLOBULIN",treatments))
      } else if(all(c("Inflammatory Phenotype","Recieved IVIG") %in% input$inflam)){
        filteredDF <- filteredDF %>% filter(Phenotype1=="INFLAMATORY SYNDROM" | Phenotype2 == "Inflammatory" |
                                               grepl("IMMUNOGLOBULIN",treatments))
      }else if(all(c("Covid-Related Inflammation","Recieved IVIG") %in% input$inflam)){
        filteredDF <- filteredDF %>% filter(inflamSyndrome == "COVID-RELATED INFLAMMATION"|
                                              grepl("IMMUNOGLOBULIN",treatments))
      } else if(all(c("Any Inflammatory Syndrome","Recieved IVIG") %in% input$inflam)){
        filteredDF <- filteredDF %>% filter(inflamSyndrome != "NULL" |
                                              grepl("IMMUNOGLOBULIN",treatments))
      } else if(all(c("Inflammatory Phenotype","Covid-Related Inflammation") %in% input$inflam)){
        filteredDF <- filteredDF %>% filter(Phenotype1=="INFLAMATORY SYNDROM" | Phenotype2 == "Inflammatory" |
                                              inflamSyndrome == "COVID-RELATED INFLAMMATION")
      } else if(all(c("Inflammatory Phenotype","Any Inflammatory Syndrome") %in% input$inflam)){
        filteredDF <- filteredDF %>% filter(Phenotype1=="INFLAMATORY SYNDROM" | Phenotype2 == "Inflammatory" |
                                              inflamSyndrome != "NULL")
      } else if ("Any Inflammatory Syndrome" %in% input$inflam){
        filteredDF <- filteredDF %>% filter(inflamSyndrome != "NULL")
      }else if ("Covid-Related Inflammation" %in% input$inflam){
        filteredDF <- filteredDF %>% filter(inflamSyndrome == "COVID-RELATED INFLAMMATION")
      }else if ("Inflammatory Phenotype" %in% input$inflam){
        filteredDF <- filteredDF %>% filter(Phenotype1=="INFLAMATORY SYNDROM" | Phenotype2 == "Inflammatory")
      }else if("Recieved IVIG" %in% input$inflam){
        filteredDF <- filteredDF %>% filter(grepl("IMMUNOGLOBULIN",treatments))
      } 
      
      filteredDF <- filteredDF %>% filter(Phenotype1 %in% input$phenotypes)
      filteredDF <- filteredDF %>% filter(grepl(paste(input$treatments,collapse = "|"), treatments))
      
      
      
      output$finalTable <- DT::renderDataTable({
        datatable(filteredDF, options=list(autoWidth=TRUE),
                  rownames=FALSE)
        
        
      })
      output$count <- renderText(nrow(filteredDF))
      
      
    })
    

    #diagrammeR output
    
    
    #plotTable
    
 
})
