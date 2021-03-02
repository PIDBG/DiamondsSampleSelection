#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DiagrammeR)
library(shiny)
library(DT)
library(dplyr)
library(openxlsx)



covidPims <- read.csv("data/finalCovidPims.csv")
filterable <- covidPims[,c(3:21)]
treatments <- c("ANTIBIOTIC", "ANTIVIRATL","IMMUNOGLOBULIN","STEROID","ANTIMALARIAL","MONOCLONAL_AB","OTHER","STUDY_TREATMENT")

# Define UI for application 
ui <- fluidPage(

   
    
    
    # Application title
    titlePanel("Diamonds Search COVID/MIS-C Filtering"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h2("Filters"),
            checkboxInput("ageIncludeNA", "Include Patients where Age Can't Be Calculated", value = T),
            sliderInput("age","Age Range", value = c(0,110),min=0,max=110),
            checkboxInput("feverIncludeNA", "Include Patients where Fever Length Can't Be Calculated", value = T),
            sliderInput("feverDays", "Days of Fever >=", value = 0, min = 0, max = 10, step=1),
            
            
            checkboxInput("partial","Include Patients with Missing Data"),
            radioButtons("KDfeatures", ">=4 KD Features",
                         choiceNames = c("NA", "Yes", "No"),
                         choiceValues = c("null","yes","no")),
            radioButtons("MisCfeatures", ">=2 Mis-C Features",
                        choiceNames = c("NA","Yes", "No"),
                        choiceValues = c("null","yes","no")),
            
            
            checkboxInput("raisedInflamMarkers","Raised CRP / PCT"),
            checkboxInput("cardiac","Zscore > Limit"),
            checkboxGroupInput("othercause","Exclude Other Microbial Cause of Inflammation:",c("Viral","Bacterial","Other")),
            checkboxGroupInput("covid19","Covid19 Positivity, any of:",c("PCR","IgG","IgM","Previous History","COVID19 Listed in Microbiology Section")),
            checkboxGroupInput("inflam", "Inflammatory Status, any of:", c("Inflammatory Phenotype","Covid-Related Inflammation","Any Inflammatory Syndrome", "Recieved IVIG")),
            checkboxGroupInput("phenotypes","Include the following (primary) phenotypes: ", levels(as.factor(covidPims$Phenotype1)), selected = levels(as.factor(covidPims$Phenotype1))),
            checkboxGroupInput("treatments", "Only include patients who the selected treatments (If none selected, do not filter based on treatment)", treatments),
            
            h4("Consent"),
            checkboxInput("hasConsent", "Has Consent"),
            checkboxInput("naConsent", "Consent not Entered on DB"),
            
            h4("Samples"),
            checkboxInput("pax", "PAX Sample"),
            checkboxInput("edta","EDTA Sample"),
            checkboxInput("smart", "SMART tube"),
            checkboxInput("throat", "Throat Swab"),
            
            radioButtons("alreadyRNA",
                         label = "Already Had RNA Seq",
                         choiceNames = c("Unfiltered","Yes","No"),
                         choiceValues = c("U","Y","N"))
            
            
            
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Set Upper Limits of Normal"),
           fluidRow(sliderInput("CRPLimit", "CRP Upper Limit", value = 10, min = 0, max = 500),
                    sliderInput("PCTLimit", "PCT Upper Limit", value = 0.45, min = 0, max = 5, step = 0.05),
                    sliderInput("tropLimit", "Troponin Upper Limit", value = 15, min = 0, max= 50),
                    sliderInput("BNPLimit", "BNP Upper Limit", value = 500, min = 0, max = 1500),
                    sliderInput("ddimerLimit", "D-dimer Upper Limit", value = 850, min = 0, max = 2000),
                    sliderInput("ptLimit", "PT Upper Limit", value = 15.8, min=0, max=50,step = 0.2),
                    sliderInput("zScoreLimit", "Z-Score Limit", value = 2.5, min=0, max=10, step = 0.5)),
           
           h2("Number of patients meeting filtering requirements:"),
           div(style="text-align:center;
        position:relative;",
               style="colour:red",
           h1(textOutput("count")),
           h3("Scroll Down For Table")
           )
            
        )
    ),
    
    div(style="text-align:center;position:relative;",
        downloadButton("downloadFinalTable", "Download Table")
        ),
    DT::dataTableOutput("finalTable")
)

