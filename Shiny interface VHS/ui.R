library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(xlsx)

scorecard = xlsx::read.xlsx("Triage_score_card.xlsx",
                     sheetIndex = 1, header = TRUE)

zipcode_info = read.csv("zipcode_info.csv",
                      stringsAsFactors = FALSE, header = TRUE)
facility = read.csv("facility.csv",
                         stringsAsFactors = TRUE, header = TRUE)

Item1 = fluidRow(height=400,
        column(width=4, 
         fluidRow(height=270,       
           box(title = "RAPID TRANSPORT DECISION", width = NULL, height=150,  status="warning",
               solidHeader = TRUE,
             h1(tags$b(textOutput("rapid_points")))),
           box(title="Geography", width = NULL, height=120, status="warning", solidHeader = TRUE,
               h1(tags$b(textOutput("rural_ind")))
           )
        )),
        column(width=4, height=270, 
         box(title = "TRIAGE SCORE", width = NULL, height=120, status="warning", solidHeader = TRUE,
             h1(tags$b(textOutput("Total_points")))),
         
         box(title = "Ground Distance/Time", width = NULL, height=150,  status="warning",
             solidHeader = TRUE,
             column(width=6,
             h1(tags$b(textOutput("distance"))),
             tags$b("miles")
               ),
             column(width=6,
             h1(tags$b(textOutput("time"))),
             tags$b("minutes")
               )
          )),
        column(width=4, height=290,  
         box(title="OUTCOME", width = NULL, status="warning", solidHeader = TRUE, height=290,
             background ="yellow",
                h3(tags$b(textOutput("Outcome")))
             ))
        
       )
       
Item2 = fluidRow( 
          column(width=3, 
            box(title="Patient information", width = NULL, 
                column(width=10,
                  column(width=5,
                      textInput("age", "Age", value=1)),
                  column(width=7,
                      selectInput("units", "Age Units", 
                      choices=c('Years', 'Months', 'Days'))),
                 
                  selectInput("Gender", "Gender of injured", 
                  scorecard$Bin[scorecard$Variable == "gender"]),
                
                  selectInput("Vehicle_Accident", "Is this a vehicle Accident?", 
                  choices=scorecard$Bin[scorecard$Variable == "vehicle accident"]) ),
                column(width=1,
                   tags$b("Points"), br(), br(),
                   textOutput("Age_points"), br(), br(), br(), 
                   textOutput("gender_points"), br(), br(), br(),
                   textOutput("accident_points")
                )
          )),
          column(width=3, 
           box(title = "GCS scores", width = NULL,
               column(width=10,
                 selectInput("gcs_motor", "GCS Motor score of injured",
                 choices=scorecard$Bin[scorecard$Variable == "GCS motor"]),
                 
                 selectInput("gcs_verbal", "GCS Verbal score of injured",
                 choices=scorecard$Bin[scorecard$Variable == "GCS verbal"])
              ),
               column(width=1,
                      tags$b("Points"), br(), br(),
                      textOutput("motor_points"), br(), br(), br(),
                      textOutput("verbal_points")
              ))
          ),
          column(width=3, 
            box(title="Prehospital Parameters", width = NULL,
               column(width=10,
                 textInput("oximetry", "Oximetry of injured (0-100%)", value=95),
                 
                 textInput("sbp", "Systolic blood pressure of injured(0-250 mm Hg)", value=120),
                
                 textInput("pulse_rate", "Pulse rate of injured (beats per minute)", value=80)
                ),
               column(width=1,
                      tags$b("Points"), br(), br(),
                      textOutput("oxi_points"), br(), br(), br(), br(),
                      textOutput("sbp_points"), br(), br(), br(), br(),
                      textOutput("pulse_points")
               )
          )),
          column(width=3, 
            box(title = "Geographical information", width = NULL, solidHeader = TRUE, 
              textInput("injury_zip", "Injury zip code", value="23219"),
              selectInput("facility", "Choose Facility Name",
                          choices=unique(facility$facility_name))
              )) 
        )
      
        
shinyUI(dashboardPage(skin="yellow", 
  dashboardHeader(title = "VCU HEALTH SYSTEMS",
                  titleWidth=350),
 
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(Item1,
                Item2)
 )
)