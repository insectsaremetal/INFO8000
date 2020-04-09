library(shiny)
library(shinyjs)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set directory to file location
m_gam1 <<- readRDS("insect_predict.rds")
#Fakedata <<- data.frame(Longitude = -85.45714, Latitude = 32.95954) #until new values are put in

ui <<- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(type = 'text/css', '#title{font-size: 48px; font-family: calibri light; 
               background-color: rgba(255,255,255,0.40); color: blue; border-style: none;}')
  ),
  
  fluidRow(
    column(textOutput("title"), offset = 0, width = 12)),
  
  fluidRow(
  column(radioButtons("YearButton", 
                      "What Year of Data Would You Like to See?", 
                      c("2016", "2017", "2018", "All")), width = 4),
  column(width = 2, textOutput("predicted")),
  column(plotOutput("file"), width = 6)),
  
  fluidRow(
    column(width = 2,
           numericInput("lat", "Latitude (DD)", value = 31.95954)),
    column(width = 2,
           numericInput("lon", "Longitude (DD)", value = -85.45714)),
    column(width = 2,
           actionButton("coords", "Predict"))),
    
  fluidRow(
  column(width = 4, fileInput("file", h3("Add Outbreak Data (requires log in)"), 
           accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
  column(width = 3, textOutput("uploaded"))),
  
  fluidRow(
    column(width = 2,
           textInput("user", "User Name", value = "example")),
    column(width = 2,
           passwordInput("PW", "Password", value = "password")),
    column(width = 2,
           actionButton("button", "Upload")))
    
)
