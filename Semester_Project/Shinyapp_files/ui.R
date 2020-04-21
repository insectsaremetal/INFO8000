library(shiny)
library(shinyjs)
library(readr)
library(knitr)
library(daymetr)
library(raster)
library(lattice)
library(latticeExtra)
library(mgcv)
library(RCurl)
library(rgdal)
library(usmap)
library(ggplot2)
library(maptools)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set directory to file location
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
                      c("2016", "2017", "2018", "All")), width = 3),
  column(width = 2, textOutput("predicted")),
  column(width = 2, textOutput("predictors")),
  column(plotOutput("file"), width = 5)),
  
  fluidRow(
    column(width = 2,
           numericInput("lat", "Latitude (DD)", value = 31.95954, min = 25, max = 39, step = .20000), tags$div(tags$p("Please enter a latitude between 25 and 39"))),
    column(width = 2,
           numericInput("lon", "Longitude (DD)", value = -85.45714, min = -95, max = -76, step = .20000), tags$div(tags$p("Please enter a longitude between -95 and -76"))),
    column(width = 2,
           numericInput("predyear", "Year", value = 2020, min = 1990, max= 2020), tags$div(tags$p("Please enter a Year between 1990 and 2020"))),
    column(width = 2,
           actionButton("coords", "Predict"), 
           tags$div(tags$p("Hit this button to calculate the predicted outbreak acreage")))),
  
  fluidRow(
  column(width = 4, fileInput("file", h3("Add Outbreak Data (requires log in)"), 
           accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
  tags$div(
    tags$p(" "),
    tags$p("Upload expects a csv file with a minimum of 3 columns:"),
      tags$p("Latitude, Longitude and Year"),
    tags$p("All other columns will be ignored")),
  column(width = 3, textOutput("uploaded")),
  column(width = 3, textOutput("runmodel"))),
  
  fluidRow(
    column(width = 2,
           textInput("user", "User Name", value = "example")),
    column(width = 2,
           passwordInput("PW", "Password", value = "password")),
    column(width = 2,
           actionButton("button", "Upload")),
  column(width = 2,
         actionButton("optimize", "Re-Run Model")),
  tags$div(
    tags$p("Rerunning the model will use the newly uploaded data to fit a new GAM. This may take a few moments")))
)
