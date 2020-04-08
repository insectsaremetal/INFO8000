# Define server logic ----
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$file <- renderPlot({
    Outbreakdata <- read.csv("Outbreak_16_18.csv")
    Outbreakdata2 <- usmap_transform(Outbreakdata[,c(5:4, 2:3, 6:11)])
    Outbreakdata2$Year <- as.factor(Outbreakdata2$Year)
    Years <- switch(input$YearButton, "2016" = "2016", "2017" = "2017", "2018" = "2018", "All" = c("2016", "2017", "2018"))
    Outbreakdata3 <- Outbreakdata2[Outbreakdata2$Year %in% Years, ]
    
    plot_usmap("counties", 
               include = c(.south_region), exclude= c("Texas", "Oklahoma"))+ 
    geom_point(data = Outbreakdata3, aes(x = Longitude.1, y= Latitude.1, col = Year))
  })
  
  
  output$title <- renderText({"Predicted Southern Pine Beetle Outbreaks"})

  
  observeEvent(input$button, {
    if (input$user == "Insects" & input$PW == "RCool" & !is.null(input$file))
      {newdat <- read.csv(isolate({input$file})$datapath)
       output$uploaded <- renderText({"File Uploaded"})} 
    else 
         output$uploaded <- renderText({"Please log in and upload a .csv file to add data"})})
  
}
