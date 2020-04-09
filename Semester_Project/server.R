# Define server logic ----
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$title <- renderText({"Predicted Southern Pine Beetle Outbreaks"})
  
  output$file <- renderPlot({
    Outbreakdata <- read.csv("Outbreak_data.csv")
    Outbreakdata2 <- usmap_transform(Outbreakdata[,c(5:4, 2:3, 6:11)])
    Outbreakdata2$Year <- as.factor(Outbreakdata2$Year)
    Years <- switch(input$YearButton, "2016" = "2016", "2017" = "2017", "2018" = "2018", "All" = c("2016", "2017", "2018"))
    Outbreakdata3 <- Outbreakdata2[Outbreakdata2$Year %in% Years, ]
    Fakedata <- data.frame(Longitude = input$lon,Latitude = input$lat)
    Fakedata2 <- usmap_transform(Fakedata)
    
    G1 <<- plot_usmap("states", 
               include = c(.south_region), exclude= c("Texas", "Oklahoma"))+ 
    geom_point(data = Outbreakdata3, aes(x = Longitude.1, y= Latitude.1, col = Year))+
      scale_color_discrete(drop=FALSE)+
      geom_point(data = Fakedata2, aes(x = Longitude.1, y = Latitude.1))
    
    G1
  })
  
  observeEvent(input$coords, {
    Fakedata <- data.frame(Latitude = input$lat, Longitude = input$lon)
    daymet <- download_daymet(site = "mysite",
                              lat = Fakedata$Latitude,
                              lon = Fakedata$Longitude,
                              start = 2018,
                              end = 2019,
                              internal = TRUE,
                              simplify = F) # returns tidy data! 
    Fakedata$JANMINTEMP <- mean(daymet$data$tmin..deg.c.[366:396]) #2019
    Fakedata$FEBMINTEMP <- mean(daymet$data$tmin..deg.c.[397:424]) #2019
    Fakedata$MARMINTEMP <- mean(daymet$data$tmin..deg.c.[425:455]) #2019
    Fakedata$Prcp <- sum(daymet$data$prcp..mm.day.[365:730]) #2019
    Fakedata$AUGMAXTEMP <- mean(daymet$data$tmax..deg.c.[213:243]) #2018!!
    preds <- predict(m_gam1,Fakedata,type="response", se.fit = T)
    output$predicted <- renderText({paste("Current Model Predicts a ", round(preds$fit, 2), "Acre Outbreak", "SE = ", round(preds$se.fit,2)) })
  })
  
  

  
  observeEvent(input$button, {
    if (input$user == "Insects" & input$PW == "RCool" & !is.null(input$file))
      {newdat <- read.csv(isolate({input$file})$datapath)
       output$uploaded <- renderText({"File Uploaded"})} 
    else 
         output$uploaded <- renderText({"Please log in and upload a .csv file to add data"})})
  
}
