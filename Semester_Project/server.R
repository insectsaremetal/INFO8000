# Define server logic ----
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$title <- renderText({"Predicted Southern Pine Beetle Outbreaks"})
  
  output$file <- renderPlot({
    Outbreakdata <<- read.csv("Outbreak_data.csv")
    Outbreakdata2 <- usmap_transform(Outbreakdata[,c("Longitude", "Latitude", "Year")])
    Outbreakdata2$Year <- as.factor(Outbreakdata2$Year)
    Years <- switch(input$YearButton, "2016" = "2016", "2017" = "2017", 
                    "2018" = "2018", "All" = Outbreakdata2$Year)
    Outbreakdata3 <- Outbreakdata2[Outbreakdata2$Year %in% Years, ]
    Fakedata <- data.frame(Longitude = input$lon,Latitude = input$lat)
    Fakedata2 <- usmap_transform(Fakedata)
    
    G1 <<- plot_usmap("states", 
               include = c(.south_region), exclude= c("Texas", "Oklahoma"))+ 
    geom_point(data = Outbreakdata3, aes(x = Longitude.1, y= Latitude.1, col = Year))+
      scale_color_discrete(drop=FALSE)+
      geom_point(data = Fakedata2, aes(x = Longitude.1, y = Latitude.1), pch = 4, size =3)
    
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
    m_gam1 <<- readRDS("insect_predict.rds")
    preds <- predict(m_gam1,Fakedata,type="response", se.fit = T)
    output$predicted <- renderText({paste("Current Model Predicts a ", round(preds$fit, 2), "Acre Outbreak in 2019", "SE = ", round(preds$se.fit,2)) })
  })
  
  

  
  observeEvent(input$button, {
    if (input$user == "Insects" & input$PW == "RCool" & !is.null(input$file))
      {newdat <<- read.csv(isolate({input$file})$datapath)
       output$uploaded <- renderText({"File Uploaded"})} 
    else 
         output$uploaded <- renderText({"Please log in and upload a .csv file to add data"})})
  
  observeEvent(input$optimize, {
    if (!is.null(newdat)){
    for (i in 1: nrow(newdat)){
    daymet <- download_daymet(site = "mysite",
                              lat = newdat$Latitude[i],
                              lon = newdat$Longitude[i],
                              start = newdat$Year[i] - 1,
                              end = newdat$Year[i],
                              internal = TRUE,
                              simplify = F) # returns tidy data! 
    newdat$JANMINTEMP[i] <- mean(daymet$data$tmin..deg.c.[366:396]) #my year
    newdat$FEBMINTEMP[i] <- mean(daymet$data$tmin..deg.c.[397:424]) #my year
    newdat$MARMINTEMP[i] <- mean(daymet$data$tmin..deg.c.[425:455]) #my year
    newdat$Prcp[i] <- sum(daymet$data$prcp..mm.day.[365:730]) #my year 
    newdat$AUGMAXTEMP[i] <- mean(daymet$data$tmax..deg.c.[213:243]) #year BEFORE 
    } #get all the variables for the new data
    
    alldata <-merge(newdat, Outbreakdata, all = T) #combine with previous data
    write.csv(alldata, "Outbreak_data.csv") #write to file
    m_gam  <- gam(Acres ~ 
                     s(AUGMAXTEMP) + s(Prcp) +
                     s(JANMINTEMP,FEBMINTEMP,MARMINTEMP) +
                     s(Longitude, Latitude),
                   family = nb(), data = alldata,
                   method = "REML") #re-fit model
    saveRDS(m_gam, "insect_predict.rds") #save it as new working model
    output$runmodel <- renderText({"Model Re-Optimized!"})
    }
  })
  
  
}
