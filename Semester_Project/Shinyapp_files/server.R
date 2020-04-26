# Define server logic ----
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$title <- renderText({"Predicted Southern Pine Beetle Outbreaks"})
  
  output$file <- renderPlot({
    Outbreakdata <<- read.csv("Outbreak_data.csv")
    Outbreakdata2 <- usmap_transform(Outbreakdata[Outbreakdata$PRESENT == 1 ,c("Longitude", "Latitude", "Year")])
    Outbreakdata2$Year <- as.factor(Outbreakdata2$Year)
    Years <- switch(input$YearButton, "2016" = "2016", "2017" = "2017", 
                    "2018" = "2018", "All" = Outbreakdata2$Year)
    Outbreakdata3 <- Outbreakdata2[Outbreakdata2$Year %in% Years, ]
    Fakedata <- data.frame(Longitude = as.numeric(input$lon),Latitude = as.numeric(input$lat))
    Fakedata2 <- usmap_transform(Fakedata)
    
    G1 <<- plot_usmap("states", 
               include = c(.south_region), exclude= c("Texas", "Oklahoma"))+ 
    geom_point(data = Outbreakdata3, aes(x = Longitude.1, y= Latitude.1, col = Year))+
      scale_color_discrete(drop=FALSE)+
      geom_point(data = Fakedata2, aes(x = Longitude.1, y = Latitude.1), pch = 4, size =4)
    
    G1
  })
  
  observeEvent(input$coords, {
    Fakedata <<- data.frame(Longitude = input$lon, Latitude = input$lat, Year = input$predyear)
    if(input$predyear != 2020){
    daymet <- download_daymet(site = "mysite",
                              lat = Fakedata$Latitude,
                              lon = Fakedata$Longitude,
                              start = input$predyear -1,
                              end = input$predyear,
                              internal = TRUE,
                              simplify = F) # returns tidy data! 
    Fakedata$JANMAXTEMP <- mean(daymet$data$tmax..deg.c.[366:396]) 
    Fakedata$FEBMAXTEMP <- mean(daymet$data$tmax..deg.c.[397:424])
    Fakedata$MARMAXTEMP <- mean(daymet$data$tmax..deg.c.[425:455]) 
    Fakedata$Prcp <- sum(daymet$data$prcp..mm.day.[365:730]) 
    Fakedata$AUGMAXTEMP <- mean(daymet$data$tmax..deg.c.[213:243]) #year before!!
    Fakedata$JANMINTEMP <- mean(daymet$data$tmin..deg.c.[366:396]) 
    Fakedata$FEBMINTEMP <- mean(daymet$data$tmin..deg.c.[397:424]) 
    Fakedata$MARMINTEMP <- mean(daymet$data$tmin..deg.c.[425:455]) 
    }
    
if(input$predyear == 2020){
      predme <- Fakedata
      coordinates(predme) <- ~Longitude+Latitude
      Jan <<- raster("2020_Jan_max.grd")
      Feb <<- raster("2020_Feb_max.grd")
      March <<- raster("2020_Mar_max.grd")
      Jan.min <<- raster("2020_Jan_min.grd")
      Feb.min <<- raster("2020_Feb_min.grd")
      March.min <<- raster("2020_March_min.grd")
      Aug <<- raster("2019_August_max.grd")
      Precip <<- raster("2019_ppt.grd")
      
      Fakedata$JANMAXTEMP <- extract(Jan, predme)
      Fakedata$FEBMAXTEMP <- extract(Feb, predme)
      Fakedata$MARMAXTEMP <- extract(March, predme)
      Fakedata$Prcp <- extract(Precip, predme)
      Fakedata$AUGMAXTEMP <- extract(Aug, predme)
      Fakedata$JANMINTEMP <- extract(Jan.min, predme)
      Fakedata$FEBMINTEMP <- extract(Feb.min, predme)
      Fakedata$MARMINTEMP <- extract(March.min, predme)
}
    temp.out <- Fakedata[,c("Longitude", "Latitude")]
    coordinates(temp.out) <- ~Longitude+Latitude
    proj4string(temp.out) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
    pts <- spTransform(temp.out,CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
    Fakedata$FOREST <- extract(forests, y = pts)
    
    
    m_gam1 <<- readRDS("insect_predict.rds")
    preds <- predict(m_gam1,Fakedata,type="response", se.fit = T, link = logit)
    output$predicted <- renderText({paste("For coordinates (", Fakedata$Longitude, ", ", Fakedata$Latitude, ")  the current model predicts a ", round(plogis(preds$fit), digits = 2)*100, "%  likelihood of an outbreak in ", Fakedata$Year, ". The current model is based off of data from ", min(Outbreakdata$Year), " to ", max(Outbreakdata$Year), sep = "") })
    output$predictors <- renderText({paste("Predictions based on ",round(Fakedata$FOREST, digits = 2)*100, " percent forest cover at the chosen point, January max temp of", round(Fakedata$JANMAXTEMP, digits = 0), "\u00B0C, February max temp of", round(Fakedata$FEBMAXTEMP, digits = 0), "\u00B0C, March max temp of  ", round(Fakedata$MARMAXTEMP, digits = 0), "\u00B0C, August ", Fakedata$Year- 1, " max temperature of ", round(Fakedata$AUGMAXTEMP, digits = 0), "\u00B0C, and annual precipitation of", round(Fakedata$Prcp, digits =2), "mm.") })
  })

  
  observeEvent(input$button, {
    if (input$user == "Insects" & input$PW == "RCool" & !is.null(input$file))
      {newdat <<- read.csv(isolate({input$file})$datapath)
       output$uploaded <- renderText({"File Uploaded"})} 
    else 
         output$uploaded <- renderText({"Please log in and upload a .csv file to add data"})})
  
  observeEvent(input$optimize, {
    if (!exists("newdat")){output$runmodel <- renderText({"No new data was found. Please enter new data."})}
    if (exists("newdat")){
    for (i in 1: nrow(newdat)){
    daymet <- download_daymet(site = "mysite",
                              lat = newdat$Latitude[i],
                              lon = newdat$Longitude[i],
                              start = newdat$Year[i] - 1,
                              end = newdat$Year[i],
                              internal = TRUE,
                              simplify = F) # returns tidy data! 
    newdat$JANMAXTEMP[i] <- mean(daymet$data$tmax..deg.c.[366:396]) #my year
    newdat$FEBMAXTEMP[i] <- mean(daymet$data$tmax..deg.c.[397:424]) #my year
    newdat$MARMAXTEMP[i] <- mean(daymet$data$tmax..deg.c.[425:455]) #my year
    newdat$Prcp[i] <- sum(daymet$data$prcp..mm.day.[365:730]) #my year 
    newdat$AUGMAXTEMP[i] <- mean(daymet$data$tmax..deg.c.[213:243]) #year BEFORE
    newdat$JANMINTEMP[i] <- mean(daymet$data$tmin..deg.c.[366:396]) 
    newdat$FEBMINTEMP[i] <- mean(daymet$data$tmin..deg.c.[397:424]) 
    newdat$MARMINTEMP[i] <- mean(daymet$data$tmin..deg.c.[425:455])
    } 
      temp.out <- newdat[,c("Longitude", "Latitude")]
      coordinates(temp.out) <- ~Longitude+Latitude
      proj4string(temp.out) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
      pts <- spTransform(temp.out,CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
      newdat$FOREST <- extract(forests, y = pts)  
      
    #get all the variables for the new data
    
    alldata <-merge(newdat, Outbreakdata, all = T) #combine with previous data
    write.csv(alldata, "Outbreak_data.csv") #write to file
    m_gam  <- gam(PRESENT ~ 
                    s(AUGMAXTEMP) + 
                    s(FEBMAXTEMP,JANMAXTEMP, MARMAXTEMP)+
                    s(FEBMINTEMP,JANMINTEMP, MARMINTEMP)+
                    s(Longitude, Latitude) + 
                    s(FOREST)+
                    s(Prcp),
                  family = binomial(), 
                  data = test,
                  method = "REML") #re-fit model
    saveRDS(m_gam, "insect_predict.rds") #save it as new working model
    output$runmodel <- renderText({"Model Re-Optimized!"})
    }
  })
  
  output$modfit <- renderPlot({
    param <- switch(input$params, "Forest Cover" = 5, "Latitude/Longitude" = 4, "Annual Precipitation" = 6, "Previous Year's August Max Temperature" = 1)
    plot(m_gam1, select = param)})
  
  output$deviance <- renderText({paste("Deviance explained by current model: ", round(summary(m_gam1)$dev.expl*100, digits =2), "%")})
  
  #could add in a toggle to plot raw data/stuff 
}
