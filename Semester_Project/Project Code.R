######## INFO8000 PROJECT ########

# Required libraries
library(readr)
library(knitr)
library(daymetr)
library(raster)
library(lattice)
library(latticeExtra)
library(mgcv)
library(RCurl)
library(rgdal)


#############################################################
# PART 2: MODEL AND PREDICTIONS
#############################################################

##########################
# CREATING THE MODEL     #
##########################

# Models that had low predictive power were removed
# These can be viewed by reviewing the Github history

Outbreakdata <- ("https://raw.githubusercontent.com/insectsaremetal/INFO8000/master/Semester_Project/Outbreak_data.csv")
Outbreakdata <- read.csv(Outbreakdata)

# Possible response variables are:
# Acres - size of outbreak
# Risk - Low vs high risk for outbreak 
# Presence or absence of an outbreak

# Possible Predictor variables are:
# Monthly minimum temps: Jan, Feb, Mar
# Monthly maximum temps: Jan, Feb, Mar, Aug (prior year)
# Prior year annual precipitation
# Forest measures


m_gam3  <- gam(PRESENT ~ 
                 s(AUGMAXTEMP) + 
                 s(FEBMAXTEMP,JANMAXTEMP, MARMAXTEMP)+
                 s(FEBMINTEMP,JANMINTEMP, MARMINTEMP)+
                 s(Longitude, Latitude) + 
                 s(FOREST)+
                 s(Prcp),
               family = binomial(link = logit), 
               data = test,
               method = "REML")
summary(m_gam3)
gam.check(m_gam3)

saveRDS(m_gam3, "insect_predict.rds")


#summary(m_gam1)
#gam.check(m_gam1)



## Test prediction 
Fakedata <- data.frame(Latitude = 32.95954, 
                       Longitude = -85.45714, 
                       Year = 2019)
daymet <- download_daymet(site = "mysite",
                          lat = Fakedata$Latitude,
                          lon = Fakedata$Longitude,
                          start = 2018,
                          end = 2019,
                          internal = TRUE,
                          simplify = F) # returns tidy data! 
Fakedata$JANMAXTEMP <- mean(daymet$data$tmax..deg.c.[366:396]) #2019
Fakedata$FEBMAXTEMP <- mean(daymet$data$tmax..deg.c.[397:424]) #2019
Fakedata$MARMAXTEMP <- mean(daymet$data$tmax..deg.c.[425:455]) #2019
Fakedata$Prcp <- sum(daymet$data$prcp..mm.day.[365:730]) #2019
Fakedata$AUGMAXTEMP <- mean(daymet$data$tmax..deg.c.[213:243]) #2018!!
Fakedata$JANMINTEMP <- mean(daymet$data$tmin..deg.c.[366:396]) #2019
Fakedata$FEBMINTEMP <- mean(daymet$data$tmin..deg.c.[397:424]) #2019
Fakedata$MARMINTEMP <- mean(daymet$data$tmin..deg.c.[425:455]) #2019

temp.out <- Fakedata[,c("Longitude", "Latitude")]
coordinates(temp.out) <- ~Longitude+Latitude
proj4string(temp.out) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
pts <- spTransform(temp.out,CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Fakedata$FOREST <- extract(forests, y = pts)

pred.outbreak <- predict(m_gam3,Fakedata,type="response", link = logit)
plot(test$Longitude, test$Latitude)
points(temp.out, col = "red")
