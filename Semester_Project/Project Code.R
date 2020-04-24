
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



############################
# Reading in outbreak data #
############################


myfile16 <- ("https://raw.githubusercontent.com/insectsaremetal/INFO8000/master/Semester_Project/Outbreak_2016.csv")
Outbreak16 <- read.csv(myfile16)
names(Outbreak16)[1] <- "Year"

myfile17 <- ("https://raw.githubusercontent.com/insectsaremetal/INFO8000/master/Semester_Project/Outbreak_2017.csv")
Outbreak17 <- read.csv(myfile17)
names(Outbreak17)[1] <- "Year"

myfile18 <- ("https://raw.githubusercontent.com/insectsaremetal/INFO8000/master/Semester_Project/Outbreak_2018.csv")
Outbreak18 <- read.csv(myfile18)
names(Outbreak18)[1] <- "Year"

# Letting R know these are spatial files
# And reprojecting it so it matches other files

coordinates(Outbreak16) <- ~Longitude+Latitude
proj4string(Outbreak16) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

coordinates(Outbreak17) <- ~Longitude+Latitude
proj4string(Outbreak17) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

coordinates(Outbreak18) <- ~Longitude+Latitude
proj4string(Outbreak18) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")



###############################
# Reading in temperature data #
###############################


# Reading in a shapefile to create an extent
map_US <- readOGR("states_21basic", "states")
map_SE <- subset(map_US, STATE_NAME == "Georgia" | 
                   STATE_NAME == "Alabama" | 
                   STATE_NAME == "South Carolina" | 
                   STATE_NAME == "North Carolina" | 
                   STATE_NAME == "Tennessee" | 
                   STATE_NAME == "Florida" |
                   STATE_NAME == "Kentucky" | 
                   STATE_NAME == "Maryland" |
                   STATE_NAME == "Virginia" |
                   STATE_NAME == "Mississippi" | 
                   STATE_NAME == "Delaware" |
                   STATE_NAME == "Texas" |
                   STATE_NAME == "Louisiana" |
                   STATE_NAME == "Mississippi" |
                   STATE_NAME == "Arkansas")


#create a master raster to assist with matching projections later
#Weather data is a raster and SPB files are not, so you can't change the weather projection using the SPB data
#You have to create a raster to use to change the projection of the weather daymet data
masterraster <- spsample(map_SE, 1000000, type = "regular")
gridded(masterraster) <- TRUE
masterraster <- raster(masterraster)
masterraster$one <-1
masterraster <- mask(masterraster, map_SE)

# Reading in Daymet data

tmax_raster18 <- stack("./daymet_v3_tmax_monavg_2018_na.tif")
tmax_raster17 <- stack("./daymet_v3_tmax_monavg_2017_na.tif")
tmax_raster16 <- stack("./daymet_v3_tmax_monavg_2016_na.tif")
tmax_raster15 <- stack("./daymet_v3_tmax_monavg_2015_na.tif")

tmin_raster18 <- stack("./daymet_v3_tmin_monavg_2018_na.tif")
tmin_raster17 <- stack("./daymet_v3_tmin_monavg_2017_na.tif")
tmin_raster16 <- stack("./daymet_v3_tmin_monavg_2016_na.tif")

PRCP15 <- raster("./daymet_v3_prcp_annttl_2015_na.tif")
PRCP16 <- raster("./daymet_v3_prcp_annttl_2016_na.tif")
PRCP17 <- raster("./daymet_v3_prcp_annttl_2017_na.tif")

# Reprojecting that file, so all files have same projection
tmax_raster18 <- projectRaster(from = tmax_raster18, 
                               to= masterraster)
tmax_raster17 <- projectRaster(from = tmax_raster17, 
                               to= masterraster)
tmax_raster16 <- projectRaster(from = tmax_raster16, 
                               to= masterraster)
tmax_raster15 <- projectRaster(from = tmax_raster15, 
                               to= masterraster)
tmin_raster18 <- projectRaster(from = tmin_raster18, 
                               to= masterraster)
tmin_raster17 <- projectRaster(from = tmin_raster17, 
                               to= masterraster)
tmin_raster16 <- projectRaster(from = tmin_raster16, 
                               to= masterraster)
PRCP15 <- projectRaster(from = PRCP15,
                        to = masterraster)
PRCP16 <- projectRaster(from = PRCP16,
                        to = masterraster)
PRCP17 <- projectRaster(from = PRCP17,
                        to = masterraster)

# crop and mask so it is just for the SE

Tmax18<-crop(tmax_raster18, map_SE)
Tmax18<- mask(Tmax18,map_SE)
Tmax17<-crop(tmax_raster17, map_SE)
Tmax17<- mask(Tmax17,map_SE)
Tmax16<-crop(tmax_raster16, map_SE)
Tmax16<- mask(Tmax16,map_SE)
Tmax15<-crop(tmax_raster15, map_SE)
Tmax15<- mask(Tmax15,map_SE)

Tmin18<-crop(tmin_raster18, map_SE)
Tmin18<- mask(Tmin18,map_SE)
Tmin17<-crop(tmin_raster17, map_SE)
Tmin17<- mask(Tmin17,map_SE)
Tmin16<-crop(tmin_raster16, map_SE)
Tmin16<- mask(Tmin16,map_SE)

PRCP15<- crop(PRCP15,map_SE)
PRCP15<- mask(PRCP15,map_SE)
PRCP16<- crop(PRCP16,map_SE)
PRCP16<- mask(PRCP16,map_SE)
PRCP17<- crop(PRCP17,map_SE)
PRCP17<- mask(PRCP17,map_SE)



###########################################
# Extracting temps for each of our points #
###########################################

Outbreak16$JANMAXTEMP <- extract(Tmax16[[1]], Outbreak16)
Outbreak16$FEBMAXTEMP <- extract(Tmax16[[2]], Outbreak16)
Outbreak16$MARMAXTEMP <- extract(Tmax16[[3]], Outbreak16)
Outbreak16$AUGMAXTEMP <- extract(Tmax15[[8]], Outbreak16)
Outbreak16$JANMINTEMP <- extract(Tmin16[[1]], Outbreak16)
Outbreak16$FEBMINTEMP <- extract(Tmin16[[2]], Outbreak16)
Outbreak16$MARMINTEMP <- extract(Tmin16[[3]], Outbreak16)

Outbreak17$JANMAXTEMP <- extract(Tmax17[[1]], Outbreak17)
Outbreak17$FEBMAXTEMP <- extract(Tmax17[[2]], Outbreak17)
Outbreak17$MARMAXTEMP <- extract(Tmax17[[3]], Outbreak17)
Outbreak17$AUGMAXTEMP <- extract(Tmax16[[8]], Outbreak17)
Outbreak17$JANMINTEMP <- extract(Tmin17[[1]], Outbreak17)
Outbreak17$FEBMINTEMP <- extract(Tmin17[[2]], Outbreak17)
Outbreak17$MARMINTEMP <- extract(Tmin17[[3]], Outbreak17)

Outbreak18$JANMAXTEMP <- extract(Tmax18[[1]], Outbreak18)
Outbreak18$FEBMAXTEMP <- extract(Tmax18[[2]], Outbreak18)
Outbreak18$MARMAXTEMP <- extract(Tmax18[[3]], Outbreak18)
Outbreak18$AUGMAXTEMP <- extract(Tmax17[[8]], Outbreak18)
Outbreak18$JANMINTEMP <- extract(Tmin18[[1]], Outbreak18)
Outbreak18$FEBMINTEMP <- extract(Tmin18[[2]], Outbreak18)
Outbreak18$MARMINTEMP <- extract(Tmin18[[3]], Outbreak18)

Outbreak16$Prcp <- extract(PRCP15, Outbreak16)
Outbreak17$Prcp <- extract(PRCP16, Outbreak17)
Outbreak18$Prcp <- extract(PRCP17, Outbreak18)


# Converting back to a dataframe 
Outbreak16 <- as.data.frame(Outbreak16)
Outbreak17 <- as.data.frame(Outbreak17)
Outbreak18 <- as.data.frame(Outbreak18)

Outbreakdata <- rbind(Outbreak16, Outbreak17, Outbreak18)
write.csv(Outbreakdata, "Outbreak_data.csv")


##########################
# CREATING THE MODEL     #
##########################
Outbreakdata <- read.csv("Outbreak_data.csv")
Outbreakdata$RISK <- ifelse(Outbreakdata$Acres > 5, 1, 0)
forests <- raster("SE_Forest.tif")

temp.out <- Outbreakdata[,c("Longitude", "Latitude")]
coordinates(temp.out) <- ~Longitude+Latitude
proj4string(temp.out) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
pts <- spTransform(temp.out,CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Outbreakdata$FOREST <- extract(forests, y = pts)


  
# Possible variables are:
# Monthly minimum temps: Jan, Feb, Mar
# Monthly maximum temps: Jan, Feb, Mar, Aug (prior year)
# Prior year annual precipitation
# Forest measures

# m_gam1  <- gam(Acres ~ 
#                  s(AUGMAXTEMP) + 
#                  s(Prcp) +
#                  s(FEBMAXTEMP) + 
#                  s(JANMAXTEMP,FEBMAXTEMP,MARMAXTEMP) +
#                  s(Longitude, Latitude),
#                family = gaussian(), 
#                data = Outbreakdata,
#                method = "REML")
#deviance explained 3.64

# m_gam1  <- gam(Acres ~ 
#                  s(AUGMAXTEMP) + 
#                  s(Prcp) +
#                  s(Longitude, Latitude),
#                family = gaussian(), 
#                data = Outbreakdata,
#                method = "REML")
#deviance explained = 2.99

# m_gam1  <- gam(Acres ~ 
#                   s(AUGMAXTEMP) + 
#                   s(Prcp) +
#                  s(FEBMAXTEMP)+
#                  s(JANMAXTEMP)+
#                  s(MARMAXTEMP)+
#                   s(Longitude, Latitude),
#                 family = gaussian(), 
#                 data = Outbreakdata,
#                 method = "REML")
# #deviance explained = 3.46
# 
# m_gam1  <- gam(Acres ~ 
#                  s(AUGMAXTEMP) + 
#                  s(Prcp) +
#                  s(FEBMINTEMP)+
#                  s(JANMINTEMP)+
#                  s(MARMINTEMP)+
#                  s(Longitude, Latitude),
#                family = gaussian(), 
#                data = Outbreakdata,
#                method = "REML")
#deviance explained = 3.59

# m_gam1  <- gam(Acres ~ 
#                  s(AUGMAXTEMP) + 
#                  s(FEBMINTEMP)+
#                  s(JANMINTEMP)+
#                  s(MARMINTEMP)+
#                  s(Longitude, Latitude),
#                family = gaussian(), 
#                data = Outbreakdata,
#                method = "REML")
#deviance explained = 3.65

# m_gam1  <- gam(RISK ~ 
#                  s(AUGMAXTEMP) + 
#                  s(FEBMINTEMP)+
#                  s(JANMINTEMP)+
#                  s(MARMINTEMP)+
#                  s(Longitude, Latitude),
#                family = binomial(), 
#                data = Outbreakdata,
#                method = "REML")
#deviance explained = 23.2%

# m_gam1  <- gam(RISK ~ 
#                  s(AUGMAXTEMP) + 
#                  s(FEBMINTEMP,JANMINTEMP, MARMINTEMP)+
#                  s(Longitude, Latitude) + 
#                  s(Prcp),
#                family = binomial(), 
#                data = Outbreakdata,
#                method = "REML")
#deviance explained = 24.1

# m_gam1  <- gam(RISK ~ 
#                  s(AUGMAXTEMP) + 
#                  s(FEBMINTEMP,JANMINTEMP, MARMINTEMP)+
#                  s(FEBMAXTEMP,JANMAXTEMP, MARMAXTEMP)+
#                  s(Longitude, Latitude) + 
#                  s(Prcp),
#                family = binomial(), 
#                data = Outbreakdata,
#                method = "REML")
#Deviance explained = 25.9%

# m_gam1  <- gam(RISK ~ 
#                   s(AUGMAXTEMP) + 
#                   s(FEBMINTEMP)+
#                   s(JANMINTEMP)+
#                   s(MARMINTEMP)+
#                   s(Longitude, Latitude)+
#                   Year,
#                 family = binomial(), 
#                 data = Outbreakdata,
#                 method = "REML")
#Deviance explained = 23.5%
 
# m_gam1  <- gam(RISK ~ 
#                   AUGMAXTEMP + 
#                   FEBMINTEMP +
#                   JANMINTEMP +
#                   MARMINTEMP +
#                   s(Longitude, Latitude)+
#                   Year,
#                 family = binomial(), 
#                 data = Outbreakdata,
#                 method = "REML")
#Deviance explained = 19.5%
summary(m_gam1)

m_gam1  <- gam(RISK ~ 
  s(AUGMAXTEMP) + 
  s(FEBMAXTEMP,JANMAXTEMP, MARMAXTEMP)+
  s(Longitude, Latitude) + 
  s(Prcp) + s(FOREST),
  family = binomial(), 
  data = Outbreakdata,
  method = "REML")
summary(m_gam1)
#21.4 % deviance explained

temp.out <- test[,c("Longitude", "Latitude")]
coordinates(temp.out) <- ~Longitude+Latitude
proj4string(temp.out) <-CRS ("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
pts <- spTransform(temp.out,CRS("+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
test$FOREST <- extract(forests, y = pts)



m_gam1  <- gam(PRESENT ~ 
                 s(AUGMAXTEMP) + 
                 s(FEBMAXTEMP,JANMAXTEMP, MARMAXTEMP)+
                 s(Longitude, Latitude) + 
                 s(Prcp),
               family = binomial(), 
               data = test,
               method = "REML")
summary(m_gam1)
#66.2% deviance explained 

m_gam2  <- gam(PRESENT ~ 
                 s(AUGMAXTEMP) + 
                 s(FEBMAXTEMP,JANMAXTEMP, MARMAXTEMP)+
                 s(FEBMINTEMP,JANMINTEMP, MARMINTEMP)+
                 s(Longitude, Latitude) + 
                 s(Prcp),
               family = binomial(), 
               data = test,
               method = "REML")
summary(m_gam2)
#70.9% deviance explained

m_gam3  <- gam(PRESENT ~ 
                 s(AUGMAXTEMP) + 
                 s(FEBMAXTEMP,JANMAXTEMP, MARMAXTEMP)+
                 s(FEBMINTEMP,JANMINTEMP, MARMINTEMP)+
                 s(Longitude, Latitude) + 
                 s(FOREST)+
                 s(Prcp),
               family = binomial(), 
               data = test,
               method = "REML")
summary(m_gam3)

saveRDS(m_gam3, "insect_predict.rds")


#summary(m_gam1)
#gam.check(m_gam1)



## Test prediction 
Fakedata <- data.frame(Latitude = 32.95954, Longitude = -85.45714, Year = 2020)
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

pred.outbreak <- predict(m_gam3,Fakedata,type="response")
plot(test$Longitude, test$Latitude)
points(temp.out, col = "red")
