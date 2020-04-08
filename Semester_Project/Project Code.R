
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

vepPolygon <- polygon_from_extent(raster::extent(map_SE),
                                  proj4string="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

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

tmin_raster18 <- stack("./daymet_v3_tmin_monavg_2018_na.tif")
tmin_raster17 <- stack("./daymet_v3_tmin_monavg_2017_na.tif")
tmin_raster16 <- stack("./daymet_v3_tmin_monavg_2016_na.tif")

# Reprojecting that file, so all files have same projection
tmax_raster18 <- projectRaster(from = tmax_raster18, 
                               to= masterraster)
tmax_raster17 <- projectRaster(from = tmax_raster18, 
                               to= masterraster)
tmax_raster16 <- projectRaster(from = tmax_raster18, 
                               to= masterraster)
tmin_raster18 <- projectRaster(from = tmin_raster18, 
                               to= masterraster)
tmin_raster17 <- projectRaster(from = tmin_raster17, 
                               to= masterraster)
tmin_raster16 <- projectRaster(from = tmin_raster16, 
                               to= masterraster)

# crop and mask so it is just for the SE

Tmax18<-crop(tmax_raster18, map_SE)
Tmax18<- mask(Tmax18,map_SE)
Tmax17<-crop(tmax_raster17, map_SE)
Tmax17<- mask(Tmax17,map_SE)
Tmax16<-crop(tmax_raster16, map_SE)
Tmax16<- mask(Tmax16,map_SE)

Tmin18<-crop(tmin_raster18, map_SE)
Tmin18<- mask(Tmin18,map_SE)
Tmin17<-crop(tmin_raster17, map_SE)
Tmin17<- mask(Tmin17,map_SE)
Tmin16<-crop(tmin_raster16, map_SE)
Tmin16<- mask(Tmin16,map_SE)



###########################################
# Extracting temps for each of our points #
###########################################

Outbreak16$JANMAXTEMP <- extract(Tmax18[[1]], Outbreak16)
Outbreak16$FEBMAXTEMP <- extract(Tmax18[[2]], Outbreak16)
Outbreak16$MARMAXTEMP <- extract(Tmax18[[3]], Outbreak16)
Outbreak16$JANMINTEMP <- extract(Tmin18[[1]], Outbreak16)
Outbreak16$FEBMINTEMP <- extract(Tmin18[[2]], Outbreak16)
Outbreak16$MARMINTEMP <- extract(Tmin18[[3]], Outbreak16)

Outbreak17$JANMAXTEMP <- extract(Tmax17[[1]], Outbreak17)
Outbreak17$FEBMAXTEMP <- extract(Tmax17[[2]], Outbreak17)
Outbreak17$MARMAXTEMP <- extract(Tmax17[[3]], Outbreak17)
Outbreak17$JANMINTEMP <- extract(Tmin17[[1]], Outbreak17)
Outbreak17$FEBMINTEMP <- extract(Tmin17[[2]], Outbreak17)
Outbreak17$MARMINTEMP <- extract(Tmin17[[3]], Outbreak17)

Outbreak18$JANMAXTEMP <- extract(Tmax18[[1]], Outbreak18)
Outbreak18$FEBMAXTEMP <- extract(Tmax18[[2]], Outbreak18)
Outbreak18$MARMAXTEMP <- extract(Tmax18[[3]], Outbreak18)
Outbreak18$JANMINTEMP <- extract(Tmin18[[1]], Outbreak18)
Outbreak18$FEBMINTEMP <- extract(Tmin18[[2]], Outbreak18)
Outbreak18$MARMINTEMP <- extract(Tmin18[[3]], Outbreak18)

# Converting back to a dataframe 
Outbreak16 <- as.data.frame(Outbreak16)
Outbreak17 <- as.data.frame(Outbreak17)
Outbreak18 <- as.data.frame(Outbreak18)

Outbreakdata <- rbind(Outbreak16, Outbreak17, Outbreak18)



##########################
# CREATING THE MODEL     #
##########################

m_gam1  <- gam(Acres ~ s(JANMAXTEMP) + s(FEBMAXTEMP) + s(MARMAXTEMP) +
                 s(JANMINTEMP) + s(FEBMINTEMP) + s(MARMINTEMP) +
                 s(Longitude, Latitude),
               family = gaussian(), data = Outbreakdata,
               method = "REML")
summary(m_gam1)
gam.check(m_gam1)





