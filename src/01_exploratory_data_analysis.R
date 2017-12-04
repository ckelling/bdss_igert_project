###
### Claire Kelling
### Exploratory Data Analysis- Detroit
###
### Created 10/29/17 for exploratory analysis
### 

#libraries
library(sp)
library(ggmap)
library(tigris)
library(readr)
source("C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert/src/shaby_point_ref_function.R")

#load in data
#detroit_data <- read_csv("C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/original/Detroit_DPD__911_Calls_for_Service__September_20__2016_-_Present.csv")

#find all NA values for lat/long
#num_na <- sum(is.na(detroit_data$Longitude)) #35,288
#num_nalat <- sum(is.na(detroit_data$Latitude)) # also 35,288
#perc_na <- num_na / nrow(detroit_data) #5.9%

#subset the data to not include na's for latitude and longitude
#detroit_data <- detroit_data[!is.na(detroit_data$Longitude), ]
#save(detroit_data, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/detroit_data.Rdata")

#load in subsetted data (data that has valid points for lat/long)
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/detroit_data.Rdata")


#convert data to spatial points
coordinates(detroit_data) <- cbind(detroit_data$Longitude, detroit_data$Latitude)
proj4string(detroit_data) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#plot data
plot(detroit_data)

#making sure that all of the points lie in the correct space
max(detroit_data$Latitude) # 42.52774
min(detroit_data$Latitude) # 42
max(detroit_data$Longitude) # -82.88221
min(detroit_data$Longitude) # -83.99016

#create a map of Detroit using Google maps API
#with this level of zoom (8), I can include all of the points (any less, I miss some points)
DetroitMap <- qmap('detroit', zoom = 10, legend = 'topleft')
DetroitMap
max(DetroitMap$data$lat) # 42.3517
min(DetroitMap$data$lat) # 42.31109
max(DetroitMap$data$lon) # -83.07318
min(DetroitMap$data$lon) # -83.07318

# Plot my crime data on the map of Detroit
detroit_data <- as.data.frame(detroit_data)
DetroitMap +geom_point(aes(x = Longitude, y = Latitude), data = detroit_data )
DetroitMap+ geom_polygon(data = det_bg, aes(x = long, y = lat, group = group),colour= 'black', fill = NA)

#the crimes are largely condensed within Detroit, but there are some outside of Detroit
#this may be due to data entry error


#how many unique crime types are there?
length(unique(detroit_data$Category))
length(unique(detroit_data$Call.Code))

#get the set of block groups for Detroit
det_bg <- block_groups("Michigan", c("Wayne County"))
plot(det_bg)
x11()

#plot the data on top of the spatial polygon files
DetroitMap +geom_point(aes(x = Longitude, y = Latitude), data = detroit_data )+
  geom_polygon(data = det_bg, aes(x = long, y = lat, group = group),colour= 'black', fill = NA)


#now I would like to examine just priority 1 calls
n_p1 <- length(which(detroit_data$Priority == 1)) # 70,151
perc_p1 <- n_p1/nrow(detroit_data) #12.4%
#subsetting the data
p1_dat <- detroit_data[which(detroit_data$Priority == 1),]

#plotting the new dataset
x11()
DetroitMap +geom_point(aes(x = Longitude, y = Latitude), data = p1_dat, colour = "red", size = 1 )+
  geom_polygon(data = det_bg, aes(x = long, y = lat, group = group),colour= 'black', fill = NA)

# I will try to fit a model for response time
# first, I need to subset the data so that it does not include NA values for Total Time (most inclusive definition)

#keeping the data that doesn't have NA values for the variables I am interested in
sum(is.na(detroit_data$Total.Response.Time))/nrow(detroit_data) #5.4% of the data has NA for Total Response Time
tot_rtime_dat <- detroit_data[!is.na(detroit_data$Total.Response.Time),]
tot_rtime_dat <- tot_rtime_dat[!is.na(tot_rtime_dat$Priority),] #also not including rows that have NA for priority

hist(tot_rtime_dat$Total.Response.Time)
max(tot_rtime_dat$Total.Response.Time)

#cannot eliminate- doesn't appear to be data entry
#length(which(tot_rtime_dat$Total.Response.Time>990))
#tot_rtime_dat[which(tot_rtime_dat$Total.Response.Time>990),]

#creating linear model
lin_mod <- lm(Total.Response.Time ~ Longitude + Latitude + Priority, data = tot_rtime_dat)
summary(lin_mod)
res <- lin_mod$residuals

#plotting the residuals spatially
tot_rtime_dat$res <- res
tot_rtime_dat <- as.data.frame(tot_rtime_dat)

x11()
DetroitMap +geom_point(aes(x = Longitude, y = Latitude, colour = res), data = tot_rtime_dat, size = 1 )+scale_colour_gradient()+
  geom_polygon(data = det_bg, aes(x = long, y = lat, group = group),colour= 'black', fill = NA)
