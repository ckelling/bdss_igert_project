###
### Claire Kelling
### Aggregation of Crime Data
###
### Created 11/01/17 for aggregation of crime points to areal units (census blocks)
### Modified 10/06/18 for JQC
### 

library(sp)
library(ggmap)
library(tigris)
library(dplyr)

#load in subsetted data- only data that has valid values for Lat/Long
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/detroit_data.Rdata")
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/acs_dat.Rdata")


#load in census block group spatial polygons
det_bg <- block_groups("Michigan", c("Wayne County"))

#I will convert the files to spatial points and polygons with the same projection
coordinates(detroit_data) <- ~Longitude+Latitude
#proj4string(detroit_data) <- proj4string(det_bg)
proj4string(detroit_data) <- CRS("+proj=longlat")
det_bg <- spTransform(det_bg, CRS("+proj=longlat"))

#need to check to make sure the CRS are identical
proj4string(det_bg)
proj4string(detroit_data) #they match 

#overlay the spatial points of detroit_data onto the spatial polygons of det_bg
overlap_set <- over(detroit_data, det_bg)
nrow(detroit_data)
nrow(overlap_set) #it has classified each of the points in the dataset into a block group
sum(is.na(overlap_set$STATEFP)) #there are 2961 crimes that actually occur outside of Wayne county
#these may be crimes that the police responded to on their own or were incorrectly entered

detroit_df <- as.data.frame(detroit_data)
det_dat_over <- cbind(detroit_df, overlap_set)
#det_dat_over <- det_dat_over[!is.na(over(detroit_data,det_bg)),]
det_dat_ov <- det_dat_over[!is.na(det_dat_over$GEOID),]
#should have 563592 rows (all the rows that are included in the spatial area)

#Therefore, I will proceed with the following data-set
#  I will edit the data to only keep the useful columns
det_dat_ov <- det_dat_ov[,-c(31:37)]

agg_dat <- plyr::count(det_dat_ov, c('GEOID'))
agg_dat$GEOID <- as.factor(agg_dat$GEOID)

#now I would like to create a plot that illustrates how many crimes are occuring per block groups
num_per_bg <- as.numeric(agg_dat$freq)

#Now I will create the data structure that I need to create a plot
sp_f <- fortify(det_bg)
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, agg_dat, by = (GEOID = "GEOID"))
sp_f <- left_join(sp_f, det_bg@data[,c(13,14)])
#num <- num_per_dist

#make a color or grayscale plot to illustrate this
obs_by_dist <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = freq)) + coord_equal() +
  labs(fill = "No. of \nCrimes")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                                     fill = NA, col = "black") +
  ggtitle("Number of Crimes per Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")

save(agg_dat, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/agg_crime_dat.Rdata")



#############################################################################
# Next, I need to subset the crime to only include codes that are related to domestic violence
#############################################################################

length(unique(detroit_data$`Call Description`)) # there are 217 call codes
#length(unique(detroit_data$Category))
#length(unique(detroit_data$`Call Code`))
#              Call Description is slightly longer than category

###
# Find subset of categories to use
###
unique_codes <- unique(detroit_data$`Call Description`)
domv_codes <- c( "CHILD /  ADULT ABUSE", "CHILD / ADULT ABUSE REPORT", "RAPE REPORT", "HARASSMENT REPORT",
                 "ASSAULT OR SEX ASSAULT DELTA", "RAPE IP OR JH", "MOLESTATION REPORT",
                 "ASSAULT  NOT DANGEROUS OR PREV", "MOLESTATION")
probable <- c("ASSAULT AND BATTERY", "FELONIOUS ASSAULT JH", "FELONIOUS ASSAULT IP",
              "FELONIOUS ASSAULT REPORT", "ASSAULT AND BATTERY REPORT")
possible <- c("NOISE COMPLAINT", "SHOTS FIRED IP")
length(domv_codes) #there are 9 acceptable domv related codes

##
#Now, subset the data
##
domv_dat_detroit <- detroit_data[which(detroit_data$`Call Description` %in% domv_codes),]
coordinates(domv_dat_detroit) <- ~Longitude+Latitude
proj4string(domv_dat_detroit) <- CRS("+proj=longlat")

#
# Plot the points for our dataset over Detroit
#
geocodeQueryCheck()
key <- "AIzaSyAwyA61FavsSOqngadzhmwTNamrVquQ4tQ"
key2 <- "AIzaSyAb7DhoebCR9sTXHDW94oYsgTVikPKyBME"

register_google(key = key)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
ggmap(get_googlemap("Detroit"))
register_google(key = key)

DetroitMap <- ggmap(get_map(location=c(-83.2392,42.3528), zoom=10))
DetroitMap2 <- ggmap(get_map(location=c(-83.2392,42.3528), zoom=10))

plot_domv <- detroit_data[which(detroit_data$`Call Description` %in% domv_codes),]
plot_domv <- as.data.frame(plot_domv)

DetroitMap2 <- ggmap(get_map(location=c(-83.1,42.3528), zoom=11))
DetroitMap2 + geom_point(aes(x = Longitude, y = Latitude), data = plot_domv, col = "blue", alpha =0.1)

point_proc <- DetroitMap2  + geom_point(aes(x = Longitude, y = Latitude), data = plot_domv, col = "blue", alpha =0.1) + coord_equal() +
  ggtitle("Point Process, Crime Data")+
  theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))

##
#Overlay the points, as above, except for domestic violence
##
overlap_set <- over(domv_dat_detroit, det_bg)
nrow(domv_dat_detroit)
nrow(overlap_set) #it has classified each of the points in the dataset into a block group
sum(is.na(overlap_set$STATEFP)) #there are 35 crimes that actually occur outside of Wayne county
#these may be crimes that the police responded to on their own or were incorrectly entered

detroit_df <- as.data.frame(domv_dat_detroit)
det_dat_over <- cbind(detroit_df, overlap_set)
#det_dat_over <- det_dat_over[!is.na(over(domv_dat_detroit,det_bg)),]
det_dat_ov <- det_dat_over[!is.na(det_dat_over$GEOID),]

#Therefore, I will proceed with the following data-set
#  I will edit the data to only keep the useful columns
det_dat_ov <- det_dat_ov[,-c(31:37)]

agg_domv_dat <- plyr::count(det_dat_ov, c('GEOID'))
agg_domv_dat$GEOID <- as.factor(agg_domv_dat$GEOID)

#now I would like to create a plot that illustrates how many crimes are occuring per block groups
num_per_bg <- as.numeric(agg_domv_dat$freq)

#Now I will create the data structure that I need to create a plot
sp_f <- fortify(det_bg)
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, agg_domv_dat, by = (GEOID = "GEOID"))
sp_f <- left_join(sp_f, det_bg@data[,c(13,15)])
#num <- num_per_dist

#make a color or grayscale plot to illustrate this
obs_by_dist <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = freq.y)) + coord_equal() +
  labs(fill = "No. of \nCrimes")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
  ggtitle("Number of Crimes per Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")


#need to add acs variables

agg_domv_dat2 <- left_join(acs_dat, agg_domv_dat, by = c(GEOID = "GEOID"))
agg_domv_dat2$freq[which(is.na(agg_domv_dat2$freq))] <- 0
agg_domv_dat_comp <- agg_domv_dat2[which(complete.cases(agg_domv_dat2)),] # 1706 out of 1822

#save(agg_domv_dat_comp, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/agg_domv_crime_dat.Rdata")

