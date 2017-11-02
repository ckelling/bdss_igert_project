###
### Claire Kelling
### Aggregation of Crime Data
###
### Created 11/01/17 for aggregation of crime points to areal units (census blocks)
### 

library(sp)
library(ggmap)
library(tigris)
library(dplyr)

#load in subsetted data- only data that has valid values for Lat/Long
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/detroit_data.Rdata")

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
