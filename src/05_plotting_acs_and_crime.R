###
### Claire Kelling
### Plotting ACS and Crime data by BG
###
### Created 11/03/17 for plotting of some acs and crime variables
### 

library(acs)
library(tigris)
library(dplyr)
library(sp)
library(ggplot2)

#load crime data and acs data, and block group shape files
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/agg_crime_dat.Rdata")
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/acs_dat.Rdata")
#det_bg <- block_groups("Michigan", c("Wayne County"))
#save(det_bg, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/det_bg.Rdata")
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/det_bg.Rdata")


#combining the data files
full_dat <- left_join(acs_dat, agg_dat, by = c(GEOID = "GEOID"))


#creating the format so that I can plot these various elements
sp_f <- fortify(det_bg)
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, full_dat, by = (GEOID = "GEOID"))
sp_f <- left_join(sp_f, det_bg@data[,c(13:20)])

#make a color or grayscale plot to illustrate this
inc_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = median_income)) + coord_equal() +
  labs(fill = "Median Income")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
  ggtitle("Median Income by Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")


race_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = race_not_white)) + coord_equal() +
  labs(fill = "% not White")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                             fill = NA, col = "black") +
  ggtitle("% not White by Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")

unemp_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = upemp_rate)) + coord_equal() +
  labs(fill = "Unemployment Rate")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                           fill = NA, col = "black") +
  ggtitle("Unemployment Rate by Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")


pop_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = total_pop)) + coord_equal() +
  labs(fill = "Total Population")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                                 fill = NA, col = "black") +
  ggtitle("Total Population by Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")


age_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = med_age)) + coord_equal() +
  labs(fill = "Median Age")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                                fill = NA, col = "black") +
  ggtitle("Median Age by Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")



crime_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = freq)) + coord_equal() +
  labs(fill = "Crime Frequencey")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                          fill = NA, col = "black") +
  ggtitle("Crime Frequency by Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")
