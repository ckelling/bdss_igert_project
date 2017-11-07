###
### Claire Kelling
### Gathering of LODES data 
###
### Created 11/06/17 for gathering of data from LODES and putting it into analysis form
### 

library(sp)
library(ggmap)
library(tigris)
library(dplyr)

#installig package to load all of the lodes data
devtools::install_git("https://github.com/hrbrmstr/lodes.git")
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/acs_dat.Rdata")

library(lodes)

#download lodes data
?read_lodes
mi_lodes <- read_lodes("mi", "od", "aux", "JT00", "2015", "~/Data/lodes")
mi_lodes <- as.data.frame(mi_lodes)

#take off last three digits in mi_lodes (http://www.geolytics.com/USCensus,Geocode,Data,Geography,Products.asp)
mi_lodes$w_geocode2 <- mi_lodes$w_geocode/1000
mi_lodes$w_geocode2 <- floor(mi_lodes$w_geocode2)
View(as.data.frame(cbind(mi_lodes$w_geocode, mi_lodes$w_geocode2)))

#now I would like to only include census block groups that are in Wayne County (Detroit)
bg_geoid <- as.vector(acs_dat$GEOID)

mi_lodes_det <- mi_lodes[which(mi_lodes$w_geocode %in% bg_geoid),]

length(unique(mi_lodes$w_geocode))
options(scipen = 999)
max(mi_lodes$w_geocode)
max(bg_geoid)

mi_lodes$w_geocode[15] %in% bg_geoid

test <- 