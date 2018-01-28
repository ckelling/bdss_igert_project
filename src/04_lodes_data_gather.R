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
library(lodes)
library(acs)

#installing package to load all of the lodes data
#devtools::install_git("https://github.com/hrbrmstr/lodes.git")

#load ACS data for matching
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/acs_dat.Rdata")

#download lodes data

mi_lodes <- read_lodes("mi", "od", "main", "JT00", "2015", "~/Data/lodes")
mi_lodes <- as.data.frame(mi_lodes)

#take off last three digits in mi_lodes (http://www.geolytics.com/USCensus,Geocode,Data,Geography,Products.asp)
mi_lodes$w_geocode <- mi_lodes$w_geocode/1000
mi_lodes$w_geocode <- floor(mi_lodes$w_geocode)
#View(as.data.frame(cbind(mi_lodes$w_geocode, mi_lodes$w_geocode2))) they match!

#same thing for home geocode
mi_lodes$h_geocode <- as.numeric(mi_lodes$h_geocode)
mi_lodes$h_geocode <- mi_lodes$h_geocode/1000
mi_lodes$h_geocode <- floor(mi_lodes$h_geocode)

#now I would like to only include census block groups that are in Wayne County (Detroit)
bg_geoid <- as.vector(acs_dat$GEOID)

#subsetting the michigan data to only include detroit block groups for work
mi_lodes_det <- mi_lodes[which(mi_lodes$w_geocode %in% bg_geoid),]
#subsetting the michigan data to only include detroit block groups for home
mi_lodes_det <- mi_lodes_det[which(mi_lodes_det$h_geocode %in% bg_geoid),] 
# this dataset has 370,488 rows

# some exploratory analysis
hist(mi_lodes_det$S000)
max(mi_lodes_det$S000)
min(mi_lodes_det$S000)
median(mi_lodes_det$S000)
mean(mi_lodes_det$S000)
unique(mi_lodes_det$S000)
sum(mi_lodes_det$S000) # there are almost 400,000 employees commuting to and from these block groups

nrow(unique(mi_lodes_det[,c('w_geocode', 'h_geocode')])) # there are only 192,352 unique rows

#removing create_date variable for aggregation purposes
mi_lodes_det <- mi_lodes_det[,-c(13)]          

#aggregating by unique combinations of the work and home geocodes
mi_lodes_det_agg <- aggregate(. ~ w_geocode + h_geocode, data = mi_lodes_det, FUN = sum)      
nrow(mi_lodes_det_agg) # this should be equal to 192,352, and it is
hist(mi_lodes_det_agg$S000) #distribution is still very skewed

#save the data to access later
#save(mi_lodes_det, mi_lodes_det_agg, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/lodes_dat.Rdata")

# url = paste0("http://lehd.ces.census.gov/data/lodes/LODES7/ca/od/ca_od_main_JT00_2015.csv.gz") 
# download.file(url, 'lodes.csv.gz')
# Useful Resources:
#       meaning of codes: https://github.com/aakarner/job-growth-affordability/blob/master/00_Download%20and%20summarize%20LODES%20data.R
#       dot density map: https://github.com/mikeasilva/dot-density-map
#       read_lodes function: https://github.com/hrbrmstr/lodes
#       site for data: https://lehd.ces.census.gov/data/
       
# Next steps:
#    How to establish link between communities based off of mi_lodes S000?
    
# I'd like to compare this number to the total employment from acs
acs.detroit <- geo.make(state = 'MI', county = 'Wayne County', tract = '*', block.group = '*')
acs_employment <- acs.fetch(geography = acs.detroit, endyear = '2015', table.number = 'B23025', col.names = 'pretty')
employment <- data.frame(estimate(acs_employment))
sum(employment$Employment.Status.for.the.Population.16.Years.and.Over..In.labor.force.)
sum(mi_lodes_det$S000)
#this dataset says there are 818,898 people employed in the labor force
#some of them don't leave their block group! (almost half!)


###
# Creating a cutoff for the LODES data
### 
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/lodes_dat.Rdata")
#need to decide cutoff value
hist(mi_lodes_det_agg$S000)
length(which(mi_lodes_det_agg$S000 > 1))/nrow(mi_lodes_det_agg) #only 30% of the data has a value greater than 1
#However, this is still 65,803 network ties

###
# For now, I will use this as my cutoff to create meaningful social proximity links
###
subs_lodes <- mi_lodes_det_agg[which(mi_lodes_det_agg$S000 > 1),]
save(subs_lodes, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/subs_lodes.Rdata")
