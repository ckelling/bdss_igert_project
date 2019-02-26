###
### Claire Kelling
### Subset block groups for geographic modeling
###
### Created 2/8/18 to subset block groups, making code cleaner
### 
### 
library(sp)
library(spdep)
library(classInt)
library(fields)
library(ggplot2)
library(dplyr)
library(ade4) 
library(igraph) 
library(CARBayesdata)
library(CARBayes)
library(ngspatial)
library(pbapply)

# Load data: 
#   subsetted crime data
load(file = "C:/Users/ckell/Desktop/Google Drive/Research/bdss_igert_project/data/final/agg_domv_crime_dat.Rdata")
#   shape file
load(file = "C:/Users/ckell/Desktop/Google Drive/Research/bdss_igert_project/data/working/det_bg.Rdata")
shape_file <- det_bg

#re-formatting to add the data to the SpatialPolygonsDataFrame
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, agg_domv_dat_comp, by = (GEOID = "GEOID"))

#remove census block groups with no crimes
length(which(is.na(det_bg$median_income)))
na_dat<- det_bg[which(is.na(det_bg$median_income)),]
det_bg <- det_bg[-which(is.na(det_bg$median_income)),]

# Test for spatial dependence
#    null hypothesis of no spatial autocorrelation (alternative of positive spatial autocorrelation)
#    also computes Moran's I statistic 
#    if p-value < 0.05, we conclude there is positve spatial autocorrelation
W.nb <- poly2nb(det_bg, row.names = rownames(det_bg@data))

## Need to replace int 0 with NULL
ind <- NULL
for(i in 1:length(W.nb)){
  ind <- c(ind, W.nb[[i]][1] != 0)
}
W.nb <- subset(W.nb, ind)

#non-spatial modeling (just linear model)
det_bg_geog <- det_bg[ind,]

#save(det_bg_geog, file = "C:/Users/ckell/Desktop/Google Drive/Research/bdss_igert_project/data/working/det_bg_geog.Rdata")

#create plot of W
coords <- coordinates(det_bg_geog)
plot(shape_file, border = "gray",  main = "Geographic Proximity")
plot(W.nb, coords, pch = 1, cex = 0.6, add = TRUE)
#plot(na_dat, col= "red", density =50,add = TRUE, border = "gray")
#plot(na_dat, col= "red", density =50, border = "gray")

summary(W.nb)
