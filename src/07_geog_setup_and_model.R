###
### Claire Kelling
### Spatial Model
###
### Created 11/28/17 for preliminary spatial modeling
### 


# I will begin by modeling the crime spatially.

# Packages:
library(sp)
library(spdep)
library(classInt)
library(fields)
library(ggplot2)
library(dplyr)
library(ade4) 
library(igraph) 


# Load data: 
#   crime data
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/final/full_crime_bg.Rdata")
#   shape file
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/det_bg.Rdata")
#   social proximity lodes data
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/lodes_dat.Rdata")


#re-formatting to add the data to the SpatialPolygonsDataFrame
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, full_dat, by = (GEOID = "GEOID"))

#remove census block groups with no crimes
length(which(is.na(det_bg$median_income)))
na_dat<- det_bg[which(is.na(det_bg$median_income)),]
det_bg <- det_bg[-which(is.na(det_bg$median_income)),]
plot(det_bg)
plot(na_dat, col= "red", density =50,add = TRUE)

## Create nb object based on shared boundaries
nb.bound <- poly2nb(det_bg) # shared boundaries
summary(nb.bound)
coords <- coordinates(det_bg)
plot(det_bg, border = "gray", main = "Geographic Proximity \nNeighborhood Structure")
plot(nb.bound, coords, pch = 19, cex = 0.6, add = TRUE)
plot(na_dat, col= "red", density =50,add = TRUE, border = "gray")

## Least squares fits
mod.ols <- lm(crime_freq ~ median_income + upemp_rate+total_pop+perc_male+med_age+herf_index, data = det_bg)
summary(mod.ols) # standard errors, p-values not necessarily reliable
resid <- as.data.frame(cbind(full_dat[,1],residuals(mod.ols)))
colnames(resid) <- c("GEOID", "resid")
det_bg@data <- left_join(det_bg@data, resid, by = (GEOID = "GEOID"))
det_bg$resid <- as.numeric(as.character(det_bg$resid))

View(head(det_bg@data))

## Moran test for residuals
## Moran's test for spatial autocorrelation using a spatial weights matrix in weights list form.
moran.test(det_bg$resid, listw = nb2listw(nb.bound, style = "B"))

#convert to listw
crime_listw <- nb2listw(nb.bound, style = "B") # convert to listw

#CAR Model
mod.car <- spautolm(crime_freq ~ median_income + upemp_rate+perc_male+med_age+herf_index, 
                    data = det_bg, listw = crime_listw, family = "CAR", weights = total_pop)
mod.car <- spautolm(crime_freq ~ median_income + upemp_rate+total_pop+perc_male+med_age+herf_index, 
                    data = det_bg, listw = crime_listw, family = "CAR")
summary(mod.car)
