###
### Claire Kelling
### Social Proximity Model
###
### Created 11/29/17 now that I have completed the set up stage, I will create a preliminary model for 
###    social proximity, similarly to that of spatial proximity.
### 
### 

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
#   social proximity nb object
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/proxim_nb.Rdata")

#re-formatting to add the data to the SpatialPolygonsDataFrame
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, full_dat, by = (GEOID = "GEOID"))

#remove census block groups with no crimes
det_bg <- det_bg[-which(is.na(det_bg$median_income)),]

## Least squares fits
mod.ols <- lm(crime_freq ~ median_income + upemp_rate+total_pop+perc_male+med_age+herf_index, data = det_bg)
summary(mod.ols) # standard errors, p-values not necessarily reliable
resid <- as.data.frame(cbind(full_dat[,1],residuals(mod.ols)))
colnames(resid) <- c("GEOID", "resid")
det_bg@data <- left_join(det_bg@data, resid, by = (GEOID = "GEOID"))
det_bg$resid <- as.numeric(as.character(det_bg$resid))


## Moran test for residuals
## Moran's test for spatial autocorrelation using a spatial weights matrix in weights list form
moran.test(det_bg$resid, listw = nb2listw(proxim_nb, style = "B"))

#convert to listw
crime_listw <- nb2listw(proxim_nb, style = "B") # convert to listw

#CAR Model
mod.car <- spautolm(crime_freq ~ median_income + upemp_rate+perc_male+med_age+herf_index, 
                    data = det_bg, listw = crime_listw, family = "CAR", weights = total_pop)
mod.car <- spautolm(crime_freq ~ median_income + upemp_rate+total_pop+perc_male+med_age+herf_index, 
                    data = det_bg, listw = crime_listw, family = "CAR")
summary(mod.car)
