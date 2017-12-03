###
### Claire Kelling
### Model Comparison
###
### Created 12/1/2017 to compare models between geographic and social neighborhoods, as well as two
### different modeling techniques 
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
library(gridExtra)

# Load data: 
#   crime data
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/final/full_crime_bg.Rdata")
#   shape file
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/det_bg.Rdata")
#   social proximity lodes data
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/lodes_dat.Rdata")
#   social proximity nb object
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/proxim_nb.Rdata")


# Loading geog models
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/geog_carbayes.Rdata")
# Loading soc proxim models
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/proxim_carbayes.Rdata")

#Model Comparison:
#  social bym
model.bym.soc$modelfit
#  geog bym
model.bym.geog$modelfit
#  social ler
model.ler.soc$modelfit
#  geog ler
model.ler.geog$modelfit

#Plot of comparative results:
#re-formatting to add the data to the SpatialPolygonsDataFrame
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, full_dat, by = (GEOID = "GEOID"))

#remove census block groups with no crimes
na_dat<- det_bg[which(is.na(det_bg$median_income)),]
det_bg <- det_bg[-which(is.na(det_bg$median_income)),]
#det_bg <- spTransform(det_bg, CRS("+proj=longlat"))


#   function for plot
plot_fit <- function(spat_mod){
  fit_values <- spat_mod$fitted.values
  det_bg@data$fit_val <- fit_values
  sp_f <- fortify(det_bg)
  sp_f <- left_join(sp_f, det_bg@data[,c(13,22)])
  fit_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = fit_val)) + coord_equal() +
    labs(fill = "Fitted Values")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
    ggtitle("Fitted Values for Social Leroux")+ scale_fill_gradient(low = "lightblue", high = "navyblue")+
    theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))
  return(fit_by_bg)
}

plot_crime <- function(spat_mod){
  fit_values <- spat_mod$fitted.values
  det_bg@data$fit_val <- fit_values
  sp_f <- fortify(det_bg)
  sp_f <- left_join(sp_f, det_bg@data[,c(13,21)])
  fit_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = crime_freq)) + coord_equal() +
    labs(fill = "Number of Crimes")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
    ggtitle("Number of Crimes per block group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")+
    theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))
  return(fit_by_bg)
}

#   plotting all of the fitted values
p1 <- plot_fit(model.bym.soc)
p2 <- plot_fit(model.ler.soc)
p3 <- plot_fit(model.bym.geog)
p4 <- plot_fit(model.ler.geog)

#plot the actual crime data, using any model
p5 <- plot_crime(model.bym.geog)

grid.arrange(p1, p2, p3, p4, ncol=2)
#save as 2000, 1300