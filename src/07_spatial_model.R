###
### Claire Kelling
### Spatial Model
###
### Created 11/28/17 for preliminary spatial modeling
### 


# I will begin by attempting to model the crime spatially.
# I will proceed by modeling the crime through social proximity, rather than geographic proximity.

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
plot(det_bg, border = "gray")
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


# Now, I will work on the social proximity part.
edgelist <- mi_lodes_det_agg[,c(1,2)]

#need to delete some cases
library(mgcv)
edgelist <- edgelist[-which(edgelist$w_geocode == edgelist$h_geocode),]
for(i in 1:nrow(edgelist)){
  if(edgelist[i,1]<edgelist[i,2]){
    edgelist[i,] <- edgelist[i,]
  }else{
    edgelist[i,] <- edgelist[i,c(2,1)]
  }
}
edgelist <- unique(edgelist)


edgelist <- as.matrix(edgelist)

#R can't handle large integer values
min <- min(edgelist)
edgelist[,1] <- edgelist[,1]-min+1
edgelist[,2] <- edgelist[,2]-min+1

edgelist[,1] <- as.integer(edgelist[,1])
edgelist[,2] <- as.integer(edgelist[,2])
colnames(edgelist) <- c()
class(edgelist[,1])


#Create nb object from edge_list
#     first, i need to convert to igraph object
lode_graph <- graph_from_edgelist(edgelist, directed = FALSE)
class(lode_graph)
plot(lode_graph)
summary(lode_graph)
summary(ringGraph)


## http://r-sig-geo.2731867.n2.nabble.com/Class-nb-spdep-from-class-igraph-td7582350.html
## igraph2nb 
## Convert igraph undirected graphs (of S3 class igraph) into nb objects 
## No checking performed 
igraph2nb <- function(gr) { 
  return(neig2nb(neig(edges=get.edgelist(gr)))) 
} 

## Create simple igraph object 
ringGraph <- graph.ring(10) 

## Convert to nb object 
lodes_nb <- igraph2nb(lode_graph) 
test <- neig(edges=get.edgelist(lode_graph))
test <- neig2nb(neig(edges = edgelist))
test2 <- neig(edges=get.edgelist(lode_graph))
?neig

class(lodes_nb)
summary(lodes_nb)

## Visualize using plot.nb 
plot(ringGraph_nb, coords=cbind(runif(10), runif(10))) 

data(mafragh)
par(mfrow = c(2, 1))
provi <- deldir::deldir(mafragh$xy)
provi.neig <- neig(edges = as.matrix(provi$delsgs[, 5:6]))
edge <- as.matrix(provi$delsgs[, 5:6])
