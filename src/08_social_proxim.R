###
### Claire Kelling
### Social Proximity Model and Setup
###
### Created 11/29/17 for preliminary social proximity setup and modeling
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


edgelist <- mi_lodes_det_agg[,c(1,2)]
#edgelist <- edgelist[1:2,]

edgelist <- as.matrix(edgelist)


#need to create variable for rowname and then proceed from here
#after creating rownames, then can proceed with this
#need to create unique assignment from GEOID to rowname (from 1 to 1822)
bg_dat <- det_bg@data
new_geoid <- cbind(1:1822,bg_dat$GEOID)
new_geoid[,1] <- as.numeric(new_geoid[,1])
new_geoid[,2] <- as.numeric(new_geoid[,2])
#not working?
new_geoid[,1] <- as.numeric(new_geoid[,1])

new_el <- matrix(NA,nrow = nrow(edgelist),ncol = ncol(edgelist))

for(i in 1:nrow(edgelist)){
    new_el[i,1] <- as.numeric(new_geoid[which(edgelist[i,1] == new_geoid[,2]),1])
    new_el[i,2] <- as.numeric(new_geoid[which(edgelist[i,2] == new_geoid[,2]),1])
}

new_el[,1] <- as.integer(new_el[,1])
new_el[,2] <- as.integer(new_el[,2])

mat <- matrix(0, 1822, 1822)
mat[new_el] <- 1
sum(mat)

#need to make this matrix symmetric
for(i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    if(mat[i,j] == 1){
      mat[j,i] <- 1
    }
  }
  #need to make diagonal null (not own neighbor, just establishing links)
  mat[i,i] <- 0
}

test2 <- neig2nb(neig(mat01 = mat))
class(test2)
plot(test2)

coords <- coordinates(det_bg)
plot(det_bg, border = "gray")
plot(test2, coords, pch = 19, cex = 0.6, add = TRUE)
plot(na_dat, col= "red", density =50,add = TRUE, border = "gray")