###
### Claire Kelling
### Social Proximity Setup
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
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/full_crime_bg.Rdata")
#   shape file
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/det_bg.Rdata")
#   social proximity lodes data
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/lodes_dat.Rdata")
#   subsetted social proximity lodes data
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/subs_lodes.Rdata")
#   subsetted crime data
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/agg_domv_crime_dat.Rdata")
mi_lodes_det_agg <- subs_lodes
full_dat <- agg_domv_dat_comp

edgelist <- mi_lodes_det_agg[,c(1,2)]
edgelist <- as.matrix(edgelist)


# I need to rescale the block groups so that their ID's are not so large
bg_dat <- det_bg@data
new_geoid <- cbind(1:1822,bg_dat$GEOID)
new_geoid[,1] <- as.numeric(new_geoid[,1])
new_geoid[,2] <- as.numeric(new_geoid[,2])

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

#I need to remove those entries such that there are no ACS information, in order for modeling to work
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, full_dat, by = (GEOID = "GEOID"))
no_acs <- which(is.na(det_bg$median_income))
na_dat<- det_bg[which(is.na(det_bg$median_income)),]
det_bg <- det_bg[-which(is.na(det_bg$median_income)),]

#they are equal (checked!)
#View(cbind(det_bg$GEOID, new_geoid[,2]))
mat <- mat[-no_acs,-no_acs] #this is my new adjacency matrix where all info is complete

#create nb object
proxim_nb <- neig2nb(neig(mat01 = mat))
test2 <- neig(mat01 = mat)

#now I would like to plot the social proximity
coords <- coordinates(det_bg)
plot(det_bg, border = "gray", main = "Detroit Social Proximity Neighborhood")
plot(proxim_nb, coords, pch = 1, cex = 0.6, add = TRUE)
plot(na_dat, col= "red", density =50,add = TRUE, border = "gray")
#plot(na_dat, col= "red", density =50, border = "gray")

######################################################################################
##### Now, I will make a subset of the social proximity data, to make a clearer plot,
##### for presentation purposes
######################################################################################
# I will choose the first mention of a block group
#incl <- runif(1000, 1, 192352)
#need to include at least one from each block group
#sub_el <- new_el[incl,]

#removing ones with no blockgroups first
rem <- c()
for(i in 1:nrow(edgelist)){
  if(i %% 1000 ==0){
    print(i)
    }
  if(edgelist[i,1] %in% na_dat$GEOID){
    rem <- c(rem,i)
    #edgelist <- edgelist[-i,]
  }
  if(edgelist[i,2] %in% na_dat$GEOID){
    rem <- c(rem,i)
    #edgelist <- edgelist[-i,]
  }
}
rem <- unique(rem)
edgelist <- edgelist[-rem,]


#need to redo edgelist
new_el <- matrix(NA,nrow = nrow(edgelist),ncol = ncol(edgelist))

for(i in 1:nrow(edgelist)){
  new_el[i,1] <- as.numeric(new_geoid[which(edgelist[i,1] == new_geoid[,2]),1])
  new_el[i,2] <- as.numeric(new_geoid[which(edgelist[i,2] == new_geoid[,2]),1])
}
length(unique(c(new_el[,1],new_el[,2])))

col1 <- new_el[!duplicated(new_el[,1]),]
col2 <- new_el[!duplicated(new_el[,2]),]
sub_el_comp <- rbind(col1,col2)
length(unique(c(sub_el_comp[,1], sub_el_comp[,2])))
sub_el_comp <- unique(sub_el_comp)

#create new adjacency matrix
mat <- matrix(0, 1822, 1822)
mat[sub_el_comp] <- 1
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
sum(mat)
mat <- mat[-no_acs,-no_acs]

#create nb object
proxim_nb2 <- neig2nb(neig(mat01 = mat))
summary(proxim_nb2)

#now I would like to plot the social proximity
coords <- coordinates(det_bg)
plot(det_bg, border = "gray", main = "Social Proximity \nNeighborhood Structure (Sample)")
plot(proxim_nb2, coords, pch = 1, cex = 0.6, add = TRUE)
plot(na_dat, col= "red", density =50,add = TRUE, border = "gray")

#save(proxim_nb, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/subset_soc_proxim_nb.Rdata")
