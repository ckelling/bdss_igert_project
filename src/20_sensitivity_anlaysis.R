###
### Claire Kelling
### Creating the sensitivity analysis for the commuting data.
###
### Created       9/7/18
### Last Modified 9/9/18
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
library(CARBayesdata)
library(CARBayes)
library(ngspatial)
library(pbapply)
library(gridExtra)

####
# Creating a cutoff for the LODES data
### 

# # Full shape file
# load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/det_bg.Rdata")
# # Full LODES dataset
# load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/lodes_dat.Rdata")
# # Subsetted shape file for geographic proximity
# load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/det_bg_geog.Rdata")
# #   subsetted crime data
# load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/agg_domv_crime_dat.Rdata")
# 
# # Source the file for setup and modeling functions
# source(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/src/00_modeling_functions.R")


#Directories for running on the cluster:
load(file = "/storage/home/c/cek32/det_bg.Rdata")
# Full LODES dataset
load(file = "/storage/home/c/cek32/lodes_dat.Rdata")
# Subsetted shape file for geographic proximity
load(file = "/storage/home/c/cek32/det_bg_geog.Rdata")
#   subsetted crime data
load(file = "/storage/home/c/cek32/agg_domv_crime_dat.Rdata")

# Source the file for setup and modeling functions
source(file = "/storage/home/c/cek32/00_modeling_functions.R")



#need to decide cutoff values
hist(mi_lodes_det_agg$S000)

#Exploratory Work:
length(which(mi_lodes_det_agg$S000 > 1))/nrow(mi_lodes_det_agg) #only 30% of the data has a value greater than 1
#However, this is still 65,803 network ties
# Approximately 1% have a value greater than 15. 

#cutoff | percent above cutoff
# 1     | 34.2%
# 3     | 11.5%
# 5     | 6%
# 10    | 2%
# 15    | 0.9%

###
# Now I need to create the storage to try different cutoffs 
###
cut_vec <- c(1,3,5,10,15)

#subset the data and run the analysis
# To run the analysis, we need to create the social and geographic proximity matrices (W)

output <- NULL

#model form, including covariate information
form <- freq ~ median_income + upemp_rate+total_pop+perc_male+med_age+herf_index


for(i in cut_vec){
  #test case, comment out for full run
  #i <- 15
  
  #subset the data for that cutoff
  subs_lodes <- mi_lodes_det_agg[which(mi_lodes_det_agg$S000>i),]
  
  #generate W_geog and W_soc
  W_geog <- W_geog_setup(det_bg_geog)
  W_soc <- W_soc_setup(subs_lodes)
  
  #run all of the models for that subset and report output
  new_output <- model_func(W_geog, W_soc)
  
  #store the output
  output <- rbind(output, cbind(rep(i, nrow(new_output)), new_output))
}

#save(output, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/sens_output.Rdata")
save(output, file = "/storage/home/c/cek32/sens_output.Rdata")