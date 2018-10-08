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
library(reshape)

####
# Creating a cutoff for the LODES data
### 

# # Full shape file
 load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/det_bg.Rdata")
# # Full LODES dataset
 load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/lodes_dat.Rdata")
# # Subsetted shape file for geographic proximity
 load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/det_bg_geog.Rdata")
# #   subsetted crime data
 load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/agg_domv_crime_dat.Rdata")
# 
# # Source the file for setup and modeling functions
 source(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/src/00_modeling_functions.R")

# 
# #Directories for running on the cluster:
# load(file = "/storage/home/c/cek32/det_bg.Rdata")
# # Full LODES dataset
# load(file = "/storage/home/c/cek32/lodes_dat.Rdata")
# # Subsetted shape file for geographic proximity
# load(file = "/storage/home/c/cek32/det_bg_geog.Rdata")
# #   subsetted crime data
# load(file = "/storage/home/c/cek32/agg_domv_crime_dat.Rdata")
# 
# # Source the file for setup and modeling functions
# source(file = "/storage/home/c/cek32/00_modeling_functions.R")



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

#started at 10/4, 9pm
for(i in cut_vec){
  #test case, comment out for full run
  #i <- 15
  print(i)
  
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
#save(output, file = "/storage/home/c/cek32/sens_output.Rdata")
save(output, file = "C:/Users/Brian/Desktop/Google Drive/Drive Sync/Documents/Claire/sens_output.Rdata")

#Also need to run the geographic analysis, where we just repeat the simulation many times for geographic (not social)
geog_out <- NULL
n_rep <- 10
for(i in 1:n_rep){
  #generate W_geog and W_soc
  W_geog <- W_geog_setup(det_bg_geog)
  
  #run all of the models for that subset and report output
  new_output <- geog_func(W_geog)
  
  #store the output
  geog_out <- rbind(geog_out, cbind(rep(i, nrow(new_output)), new_output))
}

#save output
save(geog_out, file = "C:/Users/Brian/Desktop/Google Drive/Drive Sync/Documents/Claire/geog_out.Rdata")


#Analyze final output, cannot compare pD, only DIC between models
#load("C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/sens_output3.Rdata")
#load("C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/sens_output4.Rdata")
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/geog_out.Rdata")
load("C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/f_sens_output.Rdata")
load("C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/f_geog_out.Rdata")


#format and review table
### subset table to just include DIC for all models, and format variables
dic_out <- output[c(1,2,6,10,14,18),]
dic_out <- as.data.frame(dic_out)
dic_out <- dic_out[-1,]
#"BYM Geog", "Ler Geog", "SGLMM Geog"
colnames(dic_out) <- c("cutoff","BYM Soc", "Ler Soc", "SGLMM Soc")

#need to also format our repeated results for geog_out
geog_out <- geog_out[c(2,4,6,8,10,12,14,16,18,20),c(2,3,4)]
geog_out <- as.data.frame(geog_out)
colnames(geog_out) <- c("BYM Geog", "Ler Geog", "SGLMM Geog")

#sapply is not working with these factors, so just converting all of the variables to numerics
dic_out$cutoff <- as.numeric(as.character(dic_out$cutoff))
geog_out$`BYM Geog` <- as.numeric(as.character(geog_out$`BYM Geog`))
geog_out$`Ler Geog` <- as.numeric(as.character(geog_out$`Ler Geog`))
geog_out$`SGLMM Geog` <- as.numeric(as.character(geog_out$`SGLMM Geog`))
dic_out$`BYM Soc` <- as.numeric(as.character(dic_out$`BYM Soc`))
dic_out$`Ler Soc` <- as.numeric(as.character(dic_out$`Ler Soc`))
dic_out$`SGLMM Soc` <- as.numeric(as.character(dic_out$`SGLMM Soc`))

geog_out$`BYM Geog`[which(geog_out$`BYM Geog` < 0)] <- NA

#inserting the average DIC from repeated simulation of BYM, Leroux, and SGLMM
dic_out$`BYM Geog` <- rep(mean(geog_out$`BYM Geog`, na.rm =T), nrow(dic_out))
dic_out$`Ler Geog` <- rep(mean(geog_out$`Ler Geog`, na.rm =T), nrow(dic_out))
dic_out$`SGLMM Geog` <- rep(mean(geog_out$`SGLMM Geog`, na.rm = T), nrow(dic_out))


#BYM Plot
bym_dat <- melt(dic_out[,c(1,2,5)], id = c("cutoff"))
colnames(bym_dat) <- c("cutoff", "model", "dic")
bym <- ggplot()+geom_line(data=bym_dat, aes(x=cutoff, y=dic, color=model), size = 2)+labs(title = "BYM Model", x= "Cutoff", y = "DIC")

#Leroux Plot
ler_dat <- melt(dic_out[,c(1,3,6)], id = c("cutoff"))
colnames(ler_dat) <- c("cutoff", "model", "dic")
ler <- ggplot()+geom_line(data=ler_dat, aes(x=cutoff, y=dic, color=model), size =2)+labs(title = "Leroux Model", x= "Cutoff", y = "DIC")

#SGLMM Plot
sglmm_dat <- melt(dic_out[,c(1,4,7)], id = c("cutoff"))
colnames(sglmm_dat) <- c("cutoff", "model", "dic")
sglmm <- ggplot()+geom_line(data=sglmm_dat, aes(x=cutoff, y=dic, color=model), size =2)+labs(title = "SGLMM Model", x= "Cutoff", y = "DIC")

#Combine all the plots
#With this plot, we will be able to see the variation in the performance of the Social model with respect to the cutoff,
# and we can compare this to the mean of the simulations of the Geographic model (which does not vary with the cutoff)
grid.arrange(bym, ler, sglmm, nrow = 1)
#save as 1200x300

#Should also report the standard deviation of the DIC for the models
se <- rep(NA,3)
se[1] <- sd(geog_out$`BYM Geog`, na.rm=T)/sqrt(10)
se[2] <- sd(geog_out$`Ler Geog`)/sqrt(10)
se[3] <- sd(geog_out$`SGLMM Geog`)/sqrt(10)
se
