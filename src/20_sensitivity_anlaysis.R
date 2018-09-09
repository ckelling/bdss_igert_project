###
### Claire Kelling
### Creating the sensitivity analysis for the commuting data.
###
### Created       9/7/18
### Last Modified 9/9/18
### 

####
# Creating a cutoff for the LODES data
### 

# Loading the full LODES dataset
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/lodes_dat.Rdata")
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


for(i %in% cut_vec){
  
  #subset the data for that cutoff
  
  #run all of the models for that subset
  
  #store the output
}
subs_lodes <- mi_lodes_det_agg[which(mi_lodes_det_agg$S000>15),]