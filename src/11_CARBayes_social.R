###
### Claire Kelling
### CARBayes Geographic Proxim
###
### Created 11/30/17 to model the geographic proximity data using the CARBayes Poisson GLM structure
### 
### 

# https://cran.r-project.org/web/packages/CARBayes/vignettes/CARBayes.pdf
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
length(which(is.na(det_bg$median_income)))
na_dat<- det_bg[which(is.na(det_bg$median_income)),]
det_bg <- det_bg[-which(is.na(det_bg$median_income)),]

pairs(full_dat[,2:9])

#non-spatial modeling
form <- crime_freq ~ median_income + upemp_rate+total_pop+perc_male+med_age+herf_index
model <- lm(formula=form, data=det_bg@data)
summary(model)

#test for spatial dependence
#null hypothesis of no spatial autocorrelation (alternative of positive spatial autocorrelation)
#also computes Moran's I statistic 
#if p-value < 0.05, we conclude there is positve spatial autocorrelation
W.nb <- proxim_nb
W.list <- nb2listw(W.nb, style="B")
resid.model <- residuals(model)
moran.mc(x=resid.model, listw=W.list, nsim=1500)

#spatial modeling
W <- nb2mat(W.nb, style="B")
#this model took 28 minutes to run....
model.ler.soc <- S.CARleroux(formula=form, data=det_bg@data,
                             family="poisson", W=W, burnin=20000, n.sample=120000, thin=10)
#10:18
#save(model.spatial, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/social_proxim_CARBayes_mod.Rdata")
print(model.spatial)
model.spatial$modelfit
model.spatial$fitted.values

#inference
#look to see if these include 0
summarise.samples(model.spatial$samples$beta, quantiles=c(0.5, 0.025, 0.975))


########################
#######
#######   Other model within CARBayes, Sglm
#######
########################
model.bym.soc <- S.CARbym(formula=form, data=det_bg@data,
                             family="poisson", W=W, burnin=20000, n.sample=120000, thin=10)

save(model.bym.soc, model.ler.soc, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/proxim_carbayes.Rdata")
