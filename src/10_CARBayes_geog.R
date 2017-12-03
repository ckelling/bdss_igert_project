###
### Claire Kelling
### CARBayes Geographic Proxim
###
### Created 11/30/17 to model the geographic proximity data using the CARBayes Poisson GLM structure
### 
### 

# https://cran.r-project.org/web/packages/CARBayes/vignettes/CARBayes.pdf
# https://cran.r-project.org/web/packages/CARBayes/CARBayes.pdf
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
W.nb <- poly2nb(det_bg, row.names = rownames(det_bg@data))
W.list <- nb2listw(W.nb, style="B")
resid.model <- residuals(model)
moran.mc(x=resid.model, listw=W.list, nsim=1000)

#spatial modeling
W <- nb2mat(W.nb, style="B")
model.ler.geog <- S.CARleroux(formula=form, data=det_bg@data,
                             family="poisson", W=W, burnin=20000, n.sample=120000, thin=10)
print(model.spatial)
model.spatial$modelfit
fit_val <- model.spatial$fitted.values

#inference
#look to see if these include 0
summarise.samples(model.spatial$samples$beta, quantiles=c(0.5, 0.025, 0.975))

#now I would like to plot the fitted values
#need to get data in the right format
det_bg@data$fit_val <- fit_val
sp_f <- fortify(det_bg)
#num <- num_per_dist

#make a color or grayscale plot to illustrate this
obs_by_dist <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = fit_val)) + coord_equal() +
  labs(fill = "Fitted Values")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
  ggtitle("Fitted Values for CAR Bayes Model")+ scale_fill_gradient(low = "lightblue", high = "navyblue")


########################
#######
#######   Other model within CARBayes, Sglm
#######
########################
model.bym.geog <- S.CARbym(formula=form, data=det_bg@data,
                          family="poisson", W=W, burnin=20000, n.sample=120000, thin=10)

save(model.bym.geog, model.ler.geog, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/geog_carbayes.Rdata")
