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
library(ngspatial)
library(pbapply)

# Load data: 
#   crime data
load(file = "C:/Users/ckell/Desktop/Research/bdss_igert_project/data/final/full_crime_bg.Rdata")
#   shape file
load(file = "C:/Users/ckell/Desktop/Research/bdss_igert_project/data/working/det_bg.Rdata")
#   social proximity lodes data
load(file = "C:/Users/ckell/Desktop/Research/bdss_igert_project/data/working/lodes_dat.Rdata")

#re-formatting to add the data to the SpatialPolygonsDataFrame
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, full_dat, by = (GEOID = "GEOID"))

#remove census block groups with no crimes
length(which(is.na(det_bg$median_income)))
na_dat<- det_bg[which(is.na(det_bg$median_income)),]
det_bg <- det_bg[-which(is.na(det_bg$median_income)),]

#pairs(full_dat[,2:9])

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
rownames(W) <- NULL #need this for test if matrix is symmetric
model.ler.geog <- S.CARleroux(formula=form, data=det_bg@data,
                             family="poisson", W=W, burnin=20000, n.sample=120000, thin=10)


sp_sglmm_fit <- sparse.sglmm(formula = form,data=det_bg@data, family = poisson, A = W,
                         verbose = TRUE) #tune = list(sigma.s = 0.02)

#started at 9:06am, 942 MB (says 1 hr remaining)
#save(sp_sglmm_fit, file = "C:/Users/ckell/Desktop/Research/bdss_igert_project/data/final/sp_glmm_fit.Rdata")

sp_sglmm_fit$coefficients
summary(sp_sglmm_fit)

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



###
### Comparison
###
load("C:/Users/ckell/Desktop/Research/bdss_igert_project/data/working/geog_carbayes.Rdata")
summary(sp_sglmm_fit)
# Coefficients:
#   
#                 Estimate      Lower      Upper      MCSE
# (Intercept)    5.997e+00  5.958e+00  6.0150000 3.340e-03
# median_income -2.024e-05 -2.038e-05 -0.0000199 3.524e-08
# upemp_rate     2.638e-01  2.398e-01  0.2965000 1.325e-03
# total_pop      1.168e-06 -5.874e-06  0.0000085 1.149e-07
# perc_male     -2.180e-01 -2.559e-01 -0.1841000 6.865e-04
# med_age       -1.030e-02 -1.064e-02 -0.0099500 5.859e-06
# herf_index     4.661e-01  4.492e-01  0.4869000 7.202e-04
summary(model.bym.geog)
model.bym.geog$summary.results[,1:3]
#                Median    2.5%   97.5%
# (Intercept)    2.1767  1.9286  2.4927
# median_income  0.0000  0.0000  0.0000
# upemp_rate     0.4304  0.2351  0.5651
# total_pop      0.0005  0.0004  0.0005
# perc_male      0.4320  0.3627  0.5008
# med_age       -0.0015 -0.0030 -0.0004
# herf_index     0.7989  0.7704  0.8284
