###
### Claire Kelling
### All models KDD
###
### Created 2/7/18 to complete all models for JQC paper
### Last modified: 1/21/19
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
library(gridExtra)

# Load data: 
#   subsetted crime data
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/agg_domv_crime_dat.Rdata")
#   shape file
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/det_bg_final.Rdata")
#   social proximity nb object
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/proxim_nb.Rdata")
#   subsetted shape file for social proximity
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/det_bg_soc.Rdata")
#   subsetted shape file for geographic proximity
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/det_bg_geog_final.Rdata")
#   proximity matrix for social
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/W_soc_final.Rdata")
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/W_soc_10.Rdata")
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/W_soc_3.Rdata")



# Test for spatial dependence
#    null hypothesis of no spatial autocorrelation (alternative of positive spatial autocorrelation)
#    also computes Moran's I statistic 
#    if p-value < 0.05, we conclude there is positve spatial autocorrelation
W.nb <- poly2nb(det_bg_geog, row.names = rownames(det_bg_geog@data)) 

# Summary of nb's
#summary(W.nb)
#summary(proxim_nb)


#non-spatial modeling (just linear model)
form <- crime_freq ~ median_income + upemp_rate+total_pop+perc_male+med_age+herf_index
#model <- glm(formula=form, data=det_bg_geog@data, family = "poisson")
model <- lm(formula=form, data=det_bg_geog@data)
#summary(model)

W.list <- nb2listw(W.nb, style="B")
resid.model <- residuals(model)
moran.mc(x=resid.model, listw=W.list, nsim=5000) #1500 originally

##############
### Creating separate adjacency matrices from nb
##############
W_geog <- nb2mat(W.nb, style="B")
W_soc <- W.soc.3 #for table in paper, W.soc.10 (for graphing)
                # for models, use optimal of W.soc.3 (cutoff of 3)
                  #for models, W.soc

#############
###  Combining adjacency matrices
#############
#addition
add_W <- W_geog + W_soc
#binary
bin_W <- ifelse(add_W == 2, 1, add_W)
bin_nb <- neig2nb(neig(mat01 = bin_W))
summary(bin_nb)

W.list <- nb2listw(bin_nb, style="B")
moran.mc(x=resid.model, listw=W.list, nsim=1500) #1500 originally


###
### Also test Moran's I for just social matrix
###
ind <- NULL
for(i in 1:nrow(W.soc.3)){
    if(sum(W.soc.3[i,]) == 0){
      ind <- c(ind,i)
    }
}
mat <- W.soc.3[-ind,-ind]
bin_nb <- neig2nb(neig(mat01 = mat))
summary(bin_nb)

W.list <- nb2listw(bin_nb, style="B")
moran.mc(x=resid.model, listw=W.list, nsim=1000)


####
#### Geographic modeling
####
W <- nb2mat(W.nb, style="B")
rownames(W) <- NULL #need this for test if matrix is symmetric
model.bym.geog <- S.CARbym(formula=form, data=det_bg_geog@data,
                           family="poisson", W=W_geog, burnin=20000, n.sample=150000, thin=10)
model.ler.geog <- S.CARleroux(formula=form, data=det_bg_geog@data,
                             family="poisson", W=W_geog, burnin=20000, n.sample=150000, thin=10)
sp.sglmm.fit.geog <- sparse.sglmm(formula = form,data=det_bg_geog@data, family = poisson, A = W,
                         verbose = TRUE) #tune = list(sigma.s = 0.02) #started at 10pm
#save_geog_bym <- model.bym.geog
#save_geog_ler <- model.ler.geog
#save(sp_sglmm_fit, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/sp_glmm_fit.Rdata")
#save(model.bym.geog, model.ler.geog, sp.sglmm.fit.geog, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/1_21_geog.Rdata")
load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/1_21_geog.Rdata")


####
#### Social AND Geographic modeling
####
rownames(bin_W) <- NULL #need this for test if matrix is symmetric
set.seed(7698) #
model.ler.soc <- S.CARleroux(formula=form, data=det_bg_geog@data,
                             family="poisson", W=bin_W, burnin=20000, n.sample=150000, thin=10)
model.bym.soc <- S.CARbym(formula=form, data=det_bg_geog@data,
                          family="poisson", W=bin_W, burnin=20000, n.sample=150000, thin=10)
sp.sglmm.fit.soc <- sparse.sglmm(formula = form,data=det_bg_geog@data, family = poisson, A = bin_W,
                                 verbose = TRUE) #tune = list(sigma.s = 0.02) #started around 11pm

#save(model.bym.soc, model.ler.soc, sp.sglmm.fit.soc, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/1_21_soc.Rdata")
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/1_21_soc.Rdata")
#save(model.bym.soc, model.ler.soc, sp.sglmm.fit.soc, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/1_22_soc.Rdata")



#save(model.bym.geog,model.ler.geog, model.bym.geog, sp.sglmm.fit.geog, model.ler.soc, model.bym.soc, sp.sglmm.fit.soc,  file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/full_mod_fit_final.Rdata")
#save(model.bym.geog, file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/geog_bym_fit.Rdata")
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/geog_bym_fit.Rdata")

####
####  Model Comparison
####
#rm(list=ls(all=TRUE)) #have to run this if we want the next line to work
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/full_mod_fit_final.Rdata")
#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/geog_bym_fit.Rdata")
#model.bym.geog <- save_geog_bym

#Provide MSPE
geog_bym <- mean((model.bym.geog$fitted.values-det_bg_geog@data$crime_freq)^2)
geog_ler <- mean((model.ler.geog$fitted.values-det_bg_geog@data$crime_freq)^2)
geog_sglmm <- mean((sp.sglmm.fit.geog$fitted.values-det_bg_geog@data$crime_freq)^2)
soc_bym <- mean((model.bym.soc$fitted.values-det_bg_geog@data$crime_freq)^2)
soc_ler <- mean((model.ler.soc$fitted.values-det_bg_geog@data$crime_freq)^2)
soc_sglmm <- mean((sp.sglmm.fit.soc$fitted.values-det_bg_geog@data$crime_freq)^2)

mspe_vec <- c(geog_bym, geog_ler, geog_sglmm, soc_bym, soc_ler, soc_sglmm)
View(cbind(sp.sglmm.fit.geog$fitted.values-det_bg_geog@data$crime_freq, sp.sglmm.fit.geog$fitted.values,det_bg_geog@data$crime_freq))

#Plot of residual vs actual for sparse SGLMM


model.bym.geog$modelfit
model.ler.geog$modelfit 

#summary(sp.sglmm.fit.geog)
sp.sglmm.fit.geog$dic
sp.sglmm.fit.geog$pD

model.bym.soc$modelfit
model.ler.soc$modelfit

#summary(sp.sglmm.fit.soc)
sp.sglmm.fit.soc$dic
sp.sglmm.fit.soc$pD


#now I would like to plot the fitted values
#need to get data in the right format

#   function for plot


plot_crime <- function(){
  sp_f <- fortify(det_bg_geog)
  sp_f <- left_join(sp_f, det_bg_geog@data[,c(13,21)])
  #need to sub in Detroit Map 
  fit_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = crime_freq)) + coord_equal() +
    labs(fill = "Number of Crimes")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                                  fill = NA, col = "black") +
    ggtitle("Number of Domestic Violence Crimes per block group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")+
    theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))
  return(fit_by_bg)
}

plot_fit <- function(spat_mod, title){
  fit_values <- spat_mod$fitted.values
  det_bg_geog@data$fit_val <- fit_values
  sp_f <- fortify(det_bg_geog)
  sp_f <- left_join(sp_f, det_bg_geog@data[,c(13,22)])
  fit_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = fit_val)) + coord_equal() +
    labs(fill = "Fitted Values")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
    ggtitle(paste(title))+ scale_fill_gradient(low = "lightblue", high = "navyblue")+
    theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))
  return(fit_by_bg)
}


#   plotting all of the fitted values
p1 <- plot_fit(model.bym.geog, title = "Fitted Values for Geographic BYM")
p2 <- plot_fit(model.ler.geog, title = "Fitted Values for Geographic Leroux")
p3 <- plot_fit(sp.sglmm.fit.geog, title = "Fitted Values for Geographic Sparse SGLMM")
p4 <- plot_fit(model.bym.soc, title = "Fitted Values for Geog/Social BYM")
p5 <- plot_fit(model.ler.soc, title = "Fitted Values for Geog/Social Leroux")
p6 <- plot_fit(sp.sglmm.fit.soc, title = "Fitted Values for Geog/Social Sparse SGLMM")

#plot the actual crime data, using any model
p7 <- plot_crime() #1500x1500

#load(file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/fit_val_images.Rdata")
#save(p2,p3,p4,p5,p6,p7,  file = "C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/final/fit_val_images.Rdata")

#dimensions: 2500x2500
grid.arrange(p1, p4, p2, p5, p3, p6, ncol=2)
p7


###
### Comparison
###
#load("C:/Users/ckell/Desktop/Google Drive/Box Sync/claire_murali_sesa_group/crime/bdss_igert_project/data/working/geog_carbayes.Rdata")
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

summary(sp.sglmm.fit.soc)
# Estimate      Lower      Upper      MCSE
# (Intercept)    1.859e+00  1.540e+00  2.151e+00 4.040e-03
# median_income -2.563e-05 -2.812e-05 -2.289e-05 4.306e-08
# upemp_rate     2.425e-01 -3.647e-02  5.127e-01 1.759e-03
# total_pop     -1.720e-06 -7.189e-05  6.751e-05 4.672e-07
# perc_male     -8.085e-01 -1.152e+00 -4.666e-01 1.777e-03
# med_age        8.395e-04 -2.326e-03  3.980e-03 1.635e-05
# herf_index    -5.341e-01 -7.157e-01 -3.523e-01 1.710e-03

model.ler.soc$summary.results[,1:3]
# Median    2.5%   97.5%
#   (Intercept)   -0.9985 -1.7473 -0.2400
# median_income  0.0000  0.0000  0.0000
# upemp_rate     0.5071 -0.1058  1.1125
# total_pop      0.0004  0.0002  0.0005
# perc_male     -0.5588 -1.3682  0.2495
# med_age        0.0067 -0.0012  0.0145
# herf_index     0.0765 -0.4074  0.6355

#plot the actual values, with the Detroit Map
plot_fit <- function(spat_mod){
  fit_values <- spat_mod$fitted.values
  det_bg_geog@data$fit_val <- fit_values
  sp_f <- fortify(det_bg_geog)
  sp_f <- left_join(sp_f, det_bg_geog@data[,c(13,22)])
  fit_by_bg <- DetroitMap2 + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = fit_val)) + coord_equal() +
    labs(fill = "Fitted Values")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
    ggtitle("Fitted Values for Social BYM")+ scale_fill_gradient(low = "lightblue", high = "navyblue")+
    theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))
  return(fit_by_bg)
}


