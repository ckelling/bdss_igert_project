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
library(gridExtra)
library(xtable)

# Load data: 
#   crime data
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/final/full_crime_bg.Rdata")
#   shape file
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/det_bg.Rdata")
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
resid.model <- residuals(model)

#test for spatial dependence
#null hypothesis of no spatial autocorrelation (alternative of positive spatial autocorrelation)
#also computes Moran's I statistic 
#if p-value < 0.05, we conclude there is positve spatial autocorrelation
W.nb <- poly2nb(det_bg, row.names = rownames(det_bg@data))

##############
### Creating separate adjacency matrices from nb
##############
W_geog <- nb2mat(W.nb, style="B")
W_soc <- nb2mat(proxim_nb, style="B")

#############
###  Combining adjacency matrices
#############
#addition
add_W <- W_geog + W_soc
#binary
bin_W <- ifelse(add_W == 2, 1, add_W)

##############
### Converting back to nb object
##############
#addition                             CAN'T DO IT FOR NON-BINARY MATRIX
add_nb <- neig2nb(neig(mat01 = add_W))
#binary
bin_nb <- neig2nb(neig(mat01 = bin_W))

#############
### Moran's I Test
#############

#addition
W.add.list <- nb2listw(add_nb, style="B")
moran.mc(x=resid.model, listw=W.add.list, nsim=1000)

#binary
W.bin.list <- nb2listw(bin_nb, style="B")
moran.mc(x=resid.model, listw=W.bin.list, nsim=1000)

##############
#Leroux Model
##############
#addition
system.time(
model.ler.add <- S.CARleroux(formula=form, data=det_bg@data,
                              family="poisson", W=add_W, burnin=20000, n.sample=120000, thin=10)
#binary
model.ler.bin <- S.CARleroux(formula=form, data=det_bg@data,
                              family="poisson", W=bin_W, burnin=20000, n.sample=120000, thin=10)

##############
# BYM Model
##############
#addition
model.bym.add <- S.CARbym(formula=form, data=det_bg@data,
                           family="poisson", W=bin_W, burnin=20000, n.sample=120000, thin=10)
#binary
model.bym.bin <- S.CARbym(formula=form, data=det_bg@data,
                           family="poisson", W=add_W, burnin=20000, n.sample=120000, thin=10)

#save the models for later reference, takes over an hour to run
save(model.bym.add, model.bym.bin, model.ler.add, model.ler.bin, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/comb_models.Rdata")

##############
# BYM Model
##############
#  add bym
model.bym.add$modelfit
#  bin bym
model.bym.bin$modelfit
#  add ler
model.ler.add$modelfit
#  bin ler
model.ler.bin$modelfit

#Plot of comparative results:
View(head(det_bg@data))

#   function for plot
plot_fit <- function(spat_mod){
  fit_values <- spat_mod$fitted.values
  det_bg@data$fit_val <- fit_values
  sp_f <- fortify(det_bg)
  sp_f <- left_join(sp_f, det_bg@data[,c(13,22)])
  fit_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = fit_val)) + coord_equal() +
    labs(fill = "Fitted Values")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
    ggtitle("Fitted Values for Add Leroux")+ scale_fill_gradient(low = "lightblue", high = "navyblue")+
    theme(text = element_text(size=30))+theme(axis.text.x=element_text(size=20))
  return(fit_by_bg)
}

#   plotting all of the fitted values, need to change title for each graph
p1 <- plot_fit(model.bym.add)
p2 <- plot_fit(model.bym.bin)
p3 <- plot_fit(model.ler.add)
p4 <- plot_fit(model.ler.bin)

grid.arrange(p1, p2, p3, p4, ncol=2)
#save as 2000, 1300

#looking at mixing of MCMC, subsitute relevant paramter
samp_beta <- model.bym.geog$samples$beta
dim(samp_beta)
acf(samp_beta)

#analyzing values of coefficients
xtable(summarise.samples(model.bym.add$samples$beta, quantiles=c(0.5, 0.025, 0.975))$quantiles)
xtable(summarise.samples(model.bym.bin$samples$beta, quantiles=c(0.5, 0.025, 0.975))$quantiles)
xtable(summarise.samples(model.ler.add$samples$beta, quantiles=c(0.5, 0.025, 0.975))$quantiles)
xtable(summarise.samples(model.ler.bin$samples$beta, quantiles=c(0.5, 0.025, 0.975))$quantiles)

#looking at summary statistics of the two neighborhood matrices
summary(bin_nb)
