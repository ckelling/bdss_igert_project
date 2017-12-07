###
### Claire Kelling
### Point Referenced Model
###
### Created 12/6/2017 to create a more complex point referenced GP model, not by hand
### 
### 

#Libraries
library(sp)
library(gstat)
library(fields)
library(classInt)
library(maps)
library(acs)
library(tigris)
library(spdep)
library(ggplot2)
library(dplyr)
library(ade4) 
library(MASS)
library(dplyr)
library(lubridate)
library(geosphere)
library(geoR)
library(splines)
library(ggplot2)
library(spBayes)
library(rasterVis)
library(ggmap)
library(maptools)

#load in subsetted data- only data that has valid values for Lat/Long
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/detroit_data.Rdata")

#clean crime data:
#remove variables that I am not interested in
length(which(is.na(detroit_data$`Total Response Time`)))
detroit_data <- detroit_data[-which(is.na(detroit_data$`Total Response Time`)),]

#want to keep only variables that I am interested in using as response, covariates, and location
detroit_data <- detroit_data[,c(1,5,13,17,23,24)]
#take only the complete cases
keep <- which(complete.cases(detroit_data))
detroit_data <- detroit_data[keep,]

## Plotting
#deleting the cases with negative time........ (no concerns on the large positive end)
detroit_data <- detroit_data[-which(detroit_data$`Total Response Time`<0),]
#removing one outlier that is not in detroit
detroit_data <- detroit_data[-which(detroit_data$Longitude < -83.5),] #479,100 left

#recoding officer initiated into 0-1 from "No"/"Yes"
detroit_data$`Officer Initiated`[which(detroit_data$`Officer Initiated` == "Yes")] <- 1
detroit_data$`Officer Initiated`[which(detroit_data$`Officer Initiated` == "No")] <- 0
detroit_data$`Officer Initiated` <- as.numeric(detroit_data$`Officer Initiated`)

# ----------------------------------------------------------------
# set up knots
# ----------------------------------------------------------------

# choose knot locations by taking a grid and throwing out points not in Detroit
# try 20x20; throw away knots outside of Detroit (< 200)

#need to make grid over Wayne County, first need shapefile
mich_count <- counties(state = "MI")
w_count <- mich_count[which(mich_count@data$NAME=="Wayne"),]
plot(w_count)

pts <- fortify(w_count)

nknots <- 20
x <- seq( min(pts$long), max(pts$long), len=nknots )
y <- seq( min(pts$lat), max(pts$lat), len=nknots )
z <- expand.grid(x,y)
z2 <- SpatialPoints(z)
proj4string(z2) <- proj4string(w_count)
test <- over(z2, w_count)
knots <- as.matrix( z[!is.na( over(z2,w_count)$STATEFP),] )

# ----------------------------------------------------------------
# fit Gaussian process
# ----------------------------------------------------------------
sub_ind <- runif(n=15000, min=0, max=479100)
detroit_data2 <- detroit_data[sub_ind,]

# semivariogram to estimate parameters (see moth code)
variog.out <- variog(coords=cbind(detroit_data2$Longitude,detroit_data2$Latitude),
       data=detroit_data2$`Total Response Time`
)
plot(variog.out)
print(variog.out)# use to estimate priors for tausq, sigmasq, phi
# initial values: sigmasq = .1, tausq=.2, phi=.1
# max distance ~ 0.1

#Fitting the preliminary linear model
linmod <- lm(`Total Response Time` ~Priority + `Officer Initiated`+ Longitude + Latitude, data=detroit_data2)
summary(linmod)
#storing the residuals so I can plot them
length(linmod$residuals)
detroit_data2$resid <- linmod$resid
#storing the coefficients for later
beta_lin <- linmod$coefficients
beta_lin
detroit_data2 <- as.data.frame(detroit_data2)
##  Nonparametric estimation of the variogram
detroit_data3 <- detroit_data2
detroit_data3$Longitude1 <- detroit_data3$Longitude
detroit_data3$Latitude1 <- detroit_data3$Latitude

#I will convert the files to spatial points and polygons with the same projection
coordinates(detroit_data3) <- ~Longitude1+Latitude1
proj4string(detroit_data3) <- CRS("+proj=longlat")
vg <- variogram(resid ~ 1, data = detroit_data3)#, width=75)
#started at 4:48, done 4:49
print(vg)
plot(vg, xlab = "Distance", ylab = "Nonparametric Semi-variogram estimate", width=5)

##  Fitting the variogram parametrically
fitvg <- fit.variogram(vg, vgm("Wav"), fit.method = 1)
print(fitvg)
s2.hat <- fitvg$psill[2]
rho.hat <- fitvg$range[2]
tau2.hat <- fitvg$psill[1]

# specify priors, starting, tuning for acceptance
p=3 # number of parameters
# use to estimate priors for tausq, sigmasq, phi
# initial values: sigmasq = .1, tausq=.2, phi=.1
priors.1 <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                 "phi.Unif"=c(2/0.1, 2/0.01), "sigma.sq.IG"=c(2, .1),
                 "tau.sq.IG"=c(2, .2))
starting <- list("phi"=2/0.07, "sigma.sq"=.1, "tau.sq"=.2)
tuning <- list("phi"=1, "sigma.sq"=0.01, "tau.sq"=0.02)
# adaptive mcmc algorithm (#samples = n.batch * batch.length)
amcmc <- list("n.batch"=50, #need 50 when done
              "batch.length"=100,
              "accept.rate"=0.3)

# fit the GP using spLM
system.time( spReg.out <- spLM(data=detroit_data2,
                  #formula=I(travel^(1/2)) ~ miles + fyear + fmonth + fapparatus + fstation + bs(hour_frac,degree=3,df=6),
                  formula=I(`Total Response Time`) ~ Priority + `Officer Initiated`,
                  coords=cbind(detroit_data2$Longitude,detroit_data2$Latitude),
                  knots=knots,
                  cov.model="exponential",
                  amcmc=amcmc,
                  verbose=TRUE,
                  priors=priors.1, starting=starting, tuning=tuning
))
# started second time at 5:35ish
#save.image("spfit_firstresponse.RData")
#started around 4:15, done 4:25 (200 data points)
#started at 4:52, done (15,000 data points)
#spReg.out2 <- spReg.out
save(spReg.out2, file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/spReg_out2.Rdata")

# ---------------------------------------------------------------------------------------
# get plots of:
#   predicted surface for travel time vs distance to station (try all stations at once)
#   effects of regression coefficients
#   diagnostics of fit
# explain model and put plots on slides
n.samples <- 5000
burn.in <- 0.25*n.samples

##recover beta and spatial random effects
m.1 <- spRecover(spReg.out, start=burn.in, verbose=TRUE) #started 4:55, done 4:45

# samples for posterior betas
m.1.beta.summary <- summary(mcmc(m.1$p.beta.recover.samples))$quantiles[,c(3,1,5)]
# median, 2.5, 97.5 percentile of spatial random effects
m.1.w.summary <- summary(mcmc(t(m.1$p.w.recover.samples)))$quantiles[,c(3,1,5)]
#w <- response_times_test %>% select(latitude,longitude)
#w$median <- as.numeric( m.1.w.summary[,1] )

save.image("spfit_firstresponse2.RData")

# --------------------------------------
# Heatmap of predicted travel times
# --------------------------------------
# define x, y as a range over Wayne county
# predict z (use spPredict)
# throw out z not in Wayne county

npts <- 40
x <- seq( min(pts$long), max(pts$long), len=npts )
y <- seq( min(pts$lat), max(pts$lat), len=npts )
z <- expand.grid(x,y)

# use spPredict to get values at locations z
pred.covars <- spReg.out$X[1,] # can change this to change distance, reference group
for(i in 2:nrow(z)){
  pred.covars <- rbind(pred.covars,spReg.out$X[1,])
}
out <- spPredict(spReg.out, pred.coords=expand.grid(x,y), pred.covars=pred.covars, n.report=100)
#started 4:58, done 5:27
# prediction with 1600 points: (takes ~9 minutes for this 40x40 plot)

out2 <- summary(mcmc(t(out$p.y.predictive.samples)))$quantiles[,c(3,1,5)] #[,c(3,1,5)]
out2[is.na( over(z2,w_count)$STATEFP ),] <- NA

#save.image("C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/spfit_firstdet_crime.RData")
#save.image("C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/spfit_firstdet_crime2_new_spBayes.RData")

# --------------------------------------
# Plotting / analysis
# --------------------------------------

# median fitted surface for travel time
#image(x, y, matrix(out2[,1],nrow=npts), axes=FALSE, col=tim.colors(30), asp=1,xlab="",ylab="")
test2 <- out2[,1]
test <- matrix(out2[,1],nrow=npts)
test <- test[2:19, 2:10]
test <- as.numeric(test)


# smoothed median fitted surface using raster(), disaggregate()
img.out <- as.image(Z=as.numeric(out2[,1]),x=expand.grid(x,y),nx=40,ny=40)
raster.out <- raster(img.out)
img2.out <- disaggregate(raster.out, method='bilinear',fact=6)

# plot it over map of Detroit (use qmap)
# convert raster to polygon
rtp <- rasterToPolygons(img2.out)
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- fortify(rtp, data = rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

#ac_map <- get_map(location = c(long = -77.10100, lat =  38.87863),zoom = 12, color = 'bw', source = "google")
DetroitMap <- qmap('detroit', zoom = 10, legend = 'topleft')

View(head(rtpFortMer))
p <- ggplot() +
  geom_polygon(data = rtpFortMer, 
               aes(x = long, y = lat, group = group, fill = layer), 
               alpha = 0.8,
               size = 0) +
  scale_fill_gradientn(colours = tim.colors(255)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16))
p$labels$fill <-  "Total Response Time (min)"

test <- ggplot()+
  geom_polygon(data = test3,
               aes(x = long, y = lat, group = group, fill = layer), 
               alpha = 0.8,
               size = 0) +
  scale_fill_gradientn(colours = tim.colors(255)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=16),
        legend.title=element_text(size=12))+labs(title = "Predicted Median")
test$labels$fill <-  "Total Response \n Time (min)"

test3 <- rtpFortMer[which(rtpFortMer$long < -83.15),]
test3 <- rtpFortMer[which(rtpFortMer$lat < 42.352 & rtpFortMer$lat > 42.246 & rtpFortMer$long < -83.15),]

test3 <- rtpFortMer[which(rtpFortMer$long > -83.15),]
test3 <- rtpFortMer[which(rtpFortMer$lat > 42.352 & rtpFortMer$long < -83.15),]
range(rtpFortMer$lat)

length(unique(rtpFortMer$layer))
nrow(rtpFortMer)/8

# effect plots
# note: also need to add a spatial effect (control for a point in space)
dim( m.1$p.w.recover.samples )
dim( beta.samples )

# posterior samples for all beta
beta.samples <- m.1$p.beta.recover.samples
w.samples <- m.1$p.w.recover.samples

dim(w.samples) # each row has MCMC samples for a spatial location (row=location, col=MCMC iteration)
w.mean <- apply(w.samples,2,mean)

intercept <- mean(beta.samples[,1]) + w.mean

colnames(beta.samples)


coeff_effect <- cbind(s101=beta.samples[,1],
                        s102=beta.samples[,2],
                        s103=beta.samples[,3]
)
station_effect_out <- apply(coeff_effect,2,function(x){quantile(x,probs=c(0.05,0.5,0.95))})

station_effect_out <- as.data.frame(t(station_effect_out))
names(station_effect_out) <- c("lower","med","upper")
station_effect_out$name <- c("Intercept", "Priority", "Officer Initiated")
station_effect_plot <-
  ggplot(data = station_effect_out) +
  geom_errorbar(aes(x = name, ymin=lower, ymax=upper), color="orangered2") +
  geom_point(aes(x = name, y = med)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(x = "Variable", y = "Estimate" )
station_effect_plot


hist(detroit_data2$`Total Response Time`)
mean(detroit_data2$`Total Response Time`)
# ----------------------------------------------------------------
# validating the model
# ----------------------------------------------------------------

# D = -2 log(p(y|theta))   [-2 * loglikelihood]
# theta_bar = posterior mean
# D_bar = D at the posterior mean
# p_D = D_bar - D(theta_bar)   [avg of D minus D at posterior mean]
# DIC = p_D + D_bar

# compute DIC of the spatial model
spDiag(m.1)
#$DIC
#value
#bar.D       -2236.2767
#D.bar.Omega -2341.0905
#pD            104.8138
#DIC         -2131.4629
#$GP
#value
#G 232.2461
#P 262.9020
#D 495.1481
#$GRS
#[1] 2331.541


# fit a baseline linear model with non-informative (reference) priors
# lm.obj = object returned by lm, n.samples = #posterior samples
lm.ref <- lm(data=detroit_data2,
             formula=`Total Response Time` ~Priority + `Officer Initiated`+ Longitude + Latitude)
bayes.lm.ref <- bayesLMRef(lm.ref, n.samples=5000)

summary(bayes.lm.ref)
summary(bayes.lm.ref$p.beta.tauSq.samples)

# compute DIC of the baseline linear model
spDiag(bayes.lm.ref)
#$DIC
#value
#bar.D       -1767.88357
#D.bar.Omega -1805.83803
#pD             37.95446
#DIC         -1729.92911
#$GP
#value
#G 300.8704
#P 312.5853
#D 613.4557
#$GRS
#[1] 1805.033

# conclusion: spatial model has lower DIC
# accounting for spatial dependence in travel times improves our model fit


