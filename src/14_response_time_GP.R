# to do:
# -discussion of model validation; compare model DIC with a baseline linear model
# -plot fitted covariance parameter distributions vs prior distributions
#   (check to see priors are ok; bad if prior is far outside range of posterior; or posterior is at the limit of the uniform prior)

# Guassian process for travel time
#  use data-driven priors based on empirical semivariogram
#  use a Gaussian predictive process with 200 knot locations that fill the space over Arlington county

# prepare data
library(dplyr)
library(lubridate)
library(geosphere)
library(geoR)
library(splines)
library(ggplot2)
library(spBayes)

options(scipen=999)

setwd("~/sdal/projects/arl/arlington911/analysis/josh/traveltimeGP/")

# read in the final data frame
fire_data <- read.csv("~/sdal/projects/limbo/ac911_public_safety/all_acfd.csv")

stationgis <- read.csv("~/sdal/projects/arl/arlington911/data/working/fire/FireStationGIS.csv")
stationgis$Name <- c(106,108,110,102,104,101,109,105,107,103)

fire_stations_arlington <- fire_data %>% filter(station %in% as.character(101:110)) # look at fire stations in arlington

# take only one row for each incidentapparatuskey
response_times <- fire_stations_arlington %>% group_by(incidentapparatuskey) %>% slice(1) %>% group_by()

# create time interval for dispatch, arrival, enroutescene
response_times <- response_times %>% filter(!is.na(arrivaldatetime),
                                            !is.na(dispatchdatetime),
                                            !is.na(enroutescenedatetime)) #filter "na" time
response_times <- response_times %>% filter(cadcalltype=="SF") # filter only structure fires

response_times$arrivaldatetime <- as.POSIXct( sapply( response_times$arrivaldatetime, function(x){ paste( substr(x,1,10), substr(x,12,19) ) } ) )
response_times$dispatchdatetime <- as.POSIXct( sapply( response_times$dispatchdatetime, function(x){ paste( substr(x,1,10), substr(x,12,19) ) } ) )
response_times$enroutescenedatetime <- as.POSIXct( sapply( response_times$enroutescenedatetime, function(x){ paste( substr(x,1,10), substr(x,12,19) ) } ) )

# turnout time = dispatch to enroute time
response_times$turnout <- as.numeric( difftime(time1 = response_times$enroutescenedatetime, 
                                               time2 = response_times$dispatchdatetime,
                                               units = "mins") )

# travel time = enroutescene (left station) to arrival time
response_times$travel <- as.numeric( difftime(time1 = response_times$arrivaldatetime, 
                                              time2 = response_times$enroutescenedatetime,
                                              units = "mins") )

sum(response_times$turnout > 5,na.rm=T)/nrow(response_times) # <1% of turnout times are over 5 minutes
sum(response_times$turnout < 0,na.rm=T) # 1

sum(response_times$travel > 60,na.rm=T)/nrow(response_times) # <.4% of travel times over 1 hour
sum(response_times$travel < 0,na.rm=T) # 18 are negative

# remove outliers; turnout times negative or longer than 5 minutes (<1% of data)
# travel times negative or longer than 1 hour (<.4% of data)

# restrict year from 2009 to 2016
response_times$year_dis <- year(response_times$dispatchdatetime) # year that incident took place
response_times2 <- response_times %>% filter(turnout > 0 &
                                               turnout <= 5 &
                                               travel > 0 &
                                               travel <= 60 &
                                               year_dis >= 2010 &
                                               year_dis <= 2015 ) # subset data to exclude outliers and incomplete years

response_times2$hour_dis <- factor( hour(response_times2$dispatchdatetime) ) # hour that incident took place
response_times2$month_dis <- factor( month(response_times2$dispatchdatetime) ) # month that incident took place
response_times2$year_dis <- factor( response_times2$year_dis)

# compute distance from incident to station
# add station longitude, latitude
stationgis <- dplyr::select(stationgis,station=Name,stationlat=Latitude,stationlong=Longitude)
stationgis$station <- as.character(stationgis$station)
response_times2 <- left_join(response_times2,stationgis,by="station")

# compute distance
response_times2$distance <- distHaversine(p1=matrix(c(response_times2$latitude, response_times2$longitude), ncol = 2),
                                          p2=matrix(c(response_times2$stationlat, response_times2$stationlong), ncol = 2))
response_times2$miles <- (response_times2$distance)/(1609.344)

response_times2$apparatustype <- factor(response_times2$apparatustype)
response_times2$cadcalltype <- factor(response_times2$cadcalltype)
response_times2$year_dis <- factor(response_times2$year_dis)

# include only the first response to each incident
# include multiple apparatus if their arrival time was the same
length(unique(response_times$incidentkey)) # 2791
response_times2b <- response_times2 %>% group_by(incidentkey) %>%
  filter(arrivaldatetime==min(arrivaldatetime)) %>% group_by()

response_times3 <- response_times2b %>% dplyr::select(travel, turnout, miles, year_dis, hour_dis, month_dis,
                                                      apparatustype, cadcalltype, station, stationlat, stationlong, latitude, longitude)
response_times3$hour_frac <- hour(response_times2b$dispatchdatetime) + minute(response_times2b$dispatchdatetime)/60 + second(response_times2b$dispatchdatetime)/60^2
response_times3$month_frac <- 12*(yday(response_times2b$dispatchdatetime)/365.25)
response_times3$year_frac <- year(response_times2b$dispatchdatetime) + yday(response_times2b$dispatchdatetime)/365.25

# exclude incidents with missing lat/longs
response_times3 <- response_times3 %>% filter(!is.na(longitude),!is.na(latitude))

# among first responders, what fraction are >10min?
sum(response_times3$travel > 10)/nrow(response_times3) # only 1% of data; ok to treat as outliers for response
response_times3 <- response_times3 %>% filter(travel <= 10)

#fyear <- as.factor(response_times3$year_dis)
#fmonth <- as.factor(response_times3$month_dis)
fapparatus <- as.factor(response_times3$apparatustype)
fstation <- as.factor(response_times3$station)

plot(density(response_times3$travel),ylim=c(0,0.8))
lines(density(response_times3$turnout),col=2)

plot(density(response_times3$travel^(1/2))) # ~normal transformation

# ----------------------------------------------------------------
# set up knots
# ----------------------------------------------------------------

# choose knot locations by taking a grid and throwing out points not in Arlington
# try 20x20; throw away knots outside of Arlington (< 200)
domainSP <- readShapePoly("~/sdal/projects/arl/arlington911/data/original/gis/Arlington_County_Polygon/County_Polygon.shp")
pts <- fortify(domainSP)

nknots <- 20
x <- seq( min(pts$long), max(pts$long), len=nknots )
y <- seq( min(pts$lat), max(pts$lat), len=nknots )
z <- expand.grid(x,y)
knots <- as.matrix( z[!is.na( over(SpatialPoints(z),domainSP)$OBJECTID ),] )

# ----------------------------------------------------------------
# fit Gaussian process
# ----------------------------------------------------------------

# semivariogram to estimate parameters (see moth code)
variog.out <- variog(coords=cbind(response_times3$longitude,response_times3$latitude),
       data=(response_times3$travel)^(1/2)
)
plot(variog.out) # use to estimate priors for tausq, sigmasq, phi
# initial values: sigmasq = .1, tausq=.2, phi=.1
# max distance ~ 0.1

# specify priors, starting, tuning for acceptance
p=37 # number of parameters
priors.1 <- list("beta.Norm"=list(rep(0,p), diag(1000,p)),
                 "phi.Unif"=c(2/0.1, 2/0.01), "sigma.sq.IG"=c(2, .1),
                 "tau.sq.IG"=c(2, .2))
starting <- list("phi"=2/0.07, "sigma.sq"=.1, "tau.sq"=.2)
tuning <- list("phi"=1, "sigma.sq"=0.01, "tau.sq"=0.02)
# adaptive mcmc algorithm (#samples = n.batch * batch.length)
amcmc <- list("n.batch"=50,
              "batch.length"=100,
              "accept.rate"=0.3)

# fit the GP using spLM
system.time( spReg.out <- spLM(data=response_times3,
                  #formula=I(travel^(1/2)) ~ miles + fyear + fmonth + fapparatus + fstation + bs(hour_frac,degree=3,df=6),
                  formula=I(travel^(1/2)) ~ miles + fapparatus + fstation + bs(hour_frac,degree=3,df=6) + bs(month_frac,degree=3,df=6) + bs(year_frac,degree=3,df=6),
                  coords=cbind(response_times3$longitude,response_times3$latitude),
                  knots=knots,
                  cov.model="exponential",
                  amcmc=amcmc,
                  verbose=TRUE,
                  priors=priors.1, starting=starting, tuning=tuning
))

save.image("spfit_firstresponse.RData")

# ---------------------------------------------------------------------------------------
# get plots of:
#   predicted surface for travel time vs distance to station (try all stations at once)
#   effects of regression coefficients
#   diagnostics of fit
# explain model and put plots on slides
n.samples <- 5000
burn.in <- 0.25*n.samples

##recover beta and spatial random effects
m.1 <- spRecover(spReg.out, start=burn.in, verbose=FALSE)

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
# define x, y as a range over Arlington county
# predict z (use spPredict)
# throw out z not in Arlington county

# read in Arlington county shapefile
domainSP <- readShapePoly("~/mounts/lightfoot/sdal/projects/arl/arlington911/data/original/gis/Arlington_County_Polygon/County_Polygon.shp")
pts <- fortify(domainSP)

# sample plot: random noise
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
# prediction with 1600 points: (takes ~9 minutes for this 40x40 plot)
out2 <- summary(mcmc(t(out$p.y.predictive.samples)))$quantiles[,c(3,1,5)]
out2[is.na( over(SpatialPoints(z),domainSP)$OBJECTID ),] <- NA

save.image("spfit_firstresponse3.RData")

# --------------------------------------
# Plotting / analysis
# --------------------------------------

setwd("~/sdal/projects/arl/arlington911/analysis/josh/traveltimeGP/")

load("spfit_firstresponse3.RData")

library(dplyr)
library(lubridate)
library(geosphere)
library(geoR)
library(splines)
library(ggplot2)
library(spBayes)
library(fields)
library(rasterVis)
library(ggmap)
library(maptools)

# convert travel time to seconds
# out2[,1] <- out2[,1]*60
plot(density(out2[,1]^2,na.rm=T),type="l",xlim=c(0,7),ylim=c(0,2))
lines(density(out2[,2]^2,na.rm=T),type="l",xlim=c(0,7),lty=2)
lines(density(out2[,3]^2,na.rm=T),type="l",xlim=c(0,7),lty=2)


# median fitted surface for travel time
#image(x, y, matrix(out2[,1]^2,nrow=npts), axes=FALSE, col=tim.colors(30), asp=1,xlab="",ylab="")

# smoothed median fitted surface using raster(), disaggregate()
img.out <- as.image(Z=60*as.numeric(out2[,1]^2),x=expand.grid(x,y),nx=40,ny=40)
raster.out <- raster(img.out)
img2.out <- disaggregate(raster.out, method='bilinear',fact=4)

# plot it over map of Arlington (use ggmap)

# convert raster to polygon
rtp <- rasterToPolygons(img2.out)
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- fortify(rtp, data = rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

ac_map <- get_map(location = c(long = -77.10100, lat =  38.87863),zoom = 12, color = 'bw', source = "google")

# add fireboxes
fireboxes <- readShapePoly("~/sdal/projects/arl/arlington911/data/working/fire/031716/Fireboxes/Fire_Boxes_and_Hydrants.shp",
                           proj4string=CRS(proj4string(rtp)))
fbFort <- fortify(fireboxes)

# try not to 'color outside the lines'
test <- over(rtp,fireboxes)
badids <- rtp$id[is.na(test[,1])]
rtpFortMer <- rtpFortMer[-which(rtpFortMer$id %in% badids),]


p <- ggmap(ac_map) +
  geom_polygon(data = fbFort, 
               aes(x = long, y = lat, group = group), 
               alpha = 0.5,
               size = 0.2,
               colour = "black",
               fill = NA) +
  geom_polygon(data = rtpFortMer, 
               aes(x = long, y = lat, group = group, fill = layer), 
               alpha = 0.8,
               size = 0) +
  scale_fill_gradientn(colours = tim.colors(255)) +
  geom_point(data=stationgis,aes(x=stationlong,y=stationlat),color="black",size=3,show.legend = FALSE) +
  geom_label(data = stationgis,
             aes(x = stationlong, y = stationlat, label = station),
             hjust=0, vjust=0,
             color = "black",
             size = 4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16))
p$labels$fill <-  "Travel Time (sec)"

arlington_plot <- p


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

# *remake year, month plots as splines
# effect for year
#year_effect <- cbind(y2010=intercept,
#                     y2011=intercept+beta.samples[,3],
#                     y2012=intercept+beta.samples[,4],
#                     y2013=intercept+beta.samples[,5],
#                     y2014=intercept+beta.samples[,6],
#                     y2015=intercept+beta.samples[,7]
#)
#year_effect_out <- apply(60*year_effect^2,2,function(x){quantile(x,probs=c(0.05,0.5,0.95))})


# effect for station
station_effect <- cbind(s101=intercept,
                        s102=intercept+beta.samples[,11],
                        s103=intercept+beta.samples[,12],
                        s104=intercept+beta.samples[,13],
                        s105=intercept+beta.samples[,14],
                        s106=intercept+beta.samples[,15],
                        s107=intercept+beta.samples[,16],
                        s108=intercept+beta.samples[,17],
                        s109=intercept+beta.samples[,18],
                        s110=intercept+beta.samples[,19]
)
station_effect_out <- apply(60*station_effect^2,2,function(x){quantile(x,probs=c(0.05,0.5,0.95))})

# effect for apparatus
#View( table(response_times3$apparatustype) )
# apparatus w/ many types: 00, 11, 12, 71, 76, 92
apparatus_effect <- cbind(a00=intercept+beta.samples[,3],
                          a11=intercept+beta.samples[,5],
                          a12=intercept+beta.samples[,5],
                          a71=intercept+beta.samples[,8],
                          a76=intercept+beta.samples[,9],
                          a92=intercept+beta.samples[,10]
)
apparatus_effect_out <- apply(60*apparatus_effect^2,2,function(x){quantile(x,probs=c(0.05,0.5,0.95))})
# this one is significant; attach names to apparatus type and include this plot

# apparatus ggplot
apparatus_effect_out <- as.data.frame(t(apparatus_effect_out))
names(apparatus_effect_out) <- c("lower","med","upper")
# get names corresponding to 00,11,12,71,76,92 from NFIRS
apparatus_effect_out$name <- c("Other","Fire Engine","Fire Truck","Rescue Unit","ALS Unit","Chief officer car")
apparatus_effect_plot <-
  ggplot(data = apparatus_effect_out) +
  geom_errorbar(aes(x = name, ymin=lower, ymax=upper), color="orangered2") +
  geom_point(aes(x = name, y = med)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(x = "Apparatus", y = "Travel Time (sec)" )
apparatus_effect_plot
#png("apparatus_effect.png",height=400,width=600)
#apparatus_effect_plot
#dev.off()


# station ggplot
station_effect_out <- as.data.frame(t(station_effect_out))
names(station_effect_out) <- c("lower","med","upper")
station_effect_out$name <- c("101","102","103","104","105","106","107","108","109","110")
station_effect_plot <-
  ggplot(data = station_effect_out) +
  geom_errorbar(aes(x = name, ymin=lower, ymax=upper), color="orangered2") +
  geom_point(aes(x = name, y = med)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(x = "station", y = "Travel Time (sec)" )
station_effect_plot
#png("station_effect.png",height=400,width=600)
#station_effect_plot
#dev.off()


# year ggplot
#year_effect_out <- as.data.frame(t(year_effect_out))
#names(year_effect_out) <- c("lower","med","upper")
## get names corresponding to 00,11,12,71,76,92 from NFIRS
#year_effect_out$name <- c("2010","2011","2012","2013","2014","2015")
#year_effect_plot <-
#  ggplot(data = year_effect_out) +
#  geom_errorbar(aes(x = name, ymin=lower, ymax=upper), color="orangered2") +
#  geom_point(aes(x = name, y = med)) +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
#        axis.text.y = element_text(size = 10)) +
#  labs(x = "year", y = "Travel Time (sec)" )
#year_effect_plot
##png("year_effect.png",height=400,width=600)
##year_effect_plot
##dev.off()

# effect for month
#month_effect <- cbind(January=intercept,
#                      February=intercept+beta.samples[,8],
#                      March=intercept+beta.samples[,9],
#                      April=intercept+beta.samples[,10],
#                      May=intercept+beta.samples[,11],
#                      June=intercept+beta.samples[,12],
#                      July=intercept+beta.samples[,13],
#                      August=intercept+beta.samples[,14],
#                      September=intercept+beta.samples[,15],
#                      October=intercept+beta.samples[,16],
#                      November=intercept+beta.samples[,17],
#                      December=intercept+beta.samples[,18]
#)
#month_effect_out <- apply(60*month_effect^2,2,function(x){quantile(x,probs=c(0.05,0.5,0.95))})
#month_effect_out <- as.data.frame(t(month_effect_out))
#names(month_effect_out) <- c("lower","med","upper")
#month_effect_out$name <- colnames(month_effect)
#month_effect_out$name <- factor(month_effect_out$name,level=colnames(month_effect))
#
#month_effect_plot <-
#  ggplot(data = month_effect_out) +
#  geom_errorbar(aes(x = name, ymin=lower, ymax=upper), color="orangered2") +
#  geom_point(aes(x = name, y = med)) +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
#        axis.text.y = element_text(size = 10)) +
#  labs(x = "Month", y = "Travel Time (sec)" )
#month_effect_plot

# spline effect for hour of day
spline_mat <- beta.samples[,20:25]
xs <- seq(0,24,length=100)
spline_bs <- as.matrix(bs(xs,degree=3,df=6))
spline_effect <- spline_mat %*% t(spline_bs) + intercept
spline_effect_out <- apply(60*spline_effect^2,2,function(x){quantile(x,probs=c(0.05,0.5,0.95))})
hour_effect_out <- as.data.frame(t(spline_effect_out))
names(hour_effect_out) <- c("lower","med","upper")
hour_effect_out$xs <- xs
hour_effect_plot <-
  ggplot(data = hour_effect_out) +
  geom_line(aes(x=xs,y=med),color="black") +
  geom_line(aes(x=xs,y=lower),linetype="dotted",color="orangered2") +
  geom_line(aes(x=xs,y=upper),linetype="dotted",color="orangered2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(x = "Hour", y = "Travel Time (sec)" )+
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24))
hour_effect_plot

# spline effect for month of year
spline_mat <- beta.samples[,26:31]
xs <- seq(0,12,length=100)
spline_bs <- as.matrix(bs(xs,degree=3,df=6))
spline_effect <- spline_mat %*% t(spline_bs) + intercept
spline_effect_out <- apply(60*spline_effect^2,2,function(x){quantile(x,probs=c(0.05,0.5,0.95))})
month_effect_out <- as.data.frame(t(spline_effect_out))
names(month_effect_out) <- c("lower","med","upper")
month_effect_out$xs <- xs
month_effect_plot <-
  ggplot(data = month_effect_out) +
  geom_line(aes(x=xs,y=med),color="black") +
  geom_line(aes(x=xs,y=lower),linetype="dotted",color="orangered2") +
  geom_line(aes(x=xs,y=upper),linetype="dotted",color="orangered2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(x = "Month", y = "Travel Time (sec)" )+
  scale_x_continuous(breaks=1:12)
month_effect_plot


# spline effect for year
spline_mat <- beta.samples[,32:37]
xs <- seq(2010,2016,length=100)
spline_bs <- as.matrix(bs(xs,degree=3,df=6))
spline_effect <- spline_mat %*% t(spline_bs) + intercept
spline_effect_out <- apply(60*spline_effect^2,2,function(x){quantile(x,probs=c(0.05,0.5,0.95))})
year_effect_out <- as.data.frame(t(spline_effect_out))
names(year_effect_out) <- c("lower","med","upper")
year_effect_out$xs <- xs
year_effect_plot <-
  ggplot(data = year_effect_out) +
  geom_line(aes(x=xs,y=med),color="black") +
  geom_line(aes(x=xs,y=lower),linetype="dotted",color="orangered2") +
  geom_line(aes(x=xs,y=upper),linetype="dotted",color="orangered2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10)) +
  labs(x = "Year", y = "Travel Time (sec)" )+
  scale_x_continuous(breaks=2010:2016)
year_effect_plot


save(arlington_plot,apparatus_effect_plot,station_effect_plot,year_effect_plot,hour_effect_plot,month_effect_plot,file="GP_plots_firstresponse_spline.RData")

load("GP_plots_firstresponse_spline.RData")

arlington_plot
apparatus_effect_plot
station_effect_plot
year_effect_plot
hour_effect_plot
month_effect_plot


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
lm.ref <- lm(data=response_times3,
             formula=I(travel^(1/2)) ~ miles + fapparatus + fstation + bs(hour_frac,degree=3,df=6) +
               bs(month_frac,degree=3,df=6) + bs(year_frac,degree=3,df=6))
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


