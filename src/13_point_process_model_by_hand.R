###
### Claire Kelling
### Point Process Model
###
### Created 12/1/2017 to create a point process model for response time
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

#Need to refer back to original crime data to access response time
#load in subsetted data- only data that has valid values for Lat/Long
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/detroit_data.Rdata")
#   shape file
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/det_bg.Rdata")
#load("C:/Users/cek32/Desktop/OneDrive/Penn State/Research/bdss_igert_project/data/working/detroit_data.Rdata")
#load("C:/Users/cek32/Desktop/OneDrive/Penn State/Research/bdss_igert_project/data/working/det_bg.Rdata")
detroit_data$Longitude1 <- detroit_data$Longitude
detroit_data$Latitude1 <- detroit_data$Latitude

#I will convert the files to spatial points and polygons with the same projection
coordinates(detroit_data) <- ~Longitude1+Latitude1
#proj4string(detroit_data) <- proj4string(det_bg)
proj4string(detroit_data) <- CRS("+proj=longlat")
det_bg <- spTransform(det_bg, CRS("+proj=longlat"))

#remove variables that I am not interested in
length(which(is.na(detroit_data$`Total Response Time`)))
detroit_data <- detroit_data[-which(is.na(detroit_data$`Total Response Time`)),]

#want to keep only variables that I am interested in using as response, covariates, and location
detroit_data@data <- detroit_data@data[,c(1,5,13,17,23,24)]
#take only the complete cases
keep <- which(complete.cases(detroit_data@data))
#detroit_data <- detroit_data[keep,]

ploteqc <- function(spobj, z, breaks, ...){
  pal <- tim.colors(length(breaks)-1)
  fb <- classIntervals(z, n = length(pal), 
                       style = "fixed", fixedBreaks = breaks)
  col <- findColours(fb, pal)
  plot(spobj, col = col, ...)
  image.plot(legend.only = TRUE, zlim = range(breaks), col = pal)
}

## Plotting
#deleting the cases with negative time........ (no concerns on the large positive end)
detroit_data <- detroit_data[-which(detroit_data$`Total Response Time`<0),]
#removing one outlier that is not in detroit
detroit_data <- detroit_data[-which(detroit_data$Longitude < -83.5),] #479,100 left

#I'm going to randomly select 1,000 points to plot
plot_ind <- runif(n=3000, min=0, max=479100)
plot_dat <- detroit_data[plot_ind,]


#need to make grid over Wayne County, first need shapefile
mich_count <- counties(state = "MI")
w_count <- mich_count[which(mich_count@data$NAME=="Wayne"),]

#make a grid for wayne county
grid <- makegrid(w_count, cellsize = 0.01)
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(w_count)))
over_set <- over(grid, w_count)
keep <- which(!is.na(over_set$STATEFP))
grid <- makegrid(w_count, cellsize = 0.01)
grid <- grid[keep,]
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(w_count)))
plot(grid, pch = 1,main = "Grid Points for Wayne County")
plot(w_count, add = TRUE)
x <- grid@coords[,1]
y <- grid@coords[,2]
grid_data <- cbind(x,y)
colnames(grid_data) <- c('Longitude', 'Latitude')
grid_data <- as.data.frame(grid_data)
grid <- SpatialPointsDataFrame(grid, data = grid_data)

#I would like to predict over this grid, just using lat long
#I do not know any other variable at every location on the grid
## I would like to take a subset of the data for the analysis below
detroit_data <- plot_dat
#Fitting the preliminary linear model
linmod <- lm(`Total Response Time` ~ Longitude + Latitude, data=detroit_data@data)
summary(linmod)
#storing the residuals so I can plot them
length(linmod$residuals)
detroit_data$resid <- linmod$resid
#storing the coefficients for later
beta_lin <- linmod$coefficients
beta_lin

#plotting the residuals
range(detroit_data$resid)
breaks <- seq(range(detroit_data$resid)[1], range(detroit_data$resid)[2], by = 10)
ploteqc(detroit_data, detroit_data$resid, breaks, pch = 19, xlim = xlim, ylim = ylim)
plot(det_bg, add= TRUE)
title(main = "Residuals of Linear Model")

View(head(detroit_data@data))

##
# Variogram Estimation
##

##  Nonparametric estimation of the variogram
vg <- variogram(resid ~ 1, data = detroit_data)#, width=75)
print(vg)
plot(vg, xlab = "Distance", ylab = "Nonparametric Semi-variogram estimate", width=5)

##  Fitting the variogram parametrically
fitvg <- fit.variogram(vg, vgm("Wav"), fit.method = 1)
print(fitvg)
s2.hat <- fitvg$psill[2]
rho.hat <- fitvg$range[2]
tau2.hat <- fitvg$psill[1]

paste("So, my estimate for sigma^2 is", s2.hat, ", my estimate for rho is")
paste(rho.hat, ", and my estimate for tau is ", tau2.hat, ".")

plot(vg, fitvg, xlab = "Distance", 
     ylab = "Semi-variogram estimate", main = "Parametric and nonparametric \nSemi-variogram estimate, Sph")

##
# Problem 2c
##

# Use the rdist.earth function in fields to create a matrix of distances (in miles) 
# between pairs of locations in CAtemp.
#   There are some entries on the diagonal that are not zero, but this is also true
#   in the case in the documentation, so I won't worry too much about it!
d <- rdist.earth(coordinates(detroit_data), miles = TRUE)

# Create the covariance matrix, plugging in your estimates from the fitted variogram.
# add two matrices together
mat1 <- diag(tau2.hat, ncol = nrow(detroit_data), nrow=nrow(detroit_data))
#mat2 <- Matern(dist_mat, scale = s2.hat, range = rho.hat, smoothness = 0.5)
mat2 <- Exponential(d, phi=s2.hat, range=rho.hat)
# phi = Marginal variance, Exponential: phi* exp( -d/range)
cov <- mat1+mat2


# Invert the covariance matrix and store it for later reference. (takes a few seconds)
Sinv <- solve(cov)

# Create the X matrix. Hint: Use cbind.
n <- nrow(detroit_data)
m <- nrow(grid@coords)
X <- cbind(rep(1,n), detroit_data$Longitude, detroit_data$Latitude)

# Put all the pieces together to form $\hat{\beta}_{GLS}$

Y <- detroit_data$`Total Response Time`
X <- as.matrix(X)
first <- t(X)%*%Sinv%*%X
second <- t(X)%*%Sinv%*%Y
beta_gls <- solve(first)%*%second

# include estimates below
rownames(beta_gls) <- c("Intercept", "Long", "Lat")
t(beta_gls)

beta_lin

##
# Prediction
##

dcross <- rdist.earth(coordinates(detroit_data), coordinates(grid))
dpred <- rdist.earth(coordinates(grid))
Xpred <- cbind(rep(1,m), grid$Longitude, grid$Latitude)

#length(unique(d))+nrow(d)
8607000+nrow(d)
#Construct the covariance matrixes
Gamma <- exp(-d/rho.hat)
#Ginv <- solve(Gamma)
Ginv <- ginv(Gamma) #takes a couple minutes
g <- exp(-dcross/rho.hat)
Gpred <- exp(-dpred/rho.hat)

#Kriging equations: mean 
y_pred <- Xpred%*%beta_gls + t(g)%*%Ginv%*%(Y-X%*%beta_gls)
# Plot
range(y_pred)
breaks <- seq(-32, 300, by = 1)
ploteqc(grid, y_pred, breaks, pch = 19)
#plot(det_bg, add = TRUE)
title(main = "Predicted Response Time, Wav")

#Kriging equations: standard error
#as in CA temp notes
#v_pred <- s2.hat*(Gpred - t(g)%*%Ginv%*%g)

#as in lecture 4 notes
b <- t(Xpred) - t(X) %*% Ginv%*%g
v_pred <- s2.hat-diag(t(g)%*%Ginv%*%g+t(b)%*%solve(t(X)%*%Ginv%*%X)%*%b)
se_pred <- sqrt(v_pred)


range(se_pred)
breaks <- seq(range(se_pred)[1], range(se_pred)[2], by = 0.00001)
ploteqc(grid, se_pred, breaks, pch = 19)
#plot(det_bg, add = TRUE)
title(main = "Standard Error, Wav")

breaks <- seq(15.3, 15.42, by = 0.01)
ploteqc(grid, se_pred, breaks, pch = 19)
#plot(det_bg, add = TRUE)
title(main = "Standard Error with Observed Points")
points(detroit_data)

#create mean-squared error?
