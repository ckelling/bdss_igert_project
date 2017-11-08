###
### Claire Kelling
### Preliminary Model
###
### Created 11/01/17 for aggregation of crime points to areal units (census blocks)
### 

library(sp)
library(ggmap)
library(tigris)
library(dplyr)
library(MASS)

#load crime data and acs data
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/agg_crime_dat.Rdata")
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/acs_dat.Rdata")
load(file = "C:/Users/ckell/OneDrive/Penn State/Research/bdss_igert_project/data/working/det_bg.Rdata")

full_dat <- left_join(acs_dat, agg_dat, by = c(GEOID = "GEOID"))
dat <- left_join(acs_dat, agg_dat, by = c(GEOID = "GEOID"))


#I would like to substitute 0 for all block groups where there are no crimes in this time interval
colnames(full_dat)[8] <- c("crime_freq")
full_dat[is.na(full_dat$crime_freq),8]<- 0

#total population and crime frequency aren't that related
plot(full_dat$total_pop,full_dat$crime_freq)
plot(full_dat$race_not_white,full_dat$crime_freq)

full_dat <- full_dat[complete.cases(full_dat),] #0nly 1706 out of 1822 don't have any NA's

#now, I would like to fit a preliminary regression model, with crime count being my response
#first, I will fit my standard OLS regression
fit_lm <- lm(crime_freq~ ., data = full_dat[,-c(1)])
summary(fit_lm)

############## ANALYSIS THAT I MAY USE LATER, if I do not replace NA as 0 for crime frequency
# #check need for transformation
# boxcox(fit_lm)
# 
# #transform the response
# fit_transf <- lm(I(crime_freq^(1/4)) ~ ., data= full_dat[,-c(1)])
# fit_step2 <- step(fit_transf)
# summary(fit_step2)
# 
# boxcox(fit_step2)
# #bc=boxcox(fit1) #this is how to find the transformation
# #bc$x[which.max(bc$y)]

fit_step <- step(fit_lm)
summary(fit_step) #includes everything except unemployment rate

#I will test some assumptions of the model
res=fit_step$resid
hist(res,breaks=15) #DEFINITELY not normally distributed
qqnorm(res)
abline(0, 1)
#qqline(res)
plot(fit1)
yhat=fit_step$fitted.values
plot(yhat,res)
abline(h=0, col='red')
#crPlots(fit1)
AIC(fit_step)

#preparing data for poisson model
#full_dat[is.na(full_dat$crime_freq),8]<- 0 #substituting 0 for NA (no crimes reported)
#full_dat <- full_dat[complete.cases(full_dat),] #0nly 1706 out of 1822 don't have any NA's

#poisson model for count data
fit_pois <- glm(crime_freq~ ., data = full_dat[,-c(1)], family = quasipoisson(link = log))
summary(fit_pois)

#testing for model goodness of fit
with(fit_pois, cbind(res.deviance = deviance, df = df.residual,
                     p = pchisq(deviance, df.residual, lower.tail=FALSE)))  #not a great fit

#now I add residuals to rest of data file
res <- fit_pois$residuals
res_dat <- cbind(full_dat$GEOID, res)
colnames(res_dat)[1] <- "GEOID"
res_dat <- as.data.frame(res_dat)

full_dat2 <- left_join(dat, res_dat, by = c(GEOID = "GEOID"))

#need to get data in format to plot
sp_f <- fortify(det_bg)
det_bg$id <- row.names(det_bg)
det_bg@data <- left_join(det_bg@data, full_dat2, by = (GEOID = "GEOID"))
sp_f <- left_join(sp_f, det_bg@data)

class(sp_f$res)
sp_f$res <- as.numeric(as.character(sp_f$res))

res_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = res)) + coord_equal() +
  labs(fill = "Res")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                   fill = NA, col = "black") +
  ggtitle("Regression Residuals")+ scale_fill_gradient(low = "lightblue", high = "navyblue", guide = "colourbar")


#now I would like to do the same thing except for with the lm residuals instead of poisson
res <- fit_lm$residuals
res_dat <- cbind(full_dat$GEOID, res)
colnames(res_dat)[1] <- "GEOID"
res_dat <- as.data.frame(res_dat)
full_dat2 <- left_join(dat, res_dat, by = c(GEOID = "GEOID"))
det_bg@data <- left_join(det_bg@data, full_dat2, by = (GEOID = "GEOID"))
sp_f <- left_join(sp_f, det_bg@data)

class(sp_f$res)
sp_f$res <- as.numeric(as.character(sp_f$res))

res_by_bg <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = res)) + coord_equal() +
  labs(fill = "Res")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                   fill = NA, col = "black") +
  ggtitle("Linear Regression Residuals")+ scale_fill_gradient(low = "lightblue", high = "navyblue", guide = "colourbar")
