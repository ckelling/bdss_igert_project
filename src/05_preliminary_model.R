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

full_dat <- left_join(acs_dat, agg_dat, by = c(GEOID = "GEOID"))

#I would like to substitute 0 for all block groups where there are no crimes in this time interval
colnames(full_dat)[8] <- c("crime_freq")
full_dat[is.na(full_dat$crime_freq),8]<- 0


#subset the data to not include NA values for frequency


#now, I would like to fit a preliminary regression model, with crime count being my response
#first, I will fit my standard OLS regression
fit_lm <- lm(crime_freq~ ., data = full_dat[,-c(1)])
summary(fit_lm)
#check need for transformation
boxcox(fit_lm)

#transform the response
fit_transf <- lm(I(crime_freq^(1/4)) ~ ., data= full_dat[,-c(1)])
fit_step2 <- step(fit_transf)
summary(fit_step2)

boxcox(fit_step2)
bc=boxcox(fit1) #wants to do a log response
bc$x[which.max(bc$y)]


#I will test some assumptions of the model
res=fit1$resid
hist(res,breaks=15)
qqnorm(res)
abline(0, 1)
#qqline(res)
plot(fit1)
yhat=fit1$fitted.values
plot(yhat,res)
abline(h=0, col='red')
#crPlots(fit1)
AIC(fit1)

#now I will plot the residuals of the linear model
res <- fit_lm$residuals
obs_by_dist <- ggplot() + geom_polygon(data = sp_f, aes(long, lat, group = group, fill = freq)) + coord_equal() +
  labs(fill = "No. of \nCrimes")+ geom_polygon(data=sp_f,aes(long,lat, group = group), 
                                               fill = NA, col = "black") +
  ggtitle("Number of Crimes per Block Group")+ scale_fill_gradient(low = "lightblue", high = "navyblue")


#then, I will fit poisson regression

#lastly, I will fit negative binomial regression to test for overdispersion