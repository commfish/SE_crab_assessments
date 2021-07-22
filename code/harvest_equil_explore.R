# Time series autocorrelation and equilibrium harvest rate

# katie.palof@alaska.gov, 7-18-21

# refer to e-mail from Lowell Fair looking into autocorrelation in the equilibrium harvest rate analysis performed previously.


# load -----
library(tidyverse)
library(car)
library(astsa)
library(nlme)
library(forecast)
library(mgcv)
library(itsadug)

# data --
# just juneau currently
biomassH <- read.csv("./data/rkc/biomass_harvest.csv") 

# clean up ----
head(biomassH)

biomassH %>% 
  filter(Location == 'Juneau') %>% 
  mutate(HR = harvest/mature.biomass, 
         pop_change = (lead(mature.biomass) - (mature.biomass))/ (mature.biomass)) %>% 
  filter(Year < 2021) -> biomassH2

# linear regression -----
fitA = lm(pop_change~HR, data = biomassH2)
summary(fitA)

par(mfrow = c(2,2))
plot(fitA)
acf2(residuals(fitA))

#plot(fitA$residuals)
ggplot(biomassH2, aes(HR, pop_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Linear regression of mature pop change and harvest rate")
ggsave(paste0('./figures/rkc/2021/hr/linear_regression.png'), 
              dpi = 400, width = 6, height = 4.5)

biomassH2$predlm = predict(fitA)

ggplot(biomassH2, aes(HR, pop_change)) +
  geom_point() +
  geom_line(aes(y=predlm), size = 1)

### nlme package ------
# https://stats.stackexchange.com/questions/6469/simple-linear-model-with-autocorrelated-errors-in-r
fitC <- gls(pop_change~HR, data = biomassH2, corr = corAR1(form=~1)) #corr=corAR1(0.5, form=~1))
fitC
summary(fitC)
par(mfrow = c(2,2))
plot(fitC)
plot.gls(fitC)

test_data = data.frame(HR = c(0.02, 0.04, 0.06, 0.07, 0.10, 0.17))
?predict.gls
predict(fitC, test_data)

#ggplot(predict(fitC, test_data))

fitE <- gls(pop_change~HR, data = biomassH2, corr = corAR1(form=~Year)) #corr=corAR1(0.5, form=~1))
fitE
summary(fitE)

intervals(fitE) # phi is significantly different cfrom 0 therefore has to be acknowledged.

temp <- predict(fitE, test_data)
predict2 <- data.frame(test_data, temp)
biomassH2$predlm2 = predict(fitE)

ggplot(biomassH2, aes(HR, pop_change)) +
  geom_point() +
  #geom_line(aes(y=predlm), size = 1) +
  geom_smooth(method = "lm", color = "black") +
  geom_line(aes(y=predlm2), size = 1, color = "red") +
  geom_point(data = predict2, aes(HR, temp), color = "blue", size = 2.5) +
  ggtitle("AR1 model (red)")
ggsave(paste0('./figures/rkc/2021/hr/AR1_model.png'), 
       dpi = 800, width = 8, height = 6.5)

# linear regression with Year -----
fit_yr = lm(pop_change~HR +Year, data = biomassH2)
summary(fit_yr)

par(mfrow = c(2,2))
plot(fit_yr)
acf2(residuals(fit_yr))

#plot(fitA$residuals)
biomassH2$predlm_yr = predict(fit_yr)

ggplot(biomassH2, aes(Year, pop_change)) +
  geom_point() +
  geom_line(aes(y=predlm_yr), size = 1, color = "red", linetype = "dotted") 
  #geom_smooth(methods = "lm", formula = y~x)

ggplot(biomassH2, aes(HR, pop_change)) +
  geom_point() +
  geom_line(aes(y=predlm_yr), size = 1, color = "red", linetype = "dotted") +
  geom_smooth(method = "lm")

test_data2 <- data.frame(test_data, Year = rep(2021, 6))
predict(fit_yr, test_data2)

predict_yr <- data.frame(test_data2, pop_change = (predict(fit_yr, test_data2)))


ggplot(biomassH2, aes(HR, pop_change)) +
  geom_point() +
  geom_line(aes(y=predlm_yr), size = 1, color = "red", linetype = "dotted") +
  geom_smooth(method = "lm", color = "black") +
  geom_point(data = predict_yr, aes(HR, pop_change), color = "red", size = 2.5) +
  ggtitle("Linear regression with Year as a co-variate (red dashed, red dot predicted for 2021")
ggsave(paste0('./figures/rkc/2021/hr/year_linear_reg.png'), 
       dpi = 800, width = 8, height = 6.5)

#plot with year as x 
ggplot(biomassH2, aes(Year, pop_change)) +
  geom_point() +
  geom_line(aes(y=predlm_yr), size = 1, color = "red", linetype = "dotted") +
  geom_smooth(method = "lm", color = "black") +
  geom_point(data = predict_yr, aes(Year, pop_change), color = "red", size = 2.5) +
  ggtitle("X variable of Year in multi-variate linear regression")
ggsave(paste0('./figures/rkc/2021/hr/year_linear_reg2.png'), 
       dpi = 800, width = 8, height = 6.5)


### removing 2020 data ------
fit_19 <- gls(pop_change~HR, data = biomassH2[1:41, ], corr = corAR1(form=~Year)) #corr=corAR1(0.5, form=~1))
fit_19
summary(fit_19)

intervals(fit_19) # phi is significantly different cfrom 0 therefore has to be acknowledged.

temp2 <- predict(fit_19, test_data)
predict_19 <- data.frame(test_data, temp2)
#biomassH2$predlm2 = predict(fitE)

ggplot(biomassH2, aes(HR, pop_change)) +
  geom_point() +
  geom_line(aes(y=predlm), size = 1) +
  geom_line(aes(y=predlm2), size = 1, color = "red") +
  geom_point(data = predict_19, aes(HR, temp), color = "blue", size = 2.5)


# !!! ignore exploratory work below this line -----------------------------


# correlation in error terms?? https://rpubs.com/fractalbass/615815 ------
x1 = seq(1,length(fitA$residuals))
y1 = fitA$residuals
p = ggplot() + 
  geom_line(aes(x = x1, y = y1), color = "red") +
  geom_hline(yintercept=0, linetype="dashed", color='blue') +
  xlab("Sequence (year)") + ylab("Residuals")
print(p)

# durbin-watson test --
durbinWatsonTest(fitA)
# reject null hypothesis that there is no correlation among residuals

### correct for AR1 process https://online.stat.psu.edu/stat510/lesson/8/8.1
x = head(biomassH2$HR, -1)
y = head(biomassH2$pop_change, -1)
plot.ts(x,y, xy.lines =F, xy.labels = F)

fitAR = lm(y~x)
summary(fitAR)

acf2(residuals(fitAR))

ar1res = sarima (residuals(fitAR), 1,0,0, no.constant=T) #AR(1)
ar1res2 = sarima (residuals(fit_yr), 1,0,0, no.constant=T) #AR(1)

xl = ts.intersect(x, lag(x,-1)) # Create matrix with x and lag 1 x as elements
xnew=xl[,1] - 0.73*xl[,2] # Create x variable for adjustment regression
yl = ts.intersect(y,lag(y,-1)) # Create matrix with y and lag 1 y as elements
ynew=yl[,1]-0.73*yl[,2] # Create y variable for adjustment regression



### another example ----
## https://rstudio-pubs-static.s3.amazonaws.com/345790_3c1459661736433382863ed19c30ea55.html
head(biomassH2)

biomassH2 %>% 
  mutate(diff_pop = pop_change - lag(pop_change)) %>% 
  select(Year, mature.biomass, HR, pop_change, diff_pop) -> data2

plot(pop_change ~ Year, data = data2)
plot(diff_pop ~ Year, data = data2)

fitD <- lm(pop_change~HR, data = data2)
acf2(residuals(fitD))


train_series = data2$pop_change[1:31]
test_series = data2$pop_change[32:42]

AutoArimaModel = auto.arima(train_series)
AutoArimaModel

AutoArimaModel2 = auto.arima(data2$pop_change)
AutoArimaModel2

## https://cran.r-project.org/web/packages/itsadug/vignettes/acf.html ----
m1 <- bam(pop_change~HR, data = data2)
summary(m1)
# default ACF function:
acf(resid(m1), main="acf(resid(m1))")
# resid_gam:
#acf(resid_gam(m1), main="acf(resid_gam(m1))")
# acf_resid:
acf_resid(m1, main="acf_resid(m1)")

r1 <- start_value_rho(m1, plot = TRUE)
acf(resid(m1), plot=FALSE)$acf[2]
# [1] 0.7321483

# run model with AR included 
m1AR1 <- bam(pop_change~HR, data = data2, rho=r1, AR.start = )
par(mfrow=c(1,2), cex=1.1)
acf(resid(m1))
acf(resid(m1AR1))
# by default these plots are the same

# uncorrected vs. corrected residuals
par(mfrow=c(1,2), cex=1.1)
acf(resid(m1))
acf(resid(m1AR1))
# this isn't working to produce the corrected model

### https://nwfsc-timeseries.github.io/MARSS-Manual/linear-regression-with-ar1-errors.html
fitA = lm(pop_change~HR, data = biomassH2)
summary(fitA)

fitAR2 <- Arima(biomassH2$pop_change, xreg = biomassH2$HR, order = c(1,1,0))
summary(fitAR2)

fitAR_y <- auto.arima(biomassH2$pop_change) 
# ARIMA(1,0,0)
fitAR3 <- auto.arima(biomassH2$pop_change, xreg = biomassH2$HR) 
summary(fitAR3) # ARIMA(0,1,0)
