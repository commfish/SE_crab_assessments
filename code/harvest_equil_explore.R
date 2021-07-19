# Time series autocorrelation and equilibrium harvest rate

# katie.palof@alaska.gov, 7-18-21

# refer to e-mail from Lowell Fair looking into autocorrelation in the equilibrium harvest rate analysis performed previously.


# load --
library(tidyverse)
library(car)
library(astsa)

# data --
# just juneau currently
biomassH <- read.csv("./data/rkc/biomass_harvest.csv") 

# clean up ----
head(biomassH)

biomassH %>% 
  filter(Location == 'Juneau') %>% 
  mutate(HR = harvest/mature.biomass, 
         pop_change = (lead(mature.biomass) - (mature.biomass))/ (mature.biomass)) -> biomassH2

# linear regression -----
fitA = lm(pop_change~HR, data = biomassH2)
summary(fitA)

par(mfrow = c(2,2))
plot(fitA)
#plot(fitA$residuals)
#ggplot(biomassH2, aes(HR, pop_change)) +
#  geom_point() +
#  geom_abline(fitA)


# correlation in error terms?? https://rpubs.com/fractalbass/615815
x1 = seq(1,length(fitA$residuals))
y1 = fitA$residuals
p = ggplot() + 
  geom_line(aes(x = x1, y = y1), color = "red") +
  geom_hline(yintercept=0, linetype="dashed", color='blue')
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
xl = ts.intersect(x, lag(x,-1)) # Create matrix with x and lag 1 x as elements
xnew=xl[,1] - 0.73*xl[,2] # Create x variable for adjustment regression
yl = ts.intersect(y,lag(y,-1)) # Create matrix with y and lag 1 y as elements
ynew=yl[,1]-0.73*yl[,2] # Create y variable for adjustment regression

