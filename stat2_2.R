#install.packages("gvlma", dependencies = TRUE)
# standard stuff
library(tidyverse)
library(ggplot2)
library(readr)
# medical dataset
library(ISwR)
# find best linear model
library(leaps)
# variance inflation factor (vif)
library(car)
# diagnostics and model search
library(olsrr)
# gvlma does most important checks in linear model
library(gvlma)


# dataset
head(cystfibr)
plot(cystfibr)
# cystfibr has lots of variables
# lets find out if all these variables have an influence on one: "pemax"
ggplot(data=cystfibr, aes(x=age, y=height))+
  geom_point()

# create linear model
lm.pemax<-lm(data = cystfibr, pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)
summary(lm.pemax)
# returns a p-value of 0.03195 < 0.05 which is good but not a single variable is significant
# Pr(>|t|) < 0.05
# how can this be? R checks significance of variables by removing one at a time and
# checking if the model improves or gets worse

# lets let R check the variables an get a report
# nvmax: how many variables to use together maximum
# nvmax=8 because we used 9 above and got garbage
models<-regsubsets(pemax~., data=cystfibr, nvmax = 8)
summary(models)
# returns a list of variables ordered by significance
# fist line: model with one variable should use variable *
# second line: model with two variables should use variables * and *
# etc.
# but this still does not show us the best model

# one way to do this is the Bayesian information criterion BIC
res.sum<-summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2), # maximize adjusted R^2
  BIC = which.min(res.sum$bic)       # minimize BIC, which measures overfitting
)
# returns best prediction power with 4 variables 
# and least amount of overfitting with 3 variables
# so lets check both cases
# variables are take from the regsubsets results from above
model.r2<-lm(data = cystfibr, pemax~weight+bmp+fev1+rv)
summary(model.r2)
model.bic<-lm(data = cystfibr, pemax~weight+bmp+fev1)
summary(model.bic)

# in exam explain why variables are in model (quote p-value)
# and show what impact they have (positive or negative slope)

# also statistically compare them
anova(model.r2, model.bic) # anova can compare linear models if they use the same variables
# returns Pr(>F) > 0.05 which means there is no significant difference

# compare a very simple model with the complex ones
model.simple<-lm(data = cystfibr, pemax~fev1)
anova(model.bic, model.simple)
# returns a significant difference, which indicates model.bic is superior to model.simple

plot(model.bic) # always check if prerequisites for linear regression are satisfied
# explain sanity checks in exam

# there can be a relationship of independent variables
# this makes the model unstable
# a linear model is based on the fact that independent variables are not correlated
model.correlated<-lm(data = cystfibr, pemax~age+weight+height)
summary(model.correlated) # significant p-value but no significant variables

# variance inflation factor: colinearity
# shows which variables increase variance in the model
vif(model.correlated)
# result > 4: check variables
# result > 5: something is not right
# result > 8: there is a strong correlation between independent variables
# for comparison
vif(model.bic)

# this can also be done by plotting the variables
plot(cystfibr[, c(1,3,4)])
# shows that age, height and weight are correlated and thus should not be used

# doing the same using the olsrr package
ols_vif_tol(model.bic)
# vif should be < 4 
# tolerance should be close to 0 and tells us how much of the variance can not be explained by the other variables
ols_vif_tol(model.correlated)

# plot to detect nonlinearity and outliers
ols_plot_resid_fit_spread(model.bic)
ols_plot_resid_fit_spread(model.correlated)
# mean should always be on a larger scale than the residuals

ols_plot_diagnostics(model.bic)
ols_plot_diagnostics(model.correlated)

# another alternative gvlma: nice but missing some details, so meh
gvlma::gvlma(model.bic)
# output:
# global stat: do we have a linear model
# skewness & kurtosis: is there a normal distribution in the residuals
# link function: is a generalized model better or a linear model (T->linear; F->generalized)
# Heteroscedasticity: does the variance change over time or location (it should not)


# cars
cars<-read.csv2("http://statmath.wu.ac.at/~hatz/Buecher/R/R-Begleitmaterial/Daten/gebrauchtwagen.csv")
colnames(cars)<-c("price", "miles", "services", "garage", "colour")
head(cars) # check variable types
# check how R stores the data
# class returns the data type
# lapply is like map, takes data and a function and applies the function to every column
lapply(cars, class)
# unsurprisingly all are integers
unique(cars$colour) # like in numpy returns all unique values

# lets redefine garage and colour as categorical(factor) variables
cars$colour<-as.factor(cars$colour)
cars$garage<-as.factor(cars$garage)
# if R knows the correct data type, the linear model also comes out correct

lm.cars<-lm(data = cars, price~miles+services+garage+colour)
summary(lm.cars)
# factor variables are now represented as dummy variables
plot(lm.cars)
gvlma::gvlma(lm.cars)

anova(lm.cars)

# one can also manually remove variables
lm.cars.reduced<-lm(data = cars, price~miles+services+garage)
summary(lm.cars.reduced)
vif(lm.cars.reduced)

# compare the models
anova(lm.cars, lm.cars.reduced) # returns no significant result
# so there is no difference between the models
# but always prefer the least amount of variables

# another example
df.salary<-read.csv("https://daviddalpiaz.github.io/appliedstats/data/initech.csv")

ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()

lm.salary<-lm(data = df.salary, salary~years)
summary(lm.salary)

ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)

plot(lm.salary) # sanity check
# residuals vs fitted show a parabolic pattern which is not good

gvlma::gvlma(lm.salary)
# gvlma also tells us the linear model is garbage

# first approach would be to look at a log transformation of the data
# or look at a square transform eg. years^2
lm.salary.quadratic<-lm(data = df.salary, salary~I(years^2)) # I indicates a lambda formula
summary(lm.salary.quadratic) # smoothed residuals vs fitted
plot(lm.salary.quadratic)
gvlma::gvlma(lm.salary.quadratic)
ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()+
  geom_smooth(aes(x=years, y=salary), method = "lm", formula = y~I(x^2))
# new plot looks kinda quadratic, the last one was linear
# model is still linear even though the plot is quadratic

# alternative to I(): mutate the data
df.salary.mut<-df.salary%>%
  mutate(years.squared=years^2) # adds a new column

# second approach: log transform
lm.salary.log<-lm(data = df.salary, log(salary)~years)
ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()+
  geom_smooth(aes(x=years, y=salary), method = "glm", formula = y~x, method.args=list(family=gaussian(link='log')))
# returns a non-linear model with a log as a link function
# also called a general linear model
summary(lm.salary.log)


# another example
# Interaction effects: annoying people at stoplights by not going when its green
# and recoding reactions
aggression<-read.table("http://statmath.wu.ac.at/~hatz/Buecher/R/R-Begleitmaterial/Daten/aggression.dat")
colnames(aggression)<-c("car","sex","frequency","duration")
aggression<-aggression%>%
  mutate(sex=case_when(
    sex=="Frau"~ "female",
    sex=="Mann"~ "male",
    sex=="female"~ "female",
    sex=="male"~ "male",
    TRUE ~ "check missing"
  ))
View(aggression)
aggression2<-subset(aggression,frequency>0) # exclude non responders
ggplot(aggression2,aes(x=car,y=duration,fill=sex))+
  geom_boxplot()
# shows that men honk faster at the big car and women faster at the small car

interaction.plot(aggression2$sex, aggression2$car, 
                 aggression2$duration,ylab = "Average duration",xlab="Sex")
# plot interaction effects
a3 <-lm(data = aggression2, duration~car*sex) # equal to:
# a3 <-lm(data = aggression2, duration~car+sex+car:sex)
summary(a3) # p-value: 0.05178 > 0.05
# we have no independent metric variable so the model is not really useful
# so lets do a anova: use anova for groups >= 3, graph: boxplot
anova(a3)
# neither variable is significant, only the interaction of the two
plot(a3)

# analysis of covariance
View(hellung) # cell growth and glucose level
summary(hellung)
class(hellung$glucose) # returns int so we need to reassign
hellung$glucose<-factor(hellung$glucose, labels=c("yes", "no"))
summary(hellung) # now displays the number of yes and no

# scatterplot with different colors for yes and no
ggplot(hellung,aes(x=conc, y=diameter, color=glucose))+
  geom_point()+
  scale_x_continuous(trans = "log10")+ # log transformed to get a linear plot
  geom_smooth(method = "lm")
# the question now is: does glucose actually change the slope of the data?
# according to the plot, glucose does not change the slope, rather it provides 
# an upshift, so there is no real interaction between glucose and concentration
# so lets check this statement

# first split the dataset into glucose and no glucose
tethym.gluc<-hellung%>%
  subset(glucose=="yes")
tethym.nogluc<-hellung%>%
  subset(glucose=="no")
# next create 2 separate linear models from the datasets
lm.gluc<-lm(data = tethym.gluc, log10(diameter)~log10(conc))
lm.nogluc<-lm(data = tethym.nogluc, log10(diameter)~log10(conc))

summary(lm.gluc) # highly significant p-value, slope and intercept
summary(lm.nogluc) # highly significant p-value, slope and intercept
# we are still interested in the slope: do they have a different slope?
# slope glucose: -0.05320; std error: 0.00272
# slope no glucose: -0.059677; std error: 0.004125
# difference: 0.0065
# if two numbers are very similar, then a small difference might already significant
# std error between the 2: (0.00272^2+0.004125^2)^(1/2)=0.0049
# t=0.0065/0.0049=1.3 # = difference / std error
# 1.3 is not significant
# assuming a t distribution the p-value is:
# t-value+deg_of_freedom+difference
pt(1.3, 17+30, 0.0065) # returns: 0.8988945 > 0.05; highly insignificant
# so we conclude there is no significant difference between the lines

# now lets do the same thing but in the same model
var.test(lm.gluc, lm.nogluc) # check if we have the same variance in both

summary(lm(data=hellung, log10(diameter)~log10(conc)*glucose))
# significant model, but the interaction is not significant

# so a additive model is enough
summary(lm(data=hellung, log10(diameter)~log10(conc)+glucose))
# slopes are identical but there is a parallel shift

# but interaction is not obvious like in the boxplot above
# visual inspection gets very tricky if there are more than 2 variables

# order of operation can be important
anova(lm(data=hellung, log10(diameter)~log10(conc)*glucose)) # ofc this one is different

# but these two should be the same
anova(lm(data=hellung, log10(diameter)~log10(conc)+glucose))
anova(lm(data=hellung, log10(diameter)~glucose+log10(conc)))
# suddenly glucose has a different significance, but the residuals are identical
# overall the models are still the same
# this is because the order of operation


