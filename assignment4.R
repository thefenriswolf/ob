# Assignment 4
# Stefan Rohrbacher

library(tidyverse)
library(ggplot2)
library(readr)
library(reshape2)
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


# Perform a two-way analysis of variance on the tb.dilute data. 
# Modify the model to have a dose effect that is linear in log dose. 
# Compute a confidence interval for the slope. An alternative approach could be 
# to calculate a slope for each animal and perform a test based on them. 
# Compute a confidence interval for the mean slope, and compare it with the preceding result.

?tb.dilute
View(tb.dilute)
tb<-tb.dilute
tb$logdose<-as.integer(tb$logdose)
tb$logdose<-exp(tb$logdose)

ggplot(data = tb, aes(x=logdose,y=reaction, color=animal))+
  geom_boxplot()+
  geom_point()

res.aov2 <- aov(reaction ~ logdose + animal, data = tb)
summary(res.aov2)
# the p-values: 1.11e-06 for logdose and 0.00131 for the animal group suggest a significant correlation
# for predicting the response
?confint
tb.lm <- lm(data = tb, tb$reaction ~ tb$logdose+tb$animal)
summary(tb.lm)
confint(tb.lm)


# For the juul data, fit a model for igf1 with interaction between age, sex and Tanner stage for those under 25 years old. 
# Explain the interpretation of the model. Hint: A plot of the fitted values against age should be helpful. 
# Use diagnostic plots to evaluate possible transformations of the dependent variable: untransformed, log, or square root

juul25<-filter(juul, age<25)
juul.lm<-lm(data = juul25, igf1 ~ age+sex+tanner)
summary(juul.lm)
# age 0.000964, sex 0.033211 and tanner < 2e-16 are all significant values in explaining the model
# with sex being the least significant
# the model itself with a p-value of < 2.2e-16 is highly significant
plot(juul.lm)
# residuals vs fitted: the line is not straight and the dots are not equally distributed but bunched in groups
# q-q plot: the errors are quite far from the line which indicates that the data was not normally distributed
# scale location: looks fine as the line is pretty much horizontal but the errors are again bunched in groups
# residuals vs leverage: all points are within the cooks distance

juul.lm2<-lm(data = juul25, log(igf1) ~ sqrt(age)+sex+tanner)
summary(juul.lm2)
plot(juul.lm2)
# after log transforming and squaring the model the problems are reduced

gvlma::gvlma(juul.lm2)

