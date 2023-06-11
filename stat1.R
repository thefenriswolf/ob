install.packages(c("survival","survminer","leaps","car","olsrr","gvlma"),dependencies = TRUE)
if (!require("BiocManager", quietly = FALSE))
  install.packages("BiocManager")


# standard stuff
library(tidyverse)
library(ggplot2)
library(readr)
# medical dataset
library(ISwR)


## view a dataset from ISwR
# view(thuesen) # blood glucose data
## for large datasets only show head
head(thuesen, 4)

# plot data
ggplot(data=thuesen, aes(x=blood.glucose, y=short.velocity))+ # dataset and what data to plot where
  geom_point()+ # scatter plot
  geom_vline(xintercept = 13.5, colour='red')+ # x axis shift
  geom_hline(yintercept = 1.5, colour='green')+ # y axis shift
  ggtitle("Blood glucose and velocity")+ # caption
  xlab("glucose")+ # x title
  ylab("velocity") # y title

# same plot with regression line
ggplot(data=thuesen, aes(x=blood.glucose, y=short.velocity))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y~x) # lm= linear method, y~x= explain y using x

# create a linear regression model from data
# same principle as in the plot but stored as a variable
lm.velo<-lm(data=thuesen, formula = short.velocity~blood.glucose)

# nicely formatted summary
summary(lm.velo) # goodness of fit test = chi^2 test

# exam answer: i did a chi^2 test of the variables x and y and the p-value was 0.0470 which is less than 0.05 so the null-hypothesis can be rejected and the linear regression model makes sense

# intercept can mostly be ignored, coefficient of x is important
# in this case the model can be written as:
# short.velocity=intercept+blood.glucose.coefficient*blood.glucose
# short.velocity=1.09781+0.02196*blood.glucose

# manual prediction for blood.glucose=10
# r is starts indexing with 1 not 0
lm.velo$coefficients[1]+lm.velo$coefficients[2]*10

# predict some values
fitted(lm.velo)
# errors of the model
residuals(lm.velo)
#plot(lm.velo)

# prerequisites for linear regression test:
# 1. metric variables
# 2. goodness of fit test / chi^2 test was successful
# 3. visual inspection with =plot(lm.velo)=
#    3.1 residuals vs fitted: ideally horizontal red line
#        and equally distributed dots around the line
#        compares values of model vs errors
#        if errors have a pattern the pattern should be part of model instead of a error
#        R marks suspicious errors with a number
#    3.2 normal qq: errors should follow normal distribution
#        errors should follow line in plot
#    3.3 scale location: homogeneity of variance
#        variance is not depended on where a measurement is taken
#        bad if: small value -> small deviation, large value -> large deviation
#        good if: dots seemingly random, line mostly horizontal
#        if bad: check data, in extreme cases exclude obviously wrong data
#        but: any modification to done to the dataset must be documented and justified
#    3.4 residuals vs leverage: shows outliers and influence of datapoints on model
#        points outside cooks distance: outlier with strong influence

# for demonstration lets remove point 13 which is such a outlier with strong influence
# thuesen_removed<-thuesen[-c(13,20,24),] # how to remove multiple datapoints
thuesen_removed<-thuesen[-13,]
lm.velo_removed<-lm(data=thuesen_removed, formula = short.velocity~blood.glucose)
summary(lm.velo_removed) # p-value shoots up to 0.3055 which means the linear model
                         # no longer makes sense
#plot(lm.velo_removed)


# example no. 2
df_bl<-read.csv2("http://statmath.wu.ac.at/~hatz/Buecher/R/R-Begleitmaterial/Daten/bl2009.csv")
#df_bl %>% lapply(., iconv, to = "UTF-8")
colnames(df_bl)<-c("team", "marketvalue", "goals", "points")

ggplot(data=df_bl, aes(x=marketvalue, y=points))+
  geom_point()+
  #geom_text(aes(label=team), nudge_x = 0.35, nudge_y = 0.35, check_overlap = T)+ # does not work for unknown reasons
  geom_smooth(method = "lm", formula = y ~ x)

# create linear regression model from data
lm.bl.points<-lm(data=df_bl, points~marketvalue)
summary(lm.bl.points)
#plot(lm.bl.points)


# same thing with goals
ggplot(data=df_bl, aes(x=marketvalue, y=goals))+
  geom_point()+
  #geom_text(aes(label=team), nudge_x=0.35, nudge_y=0.35, check_overlap = T)+ # does not work for unknown reasons
  geom_smooth(method = "lm", formula = y ~ x)

# create linear regression model from data
lm.bl.goals<-lm(data=df_bl, goals~marketvalue)
summary(lm.bl.goals)
#plot(lm.bl.goals)


# instead of manually predicting values lets use a R function
predict(lm.bl.points) # predicts for existing data points
predict(lm.bl.points, newdata = tibble(marketvalue=5)) # predict for a new value
predict(lm.bl.points, newdata = data.frame(marketvalue=5)) # same but different library
predict(lm.bl.points, newdata = data.frame(marketvalue=c(5.65,6))) # predict multiple values

# sanity check: should return the model coefficient from =summary(lm.bl.points)=
predict(lm.bl.points, newdata = data.frame(marketvalue=6))-predict(lm.bl.points, newdata = data.frame(marketvalue=5))


# correlation check: redundant because we already have a trendline
# returns a value [-1,1]
# -1 perfect linear correlation with negative slope
# 1 perfect linear correlation with positive slope
# 0 no linear relationship

# 1. pearson assumes a normal distribution
cor.test(thuesen$blood.glucose, thuesen$short.velocity, method = "pearson")
cor(thuesen$blood.glucose, thuesen$short.velocity) # pearson is the default
# 2. 
cor(thuesen$blood.glucose, thuesen$short.velocity, use = "complete.obs")
#cor.test(thuesen$blood.glucose, thuesen$short.velocity, method = "spearman")
#cor.test(thuesen$blood.glucose, thuesen$short.velocity, method = "kendall")