# Assignment 2
# Stefan Rohrbacher

library(ISwR)
library(tidyverse)
library(ggplot2)

#############################
############ RMR ############
#############################
View(rmr)
head(rmr, 4)
#colnames(df_bl)<-c("team", "marketvalue", "goals", "points")

# 1. plot metabolic rate versus body weight
ggplot(rmr, aes(x=metabolic.rate,y=body.weight))+
  geom_point()

plot(metabolic.rate~body.weight,data=rmr)
# looking at the scatter plot, there seems to be a linear correlation


# 2. Fit a linear regression model to the relation

# I start with a visual inspection of the model
ggplot(rmr, aes(x=metabolic.rate,y=body.weight))+
  geom_point()+
  geom_smooth(method='lm', formula= y ~ x)

# then I create the model and save it in a variable
lm.metaw<-lm(data=rmr, metabolic.rate~body.weight)
summary(lm.metaw)

# the model summary returns a p-value of 7.025e-09 which is < 0.05 so the model makes sense
# the body.weight coefficient shows a positive correlation between metabolic.rate and
# body.weight, which can be written as:
# metabolic.rate=811.2267+7.0595*body.weight

# 3. According to the fitted model, what is the predicted metabolic rate
#    for a body weight of 70 kg? 
#    Give a a 95% confidence interval for the slope of the line.

# manual prediction
lm.metaw$coefficients[1]+lm.metaw$coefficients[2]*70

# prediction via R
predict(lm.metaw, newdata = data.frame(body.weight=70))
# print fitted values
fitted(lm.metaw)
# print the errors
residuals(lm.metaw)
plot(lm.metaw)

# calculate confidence interval
conf<-confint(lm.metaw, "body.weight", level = 0.95)
print(conf)

ggplot(rmr,aes(x=metabolic.rate,y=body.weight))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  geom_segment(aes(x=metabolic.rate,y=body.weight,
                   xend=metabolic.rate,yend=fitted(lm.metaw)))


#############################
############ JUUL ###########
#############################
View(juul)
head(juul, 4)
plot(igf1~age, data=juul)

# 1. fit a linear regression model
#    for the square root of the IGF-I concentration versus age 
#    to the group of subjects over 25 years old
# first i filter the dataset to only contain subjects over 25 years
juul25<-filter(juul,age>25)
plot(sqrt(igf1)~age, data=juul25)

ggplot(juul25, aes(x=sqrt(igf1),y=age))+
  geom_point()+
  geom_smooth(method='lm', formula= y ~ x)

lm.juul<-lm(data=juul25, sqrt(igf1)~age)
summary(lm.juul)
# p-value is < 2.2e-16 which  is < 0.05 so the linar model makes sense


#############################
######### MALARIA ###########
#############################
View(malaria)
head(malaria, 4)
plot(ab~age, data=malaria)
plot(log(ab)~age, data=malaria)
# data does not seem to have a linear correlation

# 1. analyze the log-transformed antibody level versus age
# Make a plot of the relation. Do you notice anything peculiar?
ggplot(malaria, aes(y=log(ab),x=age))+
  geom_point()+
  geom_smooth(method='lm', formula= y ~ x)

lm.mal<-lm(data=malaria, log(ab)~age)
summary(lm.mal)
plot(lm.mal)
# p-value is 0.01025 so < 0.05 which means the model makes sense, but the correlation
# is not very reliable
# according to the Q-Q plot the errors follow a normal distribution
# also multiple x values have more than ony y value so the data can't be linear


#############################
###### DATA Generator #######
#############################
# One can generate simulated data from the two-dimensional normal distribution 
# with a correlation of $\rho$ by the following technique: 
#   (a) Generate X as a normal variate with mean 0 and standard deviation 1; 
#   (b) Generate Y with mean $\rho X$ and standard deviation $\sqrt{1-\rho^2}. 
#   Use this to create scatterplots and simulated data with a given correlation. 
#   Compute the Spearman and Kendall statistics for some of these data sets.

set.seed(1)
rho1<-0.3
x1<-rnorm(100, mean = 0, sd = 1)
y1<-rnorm(100, mean = rho1*x1, sd = sqrt(1-rho1^2))
random_data1<-data.frame(x1,y1)

rho2<-0.7
x2<-rnorm(100, mean = 0, sd = 1)
y2<-rnorm(100, mean = rho2*x2, sd = sqrt(1-rho2^2))
random_data2<-data.frame(x2,y2)

ggplot(data=random_data1, aes(y=y1,x=x1))+
  geom_point()+ # scatter plot
  ggtitle("Random Data Scatterplot rho=0.3")+ # caption
  xlab("x")+ # x title
  ylab("y") # y title

ggplot(data=random_data2, aes(y=y2,x=x2))+
  geom_point()+ # scatter plot
  ggtitle("Random Data Scatterplot rho=0.7")+ # caption
  xlab("x")+ # x title
  ylab("y") # y title

cor.test(random_data1$x1, random_data1$y1, method = "spearman")
cor.test(random_data1$x1, random_data1$y1, method = "kendall")

cor.test(random_data2$x2, random_data2$y2, method = "spearman")
cor.test(random_data2$x2, random_data2$y2, method = "kendall")