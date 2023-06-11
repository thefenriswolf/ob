# Assignment 1
# Stefan Rohrbacher

library(ISwR)
library(tidyverse)
library(ggplot2)
library(leaps)
# variance inflation factor (vif)
library(car)
# diagnostics and model search
library(olsrr)
# gvlma does most important checks in linear model
library(gvlma)

#############################
########### react ###########
#############################
#    Do the values of the react data set
#    (notice that this is a single vector, not a data frame)
#    look reasonably normally distributed?
#    Does the mean differ significantly from zero according to a t test?
View(react)
hist(react) # data follows a gaussian curve
qqnorm(react)
qqline(react)
t.test(x=react, mu=0) 
# p-value 1.115e-13 < 0.05, which means the true mean does not differ significantly from 0


#############################
########### vitcap ##########
#############################
#  In the data set vitcap, use a t test to compare the vital capacity for the two groups.
#  Calculate a 99% confidence interval for the difference. 
#  The result of this comparision may be misleading. Why?
View(vitcap)
plot(vital.capacity~group, data = vitcap)

t_vc <- t.test(vital.capacity~group, data=vitcap)
# p-value is 0.008724 < 0.01, which means there is a statistically significant 
# correlation between group and vital capacity
t_vc.lm<-lm(vital.capacity~group, data=vitcap)
confint(t_vc.lm, level=0.99)


###########################################
########### non-parametric tests ##########
###########################################
# vitcap
?vitcap
qqnorm(vitcap$vital.capacity)
qqline(vitcap$vital.capacity)
?wilcox.test
wilcox.test(vital.capacity ~ group, data = vitcap)
# p-value: 0.01783, which tells us there is a significant difference between the groups

# react
wilcox.test(react, mu=0)
# p-value: 2.075*10^-13, so the mean differs from 0


# Perform graphical checks of the assumptions for a paired t test in the intake data set.
View(intake)
?intake
t.test(intake$pre, intake$post, paired = TRUE)
ggplot(data = intake, aes(x="kJ",y=pre))+
  geom_boxplot()+
  geom_point()

shapiro.test(intake$post) # p-value of 0.4787 which means the data is not normally distributed
plot(intake)
hist(intake$post)
hist(intake$pre)
# plotting the data can also confirm that it does not follow a normal distribution


# The function shapiro.test computes a test of normality based on the defree of linearity of the Q-Q plot.
# Apply it to the react data. Does it help to remove outliers?
qqnorm(react)
qqline(react)
shapiro.test(react)
# the qq-plot could be used to idenfitfy outliers, but the shapiro.test does not tell us
# where outliers could be found it just checks for normality


# The crossover trial in ashina can be analysed for a drug effect in a simply way (how?) 
# if you ignore the potential period effect. However, you can do better. 
# Hint: Consider the intra-individual differences; if there were only a period effect present, 
# how should the differences behave in the two groups? 
# Compare the results of the simple method and the improved method.
View(ashina)
?ashina
lapply(ashina, class)
ashina$grp<-as.factor(ashina$grp)
# 1: placebo first, 2: active first.

# split the dataset
apf<-ashina%>%
  subset(grp=="1")
aaf<-ashina%>%
  subset(grp=="2")

t.test(apf$vas.plac, aaf$vas.plac)

plot(data=ashina, vas.active~vas.plac, pch=grp)

ggplot(data=ashina, aes(x=vas.active, y=vas.plac, color=grp))+
  geom_point()

ggplot(data=apf, aes(y=grp))+
  geom_point(aes(x=vas.plac), color='red')+
  geom_point(aes(x=vas.active), color='blue')
# I don't get this example and I have no idea how to even start.
# from a medicine standpoint one would expect the VAS value to go down once treatment
# with the real medicine starts and increase or at least stagnate if a placebo is used
# this would prove that the medicine is better than a placebo


# Perform 10 one-sample t tests on simulated normally distributed data sets of 25 observations each. 
# Repeat the experiment, but instead simulate samples from a different distribution; 
# try the t distribution with 2 degrees of freedom and the exponential distribution 
# (in the latter case, test for the mean being equal to 1). 
# Can you find a way to automate this so that you can have a larger number of replications?
?rnorm
?t.test
for(j in 1:10){
  if(j==1){counter<-0}
  df_set1<-rnorm(25, mean=0, sd=1)
  # do a t-test
  t.result<-t.test(df_set1, mu=0, paired = FALSE)
  if(t.result$p.value<0.05){counter=counter+1}
}

for(j in 1:10){
  if(j==1){counter<-0}
  df_set1<-rnorm(25, mean=0.1, sd=3)
  # do a t-test
  t.result<-t.test(df_set1, mu=0.1, paired = FALSE)
  if(t.result$p.value<0.05){counter=counter+1}
}

?rexp
for(j in 1:10){
  if(j==1){counter<-0}
  df_set1<-rexp(25)
  # do a t-test
  t.result<-t.test(df_set1, mu=1, paired = FALSE)
  if(t.result$p.value<0.05){counter=counter+1}
}

