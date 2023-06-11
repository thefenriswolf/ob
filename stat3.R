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

# t-test and anova

# some data: calorie intake
daily_intake <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
# compute the mean
mean(daily_intake)
# compute median
median(daily_intake)
# compute standard deviation
sd(daily_intake)
# compute quantiles
quantile(daily_intake)

# convert to a data frame for easier handling
df_di<-as.data.frame(daily_intake)
# plot the data
ggplot(data = df_di, aes(x="intake",y=daily_intake))+
  geom_boxplot()+
  geom_point()
# geom_jitter(width=0.05) # if the points overlap too much
# data is called skewed if the median and the mean are far apart
hist(df_di$daily_intake)

# is there a difference to the recommended 7725 calories per day?
t.test(df_di$daily_intake, mu=7725)
# p-value 0.01814 < 0.05 which means there is a difference

# plot the data again but with our sample 7725 displayed
ggplot(data = df_di, aes(x="intake",y=daily_intake))+
  geom_boxplot()+
  geom_point()+
  geom_jitter(width=0.05)+
  geom_hline(yintercept = 7725, color = 'red', linetype='dashed')
  
# also check for normal distribution
# because a parametric test (t-test, anova) needs normal distributed data
qqnorm(daily_intake)
qqline(daily_intake)

# if the data is not normally distributed we need a non parametric test
# eg. wilcoxon, which does not assume any distribution in the data
wilcox.test(daily_intake, mu=7725) # p-value: 0.0293 < 0.05 so there is a difference

# two sample t-test
# one variable metric one variable categorical
attach(energy)
View(energy)
class(energy$stature) # already correct data type
#first always plot
ggplot(data = energy, aes(x=stature,y=expend))+
  geom_boxplot()+
  geom_point()

# lets test for a significant difference between the groups
#t.test(expend~stature)
t.test(expend~stature, var.equal=TRUE) # also tell R the variance is the same in both groups
# by default R assumes the variances differ and does a welch two sample t-test
# to find out if the variance differs lets do a test
var.test(expend~stature)
# p-value 0.6797 > 0.05 so the variances are equal and we DON NOT need the welch test

# also check for normality
qqnorm(expend)
qqline(expend)

# split the dataset 
df_di_lean<-energy%>%
  subset(stature=="lean")
df_di_obese<-energy%>%
  subset(stature=="obese")

qqnorm(df_di_lean$expend)
qqline(df_di_lean$expend)
qqnorm(df_di_obese$expend)
qqline(df_di_obese$expend)
# these return rather non-normally distributed data plot so lets use a non parametric test
wilcox.test(expend~stature)
detach(energy)

# ANOVA
?red.cell.folate # anesthesia data
View(red.cell.folate)
lapply(red.cell.folate, class) # already the right data type
ggplot(data = red.cell.folate, aes(x=ventilation,y=folate))+
  geom_boxplot()+
  geom_point()

# null-hypothesis of the anova: two groups are different
# analysis of variance
anova_rcf<-aov(data = red.cell.folate, folate~ventilation)
summary(anova_rcf)
# 0.0436 < 0.05: null hypothesis can be rejected and alternative hypothesis is correct
# meaning at least two groups are different
# but how? Post-hoc tests
# eg. TukeyHSD
TukeyHSD(anova_rcf)
# or pairwise t-test with a correction term
#pairwise.t.test(red.cell.folate$folate, red.cell.folate$ventilation, method = "bonferroni")
pairwise.t.test(lung$volume, lung$method, p.adj = "bonferroni")
plot(anova_rcf)

