#OR 1 One- and two-sample tests

#if you have not done so yet install the packages
#install.packages("tidyverse")
#install.packages("gggplot2")
#install.packages("reshape2")
#install.packages("ISwR")
#install.packages("car")

#Most examples are taken from Introductory Statistics with R by Peter Dalgaard

#load the packages for later use
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ISwR)
library(car)

#daily energy intake in kJ for 11 women (Altmann, 1991, p. 183)
daily.intake<-c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)

mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)

#ggplot needs dataframes to plot
df_daily.intake<-as.data.frame(daily.intake)
ggplot(df_daily.intake,aes(x="intake",y=daily.intake))+
  geom_boxplot()
#sometimes it is helpful to show all the single results
ggplot(df_daily.intake,aes(x="intake",y=daily.intake))+
  geom_boxplot()+
  geom_point()
#if the points overlap to much, this could distort the picture
ggplot(df_daily.intake,aes(x="intake",y=daily.intake))+
  geom_boxplot()+
  geom_jitter(width = 0.15) #allows to change the width

#Is there a difference to the recommendation of 7725 daily intake?
t.test(daily.intake,mu=7725) #using the vector
t.test(df_daily.intake$daily.intake,mu=7725) #using the dataframe

#show the daily recommendation in the graph
ggplot(df_daily.intake,aes(x="intake",y=daily.intake))+
  geom_boxplot()+
  geom_jitter(width = 0.15)+ #allows to change the width
  geom_hline(yintercept=7725, #specifies where the line should be
             color="red",
             linetype="dashed")

#What about the normal distribution assumption?
qqnorm(daily.intake)
qqline(daily.intake)

#If the deviation from the normal distribution assumption is too large it is recommended to use the non
#parametric Wilcoxon signed-rank test
wilcox.test(daily.intake,mu=7725)
#note the warning message. Ties cannot be handled properly, which again might lead to incorrect results
#comparing the p values from the t-test and Wilcoxon test indicates that the t-test might return easier significant results

#Two sample t test

attach(energy)
View(energy)
class(energy)

ggplot(energy,aes(x="energy",y=expend))+
  geom_boxplot()
#that does not seem right, there are two types in this dataset
ggplot(energy,aes(x="energy",y=expend))+
  geom_boxplot()+
  geom_jitter(aes(color=stature))
#we can see now the individual groups, but we cannot compare them properly
ggplot(energy,aes(x=stature,y=expend))+
  geom_boxplot()

t.test(expend~stature)
#R automatically assumes that the variances in the two groups are different
#it is possible to force R to assume that the variances are equal
t.test(expend~stature,var.equal=TRUE)
#again this can have a significant impact on the p-value

#How to check that variances are equal?
var.test(expend~stature)
#We can assume that variances are equal

#What about the normality assumption?
qqnorm(expend)
qqline(expend)
#but this is now the qq plot for all the results, we need it for the groups individually.

df.energy_lean<-energy%>%
  subset(stature=="lean")

df.energy_obese<-energy%>%
  subset(stature=="obese")

qqnorm(df.energy_lean$expend)
qqline(df.energy_lean$expend)

qqnorm(df.energy_obese$expend)
qqline(df.energy_obese$expend)

#looks odd, so it is better to use a non-parametric test

wilcox.test(expend~stature)
#again note the warning message because of the ties

detach(energy)

#What happens if we pick the wrong test.
#Scenario 1: There is no difference between the two groups (i.e. the come from the same population)

for(i in c(1:10000)){
  if(i==1){
    wilcox_false_positive_unif<-0
    ttest_false_positive_unif<-0
    wilcox_false_positive_norm<-0
    ttest_false_positive_norm<-0
    }
  group1_unif<-runif(50,min=0,max=1)
  group2_unif<-runif(50,min=0,max=1)
  group1_norm<-rnorm(50,mean=0.5,sd=0.1)
  group2_norm<-rnorm(50,mean=0.5,sd=0.1)
  df_unif<-data.frame(nr=c(1:50),
                      group1_unif=group1_unif,
                      group2_unif=group2_unif)%>%
    reshape2::melt(id.vars="nr")
  df_norm<-data.frame(nr=c(1:50),
                      group1_norm=group1_norm,
                      group2_norm=group2_norm)%>%
    reshape2::melt(id.vars="nr")
  result_wilcox_unif<-wilcox.test(df_unif$value~df_unif$variable)
  result_ttest_unif<-t.test(df_unif$value~df_unif$variable,var.equal=TRUE)
  result_wilcox_norm<-wilcox.test(df_norm$value~df_norm$variable)
  result_ttest_norm<-t.test(df_norm$value~df_norm$variable,var.equal=TRUE)
  if(result_wilcox_unif$p.value<0.05){wilcox_false_positive_unif<-wilcox_false_positive_unif+1}
  if(result_ttest_unif$p.value<0.05){ttest_false_positive_unif<-ttest_false_positive_unif+1}
  if(result_wilcox_norm$p.value<0.05){wilcox_false_positive_norm<-wilcox_false_positive_norm+1}
  if(result_ttest_norm$p.value<0.05){ttest_false_positive_norm<-ttest_false_positive_norm+1}
}
wilcox_false_positive_unif
ttest_false_positive_unif
wilcox_false_positive_norm
ttest_false_positive_norm

for(i in c(1:10000)){
  if(i==1){
    wilcox_false_negative_unif<-0
    ttest_false_negative_unif<-0
    wilcox_false_negative_norm<-0
    ttest_false_negative_norm<-0
  }
  group1_unif<-runif(50,min=0,max=1)
  group2_unif<-runif(50,min=0.2,max=1.2)
  group1_norm<-rnorm(50,mean=0.5,sd=0.1)
  group2_norm<-rnorm(50,mean=0.6,sd=0.1)
  df_unif<-data.frame(nr=c(1:50),
                      group1_unif=group1_unif,
                      group2_unif=group2_unif)%>%
    reshape2::melt(id.vars="nr")
  df_norm<-data.frame(nr=c(1:50),
                      group1_norm=group1_norm,
                      group2_norm=group2_norm)%>%
    reshape2::melt(id.vars="nr")
  result_wilcox_unif<-wilcox.test(df_unif$value~df_unif$variable)
  result_ttest_unif<-t.test(df_unif$value~df_unif$variable,var.equal=TRUE)
  result_wilcox_norm<-wilcox.test(df_norm$value~df_norm$variable)
  result_ttest_norm<-t.test(df_norm$value~df_norm$variable,var.equal=TRUE)
  if(result_wilcox_unif$p.value>0.05){wilcox_false_negative_unif<-wilcox_false_negative_unif+1}
  if(result_ttest_unif$p.value>0.05){ttest_false_negative_unif<-ttest_false_negative_unif+1}
  if(result_wilcox_norm$p.value>0.05){wilcox_false_negative_norm<-wilcox_false_negative_norm+1}
  if(result_ttest_norm$p.value>0.05){ttest_false_negative_norm<-ttest_false_negative_norm+1}
}
wilcox_false_negative_unif
ttest_false_negative_unif
wilcox_false_negative_norm
ttest_false_negative_norm


#ANOVA
?red.cell.folate
View(red.cell.folate)
lapply(red.cell.folate,class)

ggplot(data=red.cell.folate,aes(x=ventilation,y=folate))+
  geom_boxplot()

anova_red.cell.folate<-aov(folate~ventilation,data=red.cell.folate)
summary(anova_red.cell.folate)

#there is a significant difference between the groups. But between which?
#Tukey Honest Significant Differences
TukeyHSD(anova_red.cell.folate)
#Or pairwise t test with a correction term
pairwise.t.test(red.cell.folate$folate,red.cell.folate$ventilation,method="bonferroni")

#What about equal variances?
plot(anova_red.cell.folate, 1)
#there are outliers 1,2,8 and the homogenity of variance assumption could be violated.
#do a statistical test because visual inspection is not conclusive
leveneTest(folate~ventilation, data = red.cell.folate)
#assumption is violated
#But be careful:
bartlett.test(folate~ventilation, data = red.cell.folate)
#this one does not indicate different variances!
#Bartlett's test is more sensitive to violations of normal distribution assumption
#Check the normality assumption
plot(anova_red.cell.folate, 2)
#Again 1,2 and 8 are outliers the distribution seems to deviate quite a lot form a normal distribution
# Extract the residuals
aov_residuals <- residuals(object = anova_red.cell.folate )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
#But the Shapiro Wilk test indicates that the normality assumption is fine.

#in case it is not, a Kruskal-Walis test can be performed
kruskal.test(folate~ventilation,data=red.cell.folate)
#again we lose the significance of the result


#ANOVA without homogenity of variance assumption
oneway.test(folate~ventilation, data = red.cell.folate)
#There is no longer a significant result!

plot(anova_red.cell.folate)
#1st plot: homogenity of variance: Outliers are marked, there should not be any obvious pattern; red line should be horizontal
#2nd plot: Q-Q plot, points should be close to the diagonal. Outliers are marked again.
#3rd plot: there should be no pattern in the residuals. red line should be horizontal. Outliers are indicated 
#4th plot: if a point lies outside cook's distance it is an influental point, i.e. removing this point can have a big impact on the model. Check for erroneous data.

#points 1,2 and 8 appear in several of these plots. Let us remove them

red.cell.folate.adjusted<-red.cell.folate[-c(1, 2, 8), ] 
anova_red.cell.folate.adjusted<-aov(folate~ventilation,data=red.cell.folate.adjusted)
summary(anova_red.cell.folate.adjusted)  

plot(anova_red.cell.folate.adjusted)

TukeyHSD(anova_red.cell.folate.adjusted)

#ANOVA without homogenity of variance assumption
oneway.test(folate~ventilation, data = red.cell.folate.adjusted)

#Another ANOVA example
?juul
View(juul)
lapply(juul,class)

ggplot(data=juul,aes(x=tanner,y=igf1))+
  geom_boxplot()

juul$tanner<-factor(juul$tanner,labels=c("I","II","III","IV","V"))

ggplot(data=juul,aes(x=tanner,y=igf1))+
  geom_boxplot()

#exclude NA

ggplot(data=juul%>%subset(!is.na(tanner)),aes(x=tanner,y=igf1))+
  geom_boxplot()

aov.juul<-aov(igf1~tanner,data=juul)
summary(aov.juul)
plot(aov.juul)

TukeyHSD(aov.juul)
