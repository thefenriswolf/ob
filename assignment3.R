# Assignment 3
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

# The zelazo data are in the form of a list of vectors, one for each of the four groups.
# Convert the data to a form suitable for the use of lm and calculate the relevant test.
# Consider t tests comparing selected subgroups or obtained by combining groups.
View(zelazo)
?zelazo
active<-zelazo$active
passive<-zelazo$passive
none<-zelazo$none
control<-zelazo$ctr.8w
z<-data.frame(age=c(active, passive, none, control),
              group=c("active", "active", "active", "active", "active", "active", "passive", "passive", "passive", "passive", "passive", "passive", "none", "none", "none", "none", "none", "none", "control", "control", "control", "control", "control"))
z$group<-as.factor(z$group)

summary(z)
ggplot(data = z, aes(x=group,y=age))+
  geom_boxplot()+
  geom_point()

# null-hypothesis the groups are the same
anova_z<-aov(data = z, age~group)
summary(anova_z)
# p-value of 0.129 > 0.05 which means the null-hypothesis is true and the groups are the same
TukeyHSD(anova_z)
pairwise.t.test(z$age, z$group, method = "bonferroni")
plot(anova_z)


# In the lung data, do the three measurement methods give systematically different results?
# If so, which ones appear to be different?

View(lung)
?lung
anova_lung<-aov(data = lung, volume~method)
summary(anova_lung) # p-value of 0.1 > 0.05 suggests there is no difference between the groups
kruskal.test(lung$volume ~ lung$method, data = lung) # p-value of 0.1823 suggests there is no difference
TukeyHSD(anova_lung)
pairwise.t.test(lung$volume, lung$method, p.adj = "bonferroni")
kruskal.test(lung$volume ~ lung$method, data = lung)

# Repeat the previous exercises using the zelazo and lung data with the relevant nonparametric tests
lung_red<-filter(lung, method=="A"| method=="B")
wilcox.test(volume~method, data = lung_red) # p-value of 0.2963 suggest no difference
kruskal.test(lung$volume ~ lung$method, data = lung) # p-value of 0.1823 suggests there is no difference

kruskal.test(data = z, age~group) # p-value of 0.0758 suggests the groups are the same
z_red1<-filter(z, group=="active" | group=="passive")
wilcox.test(age~group, data = z_red1) # p-value of 0.0632 suggest no difference between the active and passive groups

# The igf1 variable in the juul data set is arguably skewed and has different variances across Tanner groups. 
# Try to compensate for this using logarithmic and square-root transformations, 
# and use the Welch test. However, the analysis is still problematic - why?
View(juul)
plot(juul)
hist(juul$tanner)
juul15<-filter(juul, tanner==1 | tanner==5)

t.test(sqrt(igf1)~tanner, data = juul15, var.equal=FALSE) # p-value of 2.2e-16 suggest a difference between tanner 1 and tanner 5
t.test(log(igf1)~tanner, data = juul15, var.equal=FALSE) # p-value of 2.2e-16 suggest a difference between tanner 1 and tanner 5

# the comparison still does not take into account the other variables, maybe most importantly sex
# as men an women grow differently



