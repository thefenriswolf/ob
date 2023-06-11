library(tidyverse)
library(ggplot2)
library(survminer)
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
library(survival)
library(RTCGA)

# survival analysis
# at time=0 everybody is alive then record how many people die within a time span
# survival curve S(t)=p1*p2*p3*...*pi
# p is a conditional probability: survival at time x provided they were alive at the previous time
?melanom
View(melanom)
# status:
# 1: dead
# 2: alive
# 3: died from other cause
# ulceration:
# 1: yes
# 2: no
# sex:
# 1: female
# 2: male
lapply(melanom, class)
#cars$garage<-as.factor(cars$garage)

Surv(time=melanom$days, event=melanom$status==1)
surv.all<-survfit(Surv(time=melanom$days, event=melanom$status==1)~1, data = melanom)
summary(surv.all)
plot(surv.all) # plot survival curve
# alternatively use ggplot
ggsurvplot(surv.all, data = melanom)

# plotting different survival rates
surv.bysex<-survfit(Surv(time=melanom$days, event=melanom$status==1)~sex, data = melanom)
ggsurvplot(surv.bysex, data = melanom, conf.int = TRUE)
# confidence intervals are overlapping so the difference might not be significant
# so we need to check by using a log-rank-test
survdiff(Surv(time=melanom$days, event=melanom$status==1)~sex, data = melanom)
ggsurvplot(surv.bysex, data = melanom,
           conf.int = TRUE,
         # risk.table = TRUE, # does not work on my machine
           pval = TRUE)
# p-value 0.011 < 0.05 which means there is a significant difference between the groups

# cox proportional hazard model
# analogue to the linear regression
melanom.cox<-coxph(Surv(time=melanom$days, event=melanom$status==1)~ulc+thick+sex, data = melanom)
summary(melanom.cox)
# seems like sex is not a important factor (p-value 0.08498)
# check if all conditions for cox models are met
cox.zph(melanom.cox)
# ulc has the biggest influence on the model
melanom.crosstable<-table(melanom$sex, melanom$ulc)
# cross table shows us that men with ulcerations are over represented in the data
# so sex may be a significant variable after all
# check for this case
survdiff(Surv(time=melanom$days, event=melanom$status==1)~ulc, data = melanom)
surv.byulc<-survfit(Surv(time=melanom$days, event=melanom$status==1)~ulc, data = melanom)
ggsurvplot(surv.byulc, data = melanom,
           conf.int = TRUE,
           #risk.table = TRUE, # does not work on my machine
           pval = TRUE)
# ulceration is way more telling than sex and the confidence intervals are also not overlapping
# its important to be able to explain why a model behaves the way it does

# stratify the model: first look at sex and then see if there is ulcerations
survdiff(Surv(time=melanom$days, event=melanom$status==1)~sex+strata(ulc), data = melanom)
# and then its no longer significant, meaning sex has no influence


