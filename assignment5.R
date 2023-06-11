# Assignment 5
# Stefan Rohrbacher

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

#######################################
############ graft.vs.host ############
#######################################
?graft.vs.host
View(graft.vs.host)
head(graft.vs.host, 4)
lapply(graft.vs.host, class)
graft.vs.host$preg<-as.factor(graft.vs.host$preg)
graft.vs.host$type<-as.factor(graft.vs.host$type)
graft.vs.host$gvhd<-as.factor(graft.vs.host$gvhd)
graft.vs.host$dead<-as.factor(graft.vs.host$dead)
plot(graft.vs.host)

# estimate the survival function for patients with or without GVHD
# Test the hypothesis that the survival is the same in both groups.
# Extend the analysis by including the other explanatory variables.
Surv(time=graft.vs.host$time, event=graft.vs.host$dead==1)
surv.all<-survfit(Surv(time=graft.vs.host$time, event=graft.vs.host$dead==1)~1, data = graft.vs.host)
summary(surv.all)
ggsurvplot(surv.all, data = graft.vs.host)

surv.bygvhd<-survfit(Surv(time=graft.vs.host$time, event=graft.vs.host$dead==1)~gvhd, data = graft.vs.host)
ggsurvplot(surv.bygvhd, data = graft.vs.host, conf.int = TRUE)
# survival time for people with gvhd is shorter
# confidence intervals are overlapping so the difference might not be significant
# so we need to check by using a log-rank-test
survdiff(Surv(time=graft.vs.host$time, event=graft.vs.host$dead==1)~gvhd, data = graft.vs.host)

# p-value of 0.01 indicates a significant difference, so we can reject the hypothesis that survival 
# is the same in both groups
ggsurvplot(surv.bygvhd, data = graft.vs.host,
           conf.int = TRUE,
           #risk.table = TRUE, # does not work on my machine
           pval = TRUE)

# extending the model by the other explanatory variables
surv.full<-survfit(Surv(time=graft.vs.host$time, event=graft.vs.host$dead==1)~rcpage+donage+type+preg+index+gvhd, data = graft.vs.host)
survdiff(Surv(time=graft.vs.host$time, event=graft.vs.host$dead==1)~rcpage+donage+type+preg+index+gvhd, data = graft.vs.host)
ggsurvplot(surv.full, data = graft.vs.host, conf.int = TRUE)


#######################################
################# stroke ##############
#######################################
?stroke
View(stroke)
head(stroke, 4)

# Fit Cox models to the stroke data with age and sex as predictors and with sex alone.
# Explain the difference.

# just sex
stroke.cox<-coxph(Surv(time=stroke$obsmonths, event=stroke$dead==TRUE)~sex, data = stroke)
summary(stroke.cox)
cox.zph(stroke.cox)
# shows sex is a significant factor for the model p-value: 0.000143
surv.strk<-survfit(Surv(time=stroke$obsmonths, event=stroke$dead==TRUE)~sex, data = stroke)
summary(surv.strk)
ggsurvplot(surv.strk, data = stroke, conf.int = TRUE)
survdiff(Surv(time=stroke$obsmonths, event=stroke$dead==TRUE)~sex, data = stroke)
ggsurvplot(surv.strk, data = stroke,
           conf.int = TRUE,
           #risk.table = TRUE, # does not work on my machine
           pval = TRUE)
# here the p-value is 0.00013

# age and sex
stroke.cox2<-coxph(Surv(time=stroke$obsmonths, event=stroke$dead==TRUE)~age+sex, data = stroke)
summary(stroke.cox2)
# here the p-value for sex increased to 0.825, which makes it less significant than before
# and highly insignificant for the current model
# this is because the risk of dying form a stroke increases with age, which has a p-value
# of <2+10^-16
cox.zph(stroke.cox2)
surv.strk2<-survfit(Surv(time=stroke$obsmonths, event=stroke$dead==TRUE)~age+sex, data = stroke)
summary(surv.strk2)
ggsurvplot(surv.strk2, data = stroke, conf.int = TRUE)
survdiff(Surv(time=stroke$obsmonths, event=stroke$dead==TRUE)~age+sex, data = stroke)
ggsurvplot(surv.strk2, data = stroke,
           conf.int = TRUE,
           #risk.table = TRUE, # does not work on my machine
           pval = TRUE)
