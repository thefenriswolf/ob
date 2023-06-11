library(ISwR)
library(tidyverse)
#install.packages("survival") #only run once
library(survival)
#install.packages("survminer") #only run once
library(survminer)

#if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager") #only run once

#BiocManager::install("RTCGA") #only run once
library(RTCGA)

# BiocManager::install("RTCGA.clinical") #only run once
# BiocManager::install("RTCGA.mRNA") #only run once

#The idea of a survival curve is that the survival rate at time t=0 is 1 (i.e. 100% of patients are alive) and when t goes to
#infinity the survival rate goes to 0 (i.e. nobody lives for ever)
#S(t) is then the survival probability at time t and calculated as 
#S(t)=p1*p2*p3*...*pt, where pi is the proportion of people that survive time i, and have survived till i-1 (cf. conditional propability)
#E.g. We start with 10 people, at time 1 6 are still alive at time 2 3, at time 3 still 3 and at time 4 0,then
#S(0)=1, S(1)=6/10, S(2)=6/10*3/6, S(3)=6/10*3/6*3/3, S(4)=6/10*3/6*3/3*0/3


View(melanom)
# 
# no a numeric vector, patient code.
# status a numeric vector code, survival status; 1: dead from melanoma, 2: alive, 3: dead from other cause.
# days a numeric vector, observation time.
# ulc a numeric vector code, ulceration; 1: present, 2: absent.
# thick a numeric vector, tumor thickness (1/100 mm).
# sex a numeric vector code; 1: female, 2: male.

#specify which event means death by melanomia, the rest is then treated as censored
Surv(time=melanom$days,event=melanom$status==1)

#10+ means that the patient did not die from the melanomia within the first 10 days and was then unavainable for further study
#if we then look in dataframe melanom we see that the patient who left after 10 days actually died of other causes
#+ always stands for censored observation
#censored means that a patient is not available anymore. Either because he left the study, lived beyond the end of the study, dies of other reasons...
#no plus next to a number means that the patient lived that amount of time. E.g. 185 means that this patient died 185 days after the treatment

surv.all<-survfit(Surv(time=days,event=status==1)~1,data=melanom)
summary(surv.all)

plot(surv.all)

#using the survminer package the graph can be more customised
ggsurvplot(surv.all,data=melanom)

#Plotting several survival curves
surv.bysex<-survfit(Surv(melanom$days,melanom$status==1)~sex,data=melanom)
ggsurvplot(surv.bysex,data=melanom)
#to avoid potential confusion confidence intervals are turned off, but can easily be manually turned on again
ggsurvplot(surv.bysex,data=melanom,conf.int = TRUE)
#without the confidence intervals it looked very obvious that sex 2 has a lower chance to survive, anyhow confidence intervals are overlapping quite a lot
#using a log-rank test we can check this
survdiff(Surv(melanom$days,melanom$status==1)~sex,data=melanom)
ggsurvplot(surv.bysex,data=melanom,conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE)
#The first impression was correct there is a significant difference in survival
#Does sex have an influence on survival? Compare this to the idea of an t-test/ANOVA? 
#Does the categorical variable have an influence?

#What about the non-categorical variables?
#we can use a Cox proportional hazard model
#log(h(t))=log(h0(t))+b1x1+b2x2+...+bpxp
#h1(t)=h0(t)×e^(b1x1)
#and by rearraning the terms we get
#h1(t)/h0(t)=e^(b1x1)
#e^(b1x1) is constant in t! It is called the hazard ratio
#The interpretation of the hazards ratio depends on the measurement scale of the predictor variable, 
#but in simple terms, a positive coefficient indicates worse survival and 
#a negative coefficient indicates better survival for the variable in question.

melanom.cox<-coxph(Surv(melanom$days,melanom$status==1)~ulc+thick+sex,data=melanom)

summary(melanom.cox)
#test whether the proportionate hazard assumption is satisfied
cox.zph(melanom.cox)
#at least for thick we can say that it is not. The hazard rate seems to be time dependent and should
#not be included in the model

#we also see that sex is not that significant while ulc for example is. We should check that we have not spotted
#a significance between sex and hazard rate that this actually driven by ulc

ggplot(data=melanom,aes(x=sex,y=ulc))+
  geom_point()

melanom.crosstable<-table(melanom$sex,melanom$ulc)
#There a lot of male patient with ulceration
survdiff(Surv(melanom$days,melanom$status==1)~ulc,data=melanom)
surv.byulc<-survfit(Surv(melanom$days,melanom$status==1)~ulc,data=melanom)
ggsurvplot(surv.byulc,data=melanom,conf.int = TRUE,
           pval = TRUE,
           risk.table = TRUE)
#this is much more obvious than for sex

#finally we can check this in the model by stratifying the model
#there will be a seperate model for each ulc and then checked whether sex is still significant
survdiff(Surv(melanom$days,melanom$status==1)~sex+strata(ulc),data=melanom)
#which it is not

#https://bioconductor.org/packages/release/bioc/html/RTCGA.html
# The Cancer Genome Atlas (TCGA) Data Portal provides a platform for researchers to search, 
# download, and analyze data sets generated by TCGA. It contains clinical information, 
# genomic characterization data, and high level sequence analysis of the tumor genomes. 
# The key is to understand genomics to improve cancer care. 
# RTCGA package offers download and integration of the variety and volume of TCGA 
# data using patient barcode key, what enables easier data possession. 
# This may have an benefcial infuence on impact on development of science and improvement 
# of patients' treatment. Furthermore, RTCGA package transforms TCGA data to tidy form which is convenient to use.

#https://bioconnector.github.io/workshops/r-survival.html#cox_regression
library(RTCGA.clinical)
#data about breast cancer
dim(BRCA.clinical)
names(BRCA.clinical)

# Create the clinical data
clin <- survivalTCGA(BRCA.clinical, OV.clinical, GBM.clinical, 
                     extract.cols="admin.disease_code")
# Show the first few lines
head(clin)

# How many samples of each type?
table(clin$admin.disease_code)

# Tabulate by outcome
xtabs(~admin.disease_code+patient.vital_status, data=clin) %>% addmargins()

brca.cox<-coxph(Surv(times, patient.vital_status)~admin.disease_code, data=clin)
summary(brca.cox)
#always do a model check
cox.zph(brca.cox)
#highly violated, but still we can say that there is a difference in survival

# This tells us that compared to the baseline brca group, GBM patients have a 17.9476 times 
# increase in hazards, and ovarian cancer patients have 4.6973 times worse survival. 
sfit <- survfit(Surv(times, patient.vital_status)~admin.disease_code, data=clin)
summary(sfit, times=seq(0,365*5,365))

ggsurvplot(sfit, conf.int=TRUE, pval=TRUE)

#There is also a lung dataset in the ISwR library (note the warning when loading the two packages)
#survival:: makes sure that the one from the survival library is used
head(survival::lung)

# inst:	 Institution code
# time:	 Survival time in days
# status:	 censoring status 1=censored, 2=dead
# age:	 Age in years
# sex:	 Male=1 Female=2
# ph.ecog:	 ECOG performance score as rated by the physician. 0=asymptomatic, 1= symptomatic but completely ambulatory, 2= in bed <50% of the day, 3= in bed > 50% of the day but not bedbound, 4 = bedbound
# ph.karno:	 Karnofsky performance score (bad=0-good=100) rated by physician
# pat.karno:	 Karnofsky performance score as rated by patient
# meal.cal:	 Calories consumed at meals
# wt.loss:	 Weight loss in last six months

lung<-survival::lung

lung.fit <- survfit(Surv(time, status) ~ sex, data = lung)
print(lung.fit)

ggsurvplot(lung.fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

#just to give an idea how much can actually be changed in the graph
ggsurvplot(
  lung.fit,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 200,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("Male", "Female"),    # change legend labels.
  palette = 
    c("#E7B800", "#2E9FDF")) # custom color palettes.

#shows for example the median survival time (270 for men and 426 for women)
summary(lung.fit)$table

lung.coxph<-coxph(Surv(time, status) ~ sex+age+ph.ecog+meal.cal, data = lung)

summary(lung.coxph)
ggforest(lung.coxph, data = lung)


lung.coxph.reduced<-coxph(Surv(time, status) ~ sex+ph.ecog, data = lung)
summary(lung.coxph.reduced)
ggforest(lung.coxph.reduced, data = lung)

# The p-value for sex is 0.000986, with a hazard ratio HR = exp(coef) = 0.58, 
# indicating a strong relationship between the patients' sex and decreased risk of death. 
# The hazard ratios of covariates are interpretable as multiplicative effects on the hazard. 
# For example, holding the other covariates constant, being female (sex=2) reduces 
# the hazard by a factor of 0.58, or 42%. We conclude that, being female is associated with good prognostic.

# Similarly, the p-value for ph.ecog is 4.45e-05, with a hazard ratio HR = 1.59, 
# indicating a strong relationship between the ph.ecog value and increased risk of death. 
# Holding the other covariates constant, a higher value of ph.ecog is associated with a poor survival.

# By contrast, the p-value for age is now p=0.23. The hazard ratio HR = exp(coef) = 1.01, 
# with a 95% confidence interval of 0.99 to 1.03. Because the confidence interval for HR includes 1, 
# these results indicate that age makes a smaller contribution to the difference in the HR after 
# adjusting for the ph.ecog values and patient's sex, and only trend toward significance. For example, 
# holding the other covariates constant, an additional year of age induce daily hazard of death 
# by a factor of exp(beta) = 1.01, or 1%, which is not a significant contribution.
cox.zph(lung.coxph.reduced)
#looks good

ggsurvplot(survfit(lung.coxph.reduced,data=lung), palette = "#2E9FDF",
           ggtheme = theme_minimal())
#Note that this is not the Kaplan Meier Estimate
ggsurvplot(survfit(Surv(time, status) ~ 1,data=lung), palette = "#2E9FDF",
           ggtheme = theme_minimal())

sex_df <- with(lung,
               data.frame(sex = c(1, 2), 
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)

fit <- survfit(lung.coxph.reduced, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),
           ggtheme = theme_minimal(),data=lung)
#Again note that these are estimations based on the model while this are actual data points
ggsurvplot(survfit(Surv(time, status) ~ sex,data=lung), 
           ggtheme = theme_minimal(),conf.int = TRUE)