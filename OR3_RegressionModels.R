#load the packages we need
library(ISwR)
library(tidyverse)
library(ggplot2)
#for finding the best linear model
#install.packages("leaps") #only run once
library(leaps)

#Variance inflation factor (vif)
#install.packages("car") #only run once
library(car)

#Another package for diagnostics and model search
#install.packages("olsrr") #only run once
library(olsrr)

#gvlm() does the most important checks for a linear model
#install.packages("gvlma") #only run once
library(gvlma)


head(cystfibr)
plot(cystfibr)




# This data frame contains the following columns:
# age a numeric vector, age in years.
# sex a numeric vector code, 0: male, 1:female.
# height a numeric vector, height (cm).
# weight a numeric vector, weight (kg).
# bmp a numeric vector, body mass (% of normal).
# fev1 a numeric vector, forced expiratory volume.
# rv a numeric vector, residual volume.
# frc a numeric vector, functional residual capacity.
# tlc a numeric vector, total lung capacity.
# pemax a numeric vector, maximum expiratory pressure.

ggplot(cystfibr,aes(x=age,y=height))+
  geom_point()

lm.pemax<-lm(data=cystfibr,pemax~age+sex+height+weight+bmp+fev1+rv+frc+tlc)
summary(lm.pemax)
#the model itself is significant but not a single predictor is
#this seems paradoxial, but the significance test for a single predictor only means what happens if you remove 
#this predictor in the current model

#going manually through the combinations is not really possible
models <- regsubsets(pemax~., data = cystfibr, nvmax = 8)
summary(models)
#star indicates which variable to include
#         age sex height weight bmp fev1 rv  frc tlc
# 1  ( 1 ) " " " " " "    "*"    " " " "  " " " " " "
# 2  ( 1 ) " " " " " "    "*"    "*" " "  " " " " " "
# 3  ( 1 ) " " " " " "    "*"    "*" "*"  " " " " " "
# 4  ( 1 ) " " " " " "    "*"    "*" "*"  "*" " " " "
# 5  ( 1 ) " " " " " "    "*"    "*" "*"  "*" " " "*"
# 6  ( 1 ) "*" " " "*"    "*"    "*" "*"  "*" " " " "
# 7  ( 1 ) "*" " " "*"    "*"    "*" "*"  "*" "*" " "
# 8  ( 1 ) "*" " " "*"    "*"    "*" "*"  "*" "*" "*"
#e.g. a model with 3 variables should use weight, bmp and fev1

#But how do we find the "best" modell?
#Note that there is no best modell. A metric has to be defined and
#then the modells that optimises with respect to this metric can be chosen

res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2), #Adjusted R^2
  BIC = which.min(res.sum$bic) #Bayesian information criterion
)

#Optimsed prediction power is obtained by using 4 variables, while
#while based in BIC only 3 variables should be chosen (overfitting)

model.R2<-lm(data=cystfibr,pemax~weight+bmp+fev1+rv)
summary(model.R2)
model.BIC<-lm(data=cystfibr,pemax~weight+bmp+fev1)
summary(model.BIC)
#note that the BIC model is the one which was obtained in the paper as well.
anova(model.R2,model.BIC)
#which again shows that rv is not significant
#this anova command can be used to compare to linear models, when the one models predictors are
#a subset of the other models predictors

model.easy<-lm(data=cystfibr,pemax~fev1)

anova(model.BIC,model.easy)
#shows that all additional predictors (in that case weight and bmp) as a whole are significant for the model 
#-> at least some of them should be included

plot(model.BIC)
#overall the model looks fine

#note that in the book using a "manual" algorithm they ended up with a different model
#also note that there a three variables, which -especially dealing with data on children- might be heavily correlated
model.correlated<-lm(data=cystfibr,pemax~age+weight+height)
summary(model.correlated)
vif(model.correlated)
#as a rule of thumb, values greater than 4 should be checked, values greater than 5 a questionable and values greater than 10 indicate a serious collinearity
vif(model.BIC)

#using the olsrr package
ols_vif_tol(model.correlated)

#Model fit assessment (plot to detect nonlinearity, outliers)
ols_plot_resid_fit_spread(model.BIC)

ols_plot_diagnostics(model.BIC)

#model checks gvlm
gvlma::gvlma(model.BIC)
gvlma::gvlma(model.correlated)


#Used cars
cars<-read.csv2("http://statmath.wu.ac.at/~hatz/Buecher/R/R-Begleitmaterial/Daten/gebrauchtwagen.csv")
colnames(cars)<-c("price","miles","services","garage","colour")

head(cars)
#types of variables?

lapply(cars,class) #applies the function class to all columns in the dataframe
unique(cars$colour)
#We have to let R know that garage and colour are actually not metric variables

cars$garage<-as.factor(cars$garage)
cars$colour<-as.factor(cars$colour)

lm.cars<-lm(data=cars,price~miles+services+garage+colour)
summary(lm.cars)

#Note how garage and colour is represented now. R creates a dummy variables for each factors.

plot(lm.cars)

gvlma::gvlma(lm.cars)

#Inspect the various variables individually

ggplot(data=cars,aes(x=miles,y=price))+
  geom_point()

ggplot(data=cars,aes(x=services,y=price))+
  geom_point()

ggplot(data=cars,aes(x=garage,y=price))+
  geom_point()

ggplot(data=cars,aes(x=colour,y=price))+
  geom_point()

anova(lm.cars)

#Manual backward selection
#start with a full complex model and remove not significant variables

lm.cars.reduced<-lm(data=cars,price~miles+services+garage)
summary(lm.cars.reduced)

#comparision of the two models
anova(lm.cars,lm.cars.reduced)

#lm.cars is not significantly better than lm.cars.reduced, but it needs more variables than lm.cars.reduced
#generally the model using the least number of predictors should be prefered.

#example taken from https://daviddalpiaz.github.io/appliedstats/transformations.html
#Is there a relation between years of seniority and salary?

df.salary<-read.csv("https://daviddalpiaz.github.io/appliedstats/data/initech.csv")

ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()

lm.salary.linear<-lm(data=df.salary,salary~years)
summary(lm.salary.linear)

ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()+
  geom_smooth(method="lm")

#Actually that does not look too bad.

plot(lm.salary.linear)
#Residuals vs Fitted shows a pattern, we seem to miss something in the data, especially for the small and large x value.
#By looking at the scatter plot again we see a kind of underestimation in boundaries and over estimation in the middle area

gvlma::gvlma(lm.salary.linear)

#Let us try to change something
#a very common approach is to look at log transformation of the data, but before that let us use a quadratic function

#There are at least two ways to do that
#First approach: Tell R the formula
lm.salary.quadratic<-lm(data=df.salary,salary~I(years^2)) #the I() has to be used to protect the variable years
summary(lm.salary.quadratic)
plot(lm.salary.quadratic)
gvlma::gvlma(lm.salary.quadratic)

ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()+
  stat_smooth(aes(x=years,y = salary),method = "lm", formula = y ~ I(x^2), size = 1)+
  geom_smooth(method="lm",color="red")

#Second Approach: Manually adjust the variable
df.salary.extended<-df.salary%>%
  mutate(years.squared=years^2)

#and then use the new variable in the linear model
#doing it like this makes it more obvious why a function containing a square is still a linear regression
df.salary.quadraric.variant<-lm(data=df.salary.extended,salary~years.squared)
summary(df.salary.quadraric.variant)
#which has to yield the same resuls and does so
ggplot(df.salary.extended,aes(x=years.squared,y=salary))+
  geom_point()+
  geom_smooth(method="lm")
#note the change in coordinates on the x axis!

#The model has improved with respect to adjusted R^2 and assumptions

#Another very typical transformation is a logarithmic transformation of data
lm.salary.log<-lm(data=df.salary,log(salary) ~ years)  #Note that log means logarithm with base e

ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()+
  geom_smooth(aes(x=years,y = salary),method = "glm", formula = y ~ x,
              method.args=list(family=gaussian(link='log')))

summary(lm.salary.log)
#this actually already a general linear model with logarithm as the so called link function

ggplot(df.salary,aes(x=years,y=salary))+
  geom_point()+
  geom_smooth(aes(x=years,y = salary),method = "glm", formula = y ~ x,
              method.args=list(family=gaussian(link='log')))+
  stat_smooth(aes(x=years,y = salary),method = "lm", formula = y ~ I(x^2), size = 1,color='red')

#another example for polynomial regression
lm.pemax.hq<-lm(data=cystfibr,pemax~height+I(height^2))
summary(lm.pemax.hq)
plot(lm.pemax.hq)

#do some prediction
predict(lm.pemax.hq,interval="pred",newdata=data.frame(height=seq(110,180,2))) #95% interval for a single predicted value
predict(lm.pemax.hq,interval="conf",newdata=data.frame(height=seq(110,180,2))) #95% interval around the mean prediction
#these prediction heavily rely on the fact that the residuals are normally distributed, if this is violated the interval 
#could underestimate the uncertainty of the model by a lot

df.pred<-as.data.frame(predict(lm.pemax.hq,interval="pred",newdata=data.frame(height=seq(110,180,2))))
df.conf<-as.data.frame(predict(lm.pemax.hq,interval="conf",newdata=data.frame(height=seq(110,180,2))))


ggplot(data=cystfibr,aes(x=height,y=pemax))+
  geom_point()+
  stat_smooth(aes(x=height,y = pemax),method = "lm", formula = y ~ x+I(x^2), size = 1)+
  geom_line(data=df.pred,aes(x=seq(110,180,2),y=lwr))+
  geom_line(data=df.pred,aes(x=seq(110,180,2),y=upr))+
  geom_line(data=df.conf,aes(x=seq(110,180,2),y=lwr),linetype="dashed",color="red")+
  geom_line(data=df.conf,aes(x=seq(110,180,2),y=upr),linetype="dashed",color="red")

#Interaction effects
aggression<-read.table("http://statmath.wu.ac.at/~hatz/Buecher/R/R-Begleitmaterial/Daten/aggression.dat")
colnames(aggression)<-c("car","sex","frequency","duration")
aggression<-aggression%>%
  mutate(sex=case_when(
    sex=="Frau"~ "female",
    sex=="Mann"~ "male",
    sex=="female"~ "female",
    sex=="male"~ "male",
    TRUE ~ "check missing"
  ))
View(aggression)
aggression2<-subset(aggression,frequency>0)

ggplot(aggression2,aes(x=car,y=duration,fill=sex))+
  geom_boxplot()
interaction.plot(aggression2$sex, aggression2$car, 
                 aggression2$duration,ylab = "Average duration",xlab="Sex")
anova3<-lm(duration~car*sex,data=aggression2)
anova(anova3)
plot(anova3)

#Analysis of Covariance
View(hellung)

summary(hellung)
#glucose is treated as an integer, although it is an nominal/ordinal variable (1 means yes and 2 means no)
#concentration is highly skewed (compare mean and median)
#fix the varibale glucose
class(hellung$glucose)
hellung$glucose<-factor(hellung$glucose,labels=c("yes",'no'))
summary(hellung)

ggplot(hellung,aes(x=conc,y=diameter,color=glucose))+
  geom_point()

#by using log scales the graph looks nicer (matter of taste)
ggplot(hellung,aes(x=conc,y=diameter,color=glucose))+
  geom_point()+
  scale_x_continuous(trans = 'log10')
#add the regression lines

ggplot(hellung,aes(x=conc,y=diameter,color=glucose))+
  geom_point()+
  scale_x_continuous(trans = 'log10')+
  geom_smooth(method="lm")

#create two dataframes for with glucose added one without glucose added
tethym.gluc<-hellung%>%
  subset(glucose=="yes")

tethym.nogluc<-hellung%>%
  subset(glucose=="no")

lm.gluc<-lm(log10(diameter)~log10(conc),data=tethym.gluc)
lm.nogluc<-lm(log10(diameter)~log10(conc),data=tethym.nogluc)

summary(lm.gluc)
summary(lm.nogluc)

#We can use this result to see whether we can assume that the slopes of the two regression lines are identical.
#slope glucose -0.05320
#slope no.glucose -0.059677
#difference 0.0065
#and a std error of (0.00272^2+0.004125^2)^(1/2)=0.0049
#thus t=0.0065/0.0049=1.3
#which is not significant. From this rough estimation we can say that it is fair to assume that the slope of the two
pt(1.3,17+30,0.0065)
#lines is equal (at least not significantly different)

#But it is actually desireable to do this analysis within one modell. So we need to include the interaction between glucose and concentration
#to do this we need the same variance in the two glucose groups
var.test(lm.gluc,lm.nogluc)
#which can be assumed
summary(lm(data=hellung,log10(diameter)~log10(conc)*glucose))
#the regression line for cells in glucose
#log10(diameter)=1.631344-0.053196*log10(conc)
#the regression line for cells NOT in glucose
#log10(diameter)=1.631344+0.003418-(0.053196+0.006480)*log10(conc)

anova(lm(data=hellung,log10(diameter)~log10(conc)*glucose))
#But note that the last line (log10(conc):glucoseno -0.006480   0.004821  -1.344    0.185    )shows that the interaction effect between con and glucose can be ignored  as it is not significant.
#Therefore, instead of an interaction model we can use an additive model
anova(lm(data=hellung,log10(diameter)~log10(conc)+glucose))
anova(lm(data=hellung,log10(diameter)~glucose+log10(conc)))
#Why do we see different signifcant levels?