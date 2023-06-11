#load the packages we need
library(ISwR)
library(tidyverse)
library(ggplot2)

#Linear Regression
#Dataset thuesen
#The thuesen data frame has 24 rows and 2 columns. It contains ventricular shortening velocity and
#blood glucose for type 1 diabetic patients.

View(thuesen) #shows the whole dataframe, it is not recommended to use view, because some dataframe can be very large and problems might arise when showing them
head(thuesen,4) #shows only the first 4 rows of the dataframe, but still all variables

#two metric variables. Can velocity be explained by blood glucose?

#start with visual inspection

ggplot(thuesen,aes(x=blood.glucose,y=short.velocity))+
  geom_point()

#It does not look very nice
#add a regression line

ggplot(thuesen,aes(x=blood.glucose,y=short.velocity))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

#Stores all the relevant variables of the linear regression in the variable lin.reg
lm.velo<-lm(data=thuesen,short.velocity~blood.glucose)

#Summary report of the model
summary(lm.velo)

##First important value p-value of the F-test? "Does the model make sense"?
#Yes, because the p-value is below 0.05
#The model shows that there is a positive correlation between blood glucose and ventricular shortening velocity
#The model is given by short.velocity=1.09781+0.02196blood.glucose

#The predicted values (by the model) 
fitted(lm.velo)
#The errors made by the model (difference prediction to actual value)
residuals(lm.velo)

#Quick reminder what a linear regression does
#cbind adds a column to the dataframe
#thuesen[complete.cases(thuesen),] only uses those lines, where no data is missing
#because when data is missing no comparision to the predicted value can be made
thuesen_adjusted<-cbind(thuesen[complete.cases(thuesen),],fitted(lm.velo))

ggplot(thuesen_adjusted,aes(x=blood.glucose,y=short.velocity))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  geom_segment(aes(x=blood.glucose,y=short.velocity,
                   xend=blood.glucose,yend=fitted(lm.velo))
                   )

#What about the prerequisites for a linear regression?
plot(lm.velo)

#The first plot shows the Residuals vs Fitted.
#This allows to look for non linear patterns 
#You should see more or less equally distributed points without any pattern 
#around a red horizontal line. Outliers are numbered.
#In this example 13, 20 and 24 are outliers.
#The second plot can be used whether the residuals are normally distributed.
#Again 13, 20 and 24 are outliers.
#The third plot allows to check for the homoscedasiticity assumption. Again we should expect 
#a horizontal red line and point equally spread around without any pattern.
#The forth graph shows which points have a strong impact on the shape of the regression line.
#Points with labels are considered outliers (1, 13, 20). But only points "outside" Cook's 
#distance also have a big influence on the regression line. In our case only 13 is outside.

#Let us try to remove datapoint 13 and compare the two regression models
thuesen_removed<-thuesen[-13,]
lm.velo_removed<-lm(data=thuesen_removed,short.velocity~blood.glucose)

summary(lm.velo)
summary(lm.velo_removed)

ggplot(thuesen_removed,aes(x=blood.glucose,y=short.velocity))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x)

ggplot(thuesen,aes(x=blood.glucose,y=short.velocity))+
  geom_point(aes())+
  geom_point(data = thuesen[13, ], colour = "red", size = 5)+
  geom_smooth(method='lm', formula= y~x)

#Removing the outlier wiped out the usefulness of the model

#Try to remove all three outliers that were indicated several times (13,20,24)

thuesen_removed_var2<-thuesen[-c(13,20,24),]
lm.velo_removed_var2<-lm(data=thuesen_removed,short.velocity~blood.glucose)

summary(lm.velo)
summary(lm.velo_removed_var2)
#Still the model is not significant.

#Next example is taken from Hatzinger (R Einf?hrung durch angewandte Statistik )

df_bl<-read.csv2("http://statmath.wu.ac.at/~hatz/Buecher/R/R-Begleitmaterial/Daten/bl2009.csv")
colnames(df_bl)<-c("team","marketvalue","goals","points")
#We want to see whehther there is a relationship between marketvalue and points?
ggplot(df_bl,aes(x=marketvalue,y=points))+
  geom_point()+
  geom_text(aes(label=team),nudge_x = 0.35, nudge_y = 0.35, 
            check_overlap = T)+
  geom_smooth(method="lm")

lm.bl.points<-lm(data=df_bl,points~marketvalue)
summary(lm.bl.points)
plot(lm.bl.points)

ggplot(df_bl,aes(x=marketvalue,y=goals))+
  geom_point()+
  geom_text(aes(label=team),nudge_x = 0.35, nudge_y = 0.35, 
            check_overlap = T)+
  geom_smooth(method="lm")

lm.bl.goals<-lm(data=df_bl,goals~marketvalue)
summary(lm.bl.goals)
plot(lm.bl.goals)

ggplot(df_bl,aes(x=goals,y=points))+
  geom_point()+
  geom_text(aes(label=team),nudge_x = 0.35, nudge_y = 0.35, 
            check_overlap = T)+
  geom_smooth(method="lm")

lm.bl.goals.points<-lm(data=df_bl,points~goals)
summary(lm.bl.goals.points)
plot(lm.bl.goals.points)

#How can we use the model to predict values
#predicts the values using the model for the existing data points
predict(lm.bl.points)
#predicts the values using arbitrary data points
predict(lm.bl.points,newdata=data.frame(marketvalue=5))
predict(lm.bl.points,newdata=data.frame(marketvalue=6))
#or simply combine several values 
predict(lm.bl.points,newdata=data.frame(marketvalue=c(5,6)))

#Correlation Parameters
#Pearson Correlation uses the normal distribution assumption
cor.test(thuesen$blood.glucose,thuesen$short.velocity,method = "pearson")
cor(thuesen$blood.glucose,thuesen$short.velocity)
#does not work because of incomplete data
cor(thuesen$blood.glucose,thuesen$short.velocity,use = "complete.obs")
#note that cor.test gives you the same result and in addition also the p value
cor.test(thuesen$blood.glucose,thuesen$short.velocity,method = "spearman")
cor.test(thuesen$blood.glucose,thuesen$short.velocity,method = "kendall")
