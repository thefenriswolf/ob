#Estimate Reff using AGES data

#install.packages("EpiEstim")
#install.packages("incidence")
library(tidyverse)
library(EpiEstim)
library(incidence)

#####AGES Data######
df_alter<-read.csv2("https://covid19-dashboard.ages.at/data/CovidFaelle_Altersgruppe.csv")
df_zeitreihe_at<-read.csv2("https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv")

df_zeitreihe_at$Time<-as.Date(as.character(df_zeitreihe_at$Time), "%d.%m.%Y")


df_spital_AT<-read.csv2("https://covid19-dashboard.ages.at/data/CovidFallzahlen.csv")
df_spital_AT$Meldedat<-as.Date(as.character(df_spital_AT$Meldedat), "%d.%m.%Y")

df_AT_Inzidenz<-df_zeitreihe_at%>%
  subset(BundeslandID==10)

plot(as.incidence(df_AT_Inzidenz$AnzahlFaelle, dates = df_AT_Inzidenz$Time))

#Esimate R
#https://cran.r-project.org/web/packages/EpiEstim/vignettes/demo.html

T <- nrow(df_AT_Inzidenz)
t_start <- seq(2, T-13)
t_end <- t_start + 13

#values for serial interval taken from
#https://wissenaktuell.ages.at/download/0/0/840fb2b85eb49b8f744d8d971614125ae197ab10/fileadmin/AGES2015/Wissen-Aktuell/COVID19/serial_interval_update2021_2021-06-14.pdf

res_parametric_si <- estimate_R(df_AT_Inzidenz$AnzahlFaelle,
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.46,
                                  std_si = 2.63,
                                  t_start = t_start,
                                  t_end = t_end)))
plot(res_parametric_si, legend = FALSE)
plot(res_parametric_si, "R")
head(res_parametric_si$R)
View(res_parametric_si$R)

#updated values
res_parametric_si_updated <- estimate_R(df_AT_Inzidenz$AnzahlFaelle,
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 3.37,
                                  std_si = 1.83,
                                  t_start = t_start,
                                  t_end = t_end)))
plot(res_parametric_si_updated, legend = FALSE)
plot(res_parametric_si_updated, "R")
