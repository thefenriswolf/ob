# standard stuff
library(tidyverse)
library(ggplot2)
library(readr)
# medical dataset
library(ISwR)

# Power of a test, if 2 samples are drawn from the same distribution they should
# not differ. If we do this often (1000x) and count the significant results
# we get the power of a test
# we should not get significant results: Type 1 error / false positive
# but the error is ~50 out of 1000, which is 5% so equal to alpha
# alpha is the probability for a type 1 error
# reproduceability is important, significance could be because of a false positive
for(j in 1:1000){
  if(j==1){counter<-0}
  # two identical sets
  df_set1<-rnorm(30, mean=0, sd=1)
  df_set2<-rnorm(30, mean=0, sd=1)
  # do a t-test
  t.result<-t.test(df_set1, df_set2)
  if(t.result$p.value<0.05){counter=counter+1}
}

# type 2 error: false null hypothesis is not detected
# returns only 60-80 correct results even though the samples are almost identical
# less likely to get the error if the sample size is larger and the difference is 
# more pronounced
for(j in 1:1000){
  if(j==1){counter<-0}
  # two slightly different sets
  df_set1<-rnorm(30, mean=0.1, sd=1)
  df_set2<-rnorm(30, mean=0, sd=1)
  # do a t-test
  t.result<-t.test(df_set1, df_set2, var.equal = TRUE)
  if(t.result$p.value<0.05){counter=counter+1}
}

# so lets increase the number of samples
# returns 114-118 correct results, so a improvement
for(j in 1:1000){
  if(j==1){counter<-0}
  # two slightly different sets
  df_set1<-rnorm(120, mean=0.1, sd=1)
  df_set2<-rnorm(120, mean=0, sd=1)
  # do a t-test
  t.result<-t.test(df_set1, df_set2, var.equal = TRUE)
  if(t.result$p.value<0.05){counter=counter+1}
}
# example for this: 2 sports players which are almost equally good but the variance
# is high, meaning they don't play as good as they could every day.
# so to determine who is actually the better player we need a lot of matches

# now lets eliminate the error
# returns 963 correct samples, the null hypothesis can be rejected
for(j in 1:1000){
  if(j==1){counter<-0}
  # two slightly different sets
  df_set1<-rnorm(3000, mean=0.1, sd=1)
  df_set2<-rnorm(3000, mean=0, sd=1)
  # do a t-test
  t.result<-t.test(df_set1, df_set2, var.equal = TRUE)
  if(t.result$p.value<0.05){counter=counter+1}
}

# power of the test is the ability to identify differences between two groups
# or how likely it is that the test identifies a significant difference if there is one
# depends on: number of samples, difference between samples, 


# power of a test computed by R
# requirements: delta, standard deviation, significance level
power.t.test(delta=0.5, sd=2, sig.level = 0.05, power=0.9)
# returns n: number of samples needed in each group
# for a larger significance level a smaller number is needed
# type 1 and type 2 errors are linked inversely, if one decreases the other increases

# examples:
power.t.test(delta=0.1, sd=1, sig.level = 0.05, n=30) # pwr 0.05
power.t.test(delta=0.1, sd=1, sig.level = 0.05, n=130) # pwr 0.12
power.t.test(delta=0.1, sd=1, sig.level = 0.05, n=3000) # pwr 0.97

#RESUME:00:48:00


