#hypertensive =1, not =0, male =1 female=0
getwd()
library(rio)
bab9 <- read.csv('bab9.csv')
summary(bab9)
str(bab9)
head(bab9)

summary(bab9$bweight)
mean(bab9$bweight)
sd(bab9$bweight)

hist(bab9$bweight, main='Frequency Histogram',xlab='Birthweight (g)')
hist(bab9$bweight,main='Density Histogram',xlab='Birthweight (g)', freq = FALSE)
curve(dnorm(x,mean=mean(bab9$bweight),sd=sd(bab9$bweight)),add=TRUE) #superimpose a normal distribution onto density histogram

#distributions of only male babies
bab9 %>%
  filter(sex=='1') %>%
  pull(bweight) %>%
  hist(,main='Frequency Histogram (boys)',xlab='Birthweight (g)')

table(bab9$sex) #number of male (1) and female (2) babies

#par() sets or gets graphical parameters that control the appearance of plots in base R
par(mfrow=c(1,2))
bab9 %>%
  filter(sex=='1') %>%
  pull(bweight) %>%
  hist(,main='Frequency Histogram (boys)',xlab='Birthweight (g)')
bab9 %>%
  filter(sex=='2') %>%
  pull(bweight) %>%
  hist(ylim = c(0,120),main='Frequency Histogram (girls)',xlab='Birthweight (g)')
#Try ggplot2 to superimpose them

# 4 Confidence intervals for mean

#create function for confidence intervals for the mean
mean_ci <- function(x,conf.level=0.95){
  n<-length(x)
  m<-mean(x)
  se<-sd(x)/sqrt(n)
  error<-qt(conf.level/2+0.5,n-1)*se
  c(lower=m-error,mean=m,upper=m+error)
}

library(Hmisc)
mean_ci_results <- smean.cl.normal(bab9$bweight)
mean_ci_results

#gives confidence interval and mean
mean_ci(bab9$bweight)
mean_ci(bab9$gestwks)
mean_ci(bab9$m_age)

#altering the confidence level
mean_ci(bab9$bweight,conf.level=0.99)
mean_ci(bab9$gestwks,conf.level=0.99)
mean_ci(bab9$m_age,conf.level=0.99)

#option and dash to make <-
# cmd shift m to make %>% 

# 5 Distribution of the response variable across subgroups

#Birthweight mean for each sex, 1=male, 2=female
bab9 %>% 
  group_by(sex) %>% 
  summarise(mean_bweight=mean(bweight,na.rm=TRUE))

#boxplot
par(pin = c(1, 2))
boxplot(bweight~sex,data = bab9,ylab='Birthweight (g)')

# 6 Comparing means

# one-sample test, is the true mean equal to a specific value
t.test(bab9$bweight, mu=3300)
# alt hypothesis, true mean does not equal 3300
#two-sided as don't care whether higher or lower
#t=-7   df=640   p-value=7e-11
#Strong confidence that the true mean does not equal exactly 3300

#test whether the mean bweight is the same in each sex
t.test(bab9$bweight~bab9$sex)
# alt hypothesis, true difference between sexes is not 0 (eg there is a difference)
#t=-3.2686   df=638.65   p-value=0.001139
#Good confidence that there is a difference between the bweight of male and female babies

#
var.test(bab9$bweight~bab9$sex)

t.test(bab9$bweight~bab9$sex, var.equal=FALSE)



#t.test(x,mu=val)
#One-sample t-test
#comparing one mean to a known value
#normal distribution and independent data

#t.test(y~group)
#Two-sample t-test (independent groups)
#comparing means between groups
#normal distribution and equal variances/sd

#var.test(y~group)
#Variance test (F-test)
#testing if 2 groups have equal variance
#normal distribution and independent groups

#t.test(y~group, var.equal=FALSE)
#Welch's t-test
#comparing 2 means without assuming equal sd
#normal distribution and unequal variances/sd


