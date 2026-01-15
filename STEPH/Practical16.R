##### Comparing two proportions

#Example: group 1 outcome of interest (OOI) 10% 
#Group 2 15%
power.prop.test(n=NULL, power=0.8,p1=0.1,p2=0.15,sig.level = 0.05)
#NULL is what we want to know


# Two-sample comparison of proportions power calculation 
# 
# n = 685.5969
# p1 = 0.1
# p2 = 0.15
# sig.level = 0.05
# power = 0.8
# alternative = two.sided
# 
# NOTE: n is number in *each* group

#mathematically sample size is inversely proportional to the clinical difference

#Q1 the sample size would increase if the desired power is increased

##### Comparing two proportions: different group sizes

install.packages('MESS')
library(MESS)

#ratio = second group size / first group size
power_prop_test(n=NULL,power=0.9,p1=0.1,p2=0.15,sig.level=0.05,ratio=2)

#the total sample size will be smaller when we have a greater proportion in the group with the lower expected rate. (This assumes the rate is lower than 50%; more generally, we want more people in the group with the rate furthest from 0.5.)

#####Comparing two means

#To estimate sample sizes required to detect a difference between two means, we need to use the power.t.tests function, and specify the difference between the expected mean values in each group and the standard deviation (SD) of each variable

#To calculate the sample size required to detect a difference of 0.2 kg in mean birth weight, giving 90% power, using base R, we type:
power.t.test(n=NULL,delta=0.2,sd=0.4,power=0.9,sig.level=0.05)

#Q2 Sample size would go up by around 4 times

#Q3 Increase as less variability in the data

##### Comparing two means - specifying the power and ratio of group sizes

#For example, to ensure 80% power to detect the 0.2 kg difference (or more) in mean birth weights using one sample that is 3 times the size of the other, we would type:
power_t_test(n=NULL,delta=0.2,sd=0.4,power=0.8,sig.level=0.05,ratio=3)

##### Estimating study power when sample sizes are known

power.prop.test(n=600,p1=0.1,p2=seq(from=0.11,to=0.21,by=0.01),power=NULL,sig.level=0.05)

power.t.test(n=100,delta=0.2,sd=0.4,power=NULL,sig.level=0.05)

##### Extended Exercise

#Q4 from doc
power.prop.test(n=NULL,p1=0.4,p2=0.3,power=0.8,sig.level=0.05)
#356 per group and so 712 in total

#Q5 5% reduction
power.prop.test(n=NULL,p1=0.4,p2=0.35,power=0.8,sig.level=0.05)
#1470 in each group and so 2940 in total

#Q6 power up to 90%
power.prop.test(n=NULL,p1=0.4,p2=0.3,power=0.9,sig.level=0.05)
#476 in each group and so 952 in total

#Q7 significance level at 1%
power.prop.test(n=NULL,p1=0.4,p2=0.3,power=0.8,sig.level=0.01)
#530 in each group and so 1060 in total

#Q8 from doc
power.prop.test(n=NULL,p1=0.06,p2=0.015,power=0.8,sig.level=0.05)
#279 in each group and so 558 in total

#Q9

#Q10 balance power with significance level


