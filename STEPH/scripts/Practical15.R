library(rio)
library(dplyr)

depress <- read.csv('/Users/finnwright16/Desktop/STuDy/MScEpi/data/depress.csv')

#Q1) Age could be a confounder as it can cause the outcome, its related to the exposure, and is NOT on the causal pathway

#To see if the potential confounder (age) is related to the exposure (number of children)
plot(depress$age, depress$children, xlab='Age',ylab='Number of Children',cex=0.3)

plot(depress$age,depress$depscore,xlab='Age',ylab='Depression Score',cex=0.3)

model1 <- lm(depscore ~ children, data=depress)
summary(model1)

model2 <-lm(depscore ~ children + age,data=depress)
summary(model2)

confint(model2)

# Q2) Yes age does confound the association between number of children
# and depression, as in model 1 the beta value was 0.38, 
# but in model 2 (which adjusts for age), the same beta 
# value is now -0.0049 (p-value 0.913)

# Q3) -0.0049

#Q4) Think about confounder criteria and DAGs, age can dictate number of children but number of children cannot dictate age 

#Q5) Multiple linear regression?

#Q6) Age (-> depression) -> prolapse -> depression

#Q7)
library(ggplot2)
ggplot(data=depress, aes(x = factor(any_prolapse), y = age)) +
  geom_boxplot() +
  labs(x = "Prolapse (0 = No, 1 = Yes)",
       y = "Age")

#As shown in boxplot, age seems to be related to prolapse as the median for no prolapse (25) is different to with prolaps (34)

###########

#Q8)
boxplot(age ~ type_prolapse,data=depress)
#Generally its likely age confound the association between type of prolapse and depression as it is associated with the exposure (type of prolapse), outcome (depression), and is not on the causal pathway

#model to look at crude association between type of prolapse and depression
model3 <- lm(depscore ~ as.factor(type_prolapse),data=depress)
summary(model3)
#beta for type 1 = 1.02
#beta for type 2 = 1.26

#Adjusting for age
model4 <- lm(depscore ~ as.factor(type_prolapse) + age, data=depress)
summary(model4)
#beta for type 1 = 0.08
#beta for type 2 = 0.34

#The effect of type of prolapse on depression is much reduced when adjusted for age 
#model 4 also shows that age does account for some of the depression seen compared to model 3 alone

