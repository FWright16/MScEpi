library(rio)
library(dplyr)

depress <- read.csv('/Users/finnwright16/Desktop/STuDy/MScEpi/data/depress.csv')
str(depress)

plot(depress$children, depress$depscore, xlab='Number of Children',ylab='Depression',cex=0.3)

#using jitter to move each dot slightly to view data better
plot(jitter(depress$children),jitter(depress$depscore),xlab='Number of Children',ylab='Depression',cex=0.3)

#simple regression and resulting line
model_dep <- lm(depscore ~ children, data=depress)
summary(model_dep)
confint(model_dep)

abline(lm(depress$depscore ~ depress$children))

# Q1) (a) outcome is depression and number of children is the exposure
# (b) Depression = alpha (5.95) + beta(0.38)(number of children) + residual error
# (c) Positive
# (d) For every child, the depression score increases by 0.38
# (e) As the p-value is < 0.00001 very strong evidence

###########################

depress %>% 
  group_by(any_prolapse) %>% 
  summarise(mean = mean(depscore))
#mean depression score if a prolapse (7.78) and without (6.68)
t.test(depscore ~ any_prolapse, data = depress,var.equal=TRUE)
#t-test to show that there is a statistical difference between the 2 group

#Use as.factor so R knows that the exposure variable is categorical not numerical

pro_model <- lm(depscore ~ as.factor(any_prolapse), data=depress)
summary(pro_model)
#WHEN INTERPRETING COEFFICIENTS, ALWAYS INCLUDE THE DIRECTION OF EFFECT
#depscore = alpha + beta(any_prolapse?)

###############

table(depress$type_prolapse)
#0 = no prolapse (620)
#1 = moderate prolapse (304)
#2 = severe prolapse (142)

boxplot(depscore ~ type_prolapse, data=depress)
#Q2) not a strong pattern, appears that women that have
#not had a prolapse, their depscore is lower

pro_t_model <- lm(depscore ~ as.factor(type_prolapse),data=depress)
summary(pro_t_model)
#type 1 1.02, type 2 1.26 BOTH COMPARED TO NO PROLAPSE
#“Women with history of a moderate prolapse have a mean depression score 1.02 points higher compared to women who have never had a prolapse. Women who had experienced a severe prolapse had a mean depression score 1.26 points higher than women with no prolapse.”

#Q3) beta 1 is 0 compared to 1, beta 2 is 0 compared to 2

#If we did wish to test the entire “type_prolapse” variable, we can test the null hypothesis that both β1 and β2 are equal to 0 using a global Wald test

#Wald test
library(lmtest)
waldtest(pro_t_model, terms='as.factor(type_prolapse)')

#Q4) There is strong evidence of an overall association between
#type of prolapse and depression score.
# In other words, depression scores differ across the prolapse
# types; at least one category has a mean that is significantly
# different from the others




