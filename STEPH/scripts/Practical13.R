getwd()

library(rio)
library(dplyr)

bab9 <- read.csv('/Users/finnwright16/Desktop/STuDy/MScEpi/data/bab9.csv')

summary(bab9$bweight)
hist(bab9$bweight)

#Scatter plot
plot(bab9$gestwks, bab9$bweight, xlab='Gestation (weeks)',ylab='Birthweight')
#Alt
plot(bab9$gestwks,bab9$bweight,xlab='Gestation (weeks)',ylab='Birthweight',
     cex=0.3)

#line of best fit
abline(lm(bab9$bweight ~ bab9$gestwks))

#Q1 Yes, but it doesn't explain the varying concentrations of points eg high concentration in the top right

#Makes linear model
model_1 <- lm(bweight ~ gestwks, data=bab9)
summary(model_1)
#beta is regression coefficient and alpha is y-intercept

#confidence intervals of coefficients
confint(model_1)

#coefficients
coef(model_1)

#Q2 alpha (-4865) beta (207), for every increase in gestational week, birthweight increases by 207g

#Q3 standard error of alpha (290) and beta (7.5), p-value is very low so the coefficients are statistically significant, if the line was more horizontal the p-value (for beta) would be closer to 1 as there would be no correlation between gestwks and bweight

#Add the predicted values for bweight (points on regression line)
bab9 <- bab9 %>% 
  mutate(y=model_1$fitted.values)

plot(bab9$gestwks,bab9$bweight,cex=0.3)
lines(bab9$gestwks,bab9$y) #same as doing lm function to get line

#########
#Now doing the same to see if there is an association between m_age and bweight

model_2 <- lm(bweight ~ m_age, data=bab9)
summary(model_2)
confint(model_2)
coef(model_2)

plot(bab9$m_age,bab9$bweight,xlab='Maternal age',ylab='Birthweight',
     cex=0.3)
abline(lm(bab9$bweight ~ bab9$m_age))

#This association is not strong at all, the regression coefficient is 5.7 yet it has a p-value of 0.394 so very weak evidence
#Furthermore the CI overlaps 0 demonstrating there is no evidence of an association

#To give correlation coeffcient (NOT REGRESSION COEFFICIENT)
cor(bab9$bweight, bab9$gestwks)

#Q5 r=0.74 meaning a strong positive correlation

set.seed(101)
nonlinear <- data.frame(x=1:100 + 2*runif(n=100)) %>% 
  mutate(y=(50-x)^2 + (100* runif(n=100)))

plot(nonlinear$x,nonlinear$y,cex=0.3)
#Gives a quadratic curve
abline(lm(nonlinear$y ~ nonlinear$x))
model_nl <- lm( y~x,data=nonlinear)
summary(model_nl)
#RStudio does plot a regression line, but its not valid here as the a quadratic curve is more appropriate