library(rio)
library(tidyr)
library(DescTools)
library(dplyr)

creche_data <- read.csv('/Users/finnwright16/Desktop/STuDy/MScEpi/data/creche.csv')
#socio 0=average, 1=very low
#creche 0=cared for at home, 1=attends creche
#lrti 0=no LRTI, 1=LRTI

creche_lrti_table <-CrossTable(creche_data$lrti, creche_data$creche) 
chisq.test(creche_data$lrti,creche_data$creche)

#Q1 There is a strong evidence of a statistical
# association between creche attendance and 
# LRTI 

creche_socio_table <- CrossTable(creche_data$socio, creche_data$creche)
chisq.test(creche_data$socio, creche_data$creche)
# As p-value is less than 0.001 there is good
# evidence that socioeconomic status is associated with
# creche attendance

lrti_socio_table <- CrossTable(creche_data$socio,creche_data$lrti)
chisq.test(creche_data$socio,creche_data$lrti)
# Strong evidence that socioeconomic status is associated to lrti

#make 2 smaller datasets to filter between
# a binary variable
creche_low <- creche_data %>% 
  filter(socio==1)
CrossTable(creche_low$lrti,creche_low$creche, prop.r=TRUE, chisq=TRUE)

creche_avg <- creche_data %>% 
  filter(socio==0)
CrossTable(creche_avg$lrti,creche_avg$creche, prop.r=TRUE,chisq = TRUE)
# Q3 12.3% 
# Q4 79.2%

#Using R to calculate odds ratios
table(creche_data$lrti, creche_data$creche)
#rows from first variable, columns from second

#Odds among those attending creche
odds_creche <- 27/62
odds_no_creche <- 53/499
odds_ratio <- odds_creche / odds_no_creche

#Q5 Odds of having lrti w no creche = 0.106
#Odds of having lrti w creche = 0.435
#OR = 4.1
#Over 4 times greater odds of having lrti
# for children that attend a creche than those
# that don't

OddsRatio(table(creche_data$lrti, creche_data$creche),conf.level=0.95)

#OR of creche on LRTI for lower socio
OddsRatio(table(creche_low$lrti,creche_low$creche),conf.level=0.95)
#Odds ratio 3.46

#OR of creche on LRTI for average socio
OddsRatio(table(creche_avg$lrti,creche_avg$creche),conf.level=0.95)
#Odds ratio 3.45

#Pooled odds ratio 
mantelhaen.test(creche_data$lrti, creche_data$creche, creche_data$socio, conf.level=0.95)
#Response variable (lrti) first and must be 0/1
#variable which defines the categories being compared comes next
#Variable being controlled for by stratifying comes last

#Mantel Haenszel OR controlling for socio = 3.46

#Test for homogeneity of ORs to test the hypothesis that the ORs in the 2 social class strata are the same
#i.e. test for effect modification - use the xtabs function to assemble the relevant info and the WoolfTest

tab1 <- xtabs(~lrti + creche + socio, data=creche_data)
WoolfTest(tab1)

#Q6/Q7 Socioeconomic status does have an effect on LRTI as both stratified ORs are verys imilar but different to the crude
# Q8 it has decreased as socio was a positive confounder as it inflated the crude OR and made the effect between creche attendance and lrti greater
#Q9 1.72 and 6.96 greater than 1 so is significant 
 
