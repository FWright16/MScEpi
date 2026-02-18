library(haven)
library(tidyverse)
library(gtsummary)
library(here)
library(lmtest)
library(emmeans)
library(table1)

hiv <- read_dta(here("SME/data/SMEassessment2026.dta"))
hiv <- hiv |> mutate(across(where(is.labelled), as_factor))
source(here("SME/functions/mh_functions_updated.R"))

#PRELIMINARY ANALYSIS

nrow(data)
#1769 observations

str(data)
#as we mutated to as.factor, all variables are factors

summary(is.na(data))
#cd4grp4 is the only variable with missing data, but we're not investigating this variable so its fine

data <- data %>% 
  mutate(
    agegp6 = fct_recode(agegp6,
        '<25'='<25',
        '25-29'='25-29',
        '30-34'='30-34',
        '35-44'='35-44',
        '45+'='45-54',
        '45+'='55+'
    )
  )

summary(hiv)


data |> 
  tbl_cross(row=reastest2,col=ARTstart,percent='row')


mod0 <- glm(ARTstart ~ 1,family=binomial, data=data)
mod1 <- glm(ARTstart ~ ruralsite,family=binomial,data=data)
tbl_regression(mod1,exponentiate = TRUE)
lrtest(mod0,mod1)

#Univariable
mod_crude <- glm(ARTstart ~ empstatus, family = binomial, data=data)
summary(mod_crude)
tbl_regression(mod_crude, exponentiate = TRUE)

#Stratified by age

mhor(data,outcome = 'ARTstart',exposure='empstatus',strata='agegp6')

#Stratified by sex

mhor(data,outcome = 'ARTstart',exposure='empstatus',strata='sex')

#Stratified by Rurality

mhor(data,outcome = 'ARTstart',exposure='empstatus',strata='ruralsite')

#Stratified by 
mhor(data, outcome = "ARTstart", exposure = "empstatus", strata = "educ")

hiv %>% 
  filter(agegp6=='55+') %>% 
  summarise(n_started_ART=sum(ARTstart=='yes'))


#full multivariable model
model1 <- glm(ARTstart ~ empstatus + educ + agegp6 + sex + ruralsite, 
              family = binomial, data = data)
summary(model1)
tbl_regression(model1,exponentiate = TRUE)
model1_null <-glm(ARTstart ~ educ + agegp6 + sex + ruralsite, 
                  family = binomial, data = data)
lrtest(model1_null,model1)

#multivariable model with an interaction term
model2 <- glm(ARTstart ~ empstatus * educ + agegp6 + sex + ruralsite, 
              family = binomial, data = data)
summary(model2)
tbl_regression(model2,exponentiate = TRUE)

lrtest(model1,model2)

emmeans(model2,revpairwise ~ empstatus | educ,type='response') %>% 
  pluck('contrasts') %>% confint()

#Collinearity 
library(car)

model <- glm(ARTstart ~ empstatus + agegp6 + sex + educ + ruralsite, 
             family = binomial, data = data)

vif(model)
