library(haven)
library(tidyverse)
library(gtsummary)
library(here)
library(lmtest)
library(emmeans)


mortality <- read_dta(here("SME/data/mortality.dta"))
mortality <- mortality |> mutate(across(where(is.labelled), as_factor))

#Preliminary analysis
nrow(mortality)
summary(mortality)
#Education chosen as the proxy for SES as it can influence both vimp and died but is unlikely to be caused by either. Commonly used in epidemiology as an SES proxy

table(
  VisualImpairment=factor(mortality$vimp,labels=c('Normal','Impaired')),
  Death=factor(mortality$died,labels=c('Alive','Dead'))
)

summary(mortality$age)
table(Sex=factor(mortality$sex,labels=c('Male','Female')))
table(mortality$education_cat)
table(mortality$occupation)
sum(is.na(mortality$education))
sum(is.na(mortality$occupation))
sum(is.na(mortality$district))
(2613/4298)*100

hist(mortality$age)

mortality <- mortality |> 
  mutate(education_cat = as.factor(
    case_when(
      education == 'No formal education' ~ 'None',
      education == 'Koranic education only' ~ 'Any formal',
      education == 'Adult education only' ~ 'Any formal',
      education == 'Primary' ~ 'Any formal',
      education == 'Secondary' ~ 'Any formal',
      education == 'Post Secondary' ~ 'Any formal',
    )
  ))



#General Association
mortality |> 
  count(vimp,died) |> 
  group_by(vimp) |> 
  mutate(percentage = n/sum(n) *100,
         n_pct=sprintf('%d (%.1f%%)',n,percentage)) |> 
  select(vimp,died,n_pct) |> 
  pivot_wider(names_from=died,values_from=n_pct)

tab_vimp <-table(mortality$vimp,mortality$died)

source(here("SME", "functions", "or_function.R"))
source(here("SME", "functions", "mh_functions_updated.R"))
calculate_or(tab_vimp)

#Does education_cat appear to be associated with death?
mortality |> 
  tbl_cross(row=agegrp,col=vimp,percent='row')

mod1 <- glm(died~vimp,data=mortality,family=binomial)
summary(mod1)
exp(1.717)

mortality <- mortality |> 
  mutate(
    died=as.factor(died),
    vimp=as.factor(vimp),
    agegrp=as.factor(agegrp))
  )
mhor(mortality,outcome='died',exposure='vimp',strata='agegrp')





