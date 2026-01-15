library(rio)
library(dplyr)

bab <- read.csv('/Users/finnwright16/Desktop/STuDy/MScEpi/data/bab9.csv')
library(gmodels)
library(DescTools)

summary(bab)
head(bab)
dim(bab) #6 columns, 641 rows

#variable which records low or normal birthweight
bab <- bab %>%
  mutate(lbw = if_else(bweight<2500,'1','0'))

table(bab$lbw) #number of lbw and not lbw

bab %>% 
  group_by(lbw) %>% 
  summarise(min = min(bweight),max=max(bweight),mean=mean(bweight),n =length(bweight))
#gives mix,max,mean on lbw group and not lbw group

#below turns the numbers into categories in an additional column
bab$lbw_f <- factor(bab$lbw,levels=c(0,1),labels=c('Normal','Low'))

table(bab$lbw_f)

table(bab$lbw_f,bab$ht) #1=hypertensive


#Q2 27, Q3 53

#below gives column percentages, 2=column percentages
prop.table(table(bab$lbw_f,bab$ht),margin=2)

#Q4 risk of lbw in hypertensive is 0.30337079
#risk of lbw in no hypertensive is 0.09601449
0.30337079-0.09601449
#=0.207 = 20.7%
#Q5 hypertensive 
27/62
#not hypertensive 
53/499
(27/62)/(53/499)
prop.table(table(bab$ht,bab$lbw_f),margin=1) #row percentages

#CrossTable gives both column and row percentages
CrossTable(bab$lbw,bab$ht)
#contents of each cell: frequency, contribution to a chi-square test (O-E)/E, row percentage, column percentage, total percentage
chisq.test(table(bab$lbw_f,bab$ht))

#Q6 28.301, p-value=1.038e-07, strong evidence that hypertensive preganant women are associated with lbw babies

#If you have a small sample use Fisher's exact test
fisher.test(table(bab$lbw_f,bab$ht))

ztest_table <- bab %>% 
  group_by(ht) %>% 
  summarise(n=n(),lbw=sum(lbw))

# lbw was not numeric
# str(bab$lbw)
# bab$lbw <-as.numeric(bab$lbw)

prop.test(x=c(ztest_table$lbw),n=c(ztest_table$n))
#shows 95% CI, and gives estimates eg
#prop1(ht=0)=0.096, 9.6% babies were lbw
#prop2(ht=1)=0.303, 30% babies were lbw

DescTools::RelRisk(table(bab$lbw,bab$ht),conf.level=0.95)
#order of variables import, bab$lbw is outcome, bab$ht is exposure

DescTools::OddsRatio(table(bab$lbw,bab$ht),conf.level=0.95)
#outcome first exposure second

#For both of these the variables need to be 0/1

#Relative risk =1.34
#Odds ratio=4.1

# LINEAR TREND IN PROPORTIONS
hist(bab$gestwks)

bab <- bab %>% 
  mutate(gest5 = ntile(gestwks,5))
table(bab$gest5)

bab %>% 
  group_by(gest5) %>% 
  summarise(min_gestwks=min(gestwks),max_gestwks=max(gestwks),count=n())

prop.table(table(bab$lbw_f,bab$gest5),margin=2)
chisq.test(table(bab$lbw_f,bab$gest5))
#4 df as (2-1) x (5-1) as analysing 2x5 table

#testing whether the risk of lbw changes linearly with quintile of gestational age (test for linear trend)
lbw_by_gest5 <- bab %>% 
  group_by(gest5) %>% 
  summarise(n=n(),lbw=sum(lbw))

prop.trend.test(lbw_by_gest5$lbw,lbw_by_gest5$n)
#null is that the proportions of lbw babies is the same across all quintiles
#df=1 as testing linear trend (one parameter)
#very very low p-value so significant