setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

CMV_data <- read.csv('CMV_data.csv')
nonparametric_data <- read.csv('nonparametric.csv')

# 2 Explore the data

mean(CMV_data$cmv, na.rm=TRUE)
sd(CMV_data$cmv, na.rm=TRUE)

CMV_data %>%
  pull(cmv) %>%
  hist(,main='Frequency Histogram',xlab='CMV')
#Not normally distributed 

CMV_data$log_cmv <- log(CMV_data$cmv)

hist(CMV_data$log_cmv, 
     main = "Histogram of log(CMV)", 
     xlab = "log(CMV)", 
     col = "lightblue", 
     breaks = 20)
#Histogram for the logged data


qqnorm(CMV_data$log_cmv)
qqline(CMV_data$log_cmv, col = "red")
#Q-Q plot to assess normality visually

# 3 Testing for differences

CMV_data %>% 
  filter(sex=='Male') %>% 
  pull(cmv) %>% 
  hist(,main='Males',xlab='CMV IgG OD')



CMV_data %>% 
  filter(sex=='Female') %>% 
  pull(cmv) %>% 
  hist(,main='Males',xlab='CMV IgG OD')

CMV_data %>%
  filter(sex=='Male') %>% 
  summarise(median_cmv = median(cmv,na.rm=TRUE))
#0.9835

CMV_data %>%
  filter(sex=='Female') %>% 
  summarise(median = median(cmv))
#1.0165

#Both the male and female histogram follow the same
#distribution, with the male plot having a higher frequency
table(CMV_data$sex)

wilcox.test(cmv~sex,data=CMV_data)
#W = 247215, p-value = 0.06844
#We are testing whether the central tendency (like median) of cmv is different between sexes

table(CMV_data$tb2)
#Non TB 1351 
#TB 17

CMV_data %>%
  filter(tb2=='TB') %>% 
  summarise(median_cmv = median(cmv,na.rm=TRUE))
#Median with TB is 1.357

CMV_data %>%
  filter(tb2=='Non TB') %>% 
  summarise(median_cmv = median(cmv,na.rm=TRUE))
#Median without TB is 0.996

wilcox.test(cmv~tb2,data=CMV_data)
#W = 6337, p-value = 0.001477
#There is a difference in the central tendency (median) 
#of an individual's level of CMV between people with and without TB

# 4 Testing a simple sample

#options(digits=3)

wilcox.test(CMV_data$cmv,mu=1)
#V = 472734, p-value = 0.7207

wTB <- CMV_data %>% 
  filter(tb2=='TB')

wilcox.test(wTB$sbp,mu=120) #exact = FALSE skips the correction
#V = 34.5, p-value = 0.1727

#Due to a p-value of 0.1727, there is little evidence that individual's diagnosed with TB have a sbp of 120

# 5 Paired data

hist(nonparametric_data$sfa, main='SFA', xlab='Skinfold thickness', bin =5)
hist(nonparametric_data$sfb, main='SFB', xlab='Skinfold thickness', bin =5)

wilcox.test(nonparametric_data$sfa, nonparametric_data$sfb, paired=TRUE)
#V = 110, p-value = 0.002625
#p-value is low (so accept alt hypothesis) so strong evidence
#that there is a difference between the 2 observers

t.test(nonparametric_data$sfa, nonparametric_data$sfb, paired=TRUE)

differences <- nonparametric_data$sfa - nonparametric_data$sfb
shapiro.test(differences)
qqnorm(differences); qqline(differences)
#for the shapiro test, as the p-value is greater than 0.05
#fail to reject the H0 that the data is normally distributed
#no evidence that differences are non-normal
# eg differences are reasonably normally distributed