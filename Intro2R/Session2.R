library(rio)
life_expectancy <- read.csv('/Users/finnwright16/Desktop/STuDy/MScEpi/data/life_expectancy.csv')
head(life_expectancy)
nrow(life_expectancy)
class(life_expectancy)
mean(life_expectancy$pop_100k, na.rm=TRUE)
median(life_expectancy$health_expenditure_usd,na.rm=TRUE)
min()
max()
sd()
IQR()
summary(life_expectancy)
table(life_expectancy)


is.na(life_expectancy$health_expenditure_usd)
#TRUE means its NA

mean(life_expectancy[life_expectancy$region=='Europe and Central Asia',]$life_expectancy_years, na.rm=TRUE)
#Gives average life expectancy in Europe and Central Asia

mean(life_expectancy[life_expectancy$region=='East Asia and Pacific',]$life_expectancy_years, na.rm=TRUE)

mean(life_expectancy[life_expectancy$region=='East Asia and Pacific',]$health_expenditure_usd, na.rm=TRUE)

table(is.na(life_expectancy[life_expectancy$region=='East Asia and Pacific',]$health_expenditure_usd))
#Number of NA entries in East Asia and Pacific for health expenditure

life_expectancy[order(life_expectancy$life_expectancy_years,decreasing=TRUE),]

head(life_expectancy[order(life_expectancy$life_expectancy_years,decreasing=TRUE),])


life_exp_asia_pacific <- life_expectancy[life_expectancy$region=='East Asia and Pacific',]

life_exp_asia_pacific[order(life_exp_asia_pacific$life_expectancy_years,decreasing=TRUE),]

measles_cases <- read.csv('/Users/finnwright16/Desktop/STuDy/MScEpi/data/measles_per_year.csv')

nrow(measles_cases)
colnames(measles_cases)
colnames(measles_cases) <- c('region','year','cases')
str(measles_cases)
table(is.na(measles_cases$cases))
median(measles_cases$cases)
max(measles_cases$cases)
min(measles_cases$cases)
IQR(measles_cases$cases)

max(measles_cases[measles_cases$region=='Europe',]$cases,na.rm=TRUE)

europe_measles <- measles_cases[measles_cases$region=='Europe',]
europe_measles[order(europe_measles)]




