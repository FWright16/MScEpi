# Exploratory Analysis

# -------------------------------------------------------------
# 1. Overall national trends year by year
# -------------------------------------------------------------

national_data <- analysis_data %>% 
  group_by(YEAR) %>% 
  summarise(
    total_items = sum(items),
    total_pop = sum(pop),
    rate_per_1000 = total_items / total_pop * 1000,
    .groups = 'drop'
  )

#national_data

ggplot(national_data, aes(x=YEAR,y=rate_per_1000)) +
  geom_line(size=1)+
  geom_point()+
  labs(
    title = 'National Antibiotic Prescribing Rate (per 1000)',
    y='Rate per 1000',
    x='Year'
  ) + theme_minimal()

# -------------------------------------------------------------
# 2. Variation across ICBs
# -------------------------------------------------------------

icb_year_summary <- analysis_data %>% 
  group_by(ICB_CODE,YEAR) %>% 
  summarise(
    total_items=sum(items),
    total_pop=sum(pop),
    rate=total_items/total_pop*1000,
    .groups='drop'
  )

#summary(icb_year_summary$rate) # Gives crude overall rate not accounting for age, sex, BNF antibiotic

ggplot(icb_year_summary,aes(x=factor(YEAR),y=rate))+
  geom_boxplot()+
  labs(
    title='Distribution of Prescribing Rates Across ICBs',
    x='Year',
    y='Rate per 1000'
  )+ theme_minimal()

# -------------------------------------------------------------
# 3. Age differences
# -------------------------------------------------------------

age_summary <- analysis_data %>% 
  group_by(age_group) %>% 
  summarise(mean_rate=mean(rate_per_1000),.groups='drop') %>% 
  arrange(mean_rate)
  

ggplot(age_summary,aes(x=age_group,y=mean_rate))+
  geom_col() +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(
    title = 'Mean Prescribing Rate by Age Group',
    y='Mean rate per 1000',
    x='Age group'
  )

# -------------------------------------------------------------
# 4. Sex differences
# -------------------------------------------------------------

sex_summary <- analysis_data %>% 
  group_by(sex) %>% 
  summarise(mean_rate=mean(rate_per_1000),.groups='drop')

#sex_summary  

ggplot(sex_summary,aes(x=sex,y=mean_rate,fill=sex))+
  geom_col()+
  theme_minimal()+
  labs(
    title='Mean Prescribing Rate by Sex',
    y='Mean rate per 1000'
  )+ guides(fill='none')

# -------------------------------------------------------------
# 5. Age X Sex interaction
# -------------------------------------------------------------
  
age_sex_summary <- analysis_data %>% 
  group_by(age_group,sex) %>% 
  summarise(mean_rate=mean(rate_per_1000),.groups='drop')

ggplot(age_sex_summary,aes(x=age_group,y=mean_rate,colour = sex,group=sex))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  labs(
    title='Prescribing Rates by Age and Sex',
    y='Mean rate per 1000',
    x='Age group'
  )

# -------------------------------------------------------------
# 6. Distributions Checks
# -------------------------------------------------------------

ggplot(analysis_data, aes(x = items)) +
  geom_histogram(bins = 50) +
  theme_minimal()

mean(analysis_data$items) # 17355.19
var(analysis_data$items) # 151157381

# -------------------------------------------------------------
# 7. Distributions Checks outliers?
# -------------------------------------------------------------

analysis_data %>%
  arrange(desc(rate_per_1000)) %>%
  select(ICB_NAME, YEAR, sex, age_group, rate_per_1000) %>%
  head(10) # 1735

analysis_data %>%
  arrange(rate_per_1000) %>%
  select(ICB_NAME, YEAR, sex, age_group, rate_per_1000) %>%
  head(10) # 105

summary(analysis_data$rate_per_1000)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 104.6   286.7   502.8   552.8   720.1  1735.1 

# -------------------------------------------------------------
# 8. Spatial
# -------------------------------------------------------------

icb_mean <- icb_year_summary %>%
  group_by(ICB_CODE) %>%
  summarise(mean_rate = mean(rate), .groups = "drop") %>%
  arrange(desc(mean_rate))

#icb_mean

# -------------------------------------------------------------
# 9. Checks
# -------------------------------------------------------------

# sum(analysis_data$pop == 0)
# sum(is.na(analysis_data$items))
# sum(is.na(analysis_data$pop))

# analysis_data %>%
#   filter(items / pop > 2) # Checks if ratios are above 2
