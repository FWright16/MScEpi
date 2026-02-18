#Descriptive deprivation analysis

# -------------------------------------------------------------
# 1. Creating ICB-year dataset
# -------------------------------------------------------------

icb_year_deprivation <- analysis_data %>% 
  group_by(ICB_CODE,ICB_NAME,YEAR,IMD_quintile) %>% 
  summarise(
    total_items=sum(items),
    total_pop=sum(pop),
    .groups='drop'
  ) %>% 
  mutate(
    rate_per_1000 = total_items/total_pop *1000
  )

#summary(icb_year_deprivation$rate_per_1000)

# -------------------------------------------------------------
# 2. Summary stats
# -------------------------------------------------------------

icb_year_deprivation %>%
  group_by(IMD_quintile) %>%
  summarise(
    mean_rate   = mean(rate_per_1000),
    median_rate = median(rate_per_1000),
    IQR_rate    = IQR(rate_per_1000),
    min_rate    = min(rate_per_1000),
    max_rate    = max(rate_per_1000),
    .groups = "drop"
  )

# -------------------------------------------------------------
# 3. Quintile boxplot
# -------------------------------------------------------------

ggplot(icb_year_deprivation, aes(x = factor(IMD_quintile),
                     y = rate_per_1000)) +
  geom_boxplot() +
  labs(
    x = "IMD Quintile (1 = Most deprived)",
    y = "Prescribing rate per 1,000",
    title = "ICB-level antibiotic prescribing by deprivation quintile"
  ) +
  theme_minimal()

# -------------------------------------------------------------
# 4. How IMD quintiles' mean prescribing rate changes over time
# -------------------------------------------------------------

imd_year <- icb_year_deprivation %>%
  group_by(YEAR, IMD_quintile) %>%
  summarise(
    mean_rate = mean(rate_per_1000),
    .groups = "drop"
  )
ggplot(imd_year, aes(x = YEAR,
                     y = mean_rate,
                     colour = factor(IMD_quintile))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    colour = "IMD Quintile",
    y = "Mean prescribing rate per 1,000",
    title = "Prescribing trends over time by deprivation"
  ) +
  theme_minimal()

# -------------------------------------------------------------
# 5. Poisson model?
# -------------------------------------------------------------

model_simple <- glm(
  total_items ~ factor(IMD_quintile) + factor(YEAR),
  offset = log(total_pop),
  family = poisson(),
  data = icb_year_deprivation
)

summary(model_simple)
exp(coef(model_simple))

# -------------------------------------------------------------
# 6. Checking whether negative binomial is more appropriate
# -------------------------------------------------------------

install.packages('AER')
library(AER)
dispersiontest(model_simple)

# -------------------------------------------------------------
# 7. Negative binomial model
# -------------------------------------------------------------

install.packages('MASS')
library(MASS)

model_nb <- glm.nb(
  total_items ~ factor(IMD_quintile) + factor(YEAR) +
    offset(log(total_pop)),
  data = icb_year_deprivation
)

exp(coef(model_nb))
exp(confint(model_nb))

# -------------------------------------------------------------
# 8. Negative binomial model, adjusted for age and sex
# -------------------------------------------------------------

model_nb_adj <- glm.nb(
  items ~ factor(IMD_quintile) +
    factor(age_group) +
    factor(sex) +
    factor(YEAR) +
    offset(log(pop)),
  data = analysis_data
)

library(broom)

tidy(model_nb_adj, exponentiate = TRUE, conf.int = TRUE)
