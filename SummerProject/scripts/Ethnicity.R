# Exploring ethnicity

# -------------------------------------------------------------
# 1. Is deprivation associated with 
# -------------------------------------------------------------

#Creating one row per ICB

icb_level <- analysis_data %>%
  group_by(ICB_CODE, ICB_NAME, IMD_quintile,
           prop_non_white, eth_quintile,
           prop_asian, asian_quintile) %>%
  summarise(
    total_items = sum(items, na.rm = TRUE),
    total_pop   = sum(pop, na.rm = TRUE),
    rate = total_items / total_pop * 1000,
    .groups = "drop"
  )

#nrow(icb_level) #Check should be 42 rows

ggplot(icb_level,
       aes(x = factor(IMD_quintile),
           y = prop_non_white)) +
  geom_boxplot() +
  labs(
    x = "IMD quintile (1 = most deprived)",
    y = "Proportion non-white"
  )

cor.test(icb_level$IMD_quintile,
         icb_level$prop_non_white,
         method = "spearman")

# Ethnicity unlikely to be a strong confounder of IMD at the ICB level

# -------------------------------------------------------------
# 2. Is ethnicity associated with prescribing 
# -------------------------------------------------------------
# This is combining 2016-2023, in real analysis prob look at each Year instead of aggregating fully

ggplot(icb_level,
       aes(x = factor(eth_quintile), y = rate)) +
  geom_boxplot() +
  labs(
    x = "Ethnicity quintile (1 = lowest % non-white)",
    y = "Prescribing rate per 1,000"
  )

##################

model_asian <- glm.nb(
  items ~ prop_asian + factor(YEAR) + sex + age_group +
    offset(log(pop)),
  data = analysis_data
)

summary(model_asian)
exp(coef(model_asian))
exp(confint(model_asian))

model_asian_q <- glm.nb(
  items ~ factor(asian_quintile) + factor(YEAR) + sex + age_group +
    offset(log(pop)),
  data = analysis_data
)

exp(coef(model_asian_q))
exp(confint(model_asian_q))


model_joint <- glm.nb(
  items ~ factor(IMD_quintile) + prop_asian +
    factor(YEAR) + sex + age_group +
    offset(log(pop)),
  data = analysis_data
)

exp(coef(model_joint))
exp(confint(model_joint))



