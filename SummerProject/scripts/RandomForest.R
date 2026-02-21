install.packages('ranger')
install.packages('pdp')
library(ranger)
library(dplyr)
library(pdp)

# -------------------------------------------------------------
# 1. Prepare dataset
# -------------------------------------------------------------

rf_data <- analysis_data %>%
  dplyr::select(rate_per_1000,
         IMD_quintile,
         prop_asian,
         YEAR,
         age_group,
         sex) %>%
  mutate(
    IMD_quintile = factor(IMD_quintile),
    YEAR = factor(YEAR),
    age_group = factor(age_group),
    sex = factor(sex)
  )

#view(rf_data)

# -------------------------------------------------------------
# 2. Fit random forest
# -------------------------------------------------------------

rf_model <- ranger(
  rate_per_1000 ~ IMD_quintile + prop_asian + YEAR + age_group + sex,
  data = rf_data,
  importance = "permutation",
  num.trees = 500,
  seed = 123
)

importance(rf_model)
sort(importance(rf_model), decreasing = TRUE)

rf_model_no_age <- ranger(
  rate_per_1000 ~ IMD_quintile + prop_asian + YEAR + sex,
  data = rf_data,
  importance = "permutation",
  num.trees = 500
)
sort(importance(rf_model_no_age), decreasing = TRUE)

# -------------------------------------------------------------
# 3. Partial dependence plots
# -------------------------------------------------------------

pdp::partial(rf_model,pred.var='prop_asian')


