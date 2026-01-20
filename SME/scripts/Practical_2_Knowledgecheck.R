############################################################
# 🧠 WHITEHALL PRACTICAL REVIEW — POISSON REGRESSION
# Dataset: whitehall (N = 1677)
# Goal: Understand mortality rates using Poisson regression
############################################################


##############################
# SECTION 1: DATA PREPARATION
##############################

# 1️⃣ Calculate follow-up time (in years) for each participant
# Hint: use difftime() or as.numeric() on Date columns timein and timeout
# Example: followup_years <- as.numeric(difftime(timeout, timein, units = "days")) / 365.25

whitehall <- whitehall |> 
  mutate(followup_years = as.numeric(timeout-timein)/365.25)

# 2️⃣ Create a new column 'followup_years' and summarize its mean, min, and max
summary(whitehall$followup_years)
#mean = 16.46 years of follow-up
#min = A participant had 0.151 years of follow-up (the lowest)
#max = A participant had 19.38 years of follow-up (the highest)


# 3️⃣ Using summarise(), find total deaths (all) and total person-years
# Hint: use summarise(total_deaths = sum(all), total_py = sum(followup_years))
total_deaths <- sum(whitehall$all) #403 deaths from all-cause mortality 
total_followup <- sum(whitehall$followup_years) #27605.37 total person-years

# 🧠 Logic Q:
# Why do we divide the number of deaths by total person-years 
# rather than just the number of people when calculating a rate?
Because we want to calculate rate, number of deaths / number of people would just be risk of death


######################################
# SECTION 2: CRUDE MORTALITY ESTIMATE
######################################

# 4️⃣ Compute the overall mortality rate per 1000 person-years manually
rate_per_1000 <- (sum(whitehall$all) / sum(whitehall$followup_years)) * 1000


# 5️⃣ Fit an intercept-only Poisson model for overall mortality rate
model_overall <- glm(whitehall$all ~ 1 + offset(log(whitehall$followup_years/1000)), 
                      family = poisson, data = whitehall)

# 6️⃣ Extract and exponentiate the intercept to get the rate per 1000 PY
 exp(coef(model_overall)) # intercept (crude mortality estimate) was 14.6 person-years 

# 7️⃣ Compare your manual rate with the Poisson model’s result — are they the same?
 #Yes they're the same

# 🧠 Logic Q:
# Why is an offset(log(followup_years)) used in the model,
# and why is it on the log scale?
# log scale as poisson distribution works in logarithms, and the offset takes into account that each participant has different follow-up lengths



#########################################
# SECTION 3: STRATIFIED RATE COMPARISONS
#########################################

# 8️⃣ Calculate mortality rate per 1000 PY by smoking status (smok)
 whitehall |> group_by(smok) |> 
   summarise(deaths = sum(all),
             py = sum(followup_years),
             rate_per_1000 = (deaths / py) * 1000)

# 9️⃣ Fit Poisson regression model with smoking as a covariate
 model_smoke <- glm(all ~ factor(smok) + offset(log(followup_years)), 
                    family = poisson, data = whitehall)
 summary(model_smoke)

# 🔟 Extract and interpret Incidence Rate Ratios (IRRs)
 exp(coef(model_smoke))

# 1️⃣1️⃣ Repeat for cholesterol group (cholgrp) and SBP group (sbpgrp)

# 🧠 Logic Q:
# What does an IRR of 1.5 for smokers mean in this study context?



##########################################
# SECTION 4: MULTIVARIABLE MODEL BUILDING
##########################################

# 1️⃣2️⃣ Fit a multivariable Poisson regression model including age, smoking, and cholgrp
# model_full <- glm(all ~ agein + factor(smok) + factor(cholgrp) + 
#                   offset(log(followup_years)), family = poisson, data = whitehall)

# 1️⃣3️⃣ Use summary(model_full) to check which variables are significant

# 1️⃣4️⃣ Compare model AIC between intercept-only and multivariable models
# AIC(model_overall, model_full)

# 🧠 Logic Q:
# Why is Poisson regression preferred over linear regression for count data?



######################################
# SECTION 5: MODEL DIAGNOSTICS & CHECK
######################################

# 1️⃣5️⃣ Check for overdispersion
# overdispersion_value <- summary(model_full)$deviance / summary(model_full)$df.residual
# overdispersion_value

# 1️⃣6️⃣ If overdispersed (>1.5), refit using quasi-Poisson or negative binomial model
# library(MASS)
# model_nb <- glm.nb(all ~ agein + factor(smok) + factor(cholgrp) + offset(log(followup_years)), data = whitehall)

# 1️⃣7️⃣ Plot predicted vs observed rates using ggplot2
# whitehall$predicted <- predict(model_full, type = "response")
# ggplot(whitehall, aes(x = predicted, y = all)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

# 🧠 Logic Q:
# What assumption does Poisson regression make about the relationship between the mean and variance of counts?



##############################
# ⚔️ BONUS CHALLENGE
##############################

# 1️⃣8️⃣ Write a function to calculate Poisson rate per 1000 PY with CI
# fit_poisson_rate <- function(data, outcome, exposure, covariate = NULL) {
#   formula <- as.formula(paste(outcome, "~", 
#                               ifelse(is.null(covariate), "1", covariate), 
#                               "+ offset(log(", exposure, "))"))
#   model <- glm(formula, family = poisson, data = data)
#   broom::tidy(model, conf.int = TRUE) |> 
#     mutate(across(c(estimate, conf.low, conf.high), \(x) exp(x) * 1000))
# }

# 🧠 Logic Q:
# How does adding covariates in Poisson regression adjust for confounding?



############################################################
# END OF WHITEHALL PRACTICAL REVIEW 🧙‍♂️
# Great work — now try running each block to test yourself!
############################################################