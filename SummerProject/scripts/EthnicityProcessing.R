#Processing ethnicity data

# -------------------------------------------------------------
# 1. Denominators
# -------------------------------------------------------------

analysis_data %>%
  filter(YEAR == 2021) %>%
  summarise(total_england_pop_2021 = sum(pop, na.rm = TRUE))

# Differences in denominator between Census 2021 data (for ethnicity) = 55 490 075
# and MidYr estimates for 2021 = 56 490 075

# icb_population_all_years %>%
#   filter(YEAR == 2021) %>%
#   group_by(ICB_CODE, ICB_NAME) %>%
#   summarise(total_pop_2021 = sum(pop, na.rm = TRUE), .groups = "drop") %>%
#   summarise(
#     mean_icb_pop_2021 = mean(total_pop_2021),
#     median_icb_pop_2021 = median(total_pop_2021),
#     min_icb_pop_2021 = min(total_pop_2021),
#     max_icb_pop_2021 = max(total_pop_2021)
#   )

# Looked at mean ICB population in 2021, from census = 1 345 002, from MidYr estimates = 1 346 545
# WHY DO DIFFERENCES ARISE BETWEEN CENSUS DATA AND MIDYR ESTIMATES

# -------------------------------------------------------------
# 2. Reading in ethnicity data
# -------------------------------------------------------------

eth <- read_excel(here('SummerProject','data','Ethnicity.xlsx'),skip=9)

eth <- eth %>%
  filter(!grepl("^s", Area))

eth <- eth %>%
  mutate(
    ICB_CODE = case_when(
      ICB_CODE == "E54000052" ~ "E54000063",  # Surrey
      ICB_CODE == "E54000053" ~ "E54000064",  # Sussex
      TRUE ~ ICB_CODE
    )
  ) #had to recode, mismatch between 
# Minor discrepanies in ICB codes across datasets due to NHS structural updates were harmonised using official ICB name matching

#colnames(eth)

# -------------------------------------------------------------
# 3. Processing non-white
# -------------------------------------------------------------

eth <- eth %>%
  mutate(
    non_white = Total - White
  )

eth <- eth %>%
  mutate(
    prop_non_white = non_white / Total,
    pct_non_white  = prop_non_white * 100
  )

summary(eth$prop_non_white)
hist(eth$prop_non_white)

eth <- eth %>%
  mutate(
    eth_quintile = ntile(pct_non_white, 5)
  ) #quintile 5 has the highest proportion of non-white

# -------------------------------------------------------------
# 4. Processing asian
# -------------------------------------------------------------

eth <- eth %>%
  mutate(
    asian = `Asian, Asian British or Asian Welsh`
  )

eth <- eth %>%
  mutate(
    prop_asian = asian / Total,
    pct_asian  = prop_asian * 100
  )

eth <- eth %>%
  mutate(
    asian_quintile = ntile(pct_asian, 5)
  ) #quintile 5 has highest proportion of asian

# -------------------------------------------------------------
# 5. Make quintiles a factor variable
# -------------------------------------------------------------

icb_level <- icb_level %>%
  mutate(
    eth_quintile = factor(eth_quintile),
    IMD_quintile = factor(IMD_quintile),
    asian_quintile = factor(asian_quintile)
  )

# -------------------------------------------------------------
# 6. Separate sub-ethnicity dataset
# -------------------------------------------------------------

eth_small <- eth %>%
  dplyr::select(ICB_CODE, prop_non_white, eth_quintile,
                prop_asian, asian_quintile)

# -------------------------------------------------------------
# 7. Merging to analysis_data
# -------------------------------------------------------------

analysis_data <- analysis_data %>%
  left_join(eth_small, by = "ICB_CODE")

sum(is.na(analysis_data$prop_asian))


