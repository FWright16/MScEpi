#Exploring and processing ethnicity data

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