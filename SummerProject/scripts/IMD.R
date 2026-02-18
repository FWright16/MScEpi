# IMD Processing and Merging

# -------------------------------------------------------------
# 1. Import IMD data
# -------------------------------------------------------------
# assuming IMD.xlsx is located in data/IMD.xlsx/

imd <- read_excel(here('SummerProject','data','IMD.xlsx'), sheet=2)

#glimpse(imd) #Explore IMD excel file

# -------------------------------------------------------------
# 2. Clean IMD data
# -------------------------------------------------------------

imd_clean <- imd %>%
  select(
    ICB_CODE = `Integrated Care Board Code (2024)`,
    IMD_rank = `IMD - Rank of average rank`
  ) %>%
  mutate(
    #Create IMD quintiles (5 = least deprived = high ranks)
    IMD_quintile = ntile(IMD_rank, 5)   # 5 = highest ranks = least deprived
  )

#table(imd_clean$IMD_quintile) # Check distribution of quintiles

# -------------------------------------------------------------
# 3. Merge IMD into analysis dataset
# -------------------------------------------------------------

analysis_data <- analysis_data %>%
  left_join(imd_clean %>% select(ICB_CODE, IMD_quintile),
            by = "ICB_CODE")

# -------------------------------------------------------------
# 4. Summarise prescribing by IMD quintile
# -------------------------------------------------------------
# assuming rate_per_1000 column is already added to 

analysis_data %>%
  group_by(IMD_quintile) %>%
  summarise(
    mean_rate = mean(rate_per_1000, na.rm = TRUE),
    median_rate = median(rate_per_1000, na.rm = TRUE),
    n = n()
  )


