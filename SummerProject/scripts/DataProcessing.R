# =============================================================
# Data Processing
# =============================================================

# -------------------------------------------------------------
# 1. Load Packages
# -------------------------------------------------------------
library(tidyverse)
library(readr)
library(dplyr)
library(here)
library(stringr)

# -------------------------------------------------------------
# 2. Import monthly prescribing data
# -------------------------------------------------------------
# Assumes all raw prescribing data (csv) are stored in data/ICB_Data/ relative to project root

data_dir <- here('SummerProject','data','ICB_Data')

presc_monthly <- list.files(
  path = data_dir,
  pattern = '\\.csv$',
  full.names = TRUE
) %>% 
  set_names() %>% 
  map_dfr(read_csv, show_col_types = FALSE)

# glimpse(presc_monthly) 
# Have a look at data

# -------------------------------------------------------------
# 3. Clean prescribing data
# -------------------------------------------------------------

presc_clean <-presc_monthly %>% 
  mutate(
    # Replace 2 for other values of *
    UNIQUE_PATIENT_COUNT=ifelse(UNIQUE_PATIENT_COUNT=='*',2,UNIQUE_PATIENT_COUNT),
    ITEMS=ifelse(ITEMS=='*',2,ITEMS),
    
    # Convert to numeric
    UNIQUE_PATIENT_COUNT=as.numeric(UNIQUE_PATIENT_COUNT),
    ITEMS=as.numeric(ITEMS)
  ) %>% 
  #Remove Unknown sex or age band categories
  filter(
    !GENDER %in% c('Unknown','Indeterminate'),
    !AGE_BAND %in% c('Unknown')
  ) 

# unique(presc_clean$GENDER) # Checks the gender categories
# unique(presc_clean$AGE_BAND) # Checks the age band categories
# sum(is.na(presc_clean)) # Checks for any NA values (should be 0)

# -------------------------------------------------------------
# 4. Attach current ICB codes
# -------------------------------------------------------------
# ICBLinkUp.csv has repeats (from GP practices) removed and 'areas' removed, to leave 42 ICBs

icb_codes <- read.csv(here('SummerProject','data','ICBLinkUp.csv'))

# Adds ICB codes and names onto 'true' ICBs and leaves NA values for 'areas'
presc_clean <- presc_clean %>% 
  left_join(
    icb_codes %>% 
      select(ICB24CDH, ICB24CD,ICB24NM),
    by=c('ICB_CODE'='ICB24CDH')
  )

# Remove non-ICBs
presc_clean <- presc_clean %>% 
  filter(
    !is.na(ICB24CD),
    !is.na(ICB24NM)
  )

# sum(is.na(presc_clean$ICB24CD)) #Check that there are no NA values and only ICBs are present

# -------------------------------------------------------------
# 5. Aggregate to yearly level
# -------------------------------------------------------------

presc_year_icb_all_bnf <- presc_clean %>%
  mutate(
    YEAR = floor(YEAR_MONTH / 100)
  ) %>%
  group_by(
    ICB24CD,
    ICB24NM,
    YEAR,
    GENDER,
    AGE_BAND
  ) %>%
  summarise(
    TOTAL_PATIENTS = sum(UNIQUE_PATIENT_COUNT, na.rm = TRUE),
    TOTAL_ITEMS = sum(ITEMS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    AGE_START = as.numeric(sub("-.*", "", AGE_BAND))
  ) %>%
  arrange(
    ICB24CD,
    YEAR,
    GENDER,
    AGE_START
  ) %>%
  select(-AGE_START)

# For separate BNF drugs
# presc_year_icb <- presc_clean %>% 
#   mutate(
#     YEAR = floor(YEAR_MONTH / 100)
#   ) %>% 
#   group_by(ICB24CD, ICB24NM, YEAR, BNF_CHEMICAL_SUBSTANCE_CODE, GENDER, AGE_BAND) %>% 
#   summarise(
#     TOTAL_PATIENTS = sum(UNIQUE_PATIENT_COUNT, na.rm = TRUE),
#     TOTAL_ITEMS = sum(ITEMS, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>% 
#   mutate(
#     AGE_START = as.numeric(sub('-.*', '', AGE_BAND))
#   ) %>% 
#   arrange(
#     BNF_CHEMICAL_SUBSTANCE_CODE,
#     GENDER,
#     AGE_START
#   ) %>% 
#   select(-AGE_START)

# -------------------------------------------------------------
# 6. Final formatting
# -------------------------------------------------------------

presc_year_icb_all_bnf <- presc_year_icb_all_bnf %>%
  rename(
    ICB_CODE = ICB24CD,
    ICB_NAME = ICB24NM,
    sex = GENDER,
    age_group = AGE_BAND,
    items = TOTAL_ITEMS,
    patients = TOTAL_PATIENTS
  ) %>% 
  mutate(
    age_group = as.character(age_group)
  ) %>% 
  #Remove 2015 as only april-decemeber
  filter(YEAR != 2015) %>% 
  #Remove 90+ as this doesn't match with population counts
  filter(
    !age_group %in% c('91-95','96-100','101-105','105+')
  )

#unique(presc_year_icb_all_bnf$age_group) # Check 90+ are removed

# -------------------------------------------------------------
# 7. Merge with population data
# -------------------------------------------------------------

icb_population_all_years <- read.csv(here('SummerProject','data','icb_population_all_years.csv'))

analysis_data <- presc_year_icb_all_bnf %>% 
  left_join(
    icb_population_all_years,
    by=c('ICB_CODE','ICB_NAME','YEAR','sex','age_group')
  )

#sum(is.na(analysis_data$pop)) # Check no NA values in population pop column

# -------------------------------------------------------------
# 8. Addition of rate per 1000 column
# -------------------------------------------------------------

analysis_data <- analysis_data %>% 
  mutate(
    rate_per_1000 = ifelse(
      pop > 0,
      (items / pop) * 1000,
      NA_real_
    )
  )

# -------------------------------------------------------------
# 9. Make age group and ordered factor
# -------------------------------------------------------------

age_levels <- c(
  "0-1","2-5","6-10","11-15","16-20","21-25","26-30",
  "31-35","36-40","41-45","46-50","51-55","56-60",
  "61-65","66-70","71-75","76-80","81-85","86-90"
)

analysis_data <- analysis_data %>%
  mutate(age_group = factor(age_group, levels = age_levels))
