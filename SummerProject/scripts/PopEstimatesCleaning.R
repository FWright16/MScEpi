library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
df <- read_excel('/Users/finnwright16/Desktop/STuDy/MScEpi/SummerProject/data/MidYrEstimates.xlsx',sheet=5,skip=3)


female_cols <- grep("^F\\d+$", names(df), value = TRUE)

long_f <- df %>%
  select(all_of(id_cols), all_of(female_cols)) %>%   # only IDs + F columns
  pivot_longer(
    cols = all_of(female_cols),
    names_to = "age_col",
    values_to = "n"
  ) %>%
  mutate(
    sex = "Female",
    age = as.integer(str_extract(age_col, "\\d+"))
  ) %>%
  select(all_of(id_cols), sex, age, n)               # drop age_col now

male_cols <- grep("^M\\d+$", names(df), value = TRUE)

long_m <- df %>%
  select(all_of(id_cols), all_of(male_cols)) %>%     # only IDs + M columns
  pivot_longer(
    cols = all_of(male_cols),
    names_to = "age_col",
    values_to = "n"
  ) %>%
  mutate(
    sex = "Male",
    age = as.integer(str_extract(age_col, "\\d+"))
  ) %>%
  select(all_of(id_cols), sex, age, n)

long <- bind_rows(long_f, long_m)

glimpse(long)

icb_age_sex <- long %>%
  group_by(`ICB 2024 Code`, `ICB 2024 Name`, sex, age) %>%
  summarise(pop = sum(n, na.rm = TRUE), .groups = "drop")

glimpse(icb_age_sex)

sum(icb_age_sex$pop) #53 107 169




