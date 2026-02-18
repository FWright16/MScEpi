# Cleaning population data 

clean_population_year <- function(file_path, sheet_number, year) {
  
  df <- read_excel(file_path, sheet = sheet_number, skip = 3)
  
  female_cols <- grep("^F\\d+$", names(df), value = TRUE)
  male_cols   <- grep("^M\\d+$", names(df), value = TRUE)
  
  id_cols <- setdiff(names(df), c(female_cols, male_cols))
  
  long_f <- df %>%
    select(all_of(id_cols), all_of(female_cols)) %>%
    pivot_longer(
      cols = all_of(female_cols),
      names_to = "age_col",
      values_to = "n"
    ) %>%
    mutate(
      sex = "Female",
      age = as.integer(str_extract(age_col, "\\d+"))
    ) %>%
    select(all_of(id_cols), sex, age, n)
  
  long_m <- df %>%
    select(all_of(id_cols), all_of(male_cols)) %>%
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
  
  assign_age_group <- function(age) {
    case_when(
      age <= 1 ~ "0-1",
      age <= 5 ~ "2-5",
      age <= 10 ~ "6-10",
      age <= 15 ~ "11-15",
      age <= 20 ~ "16-20",
      age <= 25 ~ "21-25",
      age <= 30 ~ "26-30",
      age <= 35 ~ "31-35",
      age <= 40 ~ "36-40",
      age <= 45 ~ "41-45",
      age <= 50 ~ "46-50",
      age <= 55 ~ "51-55",
      age <= 60 ~ "56-60",
      age <= 65 ~ "61-65",
      age <= 70 ~ "66-70",
      age <= 75 ~ "71-75",
      age <= 80 ~ "76-80",
      age <= 85 ~ "81-85",
      age <= 90 ~ "86-90",
      TRUE ~ NA_character_
    )
  }
  
  icb_age_sex <- long %>%
    mutate(age_group = assign_age_group(age)) %>%
    filter(!is.na(age_group)) %>%
    group_by(`ICB 2024 Code`, `ICB 2024 Name`, sex, age_group) %>%
    summarise(pop = sum(n, na.rm = TRUE), .groups = "drop") %>%
    rename(
      ICB_CODE = `ICB 2024 Code`,
      ICB_NAME = `ICB 2024 Name`
    ) %>%
    mutate(
      YEAR = year,
      age_group = as.character(age_group)
    )
  
  return(icb_age_sex)
}

file_path <- here('SummerProject','data','MidYrEstimates.xlsx')

years <- 2016:2022
sheet_numbers <- 10:16

pop_list <- mapply(
  clean_population_year,
  file_path=file_path,
  sheet_number=sheet_numbers,
  year=years,
  SIMPLIFY=FALSE
)
  
icb_population_all_years <- bind_rows(pop_list)

pop_2023 <- clean_population_year(file_path=here('SummerProject','data','2023pop.xlsx'),
                                  sheet_number=6,
                                  year=2023)

icb_population_all_years <- bind_rows(
  icb_population_all_years,
  pop_2023
)  

#table(icb_population_all_years$YEAR) # Checks should all be 1596 every year

# write.csv(
#   icb_population_all_years,
#   'icb_population_all_years.csv',
#   row.names=FALSE
# )
