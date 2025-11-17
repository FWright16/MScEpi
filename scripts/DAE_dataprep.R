library(dplyr)

data <- read.csv('/Users/finnwright16/Desktop/STuDy/MScEpi/data/vastcat_2025.csv')

data <- data %>%
  mutate(vita = case_when(
    vita == "Deficient" ~ 1,
    vita == "Normal"    ~ 0,
    TRUE                ~ NA_real_
  ))

data <- data %>%
  mutate(currbf = case_when(
    currbf == "Yes" ~ 1,
    currbf == "No"  ~ 0,
    TRUE            ~ NA_real_
  ))

data <- data %>%
  mutate(agegp = case_when(
    agegp == "0-11"  ~ 1,
    agegp == "12-23" ~ 2,
    agegp == "24-35" ~ 3,
    agegp == "36-47" ~ 4,
    agegp == "48-59" ~ 5,
    TRUE             ~ NA_real_
  ))

data <- data %>%
  mutate(sex = case_when(
    sex == "Male"   ~ 1,
    sex == "Female" ~ 2,
    TRUE            ~ NA_real_
  ))

data <- data %>%
  mutate(motheduc = case_when(
    motheduc == "Yes" ~ 1,
    motheduc == "No"  ~ 0,
    TRUE              ~ NA_real_
  ))

data <- data %>%
  mutate(handpump = case_when(
    handpump == "No"               ~ 0,
    handpump == "Yes, functioning"              ~ 1,
    handpump == "Not functioning"  ~ 2,
    TRUE                           ~ NA_real_
  ))

data <- data %>%
  mutate(bcgscar = case_when(
    bcgscar == "Yes" ~ 1,
    bcgscar == "No"  ~ 0,
    TRUE             ~ NA_real_
  ))

data <- data %>%
  mutate(measles = case_when(
    measles == "Yes" ~ 1,
    measles == "No"  ~ 0,
    TRUE             ~ NA_real_
  ))

data <- data %>%
  mutate(admitted = case_when(
    admitted == "Yes" ~ 1,
    admitted == "No"  ~ 0,
    TRUE              ~ NA_real_
  ))

data <- data %>%
  mutate(anaemia = case_when(
    anaemia == "Yes" ~ 1,
    anaemia == "No"  ~ 0,
    TRUE             ~ NA_real_
  ))
