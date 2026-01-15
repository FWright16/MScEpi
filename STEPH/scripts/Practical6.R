# Introduction to R
# Written by: Stephen Nash
# Date: 28 September 2018
# Updated by: Julia Pescarini
# Date: 23 August 2023
# Updated by: James Cross
# Date: 22 May 2024
# Revised by: Vicky Simms, 26 June 2025

# These lines are comments - R ignores them, but they are helpful 
# for us humans to remind us what each command does.

# You can make comments in two ways:
# 1. Putting a hash (#) at the start of a line
# 2. Mark several lines as comments by starting with triple quotes (''') and ending with triple quotes (''')

# To run commands from this script, highlight the lines you want to run, and 
# press "Ctrl+Enter", or click the "Run" button above. If you don't highlight
# any lines, all of the commands will be run.

# Good luck!

# STEP 1: Tell R where your files are kept
# You'll need to tell R the folder on your computer to look for data, 
# and where it will save new datasets, graphs, and other output.
# We do this with the setwd command (set working directory)
# The command below assumes you have made a folder called "ICEMS" on your desktop, and inside this folder you have another folder called "R"

# View your working directory (where your files are kept) 
getwd()

# To change working directory
setwd("/Desktop/STEPH/R")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# You'll need to change the above line if you have folders with different names.

# STEP 2: Open some data
# First, you'll need to download the bab9.csv dataset and the Masaka spreadsheet
# from Moodle and save them to your home directory. Once you have done so...

library(rio)  # for importing files
bab9 <- import("bab9.csv")
head(bab9)

# Alternatively, import data from an Excel spreadsheet:
masaka_data <- import("Masaka.xlsx", sheet = "2012")
head(masaka_data)

# STEP 3: Explore the data

# Have a look at the variables: how many are there, what do they contain..?
# Try the following commands. They do similar things, so explore them all.

# Show the structure of the dataset
str(bab9)

# Describe the dataset
summary(bab9)

# Summarize specific variables
summary(bab9[c("ht", "gestwks")])
summary(bab9$bweight, details = TRUE) #What does details = TRUE

# EXERCISES  

# Here are the first few commands you'll need for the exercise.
# We're not including them all here, so you'll have to type the rest yourself.
#
# Don't be afraid to experiment with commands and their options,
# or try to find new commands via the Help files or looking online.

# 1. ENTERING DATA AND PRODUCING SIMPLE SUMMARIES

head(bab9, 10)
print(bab9[1:10, ]) #Both the same

table(bab9$sex)
summary(bab9$sex) #Gives a summary of the key info eg mean median min max 
table(bab9$sex, useNA = "ifany") #better than table(bab9$sex) as includes NAs

table(bab9$sex, bab9$ht)
table(bab9$sex, bab9$ht, useNA = "no") #doesn't include NAs

mean(bab9$bweight)
mean(bab9$bweight, na.rm = TRUE) #Remove NAs before calculating mean

summary(bab9$bweight)

# Summarize birthweight by maternal hypertension (ht)
bab9 |>
  group_by(ht) |>
  summarise(mean = mean(bweight), sd = sd(bweight), n = length(bweight))
#Gives mean, ds, n for each category 0 and 1 for maternal hypertension

# 2. GETTING RESULTS OUT OF R 

# Start logging to a file
sink("logfile.txt") #Makes a new txt file in wd and puts console outputs there 

# Your R commands go here
summary(bab9)
str(bab9)
# etc.

# Stop logging
sink()

# Now open the log file you have created and saved and see what is in there!
# Note that only the commands and results you typed while the file was open are saved.

# Add your own commands (and comments) below. Good luck!

###################################################

# The basic output from a cross-tabulation reports frequencies only; to include row and/or column percentages and/or percentages based on the overall total add the options prop.table:

table(bab9$ht, bab9$sex) 
prop.table(table(bab9$ht, bab9$sex), margin = 2)
# For column percentages, eg each column adds to 1


# The table command can be used with any variable provided the number of different values is not too large. However, the results are not very useful when there are a lot of possible values. To demonstrate this, try:

table(bab9$m_age) 
table(bab9$gestwks) 
#Frequency tables eg for m_age 2 mothers aged 23

# The summary function provides a quick overview, including the mean, median, minimum, and maximum values for a particular variable. For example, to describe the variable bweight, you can use:

summary(bab9$bweight) 

# For a more detailed statistical summary, including the mean, standard deviation, median, percentiles, and the minimum and maximum values, you can use summarise:


bab9 |>
  summarise(mean = mean(bweight),
            sd = sd(bweight),
            min = min(bweight),
            q25 = quantile(bweight, probs = 0.25),
            med = median(bweight),
            q75 = quantile(bweight, probs = 0.75),
            max = max(bweight))


# To obtain the means and standard deviations of bweight separately by groups (e.g., by sex), you can use the aggregate function. This allows you to compute these statistics for each group within your dataset:

# Mean of bweight by sex 
bab9 |>
  group_by(sex) |> 
  summarise(mean = mean(bweight), sd = sd(bweight))

summarise(group_by(bab9, sex), 
          mean = mean(bweight), 
          sd = sd(bweight))

bab9 %>%
  group_by(sex) %>%
  summarise(mean = mean(bweight), 
            sd = sd(bweight))


# Commands can be restricted to operate only on records which satisfy given conditions. The conditions are added to the command using subset. For example, to restrict the command to records with birthweight less than or equal to 2000g, type:

subset(bab9, bweight <= 2000) 
Lessthan <- subset(bab9, bweight <= 2000)

# To find out how many records (rows) there are in a data frame:

num_records <- nrow(bab9) #give number of rows = 641 and L for integer
as.numeric(num_records) #converts to numeric
print(num_records) #Gives number of rows

# To list (view) a specific range of rows, you can use indexing. For example, to list rows 541 to 641:

# List rows 541 to 641 
subset_bab <- bab9[541:641, ] 
print(subset_bab)

# To get a summary for a specific range of rows:

# Summary for rows 1 to 10 
summary(bab9[1:10, ])



# New variables are generated using the mutate function from the dplyr package:

library(dplyr) 
bab9 <- bab9 |> 
  mutate(mother_age_group = cut(m_age, breaks = c(-Inf, 30, 35, 40, Inf), 
                                labels = c("<30", "30-34", "35-39", "40+")))
#Creates a column mother_age_group ordered categorical
mutate_ex <- bab9 %>%
  mutate(mother_age_group = cut(m_age,
                                breaks = c(-Inf, 30, 35, 40, Inf),
                                labels = c("<30", "30-34", "35-39", "40+")))

# To make a new binary variable for groups of gestational age (below or above 37 weeks for gestational age) 
bab9 <- bab9 |>
  mutate(gestcat = ifelse(gestwks < 37, 1, 2)) 

bab9 <- bab9 %>%
  mutate(gestcat = ifelse(gestwks < 37, 1, 2))

# Create a binary variable for low birthweight (defined as above) and label the new variable 
bab9 <- bab9 |>
  mutate(lbw = ifelse(bweight < 2500, 1, 0)) 

bab9 <- bab9 %>%
  mutate(lbw = ifelse(bweight < 2500, 1, 0)) 

# The mutate function can also be used to transform a variable using an expression:

bab9 <- bab9|>
  mutate(bwtkgs = bweight/1000) 

bab9 <- bab9 %>%
  mutate(bwtkgs=bweight/1000)

# It is sensible to check the new variables:

head(bab9) 

# In R, to ensure that missing values do not interfere with your conditions, you can use the !is.na() function to explicitly exclude NA values.

# Display records where bweight >= 2000 and exclude missing values
subset_bab9 <- bab9[bab9$bweight >= 2000 & !is.na(bab9$bweight), ]

# R automatically excludes NA values in many statistical functions by using the na.rm = TRUE argument. Here’s an example:

# Calculate the mean of bweight, excluding missing values
mean_bweight <- mean(bab9$bweight, na.rm = TRUE)
print(mean_bweight)

# Calculate the standard deviation of bweight, excluding missing values
sd_bweight <- sd(bab9$bweight, na.rm = TRUE)
print(sd_bweight)

# In order to keep the dataset with these new variables for use in future sessions, use the write.csv function:

export(bab9, "bab9new.csv", row.names = FALSE) 
#row.names=FALSE stops adding an additional column at the start with generic row names

# The records in a dataset can be sorted according to the values of one or more variables. The babnew dataset is currently sorted by id, but for some purposes, it might be better to have it sorted by bweight. Try:

bab9 <- bab9[order(bab9$bweight), ] 
head(bab9, 10) 

# After Question 9 - the practical is complete! 
# If you want to learn more continue through the further exercises and read the Tips for writing R scripts section!

#hypertension 1=yes 0=no
#Gestational age weeks
#sex 1=male 2=female
#birthweight grams

count(bab9,sex) #315 male infants

count(bab9,ht) #89 mothers hypertensive during pregnancy

count(bab9,sex,ht)
nrow(filter(bab9,sex==1,ht==1)) #nrow returns number of rows
#37 male infants born to hypertensive mothers

prop.table(table(bab9$ht,bab9$sex))
#prevalence of hypertension among mothers of boys is 5.8%
#prevalence of hypertension among mothers of girls is 8.1%

prop.table(table(bab9$ht,bab9$sex), margin=1)
#Among hypertensive mothers, 41.6% of infants are male

#How does the mean bweight for males compare to females
bab9 |>
  group_by(sex) |>
  summarise(mean=mean(bweight),sd=sd(bweight))
#mean bweight for men 3044 women 3211


babnew <- read.csv('bab9new.csv')
babnew <- babnew[order(babnew$bweight), ] 
head(babnew, 5) 

