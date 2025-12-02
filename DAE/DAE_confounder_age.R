library(DescTools)

# Get the age groups that actually appear in the data
age_groups <- sort(na.omit(unique(data$agegp)))

for(a in age_groups){
  
  # Subset the data for that age group
  sub <- subset(data, agegp == a)
  
  # Make the 2×2 table: rows = currbf (0,1), cols = vita (0,1)
  tab <- table(sub$currbf, sub$vita)
  
  cat("\nAge group:", a, "\n")
  print(tab)
  
  # Calculate OR + 95% CI
  OR_out <- OddsRatio(tab, conf.level = 0.95)
  print(OR_out)
}

# Age group: 2
# 
# 0   1
# 0   4   8
# 1  83 216
# odds ratio     lwr.ci     upr.ci 
# 1.3012048  0.3816175  4.4367308 
# 
# Age group: 3 
# 
# 0   1
# 0  25  76
# 1  38 106
# odds ratio     lwr.ci     upr.ci 
# 0.9175900  0.5114965  1.6460944 
# 
# Age group: 4 
# 
# 0   1
# 0  34 139
# 1   3  18
# odds ratio     lwr.ci     upr.ci 
# 1.4676259  0.4086739  5.2705250 
# 
# Age group: 5 
# 
# 0  1
# 0 11 46
# 1  1  1
# odds ratio     lwr.ci     upr.ci 
# 0.23913043 0.01384897 4.12906946 

tab_age <- table(data$currbf, data$vita, data$agegp)
mantelhaen.test(tab_age)

Mantel-Haenszel chi-squared test without continuity correction

# data:  tab_age
# Mantel-Haenszel X-squared = 0.00041245, df = 1, p-value = 0.9838
# alternative hypothesis: true common odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.6238178 1.6189751
# sample estimates:
#   common odds ratio 
# 1.00496 

