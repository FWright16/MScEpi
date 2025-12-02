library(DescTools)

sex_groups <- sort(na.omit(unique(data$sex)))

for(s in sex_groups){
  sub <- subset(data,sex==s)
  
  tab <- table(sub$currbf, sub$vita)
  cat('\nSex group:',s,'\n')
  print(tab)
  OR_out <- OddsRatio(tab,conf.level=0.95)
  print(OR_out)
}
  
# Sex group: 1 
# 
# 0   1
# 0  40 136
# 1 110 274
# odds ratio     lwr.ci     upr.ci 
# 0.7326203  0.4831972  1.1107940 
# 
# Sex group: 2 
# 
# 0   1
# 0  34 134
# 1 118 291
# odds ratio     lwr.ci     upr.ci 
# 0.6257273  0.4058603  0.9647030 

tab_sex <- table(data$currbf, data$vita, data$sex)
mantelhaen.test(tab_sex)

# Mantel-Haenszel chi-squared test with continuity correction
# 
# data:  tab_sex
# Mantel-Haenszel X-squared = 6.0949, df = 1, p-value = 0.01356
# alternative hypothesis: true common odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.5027367 0.9156932
# sample estimates:
#   common odds ratio 
# 0.6784928 