table(data$vita, data$currbf)
table(
  "Vitamin A Deficiency" = data$vita,
  "Current Breastfeeding" = data$currbf
)
#vita, 1=def, 0=normal
#currbf, 1=yes, 0=no


#                       Vitamin A Deficiency
# Current Breastfeeding   0   1
#                      0  74 270
#                      1 228 565
# 1137 people

tab <- table(currbf = data$currbf, vita = data$vita)
tab

tab_rr <- tab[c("1", "0"), c("1", "0")]
tab_rr

prop.table(table(data$vita, data$currbf), margin = 2)
# 0         1
# 0 0.22 0.29
# 1 0.78 0.71
#Among non-breastfed children, 78.5% are vitamin A deficient and 21.5% are normal
#Among breastfed children, 71.2% are vitamin A deficient and 28.8% are normal

# Risk difference (RD) was 0.712-0.785=-0.073
# breastfeeding was associated with a 7.3% absolute reduction in vitA deficiency
#breastfeeding associated with around 7 fewer cases of vitA deficiency per 100 children

library(DescTools)
RelRisk(tab_rr, conf.level = 0.95)
# rel. risk    lwr.ci    upr.ci 
# 0.91         0.85      0.98 
#The crude RR of vitamin A deficiency among breastfed children compared to non-breastfed children was 0.91 (95% CI: 0.85-0.98)
#Children who breastfed had 9% lower relative risk of vit A deficiency
#CI doesn't include 1 so statistical evidence of an association between breastfeeding and lower riusk of vit A deficiency
#breastfeeding is protective effect

#sample size large 1137 children so chi-squared
chisq.test(tab_rr)

# Pearson's Chi-squared test with Yates'
# continuity correction
# 
# data:  tab_rr
# X-squared = 6.0814, df = 1, p-value = 0.01366

A chi-squared test with Yates' continuity correction gave X-squared=6.08 (df=1, p=0.014). WIthout continuity correction X-squared = 6.45, p-value=0.01)
Good evidence of an association between breastfeeding and vitA deficiency



# Breastfeeding.       Normal       Deficient      Total
# No                74 (21.5%)     270 (78.5%)    344
# Yes               228 (28.8%)    565 (71.2%)    793

