#Younger children much more likely to be breastfed, vitA status changes with age?
#associated with exposure and outcome

library(dplyr)
unique(data$agegp)

for(i in sort(na.omit(unique(data$agegp)))) {
  cat("\n\nAge group:", i, "\n")
  print(table(
    vita   = data$vita[data$agegp == i],
    currbf = data$currbf[data$agegp == i]
  ))
}

# Age group: 2 (311)
# currbf
# vita   0   1
# 0   4  83
# 1   8 216
# Non-BF: 0.667
# BF: 0.722
# RR: 0.722/0.667 = 1.08
# 
# Age group: 3 (245)
# currbf
# vita   0   1
# 0  25  38
# 1  76 106
# 
# 
# Age group: 4 (194)
# currbf
# vita   0   1
# 0  34   3
# 1 139  18
# 
# 
# Age group: 5 (59)
# currbf
# vita  0  1
# 0 11  1
# 1 46  1

library(DescTools)

# get unique age groups (without NA)
agegroups <- sort(na.omit(unique(data$agegp)))

results_age <- list()

for(i in agegroups){
  
  # subset the data for this age group
  sub <- subset(data, agegp == i)
  
  # build the 2x2 table: rows = breastfeeding, columns = vita
  tab <- table(currbf = sub$currbf,
               vita   = sub$vita)
  
  # reorder: row1 = BF=1, row2 = BF=0; col1 = Vita=1, col2 = Vita=0
  tab_rr <- tab[c("1","0"), c("1","0")]
  
  # calculate RR with CI
  rr_out <- RelRisk(tab_rr, conf.level = 0.95)
  
  # store
  results_age[[as.character(i)]] <- rr_out
  
  # print
  cat("\nAge group:", i, "\n")
  print(rr_out)
}

# Age group: 2 
# rel. risk    lwr.ci    upr.ci 
# 1.0836120 0.8257501 1.8564307 
# 
# Age group: 3 
# rel. risk    lwr.ci    upr.ci 
# 0.9782529 0.8446040 1.1441060 
# 
# Age group: 4 
# rel. risk    lwr.ci    upr.ci 
# 1.0668037 0.8079905 1.2266648 
# 
# Age group: 5 
# rel. risk    lwr.ci    upr.ci 
# 0.6195652 0.1167785 1.1591965 

#Total in each age group decreases as ages increase
#But RRs remain round 1 with all CIs including 1 
#When controlling for age, the association between breastfeeding and vitA deficiency largely disappeared
#From table stratum specific RRs were close to 1 and CIs included 1
#different to crude RR suggesting age confound the crude association (positive confounder)
#age band 5 had lower number