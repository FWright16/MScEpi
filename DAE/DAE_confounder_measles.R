library(DescTools)

measgroups <- sort(na.omit(unique(data$measles)))

results_meas <- list()

for(i in measgroups){
  
  sub <- subset(data, measles == i)
  
  tab <- table(currbf = sub$currbf,
               vita   = sub$vita)
  
  # reorder for RelRisk()
  tab_rr <- tab[c("1","0"), c("1","0")]
  
  rr_out <- RelRisk(tab_rr, conf.level = 0.95)
  
  results_meas[[as.character(i)]] <- rr_out
  
  cat("\nMeasles =", i, "\n")
  print(rr_out)
}

#Stratified by history of measles did not change association between breastfeeding and vitA deficiency
#No measles RR 0.91 (0.84-0.98) identical to crude
#Measles history 0.96 RR (0.68-1.29)
#straum-specific estimates close to each other and to crude RR, suggesting measles history does not confound the association
#No evidence of effect modification


