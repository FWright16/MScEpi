library(DescTools)

pumpgroups <- sort(na.omit(unique(data$handpump)))

results_pump <- list()

for(i in pumpgroups){
  
  sub <- subset(data, handpump == i)
  
  tab <- table(currbf = sub$currbf,
               vita   = sub$vita)
  
  # reorder as usual
  tab_rr <- tab[c("1","0"), c("1","0")]
  
  rr_out <- RelRisk(tab_rr, conf.level = 0.95)
  
  results_pump[[as.character(i)]] <- rr_out
  
  cat("\nHandpump =", i, "\n")
  print(rr_out)
}

#Stratified by access to water handpump, association between breastfeeding and vitA deficiency similar across strata
#RR 0.87 (0.74-1.05) without pump, 0.9 (0.84-0.98) with functioning pump, 1.09 (0.8-1.62) for non-functioning water handpump.
#Functioning waterpump slightly protective
#Estimates close to crude so handpump access doesnt confound association
#differences between strata small and CIs overlap substantially so no evidence of effect modification

