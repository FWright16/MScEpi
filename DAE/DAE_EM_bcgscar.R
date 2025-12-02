library(DescTools)

bcg_grps <- sort(na.omit(unique(data$bcgscar)))

results_bcg <- list()

for(i in bcg_grps){
  
  sub <- subset(data, bcgscar == i)
  
  tab <- table(currbf = sub$currbf,
               vita   = sub$vita)
  
  tab_rr <- tab[c("1","0"), c("1","0")]
  
  rr_out <- RelRisk(tab_rr, conf.level = 0.95)
  
  results_bcg[[as.character(i)]] <- rr_out
  
  cat("\nBCG scar =", i, "\n")
  print(rr_out)
}

#Stratified by BCG scar status, association between breastfeeding and vitA deficiency similar across strata
#RR without BCG scar 0.95 (0.86-1.05)
#RR with BCG scar 0.87 (0.79-0.97)
#Estimates close to each other and the crude, indicating no effect modification by BCG vaccination status

