library(DescTools)

admgroups <- sort(na.omit(unique(data$admitted)))

results_adm <- list()

for(i in admgroups){
  
  sub <- subset(data, admitted == i)
  
  tab <- table(currbf = sub$currbf,
               vita   = sub$vita)
  
  tab_rr <- tab[c("1","0"), c("1","0")]
  
  rr_out <- RelRisk(tab_rr, conf.level = 0.95)
  
  results_adm[[as.character(i)]] <- rr_out
  
  cat("\nAdmitted =", i, "\n")
  print(rr_out)
}

#Stratified by hospital admission in past year? association between breastfeeding and vitA deficiency is similar across strata
#No admitted RR 0.9 (0.84-0.98), almost identical to crude
#Admitted RR 0.94 (0.69-1.35) confidence interval larger due to small numbers
#Stratum-specific estimates close to each other and crude, indicating hospital admissions does not confound the association
#No evidence of effect modification 


