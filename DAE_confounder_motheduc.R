library(DescTools)

edugroups <- sort(na.omit(unique(data$motheduc)))

results_edu <- list()

for(i in edugroups){
  
  sub <- subset(data, motheduc == i)
  
  tab <- table(currbf = sub$currbf,
               vita   = sub$vita)
  
  # reorder rows (1 then 0), columns (1 then 0)
  tab_rr <- tab[c("1","0"), c("1","0")]
  
  rr_out <- RelRisk(tab_rr, conf.level = 0.95)
  
  results_edu[[as.character(i)]] <- rr_out
  
  cat("\nMaternal education =", i, "\n")
  print(rr_out)
}

#Stratified by maternal education, association between breastfeeding and vitA deficiency similar across strata
#RR 0.91 (0.85-0.99) among not educated and 0.9 (0.75-1.12) among children of educated mothers
#Estimates almost identical to crude 0.91, indicating education doesn't confound association
#No evidence of effect modification


