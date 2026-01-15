library(DescTools)

an_grps <- sort(na.omit(unique(data$anaemia)))

results_an <- list()

for(i in an_grps){
  
  sub <- subset(data, anaemia == i)
  
  tab <- table(currbf = sub$currbf,
               vita   = sub$vita)
  
  tab_rr <- tab[c("1","0"), c("1","0")]
  
  rr_out <- RelRisk(tab_rr, conf.level = 0.95)
  
  results_an[[as.character(i)]] <- rr_out
  
  cat("\nAnaemia =", i, "\n")
  print(rr_out)
}

#Stratified by anaemia status, stratum-specific RR differed
#Non-anaemic 0.84 (0.77-0.92)
#Anaemic 1.01 (0.89-1.18)
#They represent variation in the association between breastfeeding and vitA deficiency across anaemia strata
#Anaemia downstream consequence of poor diet and infection, similar to vitA deficiency

#chisq anaemia
