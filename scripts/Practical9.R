setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()
install.packages('DescTools')
library(DescTools)

#Create a matrix
nets_table <- matrix(c(27,64,46,41), nrow = 2, byrow = TRUE)
row.names(nets_table) <- c('Permethrin net','No permetherin net')
colnames(nets_table) <- c('Enlarged spleen', 'Normal spleen')
nets_table

#row percentages margin=1 row percentages, margin=2 col percentages
prop.table(nets_table,margin = 1)


#Another way is using PercTable in DescTools
PercTable(nets_table,rfrq='010')
#010 is binary for: total percentage, row percentage, col percentage

#Chi-squared test
chisq.test(nets_table)
#X-squared = 8.9625, df=1, p-value= 0.002756
#df = (r-1) * (c-1)
#To assess whether sleeping under net is associated with 
#likelihood of having an enlarged spleen
#H_o there is no association

#The absolute increase in risk (risk difference) 
#of enlarged spleen in those who slept without
#nets compared to those who slept under nets

#BinomDiffCI function to cal RD
#ExOutcome, Ex, UnOutcome, Un
BinomDiffCI(27,91,46,87,method=c('wald'))
#Difference -0.23, LowerCI=-0.37, UpperCI=-0.09
#Better to report as reduction and omit minus sign
#We found that Permethrin bednets were associated with a reduction in the risk of enlarged spleen by 23% (95% CI: 9%, 37%).

# The relative decrease (risk ratio) in risk of
# enlarged spleen in those who slept under nets
# compared to those who slept without them

#RelRisk function to calculate Relative Risk Ratio
RelRisk(nets_table,conf.level=0.95, method='wald')
#RR=0.56, LowerCI=0.39, UpperCI=0.81

# The relative decrease in odds (odds ratio) of 
# enlarged spleen in those who slept under nets 
# compared to those who slept without them

#OddsRatio function
OddsRatio(nets_table,conf.level=0.95)
# OR=0.38, LowerCI=0.2, UpperCI=0.7



nets_table2 <- matrix(c(46,41,27,64), nrow = 2, byrow = TRUE)
row.names(nets_table2) <- c('No Permethrin net','Permetherin net')
colnames(nets_table2) <- c('Enlarged spleen', 'Normal spleen')
nets_table2

chisq.test(nets_table2)
BinomDiffCI(46,91,27,87,method=c('wald'))
RelRisk(nets_table2,conf.level=0.95, method='wald')
OddsRatio(nets_table2,conf.level=0.95)

######## Paired data

#Create a matrix for the paired data
paired_table <- matrix(c(28,21,8,61),nrow=2,byrow = TRUE)
colnames(paired_table) <- c('No restrictions','Some restrictions')
rownames(paired_table) <- c('No restrictions','Some restrictions')
paired_table

#Perform McNemar's test without continuity correction to get exact p-value
mcnemar_exact <- mcnemar.test(paired_table,correct = FALSE)

#Perform McNemar's test for continuity correction for chi-squared approximation
mcnemar_chi <- mcnemar.test(paired_table,correct = T)








