##############################
#### PARTIAL CORRELATIONS ####
##############################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(ppcor)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Ct_Nmf18",names(data.NMF))]

#Makes sex numeric (necessary for pcor)
data.NMF$sex <- as.numeric(data.NMF$sex)

#Correlations controlling for covariates
NmfCorrs <- lapply(nmfComponents, function(z) {
  pcor.test(substitute(data.NMF$i, list(i = as.name(z))), data.NMF$phobias_4factorv2, data.NMF[,c("age","ageSq","sex","averageManualRating","mood_4factorv2","psychosis_4factorv2","externalizing_4factorv2","overall_psychopathology_4factorv2")], method = "pearson")
})

#Pull p-values
Corrs_p <- sapply(NmfCorrs, function(y) (y)$p.value)

#FDR correct p-values
Corrs_fdr <- p.adjust(Corrs_p,method="fdr")

#Convert to data frame
Corrs_fdr <- as.data.frame(Corrs_fdr)

#To print fdr-corrected p-values to three decimal places
Corrs_fdr_round <- round(Corrs_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Corrs_fdr <- row.names(Corrs_fdr)[Corrs_fdr<0.05]


##Pull the r values
Corrs_r <- sapply(NmfCorrs, function(x) (x)$estimate)

#Convert to data frame
Corrs_r <- as.data.frame(Corrs_r)

#To print the r values to three decimal places
Corrs_r_round <- round(Corrs_r,3)

