##########################################
#### GAM MODELS FOR T1 BIFACTOR STUDY ####
##########################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1239_T1_subjData_NoPsychMeds.rds")

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Ct_Nmf18",names(data.NMF))]

#Run gam models (GAM without TBV)
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + psychosis_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

######################
#### PSY RESULTS ####
######################

#Pull p-values
p_psy <- sapply(NmfModels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_psy <- as.data.frame(p_psy)

#Print original p-values to three decimal places
p_psy_round <- round(p_psy,3)

#FDR correct p-values
p_psy_fdr <- p.adjust(p_psy[,1],method="fdr")

#Convert to data frame
p_psy_fdr <- as.data.frame(p_psy_fdr)

#To print fdr-corrected p-values to three decimal places
p_psy_fdr_round <- round(p_psy_fdr,3)

#List the NMF components that survive FDR correction
Nmf_psy_fdr <- row.names(p_psy_fdr)[p_psy_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_psy_fdr_names <- nmfComponents[as.numeric(Nmf_psy_fdr)]

#To check direction of coefficient estimates
psy_coeff <- models[as.numeric(Nmf_psy_fdr)]
