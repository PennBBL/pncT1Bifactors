##########################################
#### GAM MODELS FOR T1 BIFACTOR STUDY ####
##########################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Ct_Nmf18",names(data.NMF))]

#Run gam models (GAM without TBV)
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + white + medu1 + mood_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

######################
#### MOOD RESULTS ####
######################

#Pull p-values
p_mood <- sapply(NmfModels, function(v) summary(v)$p.table[6,4])

#Convert to data frame
p_mood <- as.data.frame(p_mood)

#Print original p-values to three decimal places
p_mood_round <- round(p_mood,3)

#FDR correct p-values
p_mood_fdr <- p.adjust(p_mood[,1],method="fdr")

#Convert to data frame
p_mood_fdr <- as.data.frame(p_mood_fdr)

#To print fdr-corrected p-values to three decimal places
p_mood_fdr_round <- round(p_mood_fdr,3)

#List the NMF components that survive FDR correction
Nmf_mood_fdr <- row.names(p_mood_fdr)[p_mood_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_mood_fdr_names <- nmfComponents[as.numeric(Nmf_mood_fdr)]

#To check direction of coefficient estimates
mood_coeff <- models[as.numeric(Nmf_mood_fdr)]
