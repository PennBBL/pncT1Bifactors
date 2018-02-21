##########################################
#### GAM MODELS FOR T1 BIFACTOR STUDY ####
##########################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf26",names(data.NMF))]

#Run gam models (GAM with TBV)
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + externalizing_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

######################
#### ext RESULTS ####
######################

#Pull p-values
p_ext <- sapply(NmfModels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_ext <- as.data.frame(p_ext)

#Print original p-values to three decimal places
p_ext_round <- round(p_ext,3)

#FDR correct p-values
p_ext_fdr <- p.adjust(p_ext[,1],method="fdr")

#Convert to data frame
p_ext_fdr <- as.data.frame(p_ext_fdr)

#To print fdr-corrected p-values to three decimal places
p_ext_fdr_round <- round(p_ext_fdr,3)

#List the NMF components that survive FDR correction
Nmf_ext_fdr <- row.names(p_ext_fdr)[p_ext_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_ext_fdr_names <- nmfComponents[as.numeric(Nmf_ext_fdr)]

#To check direction of coefficient estimates
ext_coeff <- models[as.numeric(Nmf_ext_fdr)]
p.table_list <- lapply(ext_coeff,`[[`, 'p.table')

