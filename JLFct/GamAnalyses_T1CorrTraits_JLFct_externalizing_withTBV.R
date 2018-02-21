##########################################
#### GAM MODELS FOR T1 Bifactor STUDY ####
##########################################

#Load data
data.JLF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")

#Load library
library(mgcv)

#Get JLF variable names
jlfComponents <- names(data.JLF)[grep("mprage_jlf_ct",names(data.JLF))]

#Run gam models (GAM with TBV)
JlfModels <- lapply(jlfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + externalizing_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.JLF)
})

#Look at model summaries
models <- lapply(JlfModels, summary)

#####################
#### EXT RESULTS ####
#####################

#Pull p-values
p_ext <- sapply(JlfModels, function(v) summary(v)$p.table[4,4])

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

#List the JLF components that survive FDR correction
Jlf_ext_fdr <- row.names(p_ext_fdr)[p_ext_fdr<0.05]

#Name of the JLF components that survive FDR correction
Jlf_ext_fdr_names <- jlfComponents[as.numeric(Jlf_ext_fdr)]

#To check direction of coefficient estimates
ext_coeff <- models[as.numeric(Jlf_ext_fdr)]

