##########################################
#### GAM MODELS FOR T1 Bifactor STUDY ####
##########################################

#Load data
data.JLF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Get JLF variable names
jlfComponents <- names(data.JLF)[grep("mprage_jlf_ct",names(data.JLF))]

#Run gam models (GAM with TBV)
JlfModels <- lapply(jlfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + psychosis_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.JLF)
})

#Look at model summaries
models <- lapply(JlfModels, summary)

#####################
#### PSY RESULTS ####
#####################

#Pull p-values
p_psy <- sapply(JlfModels, function(v) summary(v)$p.table[4,4])

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

#List the JLF components that survive FDR correction
Jlf_psy_fdr <- row.names(p_psy_fdr)[p_psy_fdr<0.05]

#Name of the JLF components that survive FDR correction
Jlf_psy_fdr_names <- jlfComponents[as.numeric(Jlf_psy_fdr)]

#To check direction of coefficient estimates
psy_coeff <- models[as.numeric(Jlf_psy_fdr)]

