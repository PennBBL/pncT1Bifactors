##########################################
#### GAM MODELS FOR T1 Bifactor STUDY ####
##########################################

#Load data
data.JLF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Get JLF variable names
jlfComponents <- names(data.JLF)[grep("mprage_jlf_ct",names(data.JLF))]

#Run gam models (GAM without TBV)
JlfModels <- lapply(jlfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + fear_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.JLF)
})

#Look at model summaries
models <- lapply(JlfModels, summary)

######################
#### FEAR RESULTS ####
######################

#Pull p-values
p_fear <- sapply(JlfModels, function(v) summary(v)$p.table[3,4])

#Convert to data frame
p_fear <- as.data.frame(p_fear)

#Print original p-values to three decimal places
p_fear_round <- round(p_fear,3)

#FDR correct p-values
p_fear_fdr <- p.adjust(p_fear[,1],method="fdr")

#Convert to data frame
p_fear_fdr <- as.data.frame(p_fear_fdr)

#To print fdr-corrected p-values to three decimal places
p_fear_fdr_round <- round(p_fear_fdr,3)

#List the JLF components that survive FDR correction
Jlf_fear_fdr <- row.names(p_fear_fdr)[p_fear_fdr<0.05]

#Name of the JLF components that survive FDR correction
Jlf_fear_fdr_names <- jlfComponents[as.numeric(Jlf_fear_fdr)]

#To check direction of coefficient estimates
fear_coeff <- models[as.numeric(Jlf_fear_fdr)]

