##########################################
#### GAM MODELS FOR T1 Bifactor STUDY ####
##########################################

#Load data
data.JLF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")

#Load library
library(mgcv)

#Get JLF variable names
jlfComponents <- names(data.JLF)[grep("mprage_jlf_ct",names(data.JLF))]

#Run gam models (GAM without TBV)
JlfModels <- lapply(jlfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + mood_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.JLF)
})

#Look at model summaries
models <- lapply(JlfModels, summary)

######################
#### MOOD RESULTS ####
######################

#Pull p-values
p_mood <- sapply(JlfModels, function(v) summary(v)$p.table[3,4])

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

#List the JLF components that survive FDR correction
Jlf_mood_fdr <- row.names(p_mood_fdr)[p_mood_fdr<0.05]

#Name of the JLF components that survive FDR correction
Jlf_mood_fdr_names <- jlfComponents[as.numeric(Jlf_mood_fdr)]

#To check direction of coefficient estimates
mood_coeff <- models[as.numeric(Jlf_mood_fdr)]

