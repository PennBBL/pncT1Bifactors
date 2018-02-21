##########################################
#### GAM MODELS FOR T1 BIFACTOR STUDY ####
##########################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf18",names(data.NMF))]

#Run gam models (GAM with TBV)
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + mood_sr_corrtraits + psychosis_sr_corrtraits + externalizing_sr_corrtraits + fear_sr_corrtraits, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

######################
#### MOOD RESULTS ####
######################

#Pull p-values
p_mood <- sapply(NmfModels, function(v) summary(v)$p.table[4,4])

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

###########################
#### PSYCHOSIS RESULTS ####
###########################

#Pull p-values
p_psy <- sapply(NmfModels, function(v) summary(v)$p.table[5,4])

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

########################################
#### EXTERNALIZING BEHAVIOR RESULTS ####
########################################

#Pull p-values
p_ext <- sapply(NmfModels, function(v) summary(v)$p.table[6,4])

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

##############################
#### PHOBIA(FEAR) RESULTS ####
##############################

#Pull p-values
p_fear <- sapply(NmfModels, function(v) summary(v)$p.table[7,4])

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

#List the NMF components that survive FDR correction
Nmf_fear_fdr <- row.names(p_fear_fdr)[p_fear_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_fear_fdr_names <- nmfComponents[as.numeric(Nmf_fear_fdr)]

#To check direction of coefficient estimates
fear_coeff <- models[as.numeric(Nmf_fear_fdr)]
