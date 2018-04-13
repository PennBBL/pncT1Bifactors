##########################################
#### GAM MODELS FOR T1 BIFACTOR STUDY ####
##########################################

#Load data
data.JAC <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData_Jacobian.rds")

#Load library
library(mgcv)

#Get JAC variable names
jacComponents <- names(data.JAC)[grep("jacobian",names(data.JAC))]

#1. Mood: 
#Run gam models (GAM without TBV)
JacMoodModels <- lapply(jacComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + mood_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.JAC)
})

#Look at model summaries
mood_models <- lapply(JacMoodModels, summary)

#2. Psychosis:
#Run gam models (GAM without TBV)
JacPsyModels <- lapply(jacComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + psychosis_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.JAC)
})

#Look at model summaries
psy_models <- lapply(JacPsyModels, summary)

#3. Externalizing behavior:
#Run gam models (GAM without TBV)
JacExtModels <- lapply(jacComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + externalizing_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.JAC)
})

#Look at model summaries
ext_models <- lapply(JacExtModels, summary)

#4. Phobia (fear):
#Run gam models (GAM without TBV)
JacFearModels <- lapply(jacComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + fear_corrtraitsv2, list(i = as.name(x))), method="REML", data = data.JAC)
})

#Look at model summaries
fear_models <- lapply(JacFearModels, summary)



######################
#### MOOD RESULTS ####
######################

#Pull p-values
p_mood <- sapply(JacMoodModels, function(v) summary(v)$p.table[4,4])

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

#List the JAC components that survive FDR correction
Jac_mood_fdr <- row.names(p_mood_fdr)[p_mood_fdr<0.05]

#Name of the JAC components that survive FDR correction
Jac_mood_fdr_names <- jacComponents[as.numeric(Jac_mood_fdr)]

#To check direction of coefficient estimates
mood_coeff <- mood_models[as.numeric(Jac_mood_fdr)]
mood_p.table_list <- lapply(mood_coeff,`[[`, 'p.table')

######################
#### PSY RESULTS ####
######################

#Pull p-values
p_psy <- sapply(JacPsyModels, function(v) summary(v)$p.table[4,4])

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

#List the JAC components that survive FDR correction
Jac_psy_fdr <- row.names(p_psy_fdr)[p_psy_fdr<0.05]

#Name of the JAC components that survive FDR correction
Jac_psy_fdr_names <- jacComponents[as.numeric(Jac_psy_fdr)]

#To check direction of coefficient estimates
psy_coeff <- psy_models[as.numeric(Jac_psy_fdr)]
psy_p.table_list <- lapply(psy_coeff,`[[`, 'p.table')

######################
#### EXT RESULTS ####
######################

#Pull p-values
p_ext <- sapply(JacExtModels, function(v) summary(v)$p.table[4,4])

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

#List the JAC components that survive FDR correction
Jac_ext_fdr <- row.names(p_ext_fdr)[p_ext_fdr<0.05]

#Name of the JAC components that survive FDR correction
Jac_ext_fdr_names <- jacComponents[as.numeric(Jac_ext_fdr)]

#To check direction of coefficient estimates
ext_coeff <- ext_models[as.numeric(Jac_ext_fdr)]
ext_p.table_list <- lapply(ext_coeff,`[[`, 'p.table')


######################
#### FEAR RESULTS ####
######################

#Pull p-values
p_fear <- sapply(JacFearModels, function(v) summary(v)$p.table[4,4])

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

#List the JAC components that survive FDR correction
Jac_fear_fdr <- row.names(p_fear_fdr)[p_fear_fdr<0.05]

#Name of the JAC components that survive FDR correction
Jac_fear_fdr_names <- jacComponents[as.numeric(Jac_fear_fdr)]

#To check direction of coefficient estimates
fear_coeff <- fear_models[as.numeric(Jac_fear_fdr)]
fear_p.table_list <- lapply(fear_coeff,`[[`, 'p.table')

