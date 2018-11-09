##########################################
#### GAM MODELS FOR T1 BIFACTOR STUDY ####
##########################################

#Load data
data.JAC <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Get JAC variable names
jacComponents <- names(data.JAC)[grep("newJacobian_Nmf18",names(data.JAC))]

#Run gam models (GAM without TBV)
JacModels <- lapply(jacComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.JAC)
})

#Look at model summaries
models <- lapply(JacModels, summary)

#Look at coefficients for CT_fear
mood_coeffs <- sapply(JacModels, function(v) summary(v)$p.table[4,])
psy_coeffs <- sapply(JacModels, function(v) summary(v)$p.table[5,])
ext_coeffs <- sapply(JacModels, function(v) summary(v)$p.table[6,])
fear_coeffs <- sapply(JacModels, function(v) summary(v)$p.table[7,])
overall_coeffs <- sapply(JacModels, function(v) summary(v)$p.table[8,])

#################################################
#### Pulling out values for the result table ####
#################################################

#Pull B(coeff) for CT_mood
B_mood <- as.data.frame(mood_coeffs[1,])

#Print original p-values to two decimal places
B_mood_round <- round(B_mood,2)

#Convert to data frame
SE_mood <- as.data.frame(mood_coeffs[2,])

#Print original p-values to two decimal places
SE_mood_round <- round(SE_mood,2)

#Pull t-values for CT_mood
t_mood <- as.data.frame(mood_coeffs[3,])

#Print original p-values to two decimal places
t_mood_round <- round(t_mood,2)


#Pull B(coeff) for CT_psy
B_psy <- as.data.frame(psy_coeffs[1,])

#Print original p-values to two decimal places
B_psy_round <- round(B_psy,2)

#Convert to data frame
SE_psy <- as.data.frame(psy_coeffs[2,])

#Print original p-values to two decimal places
SE_psy_round <- round(SE_psy,2)

#Pull t-values for CT_psy
t_psy <- as.data.frame(psy_coeffs[3,])

#Print original p-values to two decimal places
t_psy_round <- round(t_psy,2)


#Pull B(coeff) for CT_ext
B_ext <- as.data.frame(ext_coeffs[1,])

#Print original p-values to two decimal places
B_ext_round <- round(B_ext,2)

#Convert to data frame
SE_ext <- as.data.frame(ext_coeffs[2,])

#Print original p-values to two decimal places
SE_ext_round <- round(SE_ext,2)

#Pull t-values for CT_ext
t_ext <- as.data.frame(ext_coeffs[3,])

#Print original p-values to two decimal places
t_ext_round <- round(t_ext,2)


#Pull B(coeff) for CT_fear
B_fear <- as.data.frame(fear_coeffs[1,])

#Print original p-values to two decimal places
B_fear_round <- round(B_fear,2)

#Convert to data frame
SE_fear <- as.data.frame(fear_coeffs[2,])

#Print original p-values to two decimal places
SE_fear_round <- round(SE_fear,2)

#Pull t-values for CT_fear
t_fear <- as.data.frame(fear_coeffs[3,])

#Print original p-values to two decimal places
t_fear_round <- round(t_fear,2)


#Pull B(coeff) for CT_overall
B_overall <- as.data.frame(overall_coeffs[1,])

#Print original p-values to two decimal places
B_overall_round <- round(B_overall,2)

#Convert to data frame
SE_overall <- as.data.frame(overall_coeffs[2,])

#Print original p-values to two decimal places
SE_overall_round <- round(SE_overall,2)

#Pull t-values for CT_overall
t_overall <- as.data.frame(overall_coeffs[3,])

#Print original p-values to two decimal places
t_overall_round <- round(t_overall,2)


######################
#### MOOD RESULTS ####
######################

#Pull p-values
p_mood <- sapply(JacModels, function(v) summary(v)$p.table[4,4])

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
mood_coeff <- models[as.numeric(Jac_mood_fdr)]

###########################
#### PSYCHOSIS RESULTS ####
###########################

#Pull p-values
p_psy <- sapply(JacModels, function(v) summary(v)$p.table[5,4])

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
psy_coeff <- models[as.numeric(Jac_psy_fdr)]

########################################
#### EXTERNALIZING BEHAVIOR RESULTS ####
########################################

#Pull p-values
p_ext <- sapply(JacModels, function(v) summary(v)$p.table[6,4])

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
ext_coeff <- models[as.numeric(Jac_ext_fdr)]

##############################
#### PHOBIA(FEAR) RESULTS ####
##############################

#Pull p-values
p_fear <- sapply(JacModels, function(v) summary(v)$p.table[7,4])

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
fear_coeff <- models[as.numeric(Jac_fear_fdr)]

#########################################
#### OVERALL PSYCHOPATHOLOGY RESULTS ####
#########################################

#Pull p-values
p_overall <- sapply(JacModels, function(v) summary(v)$p.table[8,4])

#Convert to data frame
p_overall <- as.data.frame(p_overall)

#Print original p-values to three decimal places
p_overall_round <- round(p_overall,3)

#FDR correct p-values
p_overall_fdr <- p.adjust(p_overall[,1],method="fdr")

#Convert to data frame
p_overall_fdr <- as.data.frame(p_overall_fdr)

#To print fdr-corrected p-values to three decimal places
p_overall_fdr_round <- round(p_overall_fdr,3)

#List the JAC components that survive FDR correction
Jac_overall_fdr <- row.names(p_overall_fdr)[p_overall_fdr<0.05]

#Name of the JAC components that survive FDR correction
Jac_overall_fdr_names <- jacComponents[as.numeric(Jac_overall_fdr)]

#To check direction of coefficient estimates
overall_coeff <- models[as.numeric(Jac_overall_fdr)]

