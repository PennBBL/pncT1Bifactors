##########################################
#### GAM MODELS FOR T1 Bifactor STUDY ####
##########################################

#Load data
data.JLF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")

#Load library
library(mgcv)

#Get NMF variable names
jlfAllComponents <- data.JLF[c(grep("mprage_jlf_vol",names(data.JLF)))]
##141 variables

jlfComponents_short <- jlfAllComponents[,-grep("Vent|Brain_Stem|Cerebell|Cerebral_White_Matter|CSF|Lobe_WM",names(jlfAllComponents))]
##112 variables

jlfComponents <- names(jlfComponents_short)

#Run gam models (GAM without TBV)
JlfModels <- lapply(jlfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.JLF)
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

###########################
#### PSYCHOSIS RESULTS ####
###########################

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

########################################
#### EXTERNALIZING BEHAVIOR RESULTS ####
########################################

#Pull p-values
p_ext <- sapply(JlfModels, function(v) summary(v)$p.table[5,4])

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

##############################
#### PHOBIA(FEAR) RESULTS ####
##############################

#Pull p-values
p_fear <- sapply(JlfModels, function(v) summary(v)$p.table[6,4])

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

#########################################
#### OVERALL PSYCHOPATHOLOGY RESULTS ####
#########################################

#Pull p-values
p_overall <- sapply(JlfModels, function(v) summary(v)$p.table[7,4])

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

#List the JLF components that survive FDR correction
Jlf_overall_fdr <- row.names(p_overall_fdr)[p_overall_fdr<0.05]

#Name of the JLF components that survive FDR correction
Jlf_overall_fdr_names <- jlfComponents[as.numeric(Jlf_overall_fdr)]

#To check direction of coefficient estimates
overall_coeff <- models[as.numeric(Jlf_overall_fdr)]
