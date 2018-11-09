#######################
#### RUN LM MODELS ####
#######################

#Load data {CHANGE THIS PATH TO YOUR DATA}
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Get NMF variable names {CHANGE TO YOUR NMF VARIABLE NAME AND DOUBLE CHECK IT IS PULLING THE CORRECT VARIABLES}
nmfComponents <- names(data.NMF)[grep("Ct_Nmf18",names(data.NMF))]

#Run models {UPDATE MODEL VARIABLES FOR YOUR STUDY}
NmfModels <- lapply(nmfComponents, function(x) {
  lm(substitute(i ~ age + sex + averageManualRating + phobias_4factorv2, list(i = as.name(x))), data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

#################
#### RESULTS ####
#################

#Pull p-values {"[5,4]" IS PULLING FROM THE 5TH ROW, 4TH COLUMN (THE P-VALUE IN THIS MODEL EXAMPLE) - this will need to be updated based on your model - always check that you are pulling the correct value by comparing to "models"}
p_fear <- sapply(NmfModels, function(v) summary(v)$coefficients[5,4])

#Convert to data frame
p_fear <- as.data.frame(p_fear)

#Print original p-values to three decimal places
p_fear_round <- round(p_fear,3)

#FDR correct p-values
p_fear_fdr <- p.adjust(p_fear[,1],method="fdr")

#Convert to data frame
p_fear_fdr <- as.data.frame(p_fear_fdr)

#To print fdr-corrected p-values to three decimal places {I USUALLY LOOK AT THIS ONE TO SEE WHAT SURVIVES FDR CORRECTION}
p_fear_fdr_round <- round(p_fear_fdr,3)

#List the NMF components that survive FDR correction
Nmf_fear_fdr <- row.names(p_fear_fdr)[p_fear_fdr<0.05]

#Name of the NMF components that survive FDR correction
Nmf_fear_fdr_names <- nmfComponents[as.numeric(Nmf_fear_fdr)]

#To check direction of coefficient estimates
fear_coeff <- models[as.numeric(Nmf_fear_fdr)]
