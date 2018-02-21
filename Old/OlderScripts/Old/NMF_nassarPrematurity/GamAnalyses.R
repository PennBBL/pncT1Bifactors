###########################################
#### GAM MODELS FOR NASSAR PREMATURITY ####
###########################################

#Load data
data.NMF <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/MergedNMF_20170306.csv", header=TRUE, na.strings = "NA")

#Load library
library(mgcv)

#Transform the age variable from months to years ("ageAtGo1Scan" changed to "ageAtScan1" in new demographics file)
#data.NMF$age <- (data.NMF$ageAtScan1)/12

#Recode sex as 0 (male) and 1 (female)
#data.NMF$sex<-data.NMF$sex-1

#Run models
#nmffull_noTBV<- names(data.NMF)[grep("Nmf18",names(data.NMF))]
#datanmffull_noTBV <- data.NMF[,nmffull_noTBV]
#Model <- function(x) {
#  ContModel <- gam(x~s(ageAtScan1)+sex+medu1+ga, method="REML", data=data.NMF)
#  anova(ContModel)$pTerms.table[3,3]
#}

nmffull_noTBV<- names(data.NMF)[grep("Nmf18",names(data.NMF))]
NmfModels <- lapply(nmffull_noTBV, function(x) {
  gam(substitute(i ~ s(ageAtScan1) + sex + medu1 + ga, list(i = as.name(x))), data = data.NMF)
})

lapply(NmfModels, summary)

#pvaluesfull.nmf_noTBV <- apply(datanmffull_noTBV, 2, FUN = Model)
#pvaluesfull.nmf_noTBV <- as.data.frame(pvaluesfull.nmf_noTBV)
#pvalues.adjustedfull.nmf_noTBV <- pvaluesfull.nmf_noTBV
#pvalues.adjustedfull.nmf_noTBV[,1] <- p.adjust(pvaluesfull.nmf_noTBV[,1], method = "fdr")

#row.names(pvalues.adjustedfull.nmf_noTBV)[pvalues.adjustedfull.nmf_noTBV<0.05]