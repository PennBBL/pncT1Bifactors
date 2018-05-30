##########################################
#### GAM MODELS FOR T1 BIFACTOR STUDY ####
##########################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)

#Create a total ct variable
#data.NMF$totalCT <- (data.NMF$Ct_Nmf18C1 + data.NMF$Ct_Nmf18C2 + data.NMF$Ct_Nmf18C3 + data.NMF$Ct_Nmf18C4 + 
#		     data.NMF$Ct_Nmf18C5 + data.NMF$Ct_Nmf18C6 + data.NMF$Ct_Nmf18C7 + data.NMF$Ct_Nmf18C8 + 
#		     data.NMF$Ct_Nmf18C9 + data.NMF$Ct_Nmf18C10 + data.NMF$Ct_Nmf18C11 + data.NMF$Ct_Nmf18C12 + 
#		     data.NMF$Ct_Nmf18C13 + data.NMF$Ct_Nmf18C14 + data.NMF$Ct_Nmf18C15 + data.NMF$Ct_Nmf18C16 
#		     + data.NMF$Ct_Nmf18C17 + data.NMF$Ct_Nmf18C18)/18

#Multivariate Models
#NmfGam <- lm(phobias_4factorv2 ~ age + sex + totalCT, data = data.NMF)

NmfGam <- lm(phobias_4factorv2 ~ age + sex + Ct_Nmf18C1 + Ct_Nmf18C2 + Ct_Nmf18C3 + 
                     Ct_Nmf18C4 + Ct_Nmf18C5 + Ct_Nmf18C6 + Ct_Nmf18C7 + Ct_Nmf18C8 + Ct_Nmf18C9 + 
                     Ct_Nmf18C10 + Ct_Nmf18C11 + Ct_Nmf18C12 + Ct_Nmf18C13 + Ct_Nmf18C14 + Ct_Nmf18C15 + 
                     Ct_Nmf18C16 + Ct_Nmf18C17 + Ct_Nmf18C18, data = data.NMF)

NullGam <- lm(phobias_4factorv2 ~ age + sex, data = data.NMF)

#Look at model summaries
NmfModel <- summary(NmfGam)
NullModel <- summary(NullGam)
