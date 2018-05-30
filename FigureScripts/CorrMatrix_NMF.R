#################
### LOAD DATA ###
#################

##Read in data
subjData <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load libraries
library(psych)

####################
### CORRELATIONS ###
####################

#Correlations between NMF components
#Subset only by rows needed for correlation table
data.nmf <- subjData[c(grep("Ct_Nmf18",names(subjData)))]

#Make correlation table
corTable_nmf <- cor(data.nmf, method="pearson", use="complete.obs")

#Round correlation values to two decimal places
corTable_nmf_rounded <- round(corTable_nmf, 2)

#Save tables
write.csv(corTable_nmf_rounded,"/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Table_NMF_Correlations.csv",row.names=TRUE,quote=FALSE)

############################
### PARTIAL CORRELATIONS ###
############################

###NMF Model
##NMF~age + ageSq + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2
#Subset by rows needed for partial correlation.
data.NmfModel <- subjData[c(grep("bblid|^age$|ageSq|sex|averageManualRating|4factorv2|Ct_Nmf18",names(subjData)))]

#Make sex numeric (for correlations only)
data.NmfModel$sex <- as.numeric(data.NmfModel$sex)

#Calculate partial correlations (correlations between Fear and NMF components, controlling for age, ageSq, sex, averageManualRating, mood_4factorv2, psychosis_4factorv2, externalizing_4factorv2, and overall_psychopathology_4factorv2)
ParCorTable_NMF <- partial.r(data.NmfModel,c(8,11:28),c(2:7,9:10))

#Save table
write.csv(ParCorTable_NMF,"/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Table_NmfModel_ParCorrelations.csv",row.names=TRUE,quote=FALSE)

###Fear Model
##phobias_4factorv2~age + sex + CtNMF1 ... CtNMF18
#Subset by rows needed for partial correlation.
data.FearModel <- subjData[c(grep("bblid|^age$|sex|phobias_4factorv2|Ct_Nmf18",names(subjData)))]

#Make sex numeric (for correlations only)
data.FearModel$sex <- as.numeric(data.FearModel$sex)

#Calculate partial correlations (correlations between Fear and NMF components, controlling for age and sex)
ParCorTable_Fear <- partial.r(data.FearModel,c(4:22),c(2:3))

#Save table
write.csv(ParCorTable_Fear,"/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Table_FearModel_ParCorrelations.csv",row.names=TRUE,quote=FALSE)
