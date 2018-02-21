#################
### LOAD DATA ###
#################

##Read in data
subjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/subjectData/n1360_JLF_volCtGmd_subjData.rds")
subjData_11 <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/subjectData/n1103_JLF_11andUp_subjData.rds")

##Subset only by rows needed for correlation tables
Vars <- c("CT_gmTotal","CT_gmFrontalTotal","CT_gmOccipitalTotal","CT_gmParietalTotal","CT_gmTemporalTotal","Mood","Psychosis","Externalizing","Fear","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy","GOA_Item_Bifactor_Mood_ar","GOA_Item_Bifactor_Psychosis_ar","GOA_Item_Bifactor_Externalizing_ar","GOA_Item_Bifactor_Fear_ar","GOA_Item_Bifactor_Overall_ar","age","sex","ageSq","tanner")

subjData_short <- subjData[Vars]
subjData_subset <- subjData_11[Vars]

#Make sex numeric (for correlations)
subjData_short$sex <- as.numeric(subjData_short$sex)
subjData_subset$sex <- as.numeric(subjData_subset$sex)

#Make tanner numeric (for correlations)
subjData_short$tanner <- as.numeric(subjData_short$tanner)
subjData_subset$tanner <- as.numeric(subjData_subset$tanner)

#Load library
library(psych)


####################
### CORRELATIONS ###
####################

#make correlation table
corTable <- cor(subjData_short, method="spearman", use="complete.obs")

#Round correlation values to two decimal places
corTable_rounded <- round(corTable, 2)

#save table
write.csv(corTable_rounded,"/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/CorrMatrix_AllSubjects.csv",row.names=TRUE,quote=FALSE)


############################
### PARTIAL CORRELATIONS ###
############################

#Calculate partial correlations
parCorTable <- partial.r(subjData_short,c(1:19),c(20:22))

#save table
write.csv(parCorTable,"/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/PartialCorrMatrix.csv",row.names=TRUE,quote=FALSE)


#############
### PLOTS ###
#############

jpeg("/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/AgeByOverall.jpg")
AgeByOverall <- plot(subjData$age, subjData$goassessItemBifactor4FactorOverallPsy)
dev.off()


####################
### INTERACTIONS ###
####################

#Age interaction with sex as covariate
CTgm_Mood_bifactor<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_short)
CTfrontal_Mood_bifactor<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_short)
CToccipital_Mood_bifactor<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_short)
CTparietal_Mood_bifactor<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_short)
CTtemporal_Mood_bifactor<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_short)

CTgm_Psych_bifactor<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_short)
CTfrontal_Psych_bifactor<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_short)
CToccipital_Psych_bifactor<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_short)
CTparietal_Psych_bifactor<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_short)
CTtemporal_Psych_bifactor<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_short)

CTgm_Ext_bifactor<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_short)
CTfrontal_Ext_bifactor<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_short)
CToccipital_Ext_bifactor<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_short)
CTparietal_Ext_bifactor<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_short)
CTtemporal_Ext_bifactor<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_short)

CTgm_Phb_bifactor<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_short)
CTfrontal_Phb_bifactor<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_short)
CToccipital_Phb_bifactor<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_short)
CTparietal_Phb_bifactor<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_short)
CTtemporal_Phb_bifactor<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_short)

CTgm_OverallPsy_bifactor<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)
CTfrontal_OverallPsy_bifactor<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)
CToccipital_OverallPsy_bifactor<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)
CTparietal_OverallPsy_bifactor<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)
CTtemporal_OverallPsy_bifactor<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)



#Tanner interaction with sex as covariate
CTgm_Mood_bifactor_tanner<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorMood*tanner, data=subjData_short)
CTfrontal_Mood_bifactor_tanner<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorMood*tanner, data=subjData_short)
CToccipital_Mood_bifactor_tanner<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorMood*tanner, data=subjData_short)
CTparietal_Mood_bifactor_tanner<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorMood*tanner, data=subjData_short)
CTtemporal_Mood_bifactor_tanner<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorMood*tanner, data=subjData_short)

CTgm_Psych_bifactor_tanner<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorPsych*tanner, data=subjData_short)
CTfrontal_Psych_bifactor_tanner<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorPsych*tanner, data=subjData_short)
CToccipital_Psych_bifactor_tanner<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorPsych*tanner, data=subjData_short)
CTparietal_Psych_bifactor_tanner<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorPsych*tanner, data=subjData_short)
CTtemporal_Psych_bifactor_tanner<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorPsych*tanner, data=subjData_short)

CTgm_Ext_bifactor_tanner<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorExt*tanner, data=subjData_short)
CTfrontal_Ext_bifactor_tanner<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorExt*tanner, data=subjData_short)
CToccipital_Ext_bifactor_tanner<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorExt*tanner, data=subjData_short)
CTparietal_Ext_bifactor_tanner<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorExt*tanner, data=subjData_short)
CTtemporal_Ext_bifactor_tanner<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorExt*tanner, data=subjData_short)

CTgm_Phb_bifactor_tanner<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorPhb*tanner, data=subjData_short)
CTfrontal_Phb_bifactor_tanner<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorPhb*tanner, data=subjData_short)
CToccipital_Phb_bifactor_tanner<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorPhb*tanner, data=subjData_short)
CTparietal_Phb_bifactor_tanner<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorPhb*tanner, data=subjData_short)
CTtemporal_Phb_bifactor_tanner<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorPhb*tanner, data=subjData_short)

CTgm_OverallPsy_bifactor_tanner<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorOverallPsy*tanner, data=subjData_short)
CTfrontal_OverallPsy_bifactor_tanner<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorOverallPsy*tanner, data=subjData_short)
CToccipital_OverallPsy_bifactor_tanner<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorOverallPsy*tanner, data=subjData_short)
CTparietal_OverallPsy_bifactor_tanner<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorOverallPsy*tanner, data=subjData_short)
CTtemporal_OverallPsy_bifactor_tanner<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorOverallPsy*tanner, data=subjData_short)


#Age interaction without sex as covariate
CTgm_Mood_bifactor2<-lm(CT_gmTotal~goassessItemBifactor4FactorMood*age, data=subjData_short)
CTfrontal_Mood_bifactor2<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorMood*age, data=subjData_short)
CToccipital_Mood_bifactor2<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorMood*age, data=subjData_short)
CTparietal_Mood_bifactor2<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorMood*age, data=subjData_short)
CTtemporal_Mood_bifactor2<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorMood*age, data=subjData_short)

CTgm_Psych_bifactor2<-lm(CT_gmTotal~goassessItemBifactor4FactorPsych*age, data=subjData_short)
CTfrontal_Psych_bifactor2<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorPsych*age, data=subjData_short)
CToccipital_Psych_bifactor2<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorPsych*age, data=subjData_short)
CTparietal_Psych_bifactor2<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorPsych*age, data=subjData_short)
CTtemporal_Psych_bifactor2<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorPsych*age, data=subjData_short)

CTgm_Ext_bifactor2<-lm(CT_gmTotal~goassessItemBifactor4FactorExt*age, data=subjData_short)
CTfrontal_Ext_bifactor2<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorExt*age, data=subjData_short)
CToccipital_Ext_bifactor2<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorExt*age, data=subjData_short)
CTparietal_Ext_bifactor2<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorExt*age, data=subjData_short)
CTtemporal_Ext_bifactor2<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorExt*age, data=subjData_short)

CTgm_Phb_bifactor2<-lm(CT_gmTotal~goassessItemBifactor4FactorPhb*age, data=subjData_short)
CTfrontal_Phb_bifactor2<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorPhb*age, data=subjData_short)
CToccipital_Phb_bifactor2<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorPhb*age, data=subjData_short)
CTparietal_Phb_bifactor2<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorPhb*age, data=subjData_short)
CTtemporal_Phb_bifactor2<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorPhb*age, data=subjData_short)

CTgm_OverallPsy_bifactor2<-lm(CT_gmTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)
CTfrontal_OverallPsy_bifactor2<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)
CToccipital_OverallPsy_bifactor2<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)
CTparietal_OverallPsy_bifactor2<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)
CTtemporal_OverallPsy_bifactor2<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_short)



##############################################################################################################################################

#################
### 11 and up ###
#################

#remove ACROSS.INCLUDE.11 from the short dataset
subjData_subset$ACROSS.INCLUDE.11 <- NULL


############
### PLOT ###
############

jpeg("/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/AgeByOverall_11andUp.jpg")
AgeByOverall_subset <- plot(subjData_subset$age, subjData_subset$goassessItemBifactor4FactorOverallPsy)
dev.off()


####################
### CORRELATIONS ###
####################

#make correlation table
corTable_subset <- cor(subjData_subset, method="spearman", use="complete.obs")

#Round correlation values to two decimal places
corTable_rounded_subset <- round(corTable_subset, 2)

#save table
write.csv(corTable_rounded_subset,"/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/CorrMatrix_11andUp.csv",row.names=TRUE,quote=FALSE)


############################
### PARTIAL CORRELATIONS ###
############################

#Calculate partial correlations
parCorTable_subset <- partial.r(subjData_subset,c(1:19),c(20:22))

#save table
write.csv(parCorTable_subset,"/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/PartialCorrMatrix_11andUp.csv",row.names=TRUE,quote=FALSE)


####################
### INTERACTIONS ###
####################

#With sex as a covariate
CTgm_Mood_bifactor3<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_subset)
CTfrontal_Mood_bifactor3<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_subset)
CToccipital_Mood_bifactor3<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_subset)
CTparietal_Mood_bifactor3<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_subset)
CTtemporal_Mood_bifactor3<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorMood*age, data=subjData_subset)

CTgm_Psych_bifactor3<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_subset)
CTfrontal_Psych_bifactor3<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_subset)
CToccipital_Psych_bifactor3<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_subset)
CTparietal_Psych_bifactor3<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_subset)
CTtemporal_Psych_bifactor3<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorPsych*age, data=subjData_subset)

CTgm_Ext_bifactor3<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_subset)
CTfrontal_Ext_bifactor3<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_subset)
CToccipital_Ext_bifactor3<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_subset)
CTparietal_Ext_bifactor3<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_subset)
CTtemporal_Ext_bifactor3<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorExt*age, data=subjData_subset)

CTgm_Phb_bifactor3<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_subset)
CTfrontal_Phb_bifactor3<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_subset)
CToccipital_Phb_bifactor3<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_subset)
CTparietal_Phb_bifactor3<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_subset)
CTtemporal_Phb_bifactor3<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorPhb*age, data=subjData_subset)

CTgm_OverallPsy_bifactor3<-lm(CT_gmTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)
CTfrontal_OverallPsy_bifactor3<-lm(CT_gmFrontalTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)
CToccipital_OverallPsy_bifactor3<-lm(CT_gmOccipitalTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)
CTparietal_OverallPsy_bifactor3<-lm(CT_gmParietalTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)
CTtemporal_OverallPsy_bifactor3<-lm(CT_gmTemporalTotal~sex+goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)


#Without sex as a covariate
CTgm_Mood_bifactor4<-lm(CT_gmTotal~goassessItemBifactor4FactorMood*age, data=subjData_subset)
CTfrontal_Mood_bifactor4<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorMood*age, data=subjData_subset)
CToccipital_Mood_bifactor4<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorMood*age, data=subjData_subset)
CTparietal_Mood_bifactor4<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorMood*age, data=subjData_subset)
CTtemporal_Mood_bifactor4<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorMood*age, data=subjData_subset)

CTgm_Psych_bifactor4<-lm(CT_gmTotal~goassessItemBifactor4FactorPsych*age, data=subjData_subset)
CTfrontal_Psych_bifactor4<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorPsych*age, data=subjData_subset)
CToccipital_Psych_bifactor4<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorPsych*age, data=subjData_subset)
CTparietal_Psych_bifactor4<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorPsych*age, data=subjData_subset)
CTtemporal_Psych_bifactor4<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorPsych*age, data=subjData_subset)

CTgm_Ext_bifactor4<-lm(CT_gmTotal~goassessItemBifactor4FactorExt*age, data=subjData_subset)
CTfrontal_Ext_bifactor4<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorExt*age, data=subjData_subset)
CToccipital_Ext_bifactor4<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorExt*age, data=subjData_subset)
CTparietal_Ext_bifactor4<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorExt*age, data=subjData_subset)
CTtemporal_Ext_bifactor4<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorExt*age, data=subjData_subset)

CTgm_Phb_bifactor4<-lm(CT_gmTotal~goassessItemBifactor4FactorPhb*age, data=subjData_subset)
CTfrontal_Phb_bifactor4<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorPhb*age, data=subjData_subset)
CToccipital_Phb_bifactor4<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorPhb*age, data=subjData_subset)
CTparietal_Phb_bifactor4<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorPhb*age, data=subjData_subset)
CTtemporal_Phb_bifactor4<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorPhb*age, data=subjData_subset)

CTgm_OverallPsy_bifactor4<-lm(CT_gmTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)
CTfrontal_OverallPsy_bifactor4<-lm(CT_gmFrontalTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)
CToccipital_OverallPsy_bifactor4<-lm(CT_gmOccipitalTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)
CTparietal_OverallPsy_bifactor4<-lm(CT_gmParietalTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)
CTtemporal_OverallPsy_bifactor4<-lm(CT_gmTemporalTotal~goassessItemBifactor4FactorOverallPsy*age, data=subjData_subset)



########################################################################################################################################

########################
### CREATE QUARTILES ###
########################

#Divide age into quartiles
subjData_quart <- subjData_short
subjData_quart$quartile <- with(subjData_quart, factor(
                            findInterval( subjData_quart$age, c(-Inf,
                               quantile(subjData_quart$age, probs=c(0.25, .5, .75)), Inf)),
                            labels=c("Q1","Q2","Q3","Q4")
      ))


#Split the file by the quartiles and put each subset into a separate data frame.
subsets<-split(subjData_quart, subjData_quart$quartile, drop=TRUE)
age1<-subsets[[1]]
age2<-subsets[[2]]
age3<-subsets[[3]]
age4<-subsets[[4]]

#Remove quartile factor variable before running correlations (otherwise get the error that x must be numeric). 
age1$quartile <- NULL
age2$quartile <- NULL
age3$quartile <- NULL
age4$quartile <- NULL



####################
### CORRELATIONS ###
####################


### AGE 1 (8 to 11.8 years)
#make correlation table
corTable_age1 <- cor(age1, method="pearson", use="complete.obs")

#Round correlation values to two decimal places
corTable_age1_round <- round(corTable_age1, 2)

#save table
write.csv(corTable_age1_round,"/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/CorrMatrix_Age1.csv",row.names=TRUE,quote=FALSE)

### AGE 2 (11.9 to 15.1 years)
#make correlation table
corTable_age2 <- cor(age2, method="pearson", use="complete.obs")

#Round correlation values to two decimal places
corTable_age2_round <- round(corTable_age2, 2)

#save table
write.csv(corTable_age2_round,"/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/CorrMatrix_Age2.csv",row.names=TRUE,quote=FALSE)

### AGE	3 (15.2 to 17.8 years)
#make correlation table
corTable_age3 <- cor(age3, method="pearson", use="complete.obs")

#Round correlation values to two decimal places
corTable_age3_round <- round(corTable_age3, 2)

#save table
write.csv(corTable_age3_round,"/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/CorrMatrix_Age3.csv",row.names=TRUE,quote=FALSE)

### AGE	4 (17.9 to 23.1 years)
#make correlation table
corTable_age4 <- cor(age4, method="pearson", use="complete.obs")

#Round correlation values to two decimal places
corTable_age4_round <- round(corTable_age4, 2)

#save table
write.csv(corTable_age4_round,"/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/TablesFigures/CorrMatrix_Age4.csv",row.names=TRUE,quote=FALSE)

#Graph the corr results
#library(corrgram)
#corrgram(age1, order=TRUE, lower.panel=panel.shade,
#  upper.panel=NULL, text.panel=panel.txt,
#  main="Youth ages 8 to 11.8 years")

