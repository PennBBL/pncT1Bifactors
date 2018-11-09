######################################
#### NMF AS PREDICTORS OF OVERALL ####
######################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Remove NAs (125 subjects are missing cognitive data on all 12 cog accuracy factors)
data.NMF <- data.NMF[!is.na(data.NMF$abfAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$attAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$wmAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$vmemAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$fmemAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$smemAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$lanAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$nvrAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$spaAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$eidAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$ediAZ),]
data.NMF <- data.NMF[!is.na(data.NMF$adiAZ),]
n <- nrow(data.NMF)
      
#CT components predicting Overall
NmfModel <- lm(overall_psychopathology_4factorv2 ~ age + sex + 
                 abfAZ + attAZ + wmAZ + vmemAZ + fmemAZ + smemAZ + lanAZ + nvrAZ + spaAZ + eidAZ + ediAZ + adiAZ +
    	    	     Ct_Nmf18C1 + Ct_Nmf18C2 + Ct_Nmf18C3 + Ct_Nmf18C4 + Ct_Nmf18C5 + Ct_Nmf18C6 + Ct_Nmf18C7 + Ct_Nmf18C8 + 
    	    	     Ct_Nmf18C9 + Ct_Nmf18C10 + Ct_Nmf18C11 + Ct_Nmf18C12 + Ct_Nmf18C13 + Ct_Nmf18C14 + Ct_Nmf18C15 + 
                 Ct_Nmf18C16 + Ct_Nmf18C17 + Ct_Nmf18C18, data = data.NMF)

NullModel <- lm(overall_psychopathology_4factorv2 ~ age + sex + abfAZ + attAZ + wmAZ + vmemAZ + fmemAZ + smemAZ + lanAZ +nvrAZ + spaAZ + eidAZ + ediAZ + adiAZ, data = data.NMF)

#Look at model summaries
NmfSummary <- summary(NmfModel)
NullSummary <- summary(NullModel)

#Correlation between actual overall scores and predicted overall scores in a null model
nullCor <- cor.test(predict(NullModel), data.NMF$overall_psychopathology_4factorv2)

#Correlation between actual overall scores and predicted overall scores in a model with age, sex, cognition, and the 18 NMF networks
nmfCor <- cor.test(predict(NmfModel), data.NMF$overall_psychopathology_4factorv2)

#Compare the null model to the Nmf model with an F test
Ftest <- anova(NmfModel,NullModel)
