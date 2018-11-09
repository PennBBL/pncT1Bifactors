#######################
#### PULL T VALUES ####
#######################

#Pull t-values {DOUBLE CHECK THAT [7,3] IS CORRECT FOR YOUR T-VALUES}
tNmf <- sapply(NmfModels, function(x) summary(x)$coefficients[7,3])

#Print to two decimal places and only keep the components that survived FDR correction
tNmf_round <- round(tNmf,2)[p_mood_fdr<0.05]

#Convert to data frame
t <- as.data.frame(tNmf_round)

#Convert the names to data frame
names <- as.data.frame(Nmf_mood_fdr_names)

#Combine NMF component names and t values into one dataframe
combined <- cbind(names,t)

#Rename variables
data.final <- rename(combined, NMFcomponent = Nmf_mood_fdr_names, tvalue = tNmf_round)

#Save as a .csv {CHANGE PATH}
write.csv(data.final, file="/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/FileName.csv", row.names=F, quote=F)
