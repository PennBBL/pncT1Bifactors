#Merge the 500 NMF components with the variables needed for Zaixu to do multivariate analyses. 

#################
### LOAD DATA ###
#################
subjData <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")
nmfData <- read.csv("/data/jux/BBL/projects/pncNmf/subjectData/n1396_Nmf500Bases_CT_bblids.csv", header=TRUE)

##################################
### PULL VARIABLES OF INTEREST ###
##################################
#Covariates = age and sex; DV = fear.
Vars <- subjData[c(grep("bblid|scanid|^age$|sex|phobias_4factorv2",names(subjData)))]

##################
### MERGE DATA ###
##################
#NOTE: We use all=FALSE because NMF was run on n=1396 (not removing those with missing clinical data) but the final subject level data has n=1394 (after removing two people missing clinical scores).
dataComb <-merge(Vars, nmfData, by=c("bblid","scanid"), all=FALSE)

###################################
### SAVE THE DATA AS A CSV FILE ###
###################################
write.csv(dataComb, file="/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_ctNMF500.csv", row.names=F, quote=F)

