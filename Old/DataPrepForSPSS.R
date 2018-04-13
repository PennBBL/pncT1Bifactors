#####################################################
#### Pull data for canonical correlation in SPSS ####
#####################################################

#Load data
subjData <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")

subjData_short <- subjData[c(grep("bblid|4factorv2|corrtraits|Nmf18|mprage_jlf_ct",names(subjData)))]

write.csv(subjData_short, file="/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjDataForSPSS.csv", row.names=F, quote=F)

