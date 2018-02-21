#Read in subject data
subjData<-readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_JLF_volCtGmd_subjData.rds")
ctPaths<-read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_antsCtPathsWithScanids.csv", header=FALSE)

#Add headers to CT paths file.
names(ctPaths) <- c("scanid", "ctPath")

#Merge CT paths data
data.merge <- merge(subjData, ctPaths, by=c("scanid"), all=TRUE)

#Save file.
saveRDS(data.merge, "/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_JLF_volCtGmd_subjData_CtPaths.rds")

