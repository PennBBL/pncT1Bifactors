############################################
#### Create list of bblids and scanids  ####
############################################

##Create bblid and scanid list

#Read in subject level data file
subjData<-readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_JLF_volCtGmd_subjData.rds")

#Select variables
IDs <- c("bblid", "scanid")
bblidsScanids <- subjData[IDs]

#Remove header
names(bblidsScanids) <- NULL

#Save file.
write.table(bblidsScanids, "/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_bblids_scanids.csv", row.names=F, col.names=F, sep=",")
