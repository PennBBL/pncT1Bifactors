############################################
#### Create list of bblids and scanids  ####
############################################

##Create bblid and scanid list

#Read in subject level data file
subjData<-read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n279_JLF_vol_nassarPrematurity_subjData.csv", header=TRUE, na.strings="NA")

#Select variables
IDs <- c("bblid", "scanid")
bblidsScanids <- subjData[IDs]

#Remove header
names(bblidsScanids) <- NULL

#Save file.
write.table(bblidsScanids, "/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n279_nassarPrematurity_bblids_scanids.csv", row.names=F, col.names=F, sep=",")
