##############################################################
#### Create list of bblids for voxelwise T1 data analyses ####
##############################################################

#Read in subject level data file
subjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1386_MARS_datarel_020716.rds")

#Select variables
IDs <- c("bblid", "scanid")
data.final <- subjData[IDs]

#Remove header
names(data.final) <- NULL

#Save file
write.csv(data.final, "/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/bblids_scanids.csv", row.names=F)
