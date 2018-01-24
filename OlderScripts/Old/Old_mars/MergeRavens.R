#Read in subject data
subjData<-readRDS('/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_diag.rds')
ravensPaths<-read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_RavensPaths.csv", header=FALSE)

#Add headers to ravensPaths file and change empty cells to NA.
names(ravensPaths) <- c("bblid", "scanid", "RavensPath")
ravensPaths$RavensPath[which(ravensPaths$RavensPath == "")] <- NA

#Merge Ravens paths data
#WARNING: Merging files with unequal cases will cause the non-matched bblids to be deleted (which is not what we want in this case, so we have to add "all=TRUE").
data.merge <- merge(subjData, ravensPaths, by=c("bblid","scanid"), all=TRUE)

#Make sex an ordered variable (necessary for running GAM model)
data.merge$sex <- ordered(data.merge$sex)

#To see the struture (e.g. data type) of multiple variables, use: str(data.merge)

#Save file.
saveRDS(data.merge, "/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_ravens.rds")

#Remove the 17 people with missing mom edu data and save file with different name. 
data.merge2<-data.merge[!is.na(data.merge$meduCnbGo1), ]

#Save file.
saveRDS(data.merge2, "/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1368_MARS_datarel_020716_ravens.rds")
