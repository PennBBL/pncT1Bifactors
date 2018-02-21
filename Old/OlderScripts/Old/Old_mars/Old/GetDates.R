#Merge your list of bblids that are missing their Ravens folder with the variable "date" from the latest subject data release.

#Read in subject data
subjData<-readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1386_MARS_datarel_020716.rds")
bblids<-read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/MissingDirs.csv", header=FALSE)

#Add headers to bblids file.
names(bblids) <- c("bblid", "scanid")

#Pull out bblid, scanid, and variable "date" from the subject data release.
subjDates<-subjData[,c(1,2,2330)]

#Merge bblids and dates data
#WARNING: Merging files with unequal cases will cause the non-matched bblids to be deleted (which is what we want in this case).
data.merge <- merge(bblids, subjDates, by=c("bblid","scanid"))

#Put bblid list in order
data.merge <- data.merge[order(data.merge$bblid),]

#Save file.
write.table(data.merge, "/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/MissingDirs_withDates.csv", row.names=F, col.names=F, sep=",")
