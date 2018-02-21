subjData <- read.csv("n1601_go1_datarel_020716.csv", header=TRUE, na.strings=".")
subjData$age <- (subjData$ageAtGo1Scan)/12
subjData$ACROSS.INCLUDE.12 <- 0
subjData$ACROSS.INCLUDE.12[subjData$age >= 12] <- 1
sum(subjData$ACROSS.INCLUDE.12)
subjData <- subjData[which(subjData$ACROSS.INCLUDE.12 == 1), ]
bblids <- subjData$bblid
write.table(bblids, "/data/joy/BBL/projects/pncT1AcrossDisorder_VolCT/subjectData/n1187_STAIbblids_12andUp.csv", row.names=F, col.names=F, sep=",")