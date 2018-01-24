##############################################
#### Prepare Subject-Level Data for MIDAS ####
##############################################

#Read subject file
subjData<-readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_ravens.rds")

#Make sex and race numeric indicator variabls (0 vs 1) instead of factors.
#0=Caucasian; 1=nonCaucasian
subjData$white<-as.integer(subjData$white)
subjData$whiteInd <- subjData$white-1
#0=female; 1=male
subjData$sex<-as.integer(subjData$sex)
subjData$sexInd <- subjData$sex-1 

#Replace substrings in a character vector (to change the ravens paths from chead paths to cbica paths)
library(stringi)
substr(subjData$RavensPath,1,6)

#Find position of x, the only unique identifier of position
xs <- stri_locate_all(pattern='x',subjData$RavensPath,fixed=T)
x_pos<-matrix(unlist(xs), ncol=2, byrow=T)

#Take string of just image name and add new path
subjData$cbicaRavensPath<-paste0("/cbica/home/kaczkura/t1Factors/images/tmpRavensSymLink/",substr(subjData$RavensPath, x_pos[,1]+6, nchar(as.character(subjData$RavensPath))))

#Subset only by rows needed for MIDAS-RAVENS analysis
subjData_short <- subjData[which(subjData$ACROSS.INCLUDE==1),c("cbicaRavensPath","age","sexInd","ageSq","whiteInd","meduCnbGo1","mprageMassICV","averageRating","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

#Shortened models for sanity checking
subjData_short2 <- subjData[which(subjData$ACROSS.INCLUDE==1),c("cbicaRavensPath","age")]

subjData_short3 <- subjData[which(subjData$ACROSS.INCLUDE==1),c("cbicaRavensPath","age","sexInd","mprageMassICV","goassessItemBifactor4FactorOverallPsy")]

#Save .csv to use for MIDAS
write.csv(subjData_short, '/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_ravens_MIDAS.csv', row.names=F, quote=F)
write.csv(subjData_short2, '/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_ravens_MIDAS_ageOnly.csv', row.names=F, quote=F)
write.csv(subjData_short3, '/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_ravens_MIDAS_OverallOnly.csv', row.names=F, quote=F)

