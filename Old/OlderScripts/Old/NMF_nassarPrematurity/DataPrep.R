#################
### LOAD DATA ###
#################
t1QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header=TRUE, na.strings="NA")
gestAge <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/gaData_final.csv", header=TRUE, na.strings="NA")
health <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20161214.csv", header=TRUE)
antsVol <-read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_antsCtVol_20161006.csv", header=TRUE)
jlfVol <-read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionVol_20170323.csv", header=TRUE) 
demo <-read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv", header=TRUE) 
cnb <-read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/cnb/n1601_cnb_factor_scores_tymoore_20151006.csv", header=TRUE) 

##################
#### DATA PREP ###
##################
#Remove missing GA data
gestAge2 <- gestAge[!is.na( gestAge $ ga ),]

#Count how many are not missing GA data (n=345)
NotMissingGA <- nrow(gestAge2)

#Define the preterm group as <37 weeks gestation (normal=257, preterm=88)
gestAge2 $ preterm <- 0 
gestAge2 $ preterm [which( gestAge2 $ ga < 37 )]<- 1 
NormalVsPretermTable <- table( gestAge2 $ preterm ) 

#Transform the age variable from months to years
demo$age <- (demo$ageAtScan1)/12

#Recode male as 0 and female as 1
demo$sex[which(demo$sex==1)] <- 0
demo$sex[which(demo$sex==2)] <- 1

##################
### MERGE DATA ###
##################
dataComb1 <-merge( gestAge2, t1QA, by="bblid", all=FALSE) 
dataComb2 <-merge( dataComb1, health, by=c("bblid","scanid"), all=FALSE) 
dataComb3 <-merge( dataComb2, antsVol, by=c("bblid","scanid"), all=FALSE) 
dataComb4 <-merge( dataComb3, jlfVol, by=c("bblid","scanid"), all=FALSE) 
dataComb5 <-merge( dataComb4, demo, by=c("bblid","scanid"), all=FALSE) 
dataComb6 <-merge( dataComb5, cnb, by=c("bblid","scanid"), all=FALSE)

#Count the number of subjects (still n=345)
n <- nrow(dataComb6)

#################################
### APPLY EXCLUSIONS AND SAVE ### 
#################################
#Exclude those with health problems and problems with their t1 data
data.final <- dataComb6 [which( dataComb6 $ healthExcludev2 == 0 & dataComb6 $ t1Exclude == 0 ),]

#Save final dataset
write.csv(data.final, file="/data/joy/BBL/projects/pncPreterm/subjectData/n281_nassarPrematurity_subjData.csv", row.names=FALSE)

#Save the bblids and scanids for the final sample (n=281)
IDs <- c("bblid", "scanid")
bblidsScanids <- data.final[IDs]

#Remove header
names(bblidsScanids) <- NULL

#Save list
write.table(bblidsScanids, "/data/joy/BBL/projects/pncPreterm/subjectData/n281_nassarPrematurity_bblids_scanids.csv", row.names=F, col.names=F, sep=",")
