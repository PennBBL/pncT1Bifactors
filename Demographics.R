###############################
#### Table 1: Demographics ####
###############################

###############################
### Load data and libraries ###
###############################

subjData <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")

#Load libraries
library(plyr)
library(varhandle)

#################################
### Total sample demographics ###
#################################

#Total sample means
meanAge_total <- mean(subjData$age)

#Total sample sd
sdAge_total <- sd(subjData$age)

#Total number of males and females (0=male, 1=female)
sexTable_total <- table(subjData$sex)

#Total age range
rangeAge_total <- range(subjData$age)

#Total number of whites (0=non-white, 1=white)
whiteTable_total <- table(subjData$white)

#Maternal Education Summary table
medu1_total<-table(subjData$medu1)

#medu1 - 12 years or less
medu1_12orLess <-length(which(subjData$medu1<=12))

#medu1 - greater than 12 years
medu1_13andUp <- length(which(subjData$medu1>12))

#medu1 - missing
medu1_missing <- length(which(is.na(subjData$medu)))
       
######Psychopathologies######

#Typically Developing
Td_total <- sum(subjData$Td,na.rm=TRUE)

#ADHD Diagnosis
ADHD_total <-sum(subjData$Add,na.rm=TRUE)

#Agoraphobia Diagnosis
Agr_total <-sum(subjData$Agr,na.rm=TRUE)

#Anorexia Diagnosis
Ano_total <-sum(subjData$Ano,na.rm=TRUE)

#Bulimia Diagnosis
Bul_total <-sum(subjData$Bul,na.rm=TRUE)

#Conduct Disorder Diagnosis
Con_total <-sum(subjData$Con,na.rm=TRUE)

#Generalized Anxiety Disorder Diagnosis
Gad_total <-sum(subjData$Gad,na.rm=TRUE)

#Major Depression Diagnosis
Mdd_total <-sum(subjData$Mdd,na.rm=TRUE)

#Mania Diagnosis
Man_total <-sum(subjData$Man,na.rm=TRUE)

#OCD Diagnosis
Ocd_total <-sum(subjData$Ocd,na.rm=TRUE)

#ODD Diagnosis
Odd_total <-sum(subjData$Odd,na.rm=TRUE)

#Panic Diagnosis
Pan_total <-sum(subjData$Pan,na.rm=TRUE)

#Psychosis spectrum Diagnosis
Ps_total <-sum(subjData$Ps,na.rm=TRUE)

#PTSD Diagnosis
Ptd_total <-sum(subjData$Ptd,na.rm=TRUE)

#Seperation Anxiety Diagnosis
Sep_total <-sum(subjData$Sep,na.rm=TRUE)

#Social Phobia Diagnosis
Soc_total <-sum(subjData$Soc,na.rm=TRUE)

#Specific Phobia Diagnosis
Sph_total <-sum(subjData$Sph,na.rm=TRUE)




