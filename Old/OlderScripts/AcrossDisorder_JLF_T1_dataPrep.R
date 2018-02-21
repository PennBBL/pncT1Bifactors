##########################
### LOAD DATA & SUBSET ###
##########################

#subject level data (GO1 data release; n=1601) 
data.go1release <- read.csv("/data/joy/BBL/projects/pncAslAcrossDisorder/subjectData/n1601_go1_datarel_020716.csv", header=TRUE, na.strings=".")

#Remove the old demographics variables and add the new ones
data.go1release$sex <- NULL
data.go1release$race <- NULL
data.go1release$race2 <- NULL
data.go1release$ethnicity <- NULL
data.newDemo <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreezeDec2016/demographics/n1601_demographics_go1_20161212.csv", header=TRUE, na.strings=".")

#Add the new health exclusion variable (healthExcludev2)
data.newHealthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreezeDec2016/health/n1601_health_20161214.csv", header=TRUE, na.strings=".")

#Correlated Traits(n=9350, NOTE: there are two people from the 1601 sample who are missing correlated traits data)
data.CorrTraits <- read.csv("/data/joy/BBL/projects/pncAslAcrossDisorder/subjectData/GOA_itemwise_corr-traits_scores_12-19-2016.csv", header=TRUE, na.strings = ".")

#Bifactors (n=9350, NOTE: there are two people from the 1601 sample who are missing bifactors data)
data.bifactors <- read.csv("/data/joy/BBL/projects/pncAslAcrossDisorder/subjectData/GOA_itemwise_bifactor_scores_12-17-2016.csv", header=TRUE, na.strings=".")

#STAI factors (the STAI bifactor variables already included in the data release are missing data; after removing those under 12yrs, n=1134 (State) and n=1124 (Trait))
data.state <- read.csv("/data/joy/BBL/projects/pncAslAcrossDisorder/subjectData/STAI_State_Pre-Scan_Bifactor_FScores_1601_2.csv", header=TRUE, na.strings=".")
data.trait <- read.csv("/data/joy/BBL/projects/pncAslAcrossDisorder/subjectData/STAI_Trait_Bifactor_FScores_1601_2.csv", header=TRUE, na.strings=".")

#antsCT global volumes (n=1601; total GM volume, total WM volume, CSF, etc.)
data.globalVol <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/n1601_antsCtVol.csv", header=TRUE, na.strings=".")

#JLF volume, cortical thickness, gmd, and their QA exclusion variables(n=1601)
data.vol <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/n1601_jlfVol.csv", header=TRUE, na.strings=".")
data.ct <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/n1601_jlfCt.csv", header=TRUE, na.strings=".")
data.gmd <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/n1601_jlfGMD.csv", header=TRUE, na.strings=".")
data.t1.QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/n1601_t1QaData.csv", header=TRUE, na.strings=".")
data.rest.QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/rest/n1601_RestQAData.csv", header=TRUE, na.strings="NA")

#ASL data and its QA exclusion variables (n=1601)
data.asl <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/asl/n1601_jlfPcaslValues.csv", header=TRUE, na.strings="NA")
data.asl.QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/asl/n1601_PcaslQaData.csv", header=TRUE, na.strings="NA")

##Merge data using bblid and scanid
#WARNING: Merging files with unequal cases will cause the non-matched bblids to be deleted; to merge all cases and create NAs for missing data, add all = TRUE).
data.merge1 <- merge(data.go1release, data.newDemo, by=c("bblid","scanid"), all=TRUE)
data.merge2 <- merge(data.merge1, data.newHealthExclude, by=c("bblid","scanid"), all=TRUE)
data.merge3 <- merge(data.merge2, data.CorrTraits, by="bblid", all=TRUE)
data.merge4 <- merge(data.merge3, data.bifactors, by="bblid", all=TRUE)
data.merge5 <- merge(data.merge4, data.state, by="bblid", all=TRUE)
data.merge6 <- merge(data.merge5, data.trait, by="bblid", all=TRUE)
data.merge7 <- merge(data.merge6, data.globalVol, by=c("bblid","scanid"), all=TRUE)
data.merge8 <- merge(data.merge7, data.vol, by=c("bblid","scanid"), all=TRUE)
data.merge9 <- merge(data.merge8, data.ct, by=c("bblid","scanid"), all=TRUE)
data.merge10 <- merge(data.merge9, data.gmd, by=c("bblid","scanid"), all=TRUE)
data.merge11 <- merge(data.merge10, data.t1.QA, by=c("bblid","scanid"), all=TRUE)
data.merge12 <- merge(data.merge11, data.rest.QA, by=c("bblid","scanid"), all=TRUE)
data.merge13 <- merge(data.merge12, data.asl, by=c("bblid","scanid"), all=TRUE)
data.merge14 <- merge(data.merge13, data.asl.QA, by=c("bblid","scanid"), all=TRUE)

#Retain only the n=1601 bblids
data.n1601 <- data.merge14[match(data.go1release$bblid, data.merge14$bblid, nomatch=0),] 

#Put bblids in ascending order
data.ordered <- data.n1601[order(data.n1601$bblid),]

##Create an exclusion criteria variable
#The exclusion criteria are: healthExcludev2=1 (1=Excludes those with medical rating 3/4, major incidental findings that distort anatomy, psychoactive medical medications); t1Exclude=1 (1=problems with their T1 data, which also takes into account averageManualRating=0 (bad QA)); pcasl=1 (1=problems with their ASL data).
#NOTE: the new "healthExcludev2" replaces the old more stringent exclusion criteria "healthExclude"
data.ordered$ACROSS.INCLUDE <- 1
data.ordered$ACROSS.INCLUDE[data.ordered$healthExcludev2==1] <- 0
data.ordered$ACROSS.INCLUDE[data.ordered$t1Exclude==1] <- 0

##Exclude everyone that has one or more of these exclusion criteria
data.exclude <- data.ordered[which(data.ordered$ACROSS.INCLUDE == 1), ]

#Remove the two subjects (92500 and 108631) missing bifactor scores
data.final<-data.exclude[complete.cases(data.exclude$Bifactor_Overall_Psychopathology), ]

##After exclusion criteria, there are 1375 subjects
subjData <- data.final



#################################
### CREATE GLOBAL GM VARIABLE ###
#################################

#GM-- these are the gray matter regions-- would only FDR across these  regions
#NOTE: all of the JLF ASL variables are GM regions; WM and CSF don't make sense for ASL
dataAsl <- subjData[,grep("pcasl_jlf_cbf",names(subjData))]
dataAslGm <- dataAsl

#Get total GM volume
dataAslGm <- data.matrix(dataAslGm)
subjData$Asl_gmTotal <- rowSums(dataAslGm)


##############################
### SEPARATE GM INTO LOBES ###
##############################

#cerebellum (not included in asl data)

#subcort (14? regions)
dataAslSubcort <- dataAsl[,c(grep("Caudate",names(dataAsl)), grep("Putamen",names(dataAsl)), grep("Pallidum",names(dataAsl)), grep("Accumbens",names(dataAsl)),
        grep("Thal",names(dataAsl)), grep("BasForebr",names(dataAsl)), grep("Hippo",names(dataAsl)), grep("Amyg",names(dataAsl)))]

#frontal (50 regions)
dataAslFrontal <- dataAsl[,unique(c(grep("OrG",names(dataAsl)), grep("ins",names(dataAsl)), grep("FRP",names(dataAsl)), grep("FG",names(dataAsl)), 
        grep("PrG",names(dataAsl)), grep("Gre",names(dataAsl)), grep("MFC",names(dataAsl)), grep("SCA",names(dataAsl)), grep("SMC",names(dataAsl)),
        grep("FRP",names(dataAsl)),grep("CO",names(dataAsl)), grep("FO",names(dataAsl)), grep("PO",names(dataAsl)), grep("CgG",names(dataAsl))))]

#occipital (16 regions)
dataAslOccipital <- dataAsl[,unique(c(grep("OG",names(dataAsl)), grep("OFuG",names(dataAsl)), grep("OCP",names(dataAsl)), grep("SOG",names(dataAsl)),
        grep("Calc",names(dataAsl)), grep("Cun",names(dataAsl)), grep("LiG",names(dataAsl))))]

#parietal (12 regions)
dataAslParietal <- dataAsl[,unique(c(grep("AnG",names(dataAsl)), grep("PoG",names(dataAsl)), grep("SMG",names(dataAsl)), grep("SPL",names(dataAsl)),
        grep("Pcu",names(dataAsl))))]

#temporal (20 regions)
dataAslTemporal <- dataAsl[,unique(c(grep("_FuG",names(dataAsl)), grep("TG",names(dataAsl)), grep("TMP",names(dataAsl)), grep("PP",names(dataAsl)),
        grep("PT",names(dataAsl)), grep("PHG",names(dataAsl)),grep("Ent",names(dataAsl))))]

#get total GM for lobes
dataAslSubcort <- data.matrix(dataAslSubcort)
subjData$Asl_gmSubcortTotal <- rowSums(dataAslSubcort)
dataAslFrontal <- data.matrix(dataAslFrontal)
subjData$Asl_gmFrontalTotal <- rowSums(dataAslFrontal)
dataAslOccipital <- data.matrix(dataAslOccipital)
subjData$Asl_gmOccipitalTotal <- rowSums(dataAslOccipital)
dataAslParietal <- data.matrix(dataAslParietal)
subjData$Asl_gmParietalTotal <- rowSums(dataAslParietal)
dataAslTemporal <- data.matrix(dataAslTemporal)
subjData$Asl_gmTemporalTotal <- rowSums(dataAslTemporal)



###########################
### TRANSFORM VARIABLES ###
###########################

#Transform the age variable from months to years ("ageAtGo1Scan" changed to "ageAtScan1" in new demographics file)
subjData$age <- (subjData$ageAtGo1Scan)/12

#Define age squared
subjData$ageSq <- I(scale(subjData$age, scale=FALSE, center=TRUE)^2)

#sex (needs to be an ordered variable when using spline interactions)
subjData$sex[which(subjData$sex==1)] <- "male"
subjData$sex[which(subjData$sex==2)] <- "female"
subjData$sex <- as.ordered(as.factor(subjData$sex))

#race (make white vs non-white)
subjData$white <- NA
subjData$white[which(subjData$race==1)] <- "Caucasian"
subjData$white[which(subjData$race!=1)] <- "notCaucasian"
subjData$white <- as.ordered(as.factor(subjData$white))

#Make asl motion a numeric variable (it is a factor in the data release)
subjData$pcaslRelMeanRMSMotion <- as.numeric(subjData$pcaslRelMeanRMSMotion)

#combine items "tannerGirl7" and "tannerBoy6" to create one variable
subjData$tannerBoyGirl <- subjData$tannerGirl7
subjData$tannerBoyGirl[is.na(subjData$tannerGirl7)] <- subjData$tannerBoy6[is.na(subjData$tannerGirl7)]

#tanner is a categorical variable with 1) pre-pubertal (tanner=1-3), 2) mid-pubertal (tanner=4), and 3) post-pubertal (tanner=5) groups (as per the PNAS article)
subjData$tanner<-NA
subjData$tanner[subjData$tannerBoyGirl==5]<-3
subjData$tanner[subjData$tannerBoyGirl==4]<-2
subjData$tanner[subjData$tannerBoyGirl<=3]<-1

#make tanner a factor
subjData$tanner<-as.factor(subjData$tanner)



#########################################
#### MAKE DIAGNOSIS FACTOR VARIABLES ####
#########################################

##Make variables where 1 = diagnosis.

#ADHD
subjData$Add <- NA
subjData$Add[which(subjData$goassessSmryAdd==4)] <- 1

#Agoraphobia
subjData$Agr <- NA
subjData$Agr[which(subjData$goassessSmryAgr==4)] <- 1

#Anorexia
subjData$Ano <- NA
subjData$Ano[which(subjData$goassessSmryAno==4)] <- 1

#Bulimia
subjData$Bul <- NA
subjData$Bul[which(subjData$goassessSmryBul==4)] <- 1

#Conduct Disorder
subjData$Con <- NA
subjData$Con[which(subjData$goassessSmryCon==4)] <- 1

#Generalized Anxiety Disorder
subjData$Gad <- NA
subjData$Gad[which(subjData$goassessSmryGad==4)] <- 1

#Mania
subjData$Man <- NA
subjData$Man[which(subjData$goassessSmryMan==4)] <- 1

#Major Depressive Disorder
subjData$Mdd <- NA
subjData$Mdd[which(subjData$goassessSmryDep==4)] <- 1

#OCD
subjData$Ocd <- NA
subjData$Ocd[which(subjData$goassessSmryOcd==4)] <- 1

#Oppositional Defiant Disorder
subjData$Odd <- NA
subjData$Odd[which(subjData$goassessSmryOdd==4)] <- 1

#Panic Disorder
subjData$Pan <- NA
subjData$Pan[which(subjData$goassessSmryPan==4)] <- 1

#Psychosis
subjData$Ps <- NA
subjData$Ps[which(subjData$goassessDxpmr4=="4PS")] <- 1

#Posttraumatic Stress Disorder
subjData$Ptd <- NA
subjData$Ptd[which(subjData$goassessSmryPtd==4)] <- 1

#Separation Anxiety Disorder
subjData$Sep <- NA
subjData$Sep[which(subjData$goassessSmrySep==4)] <- 1

#Social Anxiety Disorder
subjData$Soc <- NA
subjData$Soc[which(subjData$goassessSmrySoc==4)] <- 1

#Specific Phobia
subjData$Sph <- NA
subjData$Sph[which(subjData$goassessSmryPhb==4)] <- 1

#Typically Developing
dxNames <- c("bblid","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf <- data.matrix(subjData[,dxNames])
subjData$totDx <- rowSums(dxDf[,2:17], na.rm=TRUE) #This is how many people have how many diagnoses: sum(totDx==0):414, sum(totDx==1):307, sum(totDx>=2):638
subjData$Td <- 0
subjData$Td[which(subjData$totDx==0)] <- 1



#####################################
#### MAKE TD THE REFERENCE GROUP ####
#####################################

subjData$Add[which(subjData$Td==1)] <- 0
subjData$Agr[which(subjData$Td==1)] <- 0
subjData$Ano[which(subjData$Td==1)] <- 0
subjData$Bul[which(subjData$Td==1)] <- 0
subjData$Con[which(subjData$Td==1)] <- 0
subjData$Gad[which(subjData$Td==1)] <- 0
subjData$Man[which(subjData$Td==1)] <- 0
subjData$Mdd[which(subjData$Td==1)] <- 0
subjData$Ocd[which(subjData$Td==1)] <- 0
subjData$Odd[which(subjData$Td==1)] <- 0
subjData$Pan[which(subjData$Td==1)] <- 0
subjData$Ps[which(subjData$Td==1)] <- 0
subjData$Ptd[which(subjData$Td==1)] <- 0
subjData$Sep[which(subjData$Td==1)] <- 0
subjData$Soc[which(subjData$Td==1)] <- 0
subjData$Sph[which(subjData$Td==1)] <- 0



#########################################
### Create Anx vs TD factor variables ###
#########################################

##Create a factor variable that will compare TD to All Anx (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph).

subjData$AllAnxTd <- NA
subjData$AllAnxTd[subjData$Agr==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Gad==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Ocd==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Pan==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Ptd==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Sep==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Soc==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Sph==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Td==1] <- "Td"
subjData$AllAnxTd <- as.factor(subjData$AllAnxTd)

##Create "pure" anxiety groups with no anxiety comorbidities

#PTSD (n=55)
subjData$PurePtd <- 0
subjData$PurePtd[subjData$Ptd==1] <- 1
subjData$PurePtd[subjData$Agr==1] <- 0
subjData$PurePtd[subjData$Gad==1] <- 0
subjData$PurePtd[subjData$Pan==1] <- 0
subjData$PurePtd[subjData$Sep==1] <- 0
subjData$PurePtd[subjData$Soc==1] <- 0
subjData$PurePtd[subjData$Sph==1] <- 0
subjData$PurePtd[subjData$Ocd==1] <- 0

#OCD (n=3)
subjData$PureOcd <- 0
subjData$PureOcd[subjData$Ocd==1] <- 1
subjData$PureOcd[subjData$Agr==1] <- 0
subjData$PureOcd[subjData$Gad==1] <- 0
subjData$PureOcd[subjData$Pan==1] <- 0
subjData$PureOcd[subjData$Sep==1] <- 0
subjData$PureOcd[subjData$Soc==1] <- 0
subjData$PureOcd[subjData$Sph==1] <- 0
subjData$PureOcd[subjData$Ptd==1] <- 0

#Anxiety disorders (Agr, Gad, Pan, Sep, Soc, Sph) (n=488)
subjData$PureAnx <- 0
subjData$PureAnx[subjData$Agr==1] <- 1
subjData$PureAnx[subjData$Gad==1] <- 1
subjData$PureAnx[subjData$Pan==1] <- 1
subjData$PureAnx[subjData$Sep==1] <- 1
subjData$PureAnx[subjData$Soc==1] <- 1
subjData$PureAnx[subjData$Sph==1] <- 1
subjData$PureAnx[subjData$Ocd==1] <- 0
subjData$PureAnx[subjData$Ptd==1] <- 0

##Create a factor variable that will compare TD to coarsely defined "pure" anxiety groups (PTSD and the other anxiety disorders (Agr, Gad, Sep, Soc, Sph)).
#NOTE: There were only 3 subjects with "pure" OCD (no anxiety comorbidity), so OCD is not included (use table(subjData$CoarseAnxTd) to see the numbers per group).
#In order to make Td the comparison group, we need to use numbers instead of labels so that Td=1.

subjData$CoarseAnxTd <- NA
subjData$CoarseAnxTd[subjData$Td==1] <- 1
subjData$CoarseAnxTd[subjData$PurePtd==1] <- 2
subjData$CoarseAnxTd[subjData$PureAnx==1] <- 3
subjData$CoarseAnxTd <- as.factor(subjData$CoarseAnxTd)



#################
### SAVE DATA ###
#################

saveRDS(subjData,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1375_JLF_volCtGmd_subjData.rds")



###################################################
### SUBSET DATA BY ANXIETY DISORDERS CATEGORIES ###
###################################################

#All Anxiety Disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) and TD
AllAnxTdSubjData <- subjData
AllAnxTdSubjData <- AllAnxTdSubjData[which(AllAnxTdSubjData$AllAnxTd != "NA"), ]

#Save file
saveRDS(AllAnxTdSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1105_JLF_volCtGmd_AllAnxTdSubjData.rds")


#All Anxiety Disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) (No TD)
AllAnxSubjData <- subjData
AllAnxSubjData <- AllAnxSubjData[which(AllAnxSubjData$AllAnxTd == "AllAnx"), ]

#Save file
saveRDS(AllAnxSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n685_JLF_volCtGmd_AllAnxSubjData.rds")



#####################
### STAI subjData ###
#####################

#Subset the data by those 12 and up for STAI analyses (STAI requires reading level of 5th grade or higher).
staiSubjData<-subjData
staiSubjData$ACROSS.INCLUDE.12<-0
staiSubjData$ACROSS.INCLUDE.12[staiSubjData$age>=12]<-1
staiSubjData$ACROSS.INCLUDE.12[staiSubjData$ACROSS.INCLUDE==0]<-0
staiSubjData <- staiSubjData[which(staiSubjData$ACROSS.INCLUDE.12 == 1), ]

#Save file
saveRDS(staiSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1031_JLF_volCtGmd_stai_subjData.rds")


###########################
### SUBSET BY 11 and up ###
###########################

#Proband sample: Exclude those younger than 11 years because only collateral reports of psychiatric symptoms were available (Wolf et al., 2015 JAMA).
subjData_subset <- subjData
subjData_subset$ACROSS.INCLUDE.11 <- 0
subjData_subset$ACROSS.INCLUDE.11[subjData_subset$age >= 11] <- 1
subjData_subset <- subjData_subset[which(subjData_subset$ACROSS.INCLUDE.11 == 1), ]

#save file
saveRDS(subjData_subset,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1115_JLF_volCtGmd_11andUp_subjData.rds")



##################################
### SEPARATE MALES AND FEMALES ###
##################################

#Make separate datasets for males and females (for running voxelwise models without sex terms in model).

##n1274
#male
subjData_male <- subjData
subjData_male$ACROSS.INCLUDE.MALE <- 0
subjData_male$ACROSS.INCLUDE.MALE[subjData_male$sex == "male"] <- 1
subjData_male <- subjData_male[which(subjData_male$ACROSS.INCLUDE.MALE == 1), ]

#save file
saveRDS(subjData_male,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n649_JLF_volCtGmd_Male_subjData.rds")

#female
subjData_female <- subjData
subjData_female$ACROSS.INCLUDE.FEMALE <- 0
subjData_female$ACROSS.INCLUDE.FEMALE[subjData_female$sex == "female"] <- 1
subjData_female <- subjData_female[which(subjData_female$ACROSS.INCLUDE.FEMALE == 1), ]

#save file
saveRDS(subjData_female,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n726_JLF_volCtGmd_Female_subjData.rds")


##n1042
#male
subjData_male2 <- subjData_subset
subjData_male2$ACROSS.INCLUDE.MALE <- 0
subjData_male2$ACROSS.INCLUDE.MALE[subjData_male2$sex == "male"] <- 1
subjData_male2 <- subjData_male2[which(subjData_male2$ACROSS.INCLUDE.MALE == 1), ]

#save file
saveRDS(subjData_male2,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n515_JLF_volCtGmd_11andUpMale_subjData.rds")

#female
subjData_female2 <- subjData_subset
subjData_female2$ACROSS.INCLUDE.FEMALE <- 0
subjData_female2$ACROSS.INCLUDE.FEMALE[subjData_female2$sex == "female"] <- 1
subjData_female2 <- subjData_female2[which(subjData_female2$ACROSS.INCLUDE.FEMALE == 1), ]

#save file
saveRDS(subjData_female2,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n600_JLF_volCtGmd_11andUpFemale_subjData.rds")



############################
### SENSITIVITY ANALYSES ###
############################

#Exclude those on psychiatric psychotropic meds.
#NOTE: the new "psychoactiveMedPsychv2" replaces the old more stringent "psychoactiveMedPsych"

#n1274 voxelwise sample
subjData_sensitivity<-subjData
subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS <- 1
subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS[subjData_sensitivity$psychoactiveMedPsychv2==1] <- 0
subjData_sensitivity <- subjData_sensitivity[which(subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS == 1), ]

#save file (these are the sensitivity analyses for the n1274 sample)
saveRDS(subjData_sensitivity,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1223_JLF_volCtGmd_Sensitivity_subjData.rds")


#n1042 11andUp sample
subjData_sensitivity11<-subjData_subset
subjData_sensitivity11$ACROSS.INCLUDE.PSYCMEDS <- 1
subjData_sensitivity11$ACROSS.INCLUDE.PSYCMEDS[subjData_sensitivity11$psychoactiveMedPsychv2==1] <- 0
subjData_sensitivity11 <- subjData_sensitivity11[which(subjData_sensitivity11$ACROSS.INCLUDE.PSYCMEDS == 1), ]

#save file (these are the sensitivity analyses for the n1042 sample)
saveRDS(subjData_sensitivity11,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n984_JLF_volCtGmd_11andUpSensitivity_subjData.rds")

