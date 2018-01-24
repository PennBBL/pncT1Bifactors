##########################
### LOAD DATA & SUBSET ###
##########################

#subject level data (GO1 data release; n=1601) 
data.go1release <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1601_go1_datarel_020716.csv", header=TRUE, na.strings=".")

#Correlated Traits(n=9361, NOTE: there is one person from the 1601 sample who is missing correlated traits data)
data.CorrTraits <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/Itemwise_Clinical_Corr-Traits_Factor_Scores.csv", header=TRUE, na.strings = ".")

#Age regressed bifactors (n=9392)
data.bifactorsAR <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/GOA_Item_Bifactor_Scores_Age-Regressed.csv", header=TRUE, na.strings=".")

#STAI factors (the STAI bifactor variables already included in the data release are missing data; after removing those under 12yrs, n=1134 (State) and n=1124 (Trait))
data.state <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/STAI_State_Pre-Scan_Bifactor_FScores_1601_2.csv", header=TRUE, na.strings=".")
data.trait <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/STAI_Trait_Bifactor_FScores_1601_2.csv", header=TRUE, na.strings=".")

#antsCT global volumes (n=1601; total GM volume, total WM volume, CSF, etc.)
data.globalVol <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/n1601_summaryData/t1/n1601_antsCtVol.csv", header=TRUE, na.strings=".")

#JLF volume, cortical thickness, gmd, and their QA exclusion variables(n=1601)
data.vol <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/n1601_summaryData/t1/n1601_jlfVol.csv", header=TRUE, na.strings=".")
data.ct <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/n1601_summaryData/t1/n1601_jlfCt.csv", header=TRUE, na.strings=".")
data.gmd <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/n1601_summaryData/t1/n1601_jlfGMD.csv", header=TRUE, na.strings=".")
data.t1.QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/n1601_summaryData/t1/n1601_t1QaData.csv", header=TRUE, na.strings=".")

#ASL data and its QA exclusion variables (n=1601)
data.asl <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/n1601_summaryData/pcasl/n1601_jlfPcaslValues.csv", header=TRUE, na.strings="NA")
data.asl.QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/n1601_summaryData/pcasl/n1601_PcaslQaData.csv", header=TRUE, na.strings="NA")

##Merge data using bblid and scanid
#WARNING: Merging files with unequal cases will cause the non-matched bblids to be deleted; to merge all cases and create NAs for missing data, add all = TRUE).
data.merge1 <- merge(data.go1release, data.CorrTraits, by="bblid", all=TRUE)
data.merge2 <- merge(data.merge1, data.bifactorsAR, by="bblid", all=TRUE)
data.merge3 <- merge(data.merge2, data.state, by="bblid", all=TRUE)
data.merge4 <- merge(data.merge3, data.trait, by="bblid", all=TRUE)
data.merge5 <- merge(data.merge4, data.globalVol, by=c("bblid","scanid"), all=TRUE)
data.merge6 <- merge(data.merge5, data.vol, by=c("bblid","scanid"), all=TRUE)
data.merge7 <- merge(data.merge6, data.ct, by=c("bblid","scanid"), all=TRUE)
data.merge8 <- merge(data.merge7, data.gmd, by=c("bblid","scanid"), all=TRUE)
data.merge9 <- merge(data.merge8, data.t1.QA, by=c("bblid","scanid"), all=TRUE)
data.merge10 <- merge(data.merge9, data.asl, by=c("bblid","scanid"), all=TRUE)
data.merge11 <- merge(data.merge10, data.asl.QA, by=c("bblid","scanid"), all=TRUE)

#Retain only the n=1601 bblids
data.n1601 <- data.merge11[match(data.go1release$bblid, data.merge11$bblid, nomatch=0),] 

#Put bblids in ascending order
data.final <- data.n1601[order(data.n1601$bblid),]

##Create an exclusion criteria variable for Volume, CT, GMD analyses.
#The exclusion criteria are: healthExclude=1 (1=health problems); t1Exclude=1 (1=problems with their T1 data, which also takes into account averageManualRating=0 (bad QA)).
data.final$ACROSS.INCLUDE <- 1
data.final$ACROSS.INCLUDE[data.final$healthExclude==1] <- 0
data.final$ACROSS.INCLUDE[data.final$t1Exclude==1] <- 0

##Exclude everyone that has one or more of these exclusion criteria
data.final <- data.final[which(data.final$ACROSS.INCLUDE == 1), ]

##After exclusion criteria, there are 1360 subjects
subjData <- data.final



#################################
### SEPARATE INTO GM, WM, CSF ###
#################################

###VOLUME###
#NOTE: total volumes per tissue class (GM, Wm, and CBF) already exist as variables in the global volume data file and do not need to be recreated.
#But total GM does not exist in the CT or GMD files and needs to be made.

#ALL ROIS
dataVol <- subjData[,grep("mprage_jlf_vol",names(subjData))]

#GM-- these are the gray matter regions-- would only FDR across these 119 regions
dataVolGm <- dataVol[,-(unique(c( grep("Vent",names(dataVol)), grep("White",names(dataVol)), grep("DC",names(dataVol)), 
	grep("Brain_Stem",names(dataVol)), grep("CSF",names(dataVol)), grep("Vessel",names(dataVol)), grep("OpticChiasm",names(dataVol)) )))] 

#WM-- these are the white matter regions: 4 regions
dataVolWm <- dataVol[,(unique(c( grep("White_Matter",names(dataVol)) )))]

#CSF-- 7 regions
dataVolCsf <- dataVol[,c(grep("CSF",names(dataVol)),grep("Vent",names(dataVol)))]
dataVolCsf <- dataVolCsf[,-c(grep("DC",names(dataVolCsf)))]


###CORTICAL THICKNESS###

#GM-- these are the gray matter regions-- would only FDR across these 98 regions
#NOTE: all of the JLF CT variables are GM regions; WM and CSF don't make sense for CT
dataCT <- subjData[,grep("mprage_jlf_ct",names(subjData))]
dataCTGm <- dataCT

#Get total GM volume
dataCTGm <- data.matrix(dataCTGm)
subjData$CT_gmTotal <- rowSums(dataCTGm)


###GMD###

#ALL ROIS
dataGMD <- subjData[,grep("mprage_jlf_gmd",names(subjData))]

#GM-- these are the gray matter regions-- would only FDR across these 119 regions
dataGMDGm <- dataGMD[,-(unique(c( grep("Vent",names(dataGMD)), grep("White",names(dataGMD)), grep("DC",names(dataGMD)),
        grep("Brain_Stem",names(dataGMD)), grep("CSF",names(dataGMD)), grep("Vessel",names(dataGMD)), grep("OpticChiasm",names(dataGMD)) )))]
 
#Get total GM volume
dataGMDGm <- data.matrix(dataGMDGm)
subjData$GMD_gmTotal <- rowSums(dataGMDGm)


##############################
### SEPARATE GM INTO LOBES ###
##############################

###VOLUME###

#cerebellum (5 regions)
dataVolCereb <- dataVol[,c(grep("Cerebellum_Exterior",names(dataVol)),grep("CerVerLob",names(dataVol)))]

#subcort (16 regions)
dataVolSubcort <- dataVol[,c(grep("Caudate",names(dataVol)), grep("Putamen",names(dataVol)), grep("Pallidum",names(dataVol)), grep("Accumbens",names(dataVol)),
        grep("Thal",names(dataVol)), grep("BasForebr",names(dataVol)), grep("Hippo",names(dataVol)), grep("Amyg",names(dataVol)))]

#frontal (50 regions)
dataVolFrontal <- dataVol[,unique(c(grep("OrG",names(dataVol)), grep("ins",names(dataVol)), grep("FRP",names(dataVol)), grep("FG",names(dataVol)), 
        grep("PrG",names(dataVol)), grep("Gre",names(dataVol)), grep("MFC",names(dataVol)), grep("SCA",names(dataVol)), grep("SMC",names(dataVol)),
        grep("FRP",names(dataVol)),grep("CO",names(dataVol)), grep("FO",names(dataVol)), grep("PO",names(dataVol)), grep("CgG",names(dataVol))))]

#occipital (16 regions)
dataVolOccipital <- dataVol[,unique(c(grep("OG",names(dataVol)), grep("OFuG",names(dataVol)), grep("OCP",names(dataVol)), grep("SOG",names(dataVol)),
        grep("Calc",names(dataVol)), grep("Cun",names(dataVol)), grep("LiG",names(dataVol))))]

#parietal (12 regions)
dataVolParietal <- dataVol[,unique(c(grep("AnG",names(dataVol)), grep("PoG",names(dataVol)), grep("SMG",names(dataVol)), grep("SPL",names(dataVol)),
        grep("Pcu",names(dataVol))))]

#temporal (20 regions)
dataVolTemporal <- dataVol[,unique(c(grep("_FuG",names(dataVol)), grep("TG",names(dataVol)), grep("TMP",names(dataVol)), grep("PP",names(dataVol)),
        grep("PT",names(dataVol)), grep("PHG",names(dataVol)),grep("Ent",names(dataVol))))]

#get total volume for GM lobes (Divide by 1000 to change the units from cubic millimeters to cubic centimeters (cc); 1 cc = 1,000 mm3)
dataVolCereb <- data.matrix(dataVolCereb)
subjData$Vol_gmCerebTotal <- rowSums(dataVolCereb)/1000
dataVolSubcort <- data.matrix(dataVolSubcort)
subjData$Vol_gmSubcortTotal <- rowSums(dataVolSubcort)/1000
dataVolFrontal <- data.matrix(dataVolFrontal)
subjData$Vol_gmFrontalTotal <- rowSums(dataVolFrontal)/1000
dataVolOccipital <- data.matrix(dataVolOccipital)
subjData$Vol_gmOccipitalTotal <- rowSums(dataVolOccipital)/1000
dataVolParietal <- data.matrix(dataVolParietal)
subjData$Vol_gmParietalTotal <- rowSums(dataVolParietal)/1000
dataVolTemporal <- data.matrix(dataVolTemporal)
subjData$Vol_gmTemporalTotal <- rowSums(dataVolTemporal)/1000

#Change units for total GM/WM/CSF volumes to cc's
subjData$mprage_antsCT_vol_GrayMatter <- subjData$mprage_antsCT_vol_GrayMatter/1000
subjData$mprage_antsCT_vol_WhiteMatter <- subjData$mprage_antsCT_vol_WhiteMatter/1000
subjData$mprage_antsCT_vol_CSF <- subjData$mprage_antsCT_vol_CSF/1000


###CORTICAL THICKNESS###

#cerebellum and subcortical don't apply to JLF CT data.

#frontal (50 regions)
dataCTFrontal <- dataCT[,unique(c(grep("OrG",names(dataCT)), grep("ins",names(dataCT)), grep("FRP",names(dataCT)), grep("FG",names(dataCT)),
        grep("PrG",names(dataCT)), grep("Gre",names(dataCT)), grep("MFC",names(dataCT)), grep("SCA",names(dataCT)), grep("SMC",names(dataCT)),
        grep("FRP",names(dataCT)),grep("CO",names(dataCT)), grep("FO",names(dataCT)), grep("PO",names(dataCT)), grep("CgG",names(dataCT))))]

#occipital (16 regions)
dataCTOccipital <- dataCT[,unique(c(grep("OG",names(dataCT)), grep("OFuG",names(dataCT)), grep("OCP",names(dataCT)), grep("SOG",names(dataCT)),
        grep("Calc",names(dataCT)), grep("Cun",names(dataCT)), grep("LiG",names(dataCT))))]

#parietal (12 regions)
dataCTParietal <- dataCT[,unique(c(grep("AnG",names(dataCT)), grep("PoG",names(dataCT)), grep("SMG",names(dataCT)), grep("SPL",names(dataCT)),
        grep("Pcu",names(dataCT))))]

#temporal (20 regions)
dataCTTemporal <- dataCT[,unique(c(grep("_FuG",names(dataCT)), grep("TG",names(dataCT)), grep("TMP",names(dataCT)), grep("PP",names(dataCT)),
        grep("PT",names(dataCT)), grep("PHG",names(dataCT)),grep("Ent",names(dataCT))))]

#get total volume for GM subdivisions
dataCTFrontal <- data.matrix(dataCTFrontal)
subjData$CT_gmFrontalTotal <- rowSums(dataCTFrontal)
dataCTOccipital <- data.matrix(dataCTOccipital)
subjData$CT_gmOccipitalTotal <- rowSums(dataCTOccipital)
dataCTParietal <- data.matrix(dataCTParietal)
subjData$CT_gmParietalTotal <- rowSums(dataCTParietal)
dataCTTemporal <- data.matrix(dataCTTemporal)
subjData$CT_gmTemporalTotal <- rowSums(dataCTTemporal)



###GMD###

#cerebellum (5 regions)
dataGMDCereb <- dataGMD[,c(grep("Cerebellum_Exterior",names(dataGMD)),grep("CerVerLob",names(dataGMD)))]

#subcort (16 regions)
dataGMDSubcort <- dataGMD[,c(grep("Caudate",names(dataGMD)), grep("Putamen",names(dataGMD)), grep("Pallidum",names(dataGMD)), grep("Accumbens",names(dataGMD)),
        grep("Thal",names(dataGMD)), grep("BasForebr",names(dataGMD)), grep("Hippo",names(dataGMD)), grep("Amyg",names(dataGMD)))]

#frontal (50 regions)
dataGMDFrontal <- dataGMD[,unique(c(grep("OrG",names(dataGMD)), grep("ins",names(dataGMD)), grep("FRP",names(dataGMD)), grep("FG",names(dataGMD)),
        grep("PrG",names(dataGMD)), grep("Gre",names(dataGMD)), grep("MFC",names(dataGMD)), grep("SCA",names(dataGMD)), grep("SMC",names(dataGMD)),
        grep("FRP",names(dataGMD)),grep("CO",names(dataGMD)), grep("FO",names(dataGMD)), grep("PO",names(dataGMD)), grep("CgG",names(dataGMD))))]

#occipital (16 regions)
dataGMDOccipital <- dataGMD[,unique(c(grep("OG",names(dataGMD)), grep("OFuG",names(dataGMD)), grep("OCP",names(dataGMD)), grep("SOG",names(dataGMD)),
        grep("Calc",names(dataGMD)), grep("Cun",names(dataGMD)), grep("LiG",names(dataGMD))))]

#parietal (12 regions)
dataGMDParietal <- dataGMD[,unique(c(grep("AnG",names(dataGMD)), grep("PoG",names(dataGMD)), grep("SMG",names(dataGMD)), grep("SPL",names(dataGMD)),
        grep("Pcu",names(dataGMD))))]

#temporal (20 regions)
dataGMDTemporal <- dataGMD[,unique(c(grep("_FuG",names(dataGMD)), grep("TG",names(dataGMD)), grep("TMP",names(dataGMD)), grep("PP",names(dataGMD)),
        grep("PT",names(dataGMD)), grep("PHG",names(dataGMD)),grep("Ent",names(dataGMD))))]

#get total volume for GM subdivisions
dataGMDCereb <- data.matrix(dataGMDCereb)
subjData$GMD_gmCerebTotal <- rowSums(dataGMDCereb)
dataGMDSubcort <- data.matrix(dataGMDSubcort)
subjData$GMD_gmSubcortTotal <- rowSums(dataGMDSubcort)
dataGMDFrontal <- data.matrix(dataGMDFrontal)
subjData$GMD_gmFrontalTotal <- rowSums(dataGMDFrontal)
dataGMDOccipital <- data.matrix(dataGMDOccipital)
subjData$GMD_gmOccipitalTotal <- rowSums(dataGMDOccipital)
dataGMDParietal <- data.matrix(dataGMDParietal)
subjData$GMD_gmParietalTotal <- rowSums(dataGMDParietal)
dataGMDTemporal <- data.matrix(dataGMDTemporal)
subjData$GMD_gmTemporalTotal <- rowSums(dataGMDTemporal)



###########################
### TRANSFORM VARIABLES ###
###########################

#Transform the age variable from months to years
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

saveRDS(subjData,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_JLF_volCtGmd_subjData.rds")



###################################################
### SUBSET DATA BY ANXIETY DISORDERS CATEGORIES ###
###################################################

#All Anxiety Disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) and TD
AllAnxTdSubjData <- subjData
AllAnxTdSubjData <- AllAnxTdSubjData[which(AllAnxTdSubjData$AllAnxTd != "NA"), ]

#Save file
saveRDS(AllAnxTdSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1094_JLF_volCtGmd_AllAnxTdSubjData.rds")


#All Anxiety Disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) (No TD)
AllAnxSubjData <- subjData
AllAnxSubjData <- AllAnxSubjData[which(AllAnxSubjData$AllAnxTd == "AllAnx"), ]

#Save file
saveRDS(AllAnxSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n680_JLF_volCtGmd_AllAnxSubjData.rds")


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
saveRDS(staiSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1019_JLF_stai_subjData.rds")


###########################
### SUBSET BY 11 and up ###
###########################

#Exclude those younger than 11 years because only collateral reports of psychiatric symptoms were available (Wolf et al., 2015 JAMA).
subjData_subset <- subjData
subjData_subset$ACROSS.INCLUDE.11 <- 0
subjData_subset$ACROSS.INCLUDE.11[subjData_subset$age >= 11] <- 1
subjData_subset <- subjData_subset[which(subjData_subset$ACROSS.INCLUDE.11 == 1), ]

#save file
saveRDS(subjData_subset,"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1103_JLF_11andUp_subjData.rds")
