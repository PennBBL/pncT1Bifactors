##########################
### LOAD DATA & SUBSET ###
##########################

#Load subject GO1 data release (n=1601), scan dates (for finding missing ravens data later; n=1601), correlated traits data (n=9361, NOTE: there is one person from the 1601 sample who is missing correlated traits data), JLF volume data (n=1601), JLF cortical thickness data (n=1601), JLF perfusion data (n=1601), and JLF gray matter density data (n=1601). 
data.go1release <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_go1_datarel_020716.csv", header=TRUE, na.strings=".")
data.dates <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_bblids_scanids_dates.csv", header=TRUE, na.strings=".")
data.CorrTraits <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/Itemwise_Clinical_Corr-Traits_Factor_Scores.csv", header=TRUE, na.strings = ".")
data.vol <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_antsCtVol_jlfVol.csv", header=TRUE, na.strings=".")
data.ct <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_jlfCt.csv", header=TRUE, na.strings=".")
data.asl <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_jlfPcasl.csv", header=TRUE, na.strings=".")
data.gmd <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_jlfGMD.csv", header=TRUE, na.strings=".")

#Remove duplicate variables from ct, asl, and gmd data files (averageManualRating, t1Exclude, and mprage_antsCT_vol_TBV) since they already exist in the vol data file.
data.ct$averageManualRating<-NULL
data.ct$t1Exclude<-NULL
data.ct$mprage_antsCT_vol_TBV<-NULL

data.asl$t1Exclude<-NULL

data.gmd$averageManualRating<-NULL
data.gmd$t1Exclude<-NULL
data.gmd$mprage_antsCT_vol_TBV<-NULL

##Merge data using bblid and scanid
#WARNING: Merging files with unequal cases will cause the non-matched bblids to be deleted (which is what we want in the case of correlated traits).
data.merge <- merge(data.go1release, data.dates, by=c("bblid","scanid"))
data.merge2 <- merge(data.merge, data.CorrTraits, by="bblid")
data.merge3 <- merge(data.merge2, data.vol, by=c("bblid","scanid"))
data.merge4 <- merge(data.merge3, data.ct, by=c("bblid","scanid"))
data.merge5 <- merge(data.merge4, data.asl, by=c("bblid","scanid"))
data.merge6 <- merge(data.merge5, data.gmd, by=c("bblid","scanid"))

#Put bblids in ascending order
data.final <- data.merge6[order(data.merge6$bblid),]

##Create an exclusion criteria variable
#The three exclusion criteria are: healthExclude=1 (1=health problems); t1Exclude=1 (1=problems with their T1 data); and averageManualRating=0 (0=bad QA).
#NOTE: asl data has its own exclusion criteria (pcaslExclude=1 where 1=problems with asl data).
data.final$ACROSS.INCLUDE<-1
data.final$ACROSS.INCLUDE[data.final$healthExclude==1]<-0
data.final$ACROSS.INCLUDE[data.final$t1Exclude==1]<-0
data.final$ACROSS.INCLUDE[data.final$averageManualRating==0]<-0

##Exclude everyone that has one or more of these exclusion criteria
data.final <- data.final[which(data.final$ACROSS.INCLUDE == 1), ]

##After exclusion criteria, there are 1359 subjects
subjData<-data.final



#################################
### SEPARATE INTO GM, WM, CSF ###
#################################

###VOLUME###
#NOTE: total volumes per tissue class (GM, Wm, and CBF) already exist as variables in the vol data file and do not need to be recreated.
#But total GM does not exist in the CT or GMD files and needs to be made.

#ALL ROIS
dataVol<-subjData[,grep("mprage_jlf_vol",names(subjData))]

#GM-- these are the gray matter regions-- would only FDR across these 119 regions
dataVolGm<-dataVol[,-(unique(c( grep("Vent",names(dataVol)), grep("White",names(dataVol)), grep("DC",names(dataVol)), 
	grep("Brain_Stem",names(dataVol)), grep("CSF",names(dataVol)), grep("Vessel",names(dataVol)), grep("OpticChiasm",names(dataVol)) )))] 

#WM-- these are the white matter regions: 4 regions
dataVolWm<-dataVol[,(unique(c( grep("White_Matter",names(dataVol)) )))]

#CSF-- 7 regions
dataVolCsf<-dataVol[,c(grep("CSF",names(dataVol)),grep("Vent",names(dataVol)))]
dataVolCsf<-dataVolCsf[,-c(grep("DC",names(dataVolCsf)))]


###CORTICAL THICKNESS###

#GM-- these are the gray matter regions-- would only FDR across these 98 regions
#NOTE: all of the JLF CT variables are GM regions; WM and CSF don't make sense for CT
dataCT<-subjData[,grep("mprage_jlf_ct",names(subjData))]
dataCTGm<-dataCT

#Get total GM volume
dataCTGm<-data.matrix(dataCTGm)
subjData$CT_gmTotal<-rowSums(dataCTGm)


###GMD###

#ALL ROIS
dataGMD<-subjData[,grep("mprage_jlf_gmd",names(subjData))]

#GM-- these are the gray matter regions-- would only FDR across these 119 regions
dataGMDGm<-dataGMD[,-(unique(c( grep("Vent",names(dataGMD)), grep("White",names(dataGMD)), grep("DC",names(dataGMD)),
        grep("Brain_Stem",names(dataGMD)), grep("CSF",names(dataGMD)), grep("Vessel",names(dataGMD)), grep("OpticChiasm",names(dataGMD)) )))]
 
#Get total GM volume
dataGMDGm<-data.matrix(dataGMDGm)
subjData$GMD_gmTotal<-rowSums(dataGMDGm)


##############################
### SEPARATE GM INTO LOBES ###
##############################

###VOLUME###

#cerebellum (5 regions)
dataVolCereb<-dataVol[,c(grep("Cerebellum_Exterior",names(dataVol)),grep("CerVerLob",names(dataVol)))]

#subcort (16 regions)
dataVolSubcort<-dataVol[,c(grep("Caudate",names(dataVol)), grep("Putamen",names(dataVol)), grep("Pallidum",names(dataVol)), grep("Accumbens",names(dataVol)),
        grep("Thal",names(dataVol)), grep("BasForebr",names(dataVol)), grep("Hippo",names(dataVol)), grep("Amyg",names(dataVol)))]

#frontal (50 regions)
dataVolFrontal<-dataVol[,unique(c(grep("OrG",names(dataVol)), grep("ins",names(dataVol)), grep("FRP",names(dataVol)), grep("FG",names(dataVol)), 
        grep("PrG",names(dataVol)), grep("Gre",names(dataVol)), grep("MFC",names(dataVol)), grep("SCA",names(dataVol)), grep("SMC",names(dataVol)),
        grep("FRP",names(dataVol)),grep("CO",names(dataVol)), grep("FO",names(dataVol)), grep("PO",names(dataVol)), grep("CgG",names(dataVol))))]

#occipital (16 regions)
dataVolOccipital<-dataVol[,unique(c(grep("OG",names(dataVol)), grep("OFuG",names(dataVol)), grep("OCP",names(dataVol)), grep("SOG",names(dataVol)),
        grep("Calc",names(dataVol)), grep("Cun",names(dataVol)), grep("LiG",names(dataVol))))]

#parietal (12 regions)
dataVolParietal<-dataVol[,unique(c(grep("AnG",names(dataVol)), grep("PoG",names(dataVol)), grep("SMG",names(dataVol)), grep("SPL",names(dataVol)),
        grep("Pcu",names(dataVol))))]

#temporal (20 regions)
dataVolTemporal<-dataVol[,unique(c(grep("_FuG",names(dataVol)), grep("TG",names(dataVol)), grep("TMP",names(dataVol)), grep("PP",names(dataVol)),
        grep("PT",names(dataVol)), grep("PHG",names(dataVol)),grep("Ent",names(dataVol))))]

#get total volume for GM lobes (Divide by 1000 to change the units from cubic millimeters to cubic centimeters (cc); 1 cc = 1,000 mm3)
dataVolCereb<-data.matrix(dataVolCereb)
subjData$Vol_gmCerebTotal<-rowSums(dataVolCereb)/1000
dataVolSubcort<-data.matrix(dataVolSubcort)
subjData$Vol_gmSubcortTotal<-rowSums(dataVolSubcort)/1000
dataVolFrontal<-data.matrix(dataVolFrontal)
subjData$Vol_gmFrontalTotal<-rowSums(dataVolFrontal)/1000
dataVolOccipital<-data.matrix(dataVolOccipital)
subjData$Vol_gmOccipitalTotal<-rowSums(dataVolOccipital)/1000
dataVolParietal<-data.matrix(dataVolParietal)
subjData$Vol_gmParietalTotal<-rowSums(dataVolParietal)/1000
dataVolTemporal<-data.matrix(dataVolTemporal)
subjData$Vol_gmTemporalTotal<-rowSums(dataVolTemporal)/1000

#Change units for total GM/WM/CSF volumes to cc's
subjData$mprage_antsCT_vol_GrayMatter<-subjData$mprage_antsCT_vol_GrayMatter/1000
subjData$mprage_antsCT_vol_WhiteMatter<-subjData$mprage_antsCT_vol_WhiteMatter/1000
subjData$mprage_antsCT_vol_CSF<-subjData$mprage_antsCT_vol_CSF/1000


###CORTICAL THICKNESS###

#cerebellum and subcortical don't apply to JLF CT data.

#frontal (50 regions)
dataCTFrontal<-dataCT[,unique(c(grep("OrG",names(dataCT)), grep("ins",names(dataCT)), grep("FRP",names(dataCT)), grep("FG",names(dataCT)),
        grep("PrG",names(dataCT)), grep("Gre",names(dataCT)), grep("MFC",names(dataCT)), grep("SCA",names(dataCT)), grep("SMC",names(dataCT)),
        grep("FRP",names(dataCT)),grep("CO",names(dataCT)), grep("FO",names(dataCT)), grep("PO",names(dataCT)), grep("CgG",names(dataCT))))]

#occipital (16 regions)
dataCTOccipital<-dataCT[,unique(c(grep("OG",names(dataCT)), grep("OFuG",names(dataCT)), grep("OCP",names(dataCT)), grep("SOG",names(dataCT)),
        grep("Calc",names(dataCT)), grep("Cun",names(dataCT)), grep("LiG",names(dataCT))))]

#parietal (12 regions)
dataCTParietal<-dataCT[,unique(c(grep("AnG",names(dataCT)), grep("PoG",names(dataCT)), grep("SMG",names(dataCT)), grep("SPL",names(dataCT)),
        grep("Pcu",names(dataCT))))]

#temporal (20 regions)
dataCTTemporal<-dataCT[,unique(c(grep("_FuG",names(dataCT)), grep("TG",names(dataCT)), grep("TMP",names(dataCT)), grep("PP",names(dataCT)),
        grep("PT",names(dataCT)), grep("PHG",names(dataCT)),grep("Ent",names(dataCT))))]

#get total volume for GM subdivisions
dataCTFrontal<-data.matrix(dataCTFrontal)
subjData$CT_gmFrontalTotal<-rowSums(dataCTFrontal)
dataCTOccipital<-data.matrix(dataCTOccipital)
subjData$CT_gmOccipitalTotal<-rowSums(dataCTOccipital)
dataCTParietal<-data.matrix(dataCTParietal)
subjData$CT_gmParietalTotal<-rowSums(dataCTParietal)
dataCTTemporal<-data.matrix(dataCTTemporal)
subjData$CT_gmTemporalTotal<-rowSums(dataCTTemporal)



###GMD###

#cerebellum (5 regions)
dataGMDCereb<-dataGMD[,c(grep("Cerebellum_Exterior",names(dataGMD)),grep("CerVerLob",names(dataGMD)))]

#subcort (16 regions)
dataGMDSubcort<-dataGMD[,c(grep("Caudate",names(dataGMD)), grep("Putamen",names(dataGMD)), grep("Pallidum",names(dataGMD)), grep("Accumbens",names(dataGMD)),
        grep("Thal",names(dataGMD)), grep("BasForebr",names(dataGMD)), grep("Hippo",names(dataGMD)), grep("Amyg",names(dataGMD)))]

#frontal (50 regions)
dataGMDFrontal<-dataGMD[,unique(c(grep("OrG",names(dataGMD)), grep("ins",names(dataGMD)), grep("FRP",names(dataGMD)), grep("FG",names(dataGMD)),
        grep("PrG",names(dataGMD)), grep("Gre",names(dataGMD)), grep("MFC",names(dataGMD)), grep("SCA",names(dataGMD)), grep("SMC",names(dataGMD)),
        grep("FRP",names(dataGMD)),grep("CO",names(dataGMD)), grep("FO",names(dataGMD)), grep("PO",names(dataGMD)), grep("CgG",names(dataGMD))))]

#occipital (16 regions)
dataGMDOccipital<-dataGMD[,unique(c(grep("OG",names(dataGMD)), grep("OFuG",names(dataGMD)), grep("OCP",names(dataGMD)), grep("SOG",names(dataGMD)),
        grep("Calc",names(dataGMD)), grep("Cun",names(dataGMD)), grep("LiG",names(dataGMD))))]

#parietal (12 regions)
dataGMDParietal<-dataGMD[,unique(c(grep("AnG",names(dataGMD)), grep("PoG",names(dataGMD)), grep("SMG",names(dataGMD)), grep("SPL",names(dataGMD)),
        grep("Pcu",names(dataGMD))))]

#temporal (20 regions)
dataGMDTemporal<-dataGMD[,unique(c(grep("_FuG",names(dataGMD)), grep("TG",names(dataGMD)), grep("TMP",names(dataGMD)), grep("PP",names(dataGMD)),
        grep("PT",names(dataGMD)), grep("PHG",names(dataGMD)),grep("Ent",names(dataGMD))))]

#get total volume for GM subdivisions
dataGMDCereb<-data.matrix(dataGMDCereb)
subjData$GMD_gmCerebTotal<-rowSums(dataGMDCereb)
dataGMDSubcort<-data.matrix(dataGMDSubcort)
subjData$GMD_gmSubcortTotal<-rowSums(dataGMDSubcort)
dataGMDFrontal<-data.matrix(dataGMDFrontal)
subjData$GMD_gmFrontalTotal<-rowSums(dataGMDFrontal)
dataGMDOccipital<-data.matrix(dataGMDOccipital)
subjData$GMD_gmOccipitalTotal<-rowSums(dataGMDOccipital)
dataGMDParietal<-data.matrix(dataGMDParietal)
subjData$GMD_gmParietalTotal<-rowSums(dataGMDParietal)
dataGMDTemporal<-data.matrix(dataGMDTemporal)
subjData$GMD_gmTemporalTotal<-rowSums(dataGMDTemporal)



###########################
### TRANSFORM VARIABLES ###
###########################

#Transform the age variable from months to years
subjData$age<-(subjData$ageAtGo1Scan)/12

#Define age squared
subjData$ageSq<-I(scale(subjData$age, scale=FALSE, center=TRUE)^2)

#sex (needs to be an ordered variable when using spline interactions)
subjData$sex[which(subjData$sex==1)]<-"male"
subjData$sex[which(subjData$sex==2)]<-"female"
subjData$sex<-as.ordered(as.factor(subjData$sex))

#race (make white vs non-white)
subjData$white<-NA
subjData$white[which(subjData$race==1)]<-"Caucasian"
subjData$white[which(subjData$race!=1)]<-"notCaucasian"
subjData$white<-as.ordered(as.factor(subjData$white))



#########################################
#### MAKE DIAGNOSIS FACTOR VARIABLES ####
#########################################

##Make variables where 1 = diagnosis.

#ADHD
subjData$Add<-NA
subjData$Add[which(subjData$goassessSmryAdd==4)]<-1

#Agoraphobia
subjData$Agr<-NA
subjData$Agr[which(subjData$goassessSmryAgr==4)]<-1

#Anorexia
subjData$Ano<-NA
subjData$Ano[which(subjData$goassessSmryAno==4)]<-1

#Bulimia
subjData$Bul<-NA
subjData$Bul[which(subjData$goassessSmryBul==4)]<-1

#Conduct Disorder
subjData$Con<-NA
subjData$Con[which(subjData$goassessSmryCon==4)]<-1

#Generalized Anxiety Disorder
subjData$Gad<-NA
subjData$Gad[which(subjData$goassessSmryGad==4)]<-1

#Mania
subjData$Man<-NA
subjData$Man[which(subjData$goassessSmryMan==4)]<-1

#Major Depressive Disorder
subjData$Mdd<-NA
subjData$Mdd[which(subjData$goassessSmryDep==4)]<-1

#OCD
subjData$Ocd<-NA
subjData$Ocd[which(subjData$goassessSmryOcd==4)]<-1

#Oppositional Defiant Disorder
subjData$Odd<-NA
subjData$Odd[which(subjData$goassessSmryOdd==4)]<-1

#Panic Disorder
subjData$Pan<-NA
subjData$Pan[which(subjData$goassessSmryPan==4)]<-1

#Psychosis
subjData$Ps<-NA
subjData$Ps[which(subjData$goassessDxpmr4=="4PS")]<-1

#Posttraumatic Stress Disorder
subjData$Ptd<-NA
subjData$Ptd[which(subjData$goassessSmryPtd==4)]<-1

#Separation Anxiety Disorder
subjData$Sep<-NA
subjData$Sep[which(subjData$goassessSmrySep==4)]<-1

#Social Anxiety Disorder
subjData$Soc<-NA
subjData$Soc[which(subjData$goassessSmrySoc==4)]<-1

#Specific Phobia
subjData$Sph<-NA
subjData$Sph[which(subjData$goassessSmryPhb==4)]<-1

#Typically Developing
dxNames<-c("bblid","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf<-data.matrix(subjData[,dxNames])
subjData$totDx<-rowSums(dxDf[,2:17], na.rm=TRUE) #This is how many people have how many diagnoses: sum(totDx==0):414, sum(totDx==1):307, sum(totDx>=2):638
subjData$Td<-0
subjData$Td[which(subjData$totDx==0)]<-1



#####################################
#### MAKE TD THE REFERENCE GROUP ####
#####################################

subjData$Add[which(subjData$Td==1)]<-0
subjData$Agr[which(subjData$Td==1)]<-0
subjData$Ano[which(subjData$Td==1)]<-0
subjData$Bul[which(subjData$Td==1)]<-0
subjData$Con[which(subjData$Td==1)]<-0
subjData$Gad[which(subjData$Td==1)]<-0
subjData$Man[which(subjData$Td==1)]<-0
subjData$Mdd[which(subjData$Td==1)]<-0
subjData$Ocd[which(subjData$Td==1)]<-0
subjData$Odd[which(subjData$Td==1)]<-0
subjData$Pan[which(subjData$Td==1)]<-0
subjData$Ps[which(subjData$Td==1)]<-0
subjData$Ptd[which(subjData$Td==1)]<-0
subjData$Sep[which(subjData$Td==1)]<-0
subjData$Soc[which(subjData$Td==1)]<-0
subjData$Sph[which(subjData$Td==1)]<-0



#########################################
### Create Anx vs TD factor variables ###
#########################################

##Create a factor variable that will compare TD to All Anx (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph).

subjData$AllAnxTd<-NA
subjData$AllAnxTd[subjData$Agr==1]<-"AllAnx"
subjData$AllAnxTd[subjData$Gad==1]<-"AllAnx"
subjData$AllAnxTd[subjData$Ocd==1]<-"AllAnx"
subjData$AllAnxTd[subjData$Pan==1]<-"AllAnx"
subjData$AllAnxTd[subjData$Ptd==1]<-"AllAnx"
subjData$AllAnxTd[subjData$Sep==1]<-"AllAnx"
subjData$AllAnxTd[subjData$Soc==1]<-"AllAnx"
subjData$AllAnxTd[subjData$Sph==1]<-"AllAnx"
subjData$AllAnxTd[subjData$Td==1]<-"Td"
subjData$AllAnxTd<-as.factor(subjData$AllAnxTd)

##Create "pure" anxiety groups with no anxiety comorbidities

#PTSD (n=55)
subjData$PurePtd<-0
subjData$PurePtd[subjData$Ptd==1]<-1
subjData$PurePtd[subjData$Agr==1]<-0
subjData$PurePtd[subjData$Gad==1]<-0
subjData$PurePtd[subjData$Pan==1]<-0
subjData$PurePtd[subjData$Sep==1]<-0
subjData$PurePtd[subjData$Soc==1]<-0
subjData$PurePtd[subjData$Sph==1]<-0
subjData$PurePtd[subjData$Ocd==1]<-0

#OCD (n=3)
subjData$PureOcd<-0
subjData$PureOcd[subjData$Ocd==1]<-1
subjData$PureOcd[subjData$Agr==1]<-0
subjData$PureOcd[subjData$Gad==1]<-0
subjData$PureOcd[subjData$Pan==1]<-0
subjData$PureOcd[subjData$Sep==1]<-0
subjData$PureOcd[subjData$Soc==1]<-0
subjData$PureOcd[subjData$Sph==1]<-0
subjData$PureOcd[subjData$Ptd==1]<-0

#Anxiety disorders (Agr, Gad, Pan, Sep, Soc, Sph) (n=488)
subjData$PureAnx<-0
subjData$PureAnx[subjData$Agr==1]<-1
subjData$PureAnx[subjData$Gad==1]<-1
subjData$PureAnx[subjData$Pan==1]<-1
subjData$PureAnx[subjData$Sep==1]<-1
subjData$PureAnx[subjData$Soc==1]<-1
subjData$PureAnx[subjData$Sph==1]<-1
subjData$PureAnx[subjData$Ocd==1]<-0
subjData$PureAnx[subjData$Ptd==1]<-0

##Create a factor variable that will compare TD to coarsely defined "pure" anxiety groups (PTSD and the other anxiety disorders (Agr, Gad, Sep, Soc, Sph)).
#NOTE: There were only 3 subjects with "pure" OCD (no anxiety comorbidity), so OCD is not included (use table(subjData$CoarseAnxTd) to see the numbers per group).
#In order to make Td the comparison group, we need to use numbers instead of labels so that Td=1.

subjData$CoarseAnxTd<-NA
subjData$CoarseAnxTd[subjData$Td==1]<-1
subjData$CoarseAnxTd[subjData$PurePtd==1]<-2
subjData$CoarseAnxTd[subjData$PureAnx==1]<-3
subjData$CoarseAnxTd<-as.factor(subjData$CoarseAnxTd)



#################
### SAVE DATA ###
#################

saveRDS(subjData,"/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1359_volJLF_subjData.rds")



###################################################
### SUBSET DATA BY ANXIETY DISORDERS CATEGORIES ###
###################################################

#All Anxiety Disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) and TD
AllAnxTdSubjData <- subjData
AllAnxTdSubjData <- AllAnxTdSubjData[which(AllAnxTdSubjData$AllAnxTd != "NA"), ]

#Save file
saveRDS(AllAnxTdSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1092_volJLF_AllAnxTdSubjData.rds")


#All Anxiety Disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) (No TD)
AllAnxSubjData <- subjData
AllAnxSubjData <- AllAnxSubjData[which(AllAnxSubjData$AllAnxTd == "AllAnx"), ]

#Save file
saveRDS(AllAnxSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n678_volJLF_AllAnxSubjData.rds")



####################
### ASL subjData ###
####################

#Apply the additional exclusion criteria for ASL analyses and save the file with a different name.
aslSubjData<-subjData
aslSubjData$ACROSS.INCLUDE[aslSubjData$pcaslExclude==1]<-0
aslSubjData <- aslSubjData[which(aslSubjData$ACROSS.INCLUDE == 1), ]

#Save file
saveRDS(aslSubjData,"/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1308_aslJLF_subjData.rds")



#######################################
###LOAD LIBRARY AND CREATE VAR LISTS###
#######################################

##Load library for nonlinear analyses
library(mgcv)

##Create lists of variables names of interest: grouped by 1) white matter, gray matter, csf, 2) lobes (don't include cerebellum), or 3) ROIs
  
Vol_GmWmCsf_List <- names(subjData)[2342:2344]
Vol_Lobe_List <- names(subjData)[2836:2840]
Vol_ROI_List <- colnames(dataVolGm)

CT_Gm_List <- names(subjData)[2833]
CT_Lobe_List <- names(subjData)[2841:2844]
CT_ROI_List <- colnames(dataCTGm)

GMD_Gm_List <- names(subjData)[2834]
GMD_Lobe_List <- names(subjData)[2846:2850]
GMD_ROI_List <- colnames(dataGMDGm)



#####################
###VOLUME ANALYSES###
#####################
#Structural data: volumetric data from JLF: i.e., mprage_jlf_vol_*
#MODEL: gam(vol~s(age) + sex + mprage_antsCT_vol_TBV + Mood + Psychosis + Ext + Phobias + OverallPsy)


####PSYCHOPATHOLOGY BIFACTORS####

###GM, WM, CSF WITH TBV###
#GAM model
GmWmCsfModels_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GmWmCsfModels_vol, summary)

#Create a vector p-values
GmWmCsfMood_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$p.table[4,4])
GmWmCsfPsych_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$p.table[5,4])
GmWmCsfExt_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$p.table[6,4])
GmWmCsfPhb_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$p.table[7,4])
GmWmCsfOverallPsy_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
GmWmCsfMood_vol_fdr<-p.adjust(GmWmCsfMood_vol,method="fdr")
GmWmCsfPsych_vol_fdr<-p.adjust(GmWmCsfPsych_vol,method="fdr")
GmWmCsfExt_vol_fdr<-p.adjust(GmWmCsfExt_vol,method="fdr")
GmWmCsfPhb_vol_fdr<-p.adjust(GmWmCsfPhb_vol,method="fdr")
GmWmCsfOverallPsy_vol_fdr<-p.adjust(GmWmCsfOverallPsy_vol,method="fdr")


###LOBES WITH TBV###
#GAM model
LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(LobeModels_vol, summary)

#Create a vector p-values
LobeMood_vol<-sapply(LobeModels_vol, function(v) summary(v)$p.table[4,4])
LobePsych_vol<-sapply(LobeModels_vol, function(v) summary(v)$p.table[5,4])
LobeExt_vol<-sapply(LobeModels_vol, function(v) summary(v)$p.table[6,4])
LobePhb_vol<-sapply(LobeModels_vol, function(v) summary(v)$p.table[7,4])
LobeOverallPsy_vol<-sapply(LobeModels_vol, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
LobeMood_vol_fdr<-p.adjust(LobeMood_vol,method="fdr")
LobePsych_vol_fdr<-p.adjust(LobePsych_vol,method="fdr")
LobeExt_vol_fdr<-p.adjust(LobeExt_vol,method="fdr")
LobePhb_vol_fdr<-p.adjust(LobePhb_vol,method="fdr")
LobeOverallPsy_vol_fdr<-p.adjust(LobeOverallPsy_vol,method="fdr")


###ROIs###
#GAM model
ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(ROIModels_vol, summary)

#Create a vector p-values
ROIMood_vol<-sapply(ROIModels_vol, function(v) summary(v)$p.table[4,4])
ROIPsych_vol<-sapply(ROIModels_vol, function(v) summary(v)$p.table[5,4])
ROIExt_vol<-sapply(ROIModels_vol, function(v) summary(v)$p.table[6,4])
ROIPhb_vol<-sapply(ROIModels_vol, function(v) summary(v)$p.table[7,4])
ROIOverallPsy_vol<-sapply(ROIModels_vol, function(v) summary(v)$p.table[8,4])

#FDR correct p-values 
ROIMood_vol_fdr<-p.adjust(ROIMood_vol,method="fdr")
ROIPsych_vol_fdr<-p.adjust(ROIPsych_vol,method="fdr")
ROIExt_vol_fdr<-p.adjust(ROIExt_vol,method="fdr")
ROIPhb_vol_fdr<-p.adjust(ROIPhb_vol,method="fdr")
ROIOverallPsy_vol_fdr<-p.adjust(ROIOverallPsy_vol,method="fdr")




####CORRELATED TRAITS- NOT AGE REGRESSED####

###GM, WM, CSF###
#GAM model
GmWmCsfModelsMood_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GmWmCsfModelsPsych_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GmWmCsfModelsExt_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GmWmCsfModelsFear_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(GmWmCsfModelsMood_vol, summary)
lapply(GmWmCsfModelsPsych_vol, summary)
lapply(GmWmCsfModelsExt_vol, summary)
lapply(GmWmCsfModelsFear_vol, summary)

#Create a vector p-values
GmWmCsfCMood_vol<-sapply(GmWmCsfModelsMood_vol, function(v) summary(v)$p.table[4,4])
GmWmCsfCPsych_vol<-sapply(GmWmCsfModelsPsych_vol, function(v) summary(v)$p.table[4,4])
GmWmCsfCExt_vol<-sapply(GmWmCsfModelsExt_vol, function(v) summary(v)$p.table[4,4])
GmWmCsfCFear_vol<-sapply(GmWmCsfModelsFear_vol, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GmWmCsfCMood_vol_fdr<-p.adjust(GmWmCsfCMood_vol,method="fdr")
GmWmCsfCPsych_vol_fdr<-p.adjust(GmWmCsfCPsych_vol,method="fdr")
GmWmCsfCExt_vol_fdr<-p.adjust(GmWmCsfCExt_vol,method="fdr")
GmWmCsfCFear_vol_fdr<-p.adjust(GmWmCsfCFear_vol,method="fdr")


###LOBES###
#GAM model
LobeModelsMood_vol <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

LobeModelsPsych_vol <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

LobeModelsExt_vol <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

LobeModelsFear_vol <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(LobeModelsMood_vol, summary)
lapply(LobeModelsPsych_vol, summary)
lapply(LobeModelsExt_vol, summary)
lapply(LobeModelsFear_vol, summary)

#Create a vector p-values
LobeCMood_vol<-sapply(LobeModelsMood_vol, function(v) summary(v)$p.table[4,4])
LobeCPsych_vol<-sapply(LobeModelsPsych_vol, function(v) summary(v)$p.table[4,4])
LobeCExt_vol<-sapply(LobeModelsExt_vol, function(v) summary(v)$p.table[4,4])
LobeCFear_vol<-sapply(LobeModelsFear_vol, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
LobeCMood_vol_fdr<-p.adjust(LobeCMood_vol,method="fdr")
LobeCPsych_vol_fdr<-p.adjust(LobeCPsych_vol,method="fdr")
LobeCExt_vol_fdr<-p.adjust(LobeCExt_vol,method="fdr")
LobeCFear_vol_fdr<-p.adjust(LobeCFear_vol,method="fdr")


###ROIs###
#GAM model
ROIModelsMood_vol <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

ROIModelsPsych_vol <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

ROIModelsExt_vol <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

ROIModelsFear_vol <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(ROIModelsMood_vol, summary)
lapply(ROIModelsPsych_vol, summary)
lapply(ROIModelsExt_vol, summary)
lapply(ROIModelsFear_vol, summary)

#Create a vector p-values
ROICMood_vol<-sapply(ROIModelsMood_vol, function(v) summary(v)$p.table[4,4])
ROICPsych_vol<-sapply(ROIModelsPsych_vol, function(v) summary(v)$p.table[4,4])
ROICExt_vol<-sapply(ROIModelsExt_vol, function(v) summary(v)$p.table[4,4])
ROICFear_vol<-sapply(ROIModelsFear_vol, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
ROICMood_vol_fdr<-p.adjust(ROICMood_vol,method="fdr")
ROICPsych_vol_fdr<-p.adjust(ROICPsych_vol,method="fdr")
ROICExt_vol_fdr<-p.adjust(ROICExt_vol,method="fdr")
ROICFear_vol_fdr<-p.adjust(ROICFear_vol,method="fdr")



#################################
###CORTICAL THICKNESS ANALYSES###
#################################
#Cortical thickness data from JLF: i.e., mprage_jlf_ct_*
#MODEL: gam(vol~s(age) + sex + mprage_antsCT_vol_TBV + Mood + Psychosis + Ext + Phobias + OverallPsy)


####PSYCHOPATHOLOGY BIFACTORS####

###GM###
#GAM model
GmModels_ct <- lapply(CT_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GmModels_ct, summary)

#Create a vector p-values
GmMood_ct<-sapply(GmModels_ct, function(v) summary(v)$p.table[4,4])
GmPsych_ct<-sapply(GmModels_ct, function(v) summary(v)$p.table[5,4])
GmExt_ct<-sapply(GmModels_ct, function(v) summary(v)$p.table[6,4])
GmPhb_ct<-sapply(GmModels_ct, function(v) summary(v)$p.table[7,4])
GmOverallPsy_ct<-sapply(GmModels_ct, function(v) summary(v)$p.table[8,4])

#FDR correct p-values (Not needed in this case because there is only one model tested (GM only))
#GmMood_ct_fdr<-p.adjust(GmMood_ct,method="fdr")
#GmPsych_ct_fdr<-p.adjust(GmPsych_ct,method="fdr")
#GmExt_ct_fdr<-p.adjust(GmExt_ct,method="fdr")
#GmPhb_ct_fdr<-p.adjust(GmPhb_ct,method="fdr")
#GmOverallPsy_ct_fdr<-p.adjust(GmOverallPsy_ct,method="fdr")


###LOBES###
#GAM model
LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(LobeModels_ct, summary)

#Create a vector p-values
LobeMood_ct<-sapply(LobeModels_ct, function(v) summary(v)$p.table[4,4])
LobePsych_ct<-sapply(LobeModels_ct, function(v) summary(v)$p.table[5,4])
LobeExt_ct<-sapply(LobeModels_ct, function(v) summary(v)$p.table[6,4])
LobePhb_ct<-sapply(LobeModels_ct, function(v) summary(v)$p.table[7,4])
LobeOverallPsy_ct<-sapply(LobeModels_ct, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
LobeMood_ct_fdr<-p.adjust(LobeMood_ct,method="fdr")
LobePsych_ct_fdr<-p.adjust(LobePsych_ct,method="fdr")
LobeExt_ct_fdr<-p.adjust(LobeExt_ct,method="fdr")
LobePhb_ct_fdr<-p.adjust(LobePhb_ct,method="fdr")
LobeOverallPsy_ct_fdr<-p.adjust(LobeOverallPsy_ct,method="fdr")


###ROIs###
#GAM model
#Error in gam.fit(G, family = G$family, control = control, gamma = gamma,:iterative weights or data non-finite in gam.fit - regularization may help. See ?gam.control.

ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(ROIModels_ct, summary)

#Create a vector p-values
ROIMood_ct<-sapply(ROIModels_ct, function(v) summary(v)$p.table[4,4])
ROIPsych_ct<-sapply(ROIModels_ct, function(v) summary(v)$p.table[5,4])
ROIExt_ct<-sapply(ROIModels_ct, function(v) summary(v)$p.table[6,4])
ROIPhb_ct<-sapply(ROIModels_ct, function(v) summary(v)$p.table[7,4])
ROIOverallPsy_ct<-sapply(ROIModels_ct, function(v) summary(v)$p.table[8,4])

#FDR correct p-values 
ROIMood_ct_fdr<-p.adjust(ROIMood_ct,method="fdr")
ROIPsych_ct_fdr<-p.adjust(ROIPsych_ct,method="fdr")
ROIExt_ct_fdr<-p.adjust(ROIExt_ct,method="fdr")
ROIPhb_ct_fdr<-p.adjust(ROIPhb_ct,method="fdr")
ROIOverallPsy_ct_fdr<-p.adjust(ROIOverallPsy_ct,method="fdr")




####CORRELATED TRAITS- NOT AGE REGRESSED####

###GM###
#GAM model
GmModelsMood_ct <- lapply(CT_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GmModelsPsych_ct <- lapply(CT_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GmModelsExt_ct <- lapply(CT_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GmModelsFear_ct <- lapply(CT_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(GmModelsMood_ct, summary)
lapply(GmModelsPsych_ct, summary)
lapply(GmModelsExt_ct, summary)
lapply(GmModelsFear_ct, summary)

#Create a vector p-values
GmCMood_ct<-sapply(GmModelsMood_ct, function(v) summary(v)$p.table[4,4])
GmCPsych_ct<-sapply(GmModelsPsych_ct, function(v) summary(v)$p.table[4,4])
GmCExt_ct<-sapply(GmModelsExt_ct, function(v) summary(v)$p.table[4,4])
GmCFear_ct<-sapply(GmModelsFear_ct, function(v) summary(v)$p.table[4,4])

#FDR correct p-values (Not needed in this case because there is only one model tested (GM only))
#GmCMood_ct_fdr<-p.adjust(GmCMood_ct,method="fdr")
#GmCPsych_ct_fdr<-p.adjust(GmCPsych_ct,method="fdr")
#GmCExt_ct_fdr<-p.adjust(GmCExt_ct,method="fdr")
#GmCFear_ct_fdr<-p.adjust(GmCFear_ct,method="fdr")


###LOBES###
#GAM model
LobeModelsMood_ct <- lapply(CT_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

LobeModelsPsych_ct <- lapply(CT_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

LobeModelsExt_ct <- lapply(CT_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

LobeModelsFear_ct <- lapply(CT_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(LobeModelsMood_ct, summary)
lapply(LobeModelsPsych_ct, summary)
lapply(LobeModelsExt_ct, summary)
lapply(LobeModelsFear_ct, summary)

#Create a vector p-values
LobeCMood_ct<-sapply(LobeModelsMood_ct, function(v) summary(v)$p.table[4,4])
LobeCPsych_ct<-sapply(LobeModelsPsych_ct, function(v) summary(v)$p.table[4,4])
LobeCExt_ct<-sapply(LobeModelsExt_ct, function(v) summary(v)$p.table[4,4])
LobeCFear_ct<-sapply(LobeModelsFear_ct, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
LobeCMood_ct_fdr<-p.adjust(LobeCMood_ct,method="fdr")
LobeCPsych_ct_fdr<-p.adjust(LobeCPsych_ct,method="fdr")
LobeCExt_ct_fdr<-p.adjust(LobeCExt_ct,method="fdr")
LobeCFear_ct_fdr<-p.adjust(LobeCFear_ct,method="fdr")


###ROIs###
#GAM model
#Error in gam.fit(G, family = G$family, control = control, gamma = gamma,:iterative weights or data non-finite in gam.fit - regularization may help. See ?gam.control.

ROIModelsMood_ct <- lapply(CT_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

ROIModelsPsych_ct <- lapply(CT_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

ROIModelsExt_ct <- lapply(CT_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

ROIModelsFear_ct <- lapply(CT_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(ROIModelsMood_ct, summary)
lapply(ROIModelsPsych_ct, summary)
lapply(ROIModelsExt_ct, summary)
lapply(ROIModelsFear_ct, summary)

#Create a vector p-values
ROICMood_ct<-sapply(ROIModelsMood_ct, function(v) summary(v)$p.table[4,4])
ROICPsych_ct<-sapply(ROIModelsPsych_ct, function(v) summary(v)$p.table[4,4])
ROICExt_ct<-sapply(ROIModelsExt_ct, function(v) summary(v)$p.table[4,4])
ROICFear_ct<-sapply(ROIModelsFear_ct, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
ROICMood_ct_fdr<-p.adjust(ROICMood_ct,method="fdr")
ROICPsych_ct_fdr<-p.adjust(ROICPsych_ct,method="fdr")
ROICExt_ct_fdr<-p.adjust(ROICExt_ct,method="fdr")
ROICFear_ct_fdr<-p.adjust(ROICFear_ct,method="fdr")





##################
###GMD ANALYSES###
##################
#Structural data: GMD data from JLF: i.e., mprage_jlf_gmd_*
#MODEL: gam(vol~s(age) + sex + mprage_antsCT_vol_TBV + Mood + Psychosis + Ext + Phobias + OverallPsy)

####PSYCHOPATHOLOGY BIFACTORS####

###GM, WM, CSF###
#GAM model
GmModels_gmd <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GmModels_gmd, summary)

#Create a vector p-values
GmMood_gmd<-sapply(GmModels_gmd, function(v) summary(v)$p.table[4,4])
GmPsych_gmd<-sapply(GmModels_gmd, function(v) summary(v)$p.table[5,4])
GmExt_gmd<-sapply(GmModels_gmd, function(v) summary(v)$p.table[6,4])
GmPhb_gmd<-sapply(GmModels_gmd, function(v) summary(v)$p.table[7,4])
GmOverallPsy_gmd<-sapply(GmModels_gmd, function(v) summary(v)$p.table[8,4])

#FDR correct p-values (Not needed in this case because there is only one model tested (GM only))
#GmMood_gmd_fdr<-p.adjust(GmMood_gmd,method="fdr")
#GmPsych_gmd_fdr<-p.adjust(GmPsych_gmd,method="fdr")
#GmExt_gmd_fdr<-p.adjust(GmExt_gmd,method="fdr")
#GmPhb_gmd_fdr<-p.adjust(GmPhb_gmd,method="fdr")
#GmOverallPsy_gmd_fdr<-p.adjust(GmOverallPsy_gmd,method="fdr")


###LOBES###
#GAM model
LobeModels_gmd <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(LobeModels_gmd, summary)

#Create a vector p-values
LobeMood_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$p.table[4,4])
LobePsych_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$p.table[5,4])
LobeExt_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$p.table[6,4])
LobePhb_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$p.table[7,4])
LobeOverallPsy_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
LobeMood_gmd_fdr<-p.adjust(LobeMood_gmd,method="fdr")
LobePsych_gmd_fdr<-p.adjust(LobePsych_gmd,method="fdr")
LobeExt_gmd_fdr<-p.adjust(LobeExt_gmd,method="fdr")
LobePhb_gmd_fdr<-p.adjust(LobePhb_gmd,method="fdr")
LobeOverallPsy_gmd_fdr<-p.adjust(LobeOverallPsy_gmd,method="fdr")


###ROIs###
#GAM model
#Error in gam.fit(G, family = G$family, control = control, gamma = gamma,:iterative weights or data non-finite in gam.fit - regularization may help. See ?gam.control.

ROIModels_gmd <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(ROIModels_gmd, summary)

#Create a vector p-values
ROIMood_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$p.table[4,4])
ROIPsych_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$p.table[5,4])
ROIExt_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$p.table[6,4])
ROIPhb_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$p.table[7,4])
ROIOverallPsy_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$p.table[8,4])

#FDR correct p-values 
ROIMood_gmd_fdr<-p.adjust(ROIMood_gmd,method="fdr")
ROIPsych_gmd_fdr<-p.adjust(ROIPsych_gmd,method="fdr")
ROIExt_gmd_fdr<-p.adjust(ROIExt_gmd,method="fdr")
ROIPhb_gmd_fdr<-p.adjust(ROIPhb_gmd,method="fdr")
ROIOverallPsy_gmd_fdr<-p.adjust(ROIOverallPsy_gmd,method="fdr")




####CORRELATED TRAITS- NOT AGE REGRESSED####

###GM, WM, CSF###
#GAM model
GmModelsMood_gmd <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GmModelsPsych_gmd <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GmModelsExt_gmd <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GmModelsFear_gmd <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(GmModelsMood_gmd, summary)
lapply(GmModelsPsych_gmd, summary)
lapply(GmModelsExt_gmd, summary)
lapply(GmModelsFear_gmd, summary)

#Create a vector p-values
GmCMood_gmd<-sapply(GmModelsMood_gmd, function(v) summary(v)$p.table[4,4])
GmCPsych_gmd<-sapply(GmModelsPsych_gmd, function(v) summary(v)$p.table[4,4])
GmCExt_gmd<-sapply(GmModelsExt_gmd, function(v) summary(v)$p.table[4,4])
GmCFear_gmd<-sapply(GmModelsFear_gmd, function(v) summary(v)$p.table[4,4])

#FDR correct p-values (Not needed in this case because there is only one model tested (GM only))
#GmCMood_gmd_fdr<-p.adjust(GmCMood_gmd,method="fdr")
#GmCPsych_gmd_fdr<-p.adjust(GmCPsych_gmd,method="fdr")
#GmCExt_gmd_fdr<-p.adjust(GmCExt_gmd,method="fdr")
#GmCFear_gmd_fdr<-p.adjust(GmCFear_gmd,method="fdr")


###LOBES###
#GAM model
LobeModelsMood_gmd <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

LobeModelsPsych_gmd <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

LobeModelsExt_gmd <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

LobeModelsFear_gmd <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(LobeModelsMood_gmd, summary)
lapply(LobeModelsPsych_gmd, summary)
lapply(LobeModelsExt_gmd, summary)
lapply(LobeModelsFear_gmd, summary)

#Create a vector p-values
LobeCMood_gmd<-sapply(LobeModelsMood_gmd, function(v) summary(v)$p.table[4,4])
LobeCPsych_gmd<-sapply(LobeModelsPsych_gmd, function(v) summary(v)$p.table[4,4])
LobeCExt_gmd<-sapply(LobeModelsExt_gmd, function(v) summary(v)$p.table[4,4])
LobeCFear_gmd<-sapply(LobeModelsFear_gmd, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
LobeCMood_gmd_fdr<-p.adjust(LobeCMood_gmd,method="fdr")
LobeCPsych_gmd_fdr<-p.adjust(LobeCPsych_gmd,method="fdr")
LobeCExt_gmd_fdr<-p.adjust(LobeCExt_gmd,method="fdr")
LobeCFear_gmd_fdr<-p.adjust(LobeCFear_gmd,method="fdr")


###ROIs###
#GAM model
#Error in gam.fit(G, family = G$family, control = control, gamma = gamma,:iterative weights or data non-finite in gam.fit - regularization may help. See ?gam.control.

ROIModelsMood_gmd <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

ROIModelsPsych_gmd <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

ROIModelsExt_gmd <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

ROIModelsFear_gmd <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(ROIModelsMood_gmd, summary)
lapply(ROIModelsPsych_gmd, summary)
lapply(ROIModelsExt_gmd, summary)
lapply(ROIModelsFear_gmd, summary)

#Create a vector p-values
ROICMood_gmd<-sapply(ROIModelsMood_gmd, function(v) summary(v)$p.table[4,4])
ROICPsych_gmd<-sapply(ROIModelsPsych_gmd, function(v) summary(v)$p.table[4,4])
ROICExt_gmd<-sapply(ROIModelsExt_gmd, function(v) summary(v)$p.table[4,4])
ROICFear_gmd<-sapply(ROIModelsFear_gmd, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
ROICMood_gmd_fdr<-p.adjust(ROICMood_gmd,method="fdr")
ROICPsych_gmd_fdr<-p.adjust(ROICPsych_gmd,method="fdr")
ROICExt_gmd_fdr<-p.adjust(ROICExt_gmd,method="fdr")
ROICFear_gmd_fdr<-p.adjust(ROICFear_gmd,method="fdr")

