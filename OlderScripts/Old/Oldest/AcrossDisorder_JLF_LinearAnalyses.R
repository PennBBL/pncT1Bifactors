########################
###LOAD DATA & SUBSET###
########################

#Load subject GO1 data release (n=1601), scan dates (for finding missing ravens data later; n=1601), correlated traits data (n=9361, NOTE: there is one person from the 1601 sample who is missing correlated traits data), JLF volume data (n=1601), JLF cortical thickness data (n=1601), and JLF gray matter density data (n=1601). 
data.go1release <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_go1_datarel_020716.csv", header=TRUE, na.strings=".")
data.dates <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_bblids_scanids_dates.csv", header=TRUE, na.strings=".")
data.CorrTraits <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/Itemwise_Clinical_Corr-Traits_Factor_Scores.csv", header=TRUE, na.strings = ".")
data.vol <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_antsCtVol_jlfVol.csv", header=TRUE, na.strings=".")
data.ct <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_jlfCt.csv", header=TRUE, na.strings=".")
data.gmd <- read.csv("/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1601_jlfGMD.csv", header=TRUE, na.strings=".")

#Remove duplicate variables from ct and gmd data files (averageManualRating, t1Exclude, and mprage_antsCT_vol_TBV) since they already exist in the vol data file.
data.ct$averageManualRating<-NULL
data.ct$t1Exclude<-NULL
data.ct$mprage_antsCT_vol_TBV<-NULL

data.gmd$averageManualRating<-NULL
data.gmd$t1Exclude<-NULL
data.gmd$mprage_antsCT_vol_TBV<-NULL

##Merge data using bblid and scanid
#WARNING: Merging files with unequal cases will cause the non-matched bblids to be deleted (which is what we want in the case of correlated traits).
data.merge <- merge(data.go1release, data.dates, by=c("bblid","scanid"))
data.merge2 <- merge(data.merge, data.CorrTraits, by="bblid")
data.merge3 <- merge(data.merge2, data.vol, by=c("bblid","scanid"))
data.merge4 <- merge(data.merge3, data.ct, by=c("bblid","scanid"))
data.merge5 <- merge(data.merge4, data.gmd, by=c("bblid","scanid"))

#Put bblids in ascending order
data.final <- data.merge5[order(data.merge5$bblid),]

##Create an exclusion criteria variable
#The three exclusion criteria are: healthExclude=1 (1=health problems); mprageSbiaExclude=1 (1=problems with their volume data); and averageRating=0 (0=bad QA).
data.final$ACROSS.INCLUDE<-1
data.final$ACROSS.INCLUDE[data.final$healthExclude==1]<-0
data.final$ACROSS.INCLUDE[data.final$t1Exclude==1]<-0
data.final$ACROSS.INCLUDE[data.final$averageManualRating==0]<-0

##Exclude everyone that has one or more of these exclusion criteria
data.final <- data.final[which(data.final$ACROSS.INCLUDE == 1), ]

##After exclusion criteria, there are 1359 subjects
subjData<-data.final


############################
#SEPARATE INTO GM, WM, CSF
############################

###VOLUME###
#NOTE: total volumes per tissue class (GM, Wm, and CBF) already exist as variables in the vol data file.

#ALL ROIS
dataJLF<-subjData[,grep("mprage_jlf_vol",names(subjData))]

#GM-- these are the gray matter regions-- would only FDR across these 119 regions
dataJLFGm<-dataJLF[,-(unique(c( grep("WM",names(dataJLF)), grep("Vent",names(dataJLF)),grep("White",names(dataJLF)), grep("corpus",names(dataJLF)), grep("DC",names(dataJLF)), 
	grep("Brain_Stem",names(dataJLF)), grep("CSF",names(dataJLF)), grep("InC",names(dataJLF)), grep("fornix",names(dataJLF)), grep("ped",names(dataJLF)), grep("Vessel",names(dataJLF)),
	grep("OpticChiasm",names(dataJLF)) )))] 

#WM-- these are the white matter regions: 4 regions
dataJLFWm<-dataJLF[,(unique(c( grep("WM",names(dataJLF)),grep("fornix",names(dataJLF)),grep("White_Matter",names(dataJLF)), grep("corpus",names(dataJLF)), grep("InC",names(dataJLF)), 
        grep("ped",names(dataJLF)) )))]

#CSF-- 4 regions
dataJLFCsf<-dataJLF[,c(grep("CSF",names(dataJLF)),grep("Vent",names(dataJLF)))]
dataJLFCsf<-dataJLFCsf[,-c(grep("DC",names(dataJLFCsf)))]


###CORTICAL THICKNESS###

#GM-- these are the gray matter regions-- would only FDR across these 100 regions (NOTE: all of the JLF CT variables are GM regions; WM and CSF don't make sense for CT)
dataCT<-subjData[,grep("mprage_jlf_ct",names(subjData))]
dataCTGm<-dataCT

#Get total GM volume
dataCTGm<-data.matrix(dataCTGm)
subjData$CT_gmTotal<-rowSums(dataCTGm)


###GMD###

#ALL ROIS
dataGMD<-subjData[,grep("mprage_jlf_gmd",names(subjData))]

#GM-- these are the gray matter regions-- would only FDR across these 119 regions
dataGMDGm<-dataGMD[,-(unique(c( grep("WM",names(dataGMD)), grep("Vent",names(dataGMD)),grep("White",names(dataGMD)), grep("corpus",names(dataGMD)), grep("DC",names(dataGMD)), 
        grep("Brain_Stem",names(dataGMD)), grep("CSF",names(dataGMD)), grep("InC",names(dataGMD)), grep("fornix",names(dataGMD)), grep("ped",names(dataGMD)), grep("Vessel",names(dataGMD)),
	grep("OpticChiasm",names(dataGMD)) )))]
 
#Get total GM volume
dataGMDGm<-data.matrix(dataGMDGm)
subjData$GMD_gmTotal<-rowSums(dataGMDGm)


############################
###SEPARATE GM INTO LOBES###
############################

###VOLUME###

#cerebellum
dataJLFCereb<-dataJLF[,c(grep("Cerebellum_Exterior",names(dataJLF)),grep("CerVerLob",names(dataJLF)))] #5 as per atlas

#subcort
dataJLFSubcort<-dataJLF[,c(grep("Caudate",names(dataJLF)),grep("Putamen",names(dataJLF)),grep("Pallidum",names(dataJLF)),grep("Accumbens",names(dataJLF)),grep("Thal",names(dataJLF)),
        grep("BasForebr",names(dataJLF)),grep("Hippo",names(dataJLF)),grep("Amyg",names(dataJLF)))] #16 regions

#frontal
dataJLFFrontal<-dataJLF[,unique(c(grep("OrG",names(dataJLF)),grep("ins",names(dataJLF)),grep("FRP",names(dataJLF)),grep("FG",names(dataJLF)),grep("PrG",names(dataJLF)),
        grep("Gre",names(dataJLF)),grep("MFC",names(dataJLF)),grep("SCA",names(dataJLF)),grep("SMC",names(dataJLF)),grep("FRP",names(dataJLF)),grep("CO",names(dataJLF)),
        grep("FO",names(dataJLF)),grep("PO",names(dataJLF)),grep("CgG",names(dataJLF))))] #50 regions

#occipital
dataJLFOccipital<-dataJLF[,unique(c(grep("OG",names(dataJLF)),grep("OFuG",names(dataJLF)),grep("OCP",names(dataJLF)),grep("SOG",names(dataJLF)),grep("Calc",names(dataJLF)),
        grep("Cun",names(dataJLF)),grep("LiG",names(dataJLF))))] #16 regions

#parietal
dataJLFParietal<-dataJLF[,unique(c(grep("AnG",names(dataJLF)),grep("PoG",names(dataJLF)),grep("SMG",names(dataJLF)),grep("SPL",names(dataJLF)),grep("Pcu",names(dataJLF))))] #12 regions

#temporal
dataJLFTemporal<-dataJLF[,unique(c(grep("_FuG",names(dataJLF)),grep("TG",names(dataJLF)),grep("TMP",names(dataJLF)),grep("PP",names(dataJLF)),grep("PT",names(dataJLF)),
        grep("PHG",names(dataJLF)),grep("Ent",names(dataJLF))))] #20 regions

#get total volume for GM lobes
dataJLFCereb<-data.matrix(dataJLFCereb)
subjData$Vol_gmCerebTotal<-rowSums(dataJLFCereb)/1000
dataJLFSubcort<-data.matrix(dataJLFSubcort)
subjData$Vol_gmSubcortTotal<-rowSums(dataJLFSubcort)/1000
dataJLFFrontal<-data.matrix(dataJLFFrontal)
subjData$Vol_gmFrontalTotal<-rowSums(dataJLFFrontal)/1000
dataJLFOccipital<-data.matrix(dataJLFOccipital)
subjData$Vol_gmOccipitalTotal<-rowSums(dataJLFOccipital)/1000
dataJLFParietal<-data.matrix(dataJLFParietal)
subjData$Vol_gmParietalTotal<-rowSums(dataJLFParietal)/1000
dataJLFTemporal<-data.matrix(dataJLFTemporal)
subjData$Vol_gmTemporalTotal<-rowSums(dataJLFTemporal)/1000

#Change units for total GM/WM/CSF volumes to cc's
subjData$mprage_antsCT_vol_GrayMatter<-subjData$mprage_antsCT_vol_GrayMatter/1000
subjData$mprage_antsCT_vol_WhiteMatter<-subjData$mprage_antsCT_vol_WhiteMatter/1000
subjData$mprage_antsCT_vol_CSF<-subjData$mprage_antsCT_vol_CSF/1000



###CORTICAL THICKNESS###

#cerebellum (doesn't apply to JLF CT data)

#subcort (NOTE: only BasForebr exists in JLF CT data file). 
dataCTSubcort<-dataCT[,c(grep("Caudate",names(dataCT)),grep("Putamen",names(dataCT)),grep("Pallidum",names(dataCT)),grep("Accumbens",names(dataCT)),grep("Thal",names(dataCT)),
        grep("BasForebr",names(dataCT)),grep("Hippo",names(dataCT)),grep("Amyg",names(dataCT)))] #2 regions

#frontal
dataCTFrontal<-dataCT[,unique(c(grep("OrG",names(dataCT)),grep("ins",names(dataCT)),grep("FRP",names(dataCT)),grep("FG",names(dataCT)),grep("PrG",names(dataCT)),grep("Gre",names(dataCT)),
        grep("MFC",names(dataCT)),grep("SCA",names(dataCT)),grep("SMC",names(dataCT)),grep("FRP",names(dataCT)),grep("CO",names(dataCT)),grep("FO",names(dataCT)),grep("PO",names(dataCT)),
        grep("CgG",names(dataCT))))] #50 regions

#occipital
dataCTOccipital<-dataCT[,unique(c(grep("OG",names(dataCT)),grep("OFuG",names(dataCT)),grep("OCP",names(dataCT)),grep("SOG",names(dataCT)),grep("Calc",names(dataCT)),grep("Cun",names(dataCT)),
        grep("LiG",names(dataCT))))] #16 regions

#parietal
dataCTParietal<-dataCT[,unique(c(grep("AnG",names(dataCT)),grep("PoG",names(dataCT)),grep("SMG",names(dataCT)),grep("SPL",names(dataCT)),grep("Pcu",names(dataCT))))] #12 regions

#temporal
dataCTTemporal<-dataCT[,unique(c(grep("_FuG",names(dataCT)),grep("TG",names(dataCT)),grep("TMP",names(dataCT)),grep("PP",names(dataCT)),grep("PT",names(dataCT)),grep("PHG",names(dataCT)),
        grep("Ent",names(dataCT))))] #20 regions

#get total volume for GM subdivisions
dataCTSubcort<-data.matrix(dataCTSubcort)
subjData$CT_gmSubcortTotal<-rowSums(dataCTSubcort)
dataCTFrontal<-data.matrix(dataCTFrontal)
subjData$CT_gmFrontalTotal<-rowSums(dataCTFrontal)
dataCTOccipital<-data.matrix(dataCTOccipital)
subjData$CT_gmOccipitalTotal<-rowSums(dataCTOccipital)
dataCTParietal<-data.matrix(dataCTParietal)
subjData$CT_gmParietalTotal<-rowSums(dataCTParietal)
dataCTTemporal<-data.matrix(dataCTTemporal)
subjData$CT_gmTemporalTotal<-rowSums(dataCTTemporal)



###GMD###

#cerebellum
dataGMDCereb<-dataGMD[,c(grep("Cerebellum_Exterior",names(dataGMD)),grep("CerVerLob",names(dataGMD)))] #5 as per atlas

#subcort
dataGMDSubcort<-dataGMD[,c(grep("Caudate",names(dataGMD)),grep("Putamen",names(dataGMD)),grep("Pallidum",names(dataGMD)),grep("Accumbens",names(dataGMD)),grep("Thal",names(dataGMD)),
	grep("BasForebr",names(dataGMD)),grep("Hippo",names(dataGMD)),grep("Amyg",names(dataGMD)))] #16 regions

#frontal
dataGMDFrontal<-dataGMD[,unique(c(grep("OrG",names(dataGMD)),grep("ins",names(dataGMD)),grep("FRP",names(dataGMD)),grep("FG",names(dataGMD)),grep("PrG",names(dataGMD)),grep("Gre",names(dataGMD)),
        grep("MFC",names(dataGMD)),grep("SCA",names(dataGMD)),grep("SMC",names(dataGMD)),grep("FRP",names(dataGMD)),grep("CO",names(dataGMD)),grep("FO",names(dataGMD)),grep("PO",names(dataGMD)),
        grep("CgG",names(dataGMD))))] #50 regions

#occipital
dataGMDOccipital<-dataGMD[,unique(c(grep("OG",names(dataGMD)),grep("OFuG",names(dataGMD)),grep("OCP",names(dataGMD)),grep("SOG",names(dataGMD)),grep("Calc",names(dataGMD)),
        grep("Cun",names(dataGMD)),grep("LiG",names(dataGMD))))] #16 regions

#parietal
dataGMDParietal<-dataGMD[,unique(c(grep("AnG",names(dataGMD)),grep("PoG",names(dataGMD)),grep("SMG",names(dataGMD)),grep("SPL",names(dataGMD)),grep("Pcu",names(dataGMD))))] #12 regions

#temporal
dataGMDTemporal<-dataGMD[,unique(c(grep("_FuG",names(dataGMD)),grep("TG",names(dataGMD)),grep("TMP",names(dataGMD)),grep("PP",names(dataGMD)),grep("PT",names(dataGMD)),grep("PHG",names(dataGMD)),
        grep("Ent",names(dataGMD))))] #20 regions

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



#########################
###TRANSFORM VARIABLES###
#########################

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


###############
###SAVE DATA###
###############

#saveRDS(subjData,"/data/joy/BBL/projects/pncT1AcrossDisorder_Volume/subjectData/n1359_JLF_subjData.rds")



#######################################
###LOAD LIBRARY AND CREATE VAR LISTS###
#######################################

##Load library for nonlinear analyses
library(mgcv)

##Create lists of variables names of interest: grouped by 1) white matter, gray matter, csf, 2) lobes (don't include cerebellum), or 3) ROIs
  
Vol_GmWmCsf_List <- names(subjData)[2342:2344]
Vol_Lobe_List <- names(subjData)[2708:2712]
Vol_ROI_List <- colnames(dataJLFGm)

CT_Gm_List <- names(subjData)[2705]
CT_Lobe_List <- names(subjData)[2713:2717]
CT_ROI_List <- colnames(dataCTGm)

GMD_Gm_List <- names(subjData)[2706]
GMD_Lobe_List <- names(subjData)[2719:2723]
GMD_ROI_List <- colnames(dataGMDGm)



#####################
###VOLUME ANALYSES###
#####################
#Structural data: volumetric data from JLF: i.e., mprage_jlf_vol_*
#MODEL: lm(vol~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood + Psychosis + Ext + Phobias + OverallPsy)


####PSYCHOPATHOLOGY BIFACTORS####

###GM, WM, CSF###
#LM model
GmWmCsfModels_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  
    goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, 
    list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GmWmCsfModels_vol, summary)

#Create a vector p-values
GmWmCsfMood_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$coefficients[6,4])
GmWmCsfPsych_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$coefficients[7,4])
GmWmCsfExt_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$coefficients[8,4])
GmWmCsfPhb_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$coefficients[9,4])
GmWmCsfOverallPsy_vol<-sapply(GmWmCsfModels_vol, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values
GmWmCsfMood_vol_fdr<-p.adjust(GmWmCsfMood_vol,method="fdr")
GmWmCsfPsych_vol_fdr<-p.adjust(GmWmCsfPsych_vol,method="fdr")
GmWmCsfExt_vol_fdr<-p.adjust(GmWmCsfExt_vol,method="fdr")
GmWmCsfPhb_vol_fdr<-p.adjust(GmWmCsfPhb_vol,method="fdr")
GmWmCsfOverallPsy_vol_fdr<-p.adjust(GmWmCsfOverallPsy_vol,method="fdr")


###LOBES###
#LM model
LobeModels_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(LobeModels_vol, summary)

#Create a vector p-values
LobeMood_vol<-sapply(LobeModels_vol, function(v) summary(v)$coefficients[6,4])
LobePsych_vol<-sapply(LobeModels_vol, function(v) summary(v)$coefficients[7,4])
LobeExt_vol<-sapply(LobeModels_vol, function(v) summary(v)$coefficients[8,4])
LobePhb_vol<-sapply(LobeModels_vol, function(v) summary(v)$coefficients[9,4])
LobeOverallPsy_vol<-sapply(LobeModels_vol, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values
LobeMood_vol_fdr<-p.adjust(LobeMood_vol,method="fdr")
LobePsych_vol_fdr<-p.adjust(LobePsych_vol,method="fdr")
LobeExt_vol_fdr<-p.adjust(LobeExt_vol,method="fdr")
LobePhb_vol_fdr<-p.adjust(LobePhb_vol,method="fdr")
LobeOverallPsy_vol_fdr<-p.adjust(LobeOverallPsy_vol,method="fdr")


###ROIs###
#LM model
ROIModels_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(ROIModels_vol, summary)

#Create a vector p-values
ROIMood_vol<-sapply(ROIModels_vol, function(v) summary(v)$coefficients[6,4])
ROIPsych_vol<-sapply(ROIModels_vol, function(v) summary(v)$coefficients[7,4])
ROIExt_vol<-sapply(ROIModels_vol, function(v) summary(v)$coefficients[8,4])
ROIPhb_vol<-sapply(ROIModels_vol, function(v) summary(v)$coefficients[9,4])
ROIOverallPsy_vol<-sapply(ROIModels_vol, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values 
ROIMood_vol_fdr<-p.adjust(ROIMood_vol,method="fdr")
ROIPsych_vol_fdr<-p.adjust(ROIPsych_vol,method="fdr")
ROIExt_vol_fdr<-p.adjust(ROIExt_vol,method="fdr")
ROIPhb_vol_fdr<-p.adjust(ROIPhb_vol,method="fdr")
ROIOverallPsy_vol_fdr<-p.adjust(ROIOverallPsy_vol,method="fdr")




####CORRELATED TRAITS- NOT AGE REGRESSED####

###GM, WM, CSF###
#LM model
GmWmCsfModelsMood_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GmWmCsfModelsPsych_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GmWmCsfModelsExt_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GmWmCsfModelsFear_vol <- lapply(Vol_GmWmCsf_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(GmWmCsfModelsMood_vol, summary)
lapply(GmWmCsfModelsPsych_vol, summary)
lapply(GmWmCsfModelsExt_vol, summary)
lapply(GmWmCsfModelsFear_vol, summary)

#Create a vector p-values
GmWmCsfCMood_vol<-sapply(GmWmCsfModelsMood_vol, function(v) summary(v)$coefficients[6,4])
GmWmCsfCPsych_vol<-sapply(GmWmCsfModelsPsych_vol, function(v) summary(v)$coefficients[6,4])
GmWmCsfCExt_vol<-sapply(GmWmCsfModelsExt_vol, function(v) summary(v)$coefficients[6,4])
GmWmCsfCFear_vol<-sapply(GmWmCsfModelsFear_vol, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
GmWmCsfCMood_vol_fdr<-p.adjust(GmWmCsfCMood_vol,method="fdr")
GmWmCsfCPsych_vol_fdr<-p.adjust(GmWmCsfCPsych_vol,method="fdr")
GmWmCsfCExt_vol_fdr<-p.adjust(GmWmCsfCExt_vol,method="fdr")
GmWmCsfCFear_vol_fdr<-p.adjust(GmWmCsfCFear_vol,method="fdr")


###LOBES###
#LM model
LobeModelsMood_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

LobeModelsPsych_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

LobeModelsExt_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

LobeModelsFear_vol <- lapply(Vol_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(LobeModelsMood_vol, summary)
lapply(LobeModelsPsych_vol, summary)
lapply(LobeModelsExt_vol, summary)
lapply(LobeModelsFear_vol, summary)

#Create a vector p-values
LobeCMood_vol<-sapply(LobeModelsMood_vol, function(v) summary(v)$coefficients[6,4])
LobeCPsych_vol<-sapply(LobeModelsPsych_vol, function(v) summary(v)$coefficients[6,4])
LobeCExt_vol<-sapply(LobeModelsExt_vol, function(v) summary(v)$coefficients[6,4])
LobeCFear_vol<-sapply(LobeModelsFear_vol, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
LobeCMood_vol_fdr<-p.adjust(LobeCMood_vol,method="fdr")
LobeCPsych_vol_fdr<-p.adjust(LobeCPsych_vol,method="fdr")
LobeCExt_vol_fdr<-p.adjust(LobeCExt_vol,method="fdr")
LobeCFear_vol_fdr<-p.adjust(LobeCFear_vol,method="fdr")


###ROIs###
#LM model
ROIModelsMood_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

ROIModelsPsych_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

ROIModelsExt_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

ROIModelsFear_vol <- lapply(Vol_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(ROIModelsMood_vol, summary)
lapply(ROIModelsPsych_vol, summary)
lapply(ROIModelsExt_vol, summary)
lapply(ROIModelsFear_vol, summary)

#Create a vector p-values
ROICMood_vol<-sapply(ROIModelsMood_vol, function(v) summary(v)$coefficients[6,4])
ROICPsych_vol<-sapply(ROIModelsPsych_vol, function(v) summary(v)$coefficients[6,4])
ROICExt_vol<-sapply(ROIModelsExt_vol, function(v) summary(v)$coefficients[6,4])
ROICFear_vol<-sapply(ROIModelsFear_vol, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
ROICMood_vol_fdr<-p.adjust(ROICMood_vol,method="fdr")
ROICPsych_vol_fdr<-p.adjust(ROICPsych_vol,method="fdr")
ROICExt_vol_fdr<-p.adjust(ROICExt_vol,method="fdr")
ROICFear_vol_fdr<-p.adjust(ROICFear_vol,method="fdr")



#################################
###CORTICAL THICKNESS ANALYSES###
#################################
#Cortical thickness data from JLF: i.e., mprage_jlf_ct_*
#MODEL: lm(vol~age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood + Psychosis + Ext + Phobias + OverallPsy)


####PSYCHOPATHOLOGY BIFACTORS####

###GM, WM, CSF###
#LM model
GmModels_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GmModels_ct, summary)

#Create a vector p-values
GmMood_ct<-sapply(GmModels_ct, function(v) summary(v)$coefficients[6,4])
GmPsych_ct<-sapply(GmModels_ct, function(v) summary(v)$coefficients[7,4])
GmExt_ct<-sapply(GmModels_ct, function(v) summary(v)$coefficients[8,4])
GmPhb_ct<-sapply(GmModels_ct, function(v) summary(v)$coefficients[9,4])
GmOverallPsy_ct<-sapply(GmModels_ct, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values
GmMood_ct_fdr<-p.adjust(GmMood_ct,method="fdr")
GmPsych_ct_fdr<-p.adjust(GmPsych_ct,method="fdr")
GmExt_ct_fdr<-p.adjust(GmExt_ct,method="fdr")
GmPhb_ct_fdr<-p.adjust(GmPhb_ct,method="fdr")
GmOverallPsy_ct_fdr<-p.adjust(GmOverallPsy_ct,method="fdr")


###LOBES###
#LM model
LobeModels_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(LobeModels_ct, summary)

#Create a vector p-values
LobeMood_ct<-sapply(LobeModels_ct, function(v) summary(v)$coefficients[6,4])
LobePsych_ct<-sapply(LobeModels_ct, function(v) summary(v)$coefficients[7,4])
LobeExt_ct<-sapply(LobeModels_ct, function(v) summary(v)$coefficients[8,4])
LobePhb_ct<-sapply(LobeModels_ct, function(v) summary(v)$coefficients[9,4])
LobeOverallPsy_ct<-sapply(LobeModels_ct, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values
LobeMood_ct_fdr<-p.adjust(LobeMood_ct,method="fdr")
LobePsych_ct_fdr<-p.adjust(LobePsych_ct,method="fdr")
LobeExt_ct_fdr<-p.adjust(LobeExt_ct,method="fdr")
LobePhb_ct_fdr<-p.adjust(LobePhb_ct,method="fdr")
LobeOverallPsy_ct_fdr<-p.adjust(LobeOverallPsy_ct,method="fdr")


###ROIs###
#LM model

ROIModels_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(ROIModels_ct, summary)

#Create a vector p-values
ROIMood_ct<-sapply(ROIModels_ct, function(v) summary(v)$coefficients[6,4])
ROIPsych_ct<-sapply(ROIModels_ct, function(v) summary(v)$coefficients[7,4])
ROIExt_ct<-sapply(ROIModels_ct, function(v) summary(v)$coefficients[8,4])
ROIPhb_ct<-sapply(ROIModels_ct, function(v) summary(v)$coefficients[9,4])
ROIOverallPsy_ct<-sapply(ROIModels_ct, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values 
ROIMood_ct_fdr<-p.adjust(ROIMood_ct,method="fdr")
ROIPsych_ct_fdr<-p.adjust(ROIPsych_ct,method="fdr")
ROIExt_ct_fdr<-p.adjust(ROIExt_ct,method="fdr")
ROIPhb_ct_fdr<-p.adjust(ROIPhb_ct,method="fdr")
ROIOverallPsy_ct_fdr<-p.adjust(ROIOverallPsy_ct,method="fdr")




####CORRELATED TRAITS- NOT AGE REGRESSED####

###GM, WM, CSF###
#LM model
GmModelsMood_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GmModelsPsych_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GmModelsExt_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GmModelsFear_ct <- lapply(CT_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(GmModelsMood_ct, summary)
lapply(GmModelsPsych_ct, summary)
lapply(GmModelsExt_ct, summary)
lapply(GmModelsFear_ct, summary)

#Create a vector p-values
GmCMood_ct<-sapply(GmModelsMood_ct, function(v) summary(v)$coefficients[6,4])
GmCPsych_ct<-sapply(GmModelsPsych_ct, function(v) summary(v)$coefficients[6,4])
GmCExt_ct<-sapply(GmModelsExt_ct, function(v) summary(v)$coefficients[6,4])
GmCFear_ct<-sapply(GmModelsFear_ct, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
GmCMood_ct_fdr<-p.adjust(GmCMood_ct,method="fdr")
GmCPsych_ct_fdr<-p.adjust(GmCPsych_ct,method="fdr")
GmCExt_ct_fdr<-p.adjust(GmCExt_ct,method="fdr")
GmCFear_ct_fdr<-p.adjust(GmCFear_ct,method="fdr")


###LOBES###
#LM model
LobeModelsMood_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

LobeModelsPsych_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

LobeModelsExt_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

LobeModelsFear_ct <- lapply(CT_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(LobeModelsMood_ct, summary)
lapply(LobeModelsPsych_ct, summary)
lapply(LobeModelsExt_ct, summary)
lapply(LobeModelsFear_ct, summary)

#Create a vector p-values
LobeCMood_ct<-sapply(LobeModelsMood_ct, function(v) summary(v)$coefficients[6,4])
LobeCPsych_ct<-sapply(LobeModelsPsych_ct, function(v) summary(v)$coefficients[6,4])
LobeCExt_ct<-sapply(LobeModelsExt_ct, function(v) summary(v)$coefficients[6,4])
LobeCFear_ct<-sapply(LobeModelsFear_ct, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
LobeCMood_ct_fdr<-p.adjust(LobeCMood_ct,method="fdr")
LobeCPsych_ct_fdr<-p.adjust(LobeCPsych_ct,method="fdr")
LobeCExt_ct_fdr<-p.adjust(LobeCExt_ct,method="fdr")
LobeCFear_ct_fdr<-p.adjust(LobeCFear_ct,method="fdr")


###ROIs###
#LM model

ROIModelsMood_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

ROIModelsPsych_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

ROIModelsExt_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

ROIModelsFear_ct <- lapply(CT_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(ROIModelsMood_ct, summary)
lapply(ROIModelsPsych_ct, summary)
lapply(ROIModelsExt_ct, summary)
lapply(ROIModelsFear_ct, summary)

#Create a vector p-values
ROICMood_ct<-sapply(ROIModelsMood_ct, function(v) summary(v)$coefficients[6,4])
ROICPsych_ct<-sapply(ROIModelsPsych_ct, function(v) summary(v)$coefficients[6,4])
ROICExt_ct<-sapply(ROIModelsExt_ct, function(v) summary(v)$coefficients[6,4])
ROICFear_ct<-sapply(ROIModelsFear_ct, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
ROICMood_ct_fdr<-p.adjust(ROICMood_ct,method="fdr")
ROICPsych_ct_fdr<-p.adjust(ROICPsych_ct,method="fdr")
ROICExt_ct_fdr<-p.adjust(ROICExt_ct,method="fdr")
ROICFear_ct_fdr<-p.adjust(ROICFear_ct,method="fdr")





##################
###GMD ANALYSES###
##################
#Structural data: GMD data from JLF: i.e., mprage_jlf_gmd_*
#MODEL: lm(vol~age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood + Psychosis + Ext + Phobias + OverallPsy)

####PSYCHOPATHOLOGY BIFACTORS####

###GM, WM, CSF###
#LM model
GmModels_gmd <- lapply(GMD_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GmModels_gmd, summary)

#Create a vector p-values
GmMood_gmd<-sapply(GmModels_gmd, function(v) summary(v)$coefficients[6,4])
GmPsych_gmd<-sapply(GmModels_gmd, function(v) summary(v)$coefficients[7,4])
GmExt_gmd<-sapply(GmModels_gmd, function(v) summary(v)$coefficients[8,4])
GmPhb_gmd<-sapply(GmModels_gmd, function(v) summary(v)$coefficients[9,4])
GmOverallPsy_gmd<-sapply(GmModels_gmd, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values
GmMood_gmd_fdr<-p.adjust(GmMood_gmd,method="fdr")
GmPsych_gmd_fdr<-p.adjust(GmPsych_gmd,method="fdr")
GmExt_gmd_fdr<-p.adjust(GmExt_gmd,method="fdr")
GmPhb_gmd_fdr<-p.adjust(GmPhb_gmd,method="fdr")
GmOverallPsy_gmd_fdr<-p.adjust(GmOverallPsy_gmd,method="fdr")


###LOBES###
#LM model
LobeModels_gmd <- lapply(GMD_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(LobeModels_gmd, summary)

#Create a vector p-values
LobeMood_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$coefficients[6,4])
LobePsych_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$coefficients[7,4])
LobeExt_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$coefficients[8,4])
LobePhb_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$coefficients[9,4])
LobeOverallPsy_gmd<-sapply(LobeModels_gmd, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values
LobeMood_gmd_fdr<-p.adjust(LobeMood_gmd,method="fdr")
LobePsych_gmd_fdr<-p.adjust(LobePsych_gmd,method="fdr")
LobeExt_gmd_fdr<-p.adjust(LobeExt_gmd,method="fdr")
LobePhb_gmd_fdr<-p.adjust(LobePhb_gmd,method="fdr")
LobeOverallPsy_gmd_fdr<-p.adjust(LobeOverallPsy_gmd,method="fdr")


###ROIs###
#LM model

ROIModels_gmd <- lapply(GMD_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(ROIModels_gmd, summary)

#Create a vector p-values
ROIMood_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$coefficients[6,4])
ROIPsych_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$coefficients[7,4])
ROIExt_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$coefficients[8,4])
ROIPhb_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$coefficients[9,4])
ROIOverallPsy_gmd<-sapply(ROIModels_gmd, function(v) summary(v)$coefficients[10,4])

#FDR correct p-values 
ROIMood_gmd_fdr<-p.adjust(ROIMood_gmd,method="fdr")
ROIPsych_gmd_fdr<-p.adjust(ROIPsych_gmd,method="fdr")
ROIExt_gmd_fdr<-p.adjust(ROIExt_gmd,method="fdr")
ROIPhb_gmd_fdr<-p.adjust(ROIPhb_gmd,method="fdr")
ROIOverallPsy_gmd_fdr<-p.adjust(ROIOverallPsy_gmd,method="fdr")




####CORRELATED TRAITS- NOT AGE REGRESSED####

###GM, WM, CSF###
#LM model
GmModelsMood_gmd <- lapply(GMD_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GmModelsPsych_gmd <- lapply(GMD_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GmModelsExt_gmd <- lapply(GMD_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GmModelsFear_gmd <- lapply(GMD_Gm_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(GmModelsMood_gmd, summary)
lapply(GmModelsPsych_gmd, summary)
lapply(GmModelsExt_gmd, summary)
lapply(GmModelsFear_gmd, summary)

#Create a vector p-values
GmCMood_gmd<-sapply(GmModelsMood_gmd, function(v) summary(v)$coefficients[6,4])
GmCPsych_gmd<-sapply(GmModelsPsych_gmd, function(v) summary(v)$coefficients[6,4])
GmCExt_gmd<-sapply(GmModelsExt_gmd, function(v) summary(v)$coefficients[6,4])
GmCFear_gmd<-sapply(GmModelsFear_gmd, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
GmCMood_gmd_fdr<-p.adjust(GmCMood_gmd,method="fdr")
GmCPsych_gmd_fdr<-p.adjust(GmCPsych_gmd,method="fdr")
GmCExt_gmd_fdr<-p.adjust(GmCExt_gmd,method="fdr")
GmCFear_gmd_fdr<-p.adjust(GmCFear_gmd,method="fdr")


###LOBES###
#LM model
LobeModelsMood_gmd <- lapply(GMD_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

LobeModelsPsych_gmd <- lapply(GMD_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

LobeModelsExt_gmd <- lapply(GMD_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

LobeModelsFear_gmd <- lapply(GMD_Lobe_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(LobeModelsMood_gmd, summary)
lapply(LobeModelsPsych_gmd, summary)
lapply(LobeModelsExt_gmd, summary)
lapply(LobeModelsFear_gmd, summary)

#Create a vector p-values
LobeCMood_gmd<-sapply(LobeModelsMood_gmd, function(v) summary(v)$coefficients[6,4])
LobeCPsych_gmd<-sapply(LobeModelsPsych_gmd, function(v) summary(v)$coefficients[6,4])
LobeCExt_gmd<-sapply(LobeModelsExt_gmd, function(v) summary(v)$coefficients[6,4])
LobeCFear_gmd<-sapply(LobeModelsFear_gmd, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
LobeCMood_gmd_fdr<-p.adjust(LobeCMood_gmd,method="fdr")
LobeCPsych_gmd_fdr<-p.adjust(LobeCPsych_gmd,method="fdr")
LobeCExt_gmd_fdr<-p.adjust(LobeCExt_gmd,method="fdr")
LobeCFear_gmd_fdr<-p.adjust(LobeCFear_gmd,method="fdr")


###ROIs###
#LM model

ROIModelsMood_gmd <- lapply(GMD_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

ROIModelsPsych_gmd <- lapply(GMD_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

ROIModelsExt_gmd <- lapply(GMD_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

ROIModelsFear_gmd <- lapply(GMD_ROI_List, function(x) {
    lm(substitute(i ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(ROIModelsMood_gmd, summary)
lapply(ROIModelsPsych_gmd, summary)
lapply(ROIModelsExt_gmd, summary)
lapply(ROIModelsFear_gmd, summary)

#Create a vector p-values
ROICMood_gmd<-sapply(ROIModelsMood_gmd, function(v) summary(v)$coefficients[6,4])
ROICPsych_gmd<-sapply(ROIModelsPsych_gmd, function(v) summary(v)$coefficients[6,4])
ROICExt_gmd<-sapply(ROIModelsExt_gmd, function(v) summary(v)$coefficients[6,4])
ROICFear_gmd<-sapply(ROIModelsFear_gmd, function(v) summary(v)$coefficients[6,4])

#FDR correct p-values
ROICMood_gmd_fdr<-p.adjust(ROICMood_gmd,method="fdr")
ROICPsych_gmd_fdr<-p.adjust(ROICPsych_gmd,method="fdr")
ROICExt_gmd_fdr<-p.adjust(ROICExt_gmd,method="fdr")
ROICFear_gmd_fdr<-p.adjust(ROICFear_gmd,method="fdr")

