#################
### LOAD DATA ###
#################
#All n=1601 subjects (minus the exclusion criteria).
subjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_JLF_volCtGmd_subjData.rds")

#Data that has been subset to include those with anxiety disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) and Typically Developing, minus exclusion criteria.
AllAnxTdSubjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1094_JLF_volCtGmd_AllAnxTdSubjData.rds")

#Data that has been subset to only include those with anxiety disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) (No TD), minus exclusion criteria.
AllAnxSubjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n680_JLF_volCtGmd_AllAnxSubjData.rds")

#Data that has been subset to only include those age 12 and up (for STAI analyses)
staiSubjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1019_JLF_stai_subjData.rds")


#######################################
###LOAD LIBRARY AND CREATE VAR LISTS###
#######################################

##Load library for nonlinear analyses
library(mgcv)

##Create lists of variables names of interest: grouped by 1) white matter, gray matter, csf, 2) lobes (don't include cerebellum), or 3) ROIs
  
Vol_GmWmCsf_List <- names(subjData)[2350:2352]
Vol_Lobe_List <- names(subjData)[2851:2855]
dataVol <- subjData[,grep("mprage_jlf_vol",names(subjData))]
dataVolGm <- dataVol[,-(unique(c( grep("Vent",names(dataVol)), grep("White",names(dataVol)), grep("DC",names(dataVol)),
        grep("Brain_Stem",names(dataVol)), grep("CSF",names(dataVol)), grep("Vessel",names(dataVol)), grep("OpticChiasm",names(dataVol)) )))]
Vol_ROI_List <- colnames(dataVolGm)



#####################################
#####################################
###### VOLUME ANALYSES WITH TBV #####
### All Anx subjects only (no TD) ###
#####################################
#####################################
#Structural data: volumetric data from JLF: i.e., mprage_jlf_vol_*

#############################
#### STATE TRAIT ANXIETY ####
#############################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnx_Stai_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Stai_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnx_GmWmCsf_State <- sapply(Vol_TBV_AllAnx_Stai_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_Trait <- sapply(Vol_TBV_AllAnx_Stai_GmWmCsf, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnx_GmWmCsf_State_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_State,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_Trait_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_Trait,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnx_Stai_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Stai_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnx_Lobes_State <- sapply(Vol_TBV_AllAnx_Stai_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_Trait <- sapply(Vol_TBV_AllAnx_Stai_Lobes, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnx_Lobes_State_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_State,method="fdr")
Vol_TBV_AllAnx_Lobes_Trait_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_Trait,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnx_Stai_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Stai_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnx_ROIs_State <- sapply(Vol_TBV_AllAnx_Stai_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_Trait <- sapply(Vol_TBV_AllAnx_Stai_ROIs, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnx_ROIs_State_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_State,method="fdr")
Vol_TBV_AllAnx_ROIs_Trait_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_Trait,method="fdr")



############################
#### TRAIT ANXIETY ONLY ####
############################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnx_Trait_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Trait_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnx_GmWmCsf_TraitOnly <- sapply(Vol_TBV_AllAnx_Trait_GmWmCsf, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_GmWmCsf_TraitOnly_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_TraitOnly,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnx_Trait_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Trait_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnx_Lobes_TraitOnly <- sapply(Vol_TBV_AllAnx_Trait_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_Lobes_TraitOnly_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_TraitOnly,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnx_Trait_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Trait_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnx_ROIs_TraitOnly <- sapply(Vol_TBV_AllAnx_Trait_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_ROIs_TraitOnly_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_TraitOnly,method="fdr")



##############################################
#### CORRELATED TRAITS- NOT AGE REGRESSED ####
##############################################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnx_CorrTrMood_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrPsych_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrExt_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrFear_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_CorrTrMood_GmWmCsf, summary)
lapply(Vol_TBV_AllAnx_CorrTrPsych_GmWmCsf, summary)
lapply(Vol_TBV_AllAnx_CorrTrExt_GmWmCsf, summary)
lapply(Vol_TBV_AllAnx_CorrTrFear_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnx_GmWmCsf_CorrTrMood <- sapply(Vol_TBV_AllAnx_CorrTrMood_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_CorrTrPsych <- sapply(Vol_TBV_AllAnx_CorrTrPsych_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_CorrTrExt <- sapply(Vol_TBV_AllAnx_CorrTrExt_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_CorrTrFear <- sapply(Vol_TBV_AllAnx_CorrTrFear_GmWmCsf, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_GmWmCsf_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_CorrTrMood,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_CorrTrPsych,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_CorrTrExt,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_CorrTrFear,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnx_CorrTrMood_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrPsych_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrExt_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrFear_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxSubjData)
})


#Look at the model summaries
lapply(Vol_TBV_AllAnx_CorrTrMood_Lobes, summary)
lapply(Vol_TBV_AllAnx_CorrTrPsych_Lobes, summary)
lapply(Vol_TBV_AllAnx_CorrTrExt_Lobes, summary)
lapply(Vol_TBV_AllAnx_CorrTrFear_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnx_Lobes_CorrTrMood <- sapply(Vol_TBV_AllAnx_CorrTrMood_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_CorrTrPsych <- sapply(Vol_TBV_AllAnx_CorrTrPsych_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_CorrTrExt <- sapply(Vol_TBV_AllAnx_CorrTrExt_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_CorrTrFear <- sapply(Vol_TBV_AllAnx_CorrTrFear_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_Lobes_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_CorrTrMood,method="fdr")
Vol_TBV_AllAnx_Lobes_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_CorrTrPsych,method="fdr")
Vol_TBV_AllAnx_Lobes_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_CorrTrExt,method="fdr")
Vol_TBV_AllAnx_Lobes_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_CorrTrFear,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnx_CorrTrMood_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrPsych_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrExt_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxSubjData)
})

Vol_TBV_AllAnx_CorrTrFear_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxSubjData)
})


#Look at the model summaries
lapply(Vol_TBV_AllAnx_CorrTrMood_ROIs, summary)
lapply(Vol_TBV_AllAnx_CorrTrPsych_ROIs, summary)
lapply(Vol_TBV_AllAnx_CorrTrExt_ROIs, summary)
lapply(Vol_TBV_AllAnx_CorrTrFear_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnx_ROIs_CorrTrMood <- sapply(Vol_TBV_AllAnx_CorrTrMood_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_CorrTrPsych <- sapply(Vol_TBV_AllAnx_CorrTrPsych_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_CorrTrExt <- sapply(Vol_TBV_AllAnx_CorrTrExt_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_CorrTrFear <- sapply(Vol_TBV_AllAnx_CorrTrFear_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_ROIs_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_CorrTrMood,method="fdr")
Vol_TBV_AllAnx_ROIs_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_CorrTrPsych,method="fdr")
Vol_TBV_AllAnx_ROIs_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_CorrTrExt,method="fdr")
Vol_TBV_AllAnx_ROIs_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_CorrTrFear,method="fdr")



###################################
#### PSYCHOPATHOLOGY BIFACTORS ####
###################################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnx_Bifactors_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnx_GmWmCsf_Mood <- sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_Psych <- sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnx_GmWmCsf_Ext <- sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnx_GmWmCsf_Phb <- sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnx_GmWmCsf_OverallPsy <- sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllAnx_GmWmCsf_Mood_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_Mood,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_Psych_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_Psych,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_Ext_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_Ext,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_Phb_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_Phb,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_OverallPsy_fdr <- p.adjust(Vol_TBV_AllAnx_GmWmCsf_OverallPsy,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnx_Bifactors_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Bifactors_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnx_Lobes_Mood <- sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_Psych <- sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnx_Lobes_Ext <- sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnx_Lobes_Phb <- sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnx_Lobes_OverallPsy <- sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllAnx_Lobes_Mood_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_Mood,method="fdr")
Vol_TBV_AllAnx_Lobes_Psych_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_Psych,method="fdr")
Vol_TBV_AllAnx_Lobes_Ext_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_Ext,method="fdr")
Vol_TBV_AllAnx_Lobes_Phb_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_Phb,method="fdr")
Vol_TBV_AllAnx_Lobes_OverallPsy_fdr <- p.adjust(Vol_TBV_AllAnx_Lobes_OverallPsy,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnx_Bifactors_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Bifactors_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnx_ROIs_Mood <- sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_Psych <- sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnx_ROIs_Ext <- sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnx_ROIs_Phb <- sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnx_ROIs_OverallPsy <- sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[8,4])

#FDR correct p-values 
Vol_TBV_AllAnx_ROIs_Mood_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_Mood,method="fdr")
Vol_TBV_AllAnx_ROIs_Psych_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_Psych,method="fdr")
Vol_TBV_AllAnx_ROIs_Ext_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_Ext,method="fdr")
Vol_TBV_AllAnx_ROIs_Phb_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_Phb,method="fdr")
Vol_TBV_AllAnx_ROIs_OverallPsy_fdr <- p.adjust(Vol_TBV_AllAnx_ROIs_OverallPsy,method="fdr")



################################
################################
### VOLUME ANALYSES WITH TBV ###
### All Anx subjects and TD ####
################################
################################
#Structural data: volumetric data from JLF: i.e., mprage_jlf_vol_*

###########################
#### ALL ANXIETY VS TD ####
###########################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnxVsTd_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + AllAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxVsTd_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_GmWmCsf_AllAnxTd <- sapply(Vol_TBV_AllAnxVsTd_GmWmCsf, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_GmWmCsf_AllAnxTd_fdr <- p.adjust(Vol_TBV_GmWmCsf_AllAnxTd,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnxVsTd_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + AllAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxVsTd_Lobes, summary)

#Create a vector p-values
Vol_TBV_Lobes_AllAnxTd <- sapply(Vol_TBV_AllAnxVsTd_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_Lobes_AllAnxTd_fdr <- p.adjust(Vol_TBV_Lobes_AllAnxTd,method="fdr")


##ROIs###
#GAM model
Vol_TBV_AllAnxVsTd_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + AllAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxVsTd_ROIs, summary)

#Create a vector p-values
Vol_TBV_ROIs_AllAnxTd <- sapply(Vol_TBV_AllAnxVsTd_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_ROIs_AllAnxTd_fdr <- p.adjust(Vol_TBV_ROIs_AllAnxTd,method="fdr")



##############################
#### COARSE ANXIETY VS TD ####
##############################

###GM, WM, CSF###
#GAM model
Vol_TBV_CoarseAnxVsTd_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + CoarseAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_CoarseAnxVsTd_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_GmWmCsf_PtsdTd <- sapply(Vol_TBV_CoarseAnxVsTd_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_GmWmCsf_AnxTd <- sapply(Vol_TBV_CoarseAnxVsTd_GmWmCsf, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_GmWmCsf_PtsdTd_fdr <- p.adjust(Vol_TBV_GmWmCsf_PtsdTd,method="fdr")
Vol_TBV_GmWmCsf_AnxTd_fdr <- p.adjust(Vol_TBV_GmWmCsf_AnxTd,method="fdr")


###LOBES###
#GAM model
Vol_TBV_CoarseAnxVsTd_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + CoarseAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_CoarseAnxVsTd_Lobes, summary)

#Create a vector p-values
Vol_TBV_Lobes_PtsdTd <- sapply(Vol_TBV_CoarseAnxVsTd_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_Lobes_AnxTd <- sapply(Vol_TBV_CoarseAnxVsTd_Lobes, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_Lobes_PtsdTd_fdr <- p.adjust(Vol_TBV_Lobes_PtsdTd,method="fdr")
Vol_TBV_Lobes_AnxTd_fdr <- p.adjust(Vol_TBV_Lobes_AnxTd,method="fdr")


##ROIs###
#GAM model
Vol_TBV_CoarseAnxVsTd_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + CoarseAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_CoarseAnxVsTd_ROIs, summary)

#Create a vector p-values
Vol_TBV_ROIs_PtsdTd <- sapply(Vol_TBV_CoarseAnxVsTd_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_ROIs_AnxTd <- sapply(Vol_TBV_CoarseAnxVsTd_ROIs, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_ROIs_PtsdTd_fdr <- p.adjust(Vol_TBV_ROIs_PtsdTd,method="fdr")
Vol_TBV_ROIs_AnxTd_fdr <- p.adjust(Vol_TBV_ROIs_AnxTd,method="fdr")



#############################
#### STATE TRAIT ANXIETY ####
#############################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnxTd_Stai_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Stai_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_GmWmCsf_State <- sapply(Vol_TBV_AllAnxTd_Stai_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_GmWmCsf_Trait <- sapply(Vol_TBV_AllAnxTd_Stai_GmWmCsf, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_GmWmCsf_State_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_State,method="fdr")
Vol_TBV_AllAnxTd_GmWmCsf_Trait_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_Trait,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnxTd_Stai_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Stai_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_Lobes_State <- sapply(Vol_TBV_AllAnxTd_Stai_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_Lobes_Trait <- sapply(Vol_TBV_AllAnxTd_Stai_Lobes, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_Lobes_State_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_State,method="fdr")
Vol_TBV_AllAnxTd_Lobes_Trait_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_Trait,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnxTd_Stai_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Stai_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_ROIs_State <- sapply(Vol_TBV_AllAnxTd_Stai_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_ROIs_Trait <- sapply(Vol_TBV_AllAnxTd_Stai_ROIs, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_ROIs_State_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_State,method="fdr")
Vol_TBV_AllAnxTd_ROIs_Trait_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_Trait,method="fdr")



############################
#### TRAIT ANXIETY ONLY ####
############################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnxTd_Trait_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Trait_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_GmWmCsf_TraitOnly <- sapply(Vol_TBV_AllAnxTd_Trait_GmWmCsf, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_GmWmCsf_TraitOnly_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_TraitOnly,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnxTd_Trait_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Trait_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_Lobes_TraitOnly <- sapply(Vol_TBV_AllAnxTd_Trait_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_Lobes_TraitOnly_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_TraitOnly,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnxTd_Trait_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Trait_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_ROIs_TraitOnly <- sapply(Vol_TBV_AllAnxTd_Trait_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_ROIs_TraitOnly_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_TraitOnly,method="fdr")



##############################################
#### CORRELATED TRAITS- NOT AGE REGRESSED ####
##############################################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnxTd_CorrTrMood_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrPsych_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrExt_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrFear_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_CorrTrMood_GmWmCsf, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrPsych_GmWmCsf, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrExt_GmWmCsf, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrFear_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_GmWmCsf_CorrTrMood <- sapply(Vol_TBV_AllAnxTd_CorrTrMood_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_GmWmCsf_CorrTrPsych <- sapply(Vol_TBV_AllAnxTd_CorrTrPsych_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_GmWmCsf_CorrTrExt <- sapply(Vol_TBV_AllAnxTd_CorrTrExt_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_GmWmCsf_CorrTrFear <- sapply(Vol_TBV_AllAnxTd_CorrTrFear_GmWmCsf, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_GmWmCsf_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_CorrTrMood,method="fdr")
Vol_TBV_AllAnxTd_GmWmCsf_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_CorrTrPsych,method="fdr")
Vol_TBV_AllAnxTd_GmWmCsf_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_CorrTrExt,method="fdr")
Vol_TBV_AllAnxTd_GmWmCsf_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_CorrTrFear,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnxTd_CorrTrMood_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrPsych_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrExt_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrFear_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_CorrTrMood_Lobes, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrPsych_Lobes, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrExt_Lobes, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrFear_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_Lobes_CorrTrMood <- sapply(Vol_TBV_AllAnxTd_CorrTrMood_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_Lobes_CorrTrPsych <- sapply(Vol_TBV_AllAnxTd_CorrTrPsych_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_Lobes_CorrTrExt <- sapply(Vol_TBV_AllAnxTd_CorrTrExt_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_Lobes_CorrTrFear <- sapply(Vol_TBV_AllAnxTd_CorrTrFear_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_Lobes_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_CorrTrMood,method="fdr")
Vol_TBV_AllAnxTd_Lobes_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_CorrTrPsych,method="fdr")
Vol_TBV_AllAnxTd_Lobes_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_CorrTrExt,method="fdr")
Vol_TBV_AllAnxTd_Lobes_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_CorrTrFear,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnxTd_CorrTrMood_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrPsych_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrExt_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxTdSubjData)
})

Vol_TBV_AllAnxTd_CorrTrFear_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxTdSubjData)
})


#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_CorrTrMood_ROIs, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrPsych_ROIs, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrExt_ROIs, summary)
lapply(Vol_TBV_AllAnxTd_CorrTrFear_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_ROIs_CorrTrMood <- sapply(Vol_TBV_AllAnxTd_CorrTrMood_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_ROIs_CorrTrPsych <- sapply(Vol_TBV_AllAnxTd_CorrTrPsych_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_ROIs_CorrTrExt <- sapply(Vol_TBV_AllAnxTd_CorrTrExt_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_ROIs_CorrTrFear <- sapply(Vol_TBV_AllAnxTd_CorrTrFear_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_ROIs_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_CorrTrMood,method="fdr")
Vol_TBV_AllAnxTd_ROIs_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_CorrTrPsych,method="fdr")
Vol_TBV_AllAnxTd_ROIs_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_CorrTrExt,method="fdr")
Vol_TBV_AllAnxTd_ROIs_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_CorrTrFear,method="fdr")



###################################
#### PSYCHOPATHOLOGY BIFACTORS ####
###################################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllAnxTd_Bifactors_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
    +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Bifactors_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_GmWmCsf_Mood <- sapply(Vol_TBV_AllAnxTd_Bifactors_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_GmWmCsf_Psych <- sapply(Vol_TBV_AllAnxTd_Bifactors_GmWmCsf, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnxTd_GmWmCsf_Ext <- sapply(Vol_TBV_AllAnxTd_Bifactors_GmWmCsf, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnxTd_GmWmCsf_Phb <- sapply(Vol_TBV_AllAnxTd_Bifactors_GmWmCsf, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnxTd_GmWmCsf_OverallPsy <- sapply(Vol_TBV_AllAnxTd_Bifactors_GmWmCsf, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_GmWmCsf_Mood_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_Mood,method="fdr")
Vol_TBV_AllAnxTd_GmWmCsf_Psych_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_Psych,method="fdr")
Vol_TBV_AllAnxTd_GmWmCsf_Ext_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_Ext,method="fdr")
Vol_TBV_AllAnxTd_GmWmCsf_Phb_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_Phb,method="fdr")
Vol_TBV_AllAnxTd_GmWmCsf_OverallPsy_fdr <- p.adjust(Vol_TBV_AllAnxTd_GmWmCsf_OverallPsy,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnxTd_Bifactors_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Bifactors_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_Lobes_Mood <- sapply(Vol_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_Lobes_Psych <- sapply(Vol_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnxTd_Lobes_Ext <- sapply(Vol_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnxTd_Lobes_Phb <- sapply(Vol_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnxTd_Lobes_OverallPsy <- sapply(Vol_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_Lobes_Mood_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_Mood,method="fdr")
Vol_TBV_AllAnxTd_Lobes_Psych_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_Psych,method="fdr")
Vol_TBV_AllAnxTd_Lobes_Ext_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_Ext,method="fdr")
Vol_TBV_AllAnxTd_Lobes_Phb_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_Phb,method="fdr")
Vol_TBV_AllAnxTd_Lobes_OverallPsy_fdr <- p.adjust(Vol_TBV_AllAnxTd_Lobes_OverallPsy,method="fdr")


##ROIs###
#GAM model
Vol_TBV_AllAnxTd_Bifactors_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnxTd_Bifactors_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnxTd_ROIs_Mood <- sapply(Vol_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnxTd_ROIs_Psych <- sapply(Vol_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnxTd_ROIs_Ext <- sapply(Vol_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnxTd_ROIs_Phb <- sapply(Vol_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnxTd_ROIs_OverallPsy <- sapply(Vol_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllAnxTd_ROIs_Mood_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_Mood,method="fdr")
Vol_TBV_AllAnxTd_ROIs_Psych_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_Psych,method="fdr")
Vol_TBV_AllAnxTd_ROIs_Ext_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_Ext,method="fdr")
Vol_TBV_AllAnxTd_ROIs_Phb_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_Phb,method="fdr")
Vol_TBV_AllAnxTd_ROIs_OverallPsy_fdr <- p.adjust(Vol_TBV_AllAnxTd_ROIs_OverallPsy,method="fdr")



################################
################################
### VOLUME ANALYSES WITH TBV ###
##### Full sample (n=1601) #####
################################
################################
#Structural data: volumetric data from JLF: i.e., mprage_jlf_vol_*

#############################
#### STATE TRAIT ANXIETY ####
#############################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllSubj_Stai_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Stai_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllSubj_GmWmCsf_State <- sapply(Vol_TBV_AllSubj_Stai_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_GmWmCsf_Trait <- sapply(Vol_TBV_AllSubj_Stai_GmWmCsf, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllSubj_GmWmCsf_State_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_State,method="fdr")
Vol_TBV_AllSubj_GmWmCsf_Trait_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_Trait,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllSubj_Stai_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Stai_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllSubj_Lobes_State <- sapply(Vol_TBV_AllSubj_Stai_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_Lobes_Trait <- sapply(Vol_TBV_AllSubj_Stai_Lobes, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllSubj_Lobes_State_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_State,method="fdr")
Vol_TBV_AllSubj_Lobes_Trait_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_Trait,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllSubj_Stai_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Stai_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllSubj_ROIs_State <- sapply(Vol_TBV_AllSubj_Stai_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_ROIs_Trait <- sapply(Vol_TBV_AllSubj_Stai_ROIs, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllSubj_ROIs_State_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_State,method="fdr")
Vol_TBV_AllSubj_ROIs_Trait_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_Trait,method="fdr")



############################
#### TRAIT ANXIETY ONLY ####
############################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllSubj_Trait_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Trait_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllSubj_GmWmCsf_TraitOnly <- sapply(Vol_TBV_AllSubj_Trait_GmWmCsf, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllSubj_GmWmCsf_TraitOnly_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_TraitOnly,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllSubj_Trait_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Trait_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllSubj_Lobes_TraitOnly <- sapply(Vol_TBV_AllSubj_Trait_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllSubj_Lobes_TraitOnly_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_TraitOnly,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllSubj_Trait_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Trait_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllSubj_ROIs_TraitOnly <- sapply(Vol_TBV_AllSubj_Trait_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllSubj_ROIs_TraitOnly_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_TraitOnly,method="fdr")



##############################################
#### CORRELATED TRAITS- NOT AGE REGRESSED ####
##############################################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllSubj_CorrTrMood_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrPsych_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrExt_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrFear_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_CorrTrMood_GmWmCsf, summary)
lapply(Vol_TBV_AllSubj_CorrTrPsych_GmWmCsf, summary)
lapply(Vol_TBV_AllSubj_CorrTrExt_GmWmCsf, summary)
lapply(Vol_TBV_AllSubj_CorrTrFear_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllSubj_GmWmCsf_CorrTrMood <- sapply(Vol_TBV_AllSubj_CorrTrMood_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_GmWmCsf_CorrTrPsych <- sapply(Vol_TBV_AllSubj_CorrTrPsych_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_GmWmCsf_CorrTrExt <- sapply(Vol_TBV_AllSubj_CorrTrExt_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_GmWmCsf_CorrTrFear <- sapply(Vol_TBV_AllSubj_CorrTrFear_GmWmCsf, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllSubj_GmWmCsf_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_CorrTrMood,method="fdr")
Vol_TBV_AllSubj_GmWmCsf_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_CorrTrPsych,method="fdr")
Vol_TBV_AllSubj_GmWmCsf_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_CorrTrExt,method="fdr")
Vol_TBV_AllSubj_GmWmCsf_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_CorrTrFear,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllSubj_CorrTrMood_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrPsych_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrExt_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrFear_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(Vol_TBV_AllSubj_CorrTrMood_Lobes, summary)
lapply(Vol_TBV_AllSubj_CorrTrPsych_Lobes, summary)
lapply(Vol_TBV_AllSubj_CorrTrExt_Lobes, summary)
lapply(Vol_TBV_AllSubj_CorrTrFear_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllSubj_Lobes_CorrTrMood <- sapply(Vol_TBV_AllSubj_CorrTrMood_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_Lobes_CorrTrPsych <- sapply(Vol_TBV_AllSubj_CorrTrPsych_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_Lobes_CorrTrExt <- sapply(Vol_TBV_AllSubj_CorrTrExt_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_Lobes_CorrTrFear <- sapply(Vol_TBV_AllSubj_CorrTrFear_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllSubj_Lobes_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_CorrTrMood,method="fdr")
Vol_TBV_AllSubj_Lobes_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_CorrTrPsych,method="fdr")
Vol_TBV_AllSubj_Lobes_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_CorrTrExt,method="fdr")
Vol_TBV_AllSubj_Lobes_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_CorrTrFear,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllSubj_CorrTrMood_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrPsych_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrExt_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

Vol_TBV_AllSubj_CorrTrFear_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(Vol_TBV_AllSubj_CorrTrMood_ROIs, summary)
lapply(Vol_TBV_AllSubj_CorrTrPsych_ROIs, summary)
lapply(Vol_TBV_AllSubj_CorrTrExt_ROIs, summary)
lapply(Vol_TBV_AllSubj_CorrTrFear_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllSubj_ROIs_CorrTrMood <- sapply(Vol_TBV_AllSubj_CorrTrMood_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_ROIs_CorrTrPsych <- sapply(Vol_TBV_AllSubj_CorrTrPsych_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_ROIs_CorrTrExt <- sapply(Vol_TBV_AllSubj_CorrTrExt_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_ROIs_CorrTrFear <- sapply(Vol_TBV_AllSubj_CorrTrFear_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllSubj_ROIs_CorrTrMood_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_CorrTrMood,method="fdr")
Vol_TBV_AllSubj_ROIs_CorrTrPsych_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_CorrTrPsych,method="fdr")
Vol_TBV_AllSubj_ROIs_CorrTrExt_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_CorrTrExt,method="fdr")
Vol_TBV_AllSubj_ROIs_CorrTrFear_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_CorrTrFear,method="fdr")



###################################
#### PSYCHOPATHOLOGY BIFACTORS ####
###################################

###GM, WM, CSF###
#GAM model
Vol_TBV_AllSubj_Bifactors_GmWmCsf <- lapply(Vol_GmWmCsf_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Bifactors_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllSubj_GmWmCsf_Mood <- sapply(Vol_TBV_AllSubj_Bifactors_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_GmWmCsf_Psych <- sapply(Vol_TBV_AllSubj_Bifactors_GmWmCsf, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllSubj_GmWmCsf_Ext <- sapply(Vol_TBV_AllSubj_Bifactors_GmWmCsf, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllSubj_GmWmCsf_Phb <- sapply(Vol_TBV_AllSubj_Bifactors_GmWmCsf, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllSubj_GmWmCsf_OverallPsy <- sapply(Vol_TBV_AllSubj_Bifactors_GmWmCsf, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllSubj_GmWmCsf_Mood_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_Mood,method="fdr")
Vol_TBV_AllSubj_GmWmCsf_Psych_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_Psych,method="fdr")
Vol_TBV_AllSubj_GmWmCsf_Ext_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_Ext,method="fdr")
Vol_TBV_AllSubj_GmWmCsf_Phb_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_Phb,method="fdr")
Vol_TBV_AllSubj_GmWmCsf_OverallPsy_fdr <- p.adjust(Vol_TBV_AllSubj_GmWmCsf_OverallPsy,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllSubj_Bifactors_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Bifactors_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllSubj_Lobes_Mood <- sapply(Vol_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_Lobes_Psych <- sapply(Vol_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllSubj_Lobes_Ext <- sapply(Vol_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllSubj_Lobes_Phb <- sapply(Vol_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllSubj_Lobes_OverallPsy <- sapply(Vol_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllSubj_Lobes_Mood_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_Mood,method="fdr")
Vol_TBV_AllSubj_Lobes_Psych_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_Psych,method="fdr")
Vol_TBV_AllSubj_Lobes_Ext_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_Ext,method="fdr")
Vol_TBV_AllSubj_Lobes_Phb_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_Phb,method="fdr")
Vol_TBV_AllSubj_Lobes_OverallPsy_fdr <- p.adjust(Vol_TBV_AllSubj_Lobes_OverallPsy,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllSubj_Bifactors_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllSubj_Bifactors_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllSubj_ROIs_Mood <- sapply(Vol_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllSubj_ROIs_Psych <- sapply(Vol_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllSubj_ROIs_Ext <- sapply(Vol_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllSubj_ROIs_Phb <- sapply(Vol_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllSubj_ROIs_OverallPsy <- sapply(Vol_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllSubj_ROIs_Mood_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_Mood,method="fdr")
Vol_TBV_AllSubj_ROIs_Psych_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_Psych,method="fdr")
Vol_TBV_AllSubj_ROIs_Ext_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_Ext,method="fdr")
Vol_TBV_AllSubj_ROIs_Phb_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_Phb,method="fdr")
Vol_TBV_AllSubj_ROIs_OverallPsy_fdr <- p.adjust(Vol_TBV_AllSubj_ROIs_OverallPsy,method="fdr")
