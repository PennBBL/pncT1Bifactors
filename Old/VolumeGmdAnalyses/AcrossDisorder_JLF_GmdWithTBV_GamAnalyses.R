#################
### LOAD DATA ###
#################
#All n=1601 subjects (minus the exclusion criteria).
subjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_JLF_volCtGmd_subjData.rds")

#Data that has been subset to include those with anxiety disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) and Typically Developing, minus exclusion criteria.
AllAnxTdSubjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1094_JLF_volCtGmd_AllAnxTdSubjData.rds")

#Data that has been subset to only include those with anxiety disorders (Agr, Gad, Ocd, Pan, Ptd, Sep, Soc, Sph) (No TD), minus exclusion criteria.
AllAnxSubjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n680_JLF_volCtGmd_AllAnxSubjData.rds")

#Data that has been subset to only include those age 12	and up (for STAI analyses)
staiSubjData <- readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1019_JLF_stai_subjData.rds")


#######################################
###LOAD LIBRARY AND CREATE VAR LISTS###
#######################################

##Load library for nonlinear analyses
library(mgcv)

##Create lists of variables names of interest: 1) gray matter (don't include white matter or csf for gmd data), 2) lobes (don't include cerebellum), or 3) ROIs.

GMD_Gm_List <- names(subjData)[2849]
GMD_Lobe_List <- names(subjData)[2861:2865]
dataGMD <- subjData[,grep("mprage_jlf_gmd",names(subjData))]
dataGMDGm <- dataGMD[,-(unique(c( grep("Vent",names(dataGMD)), grep("White",names(dataGMD)), grep("DC",names(dataGMD)),
				grep("Brain_Stem",names(dataGMD)), grep("CSF",names(dataGMD)), grep("Vessel",names(dataGMD)), grep("OpticChiasm",names(dataGMD)) )))]
GMD_ROI_List <- colnames(dataGMDGm)


##################################################
##################################################
###### GRAY MATTER DENSITY ANALYSES WITH TBV #####
######## All Anx subjects only (no TD) ###########
##################################################
##################################################
#GMD data: gray matter density data from JLF: i.e., mprage_jlf_gmd_*

#############################
#### STATE TRAIT ANXIETY ####
#############################

###GM###
#GAM model
GMD_TBV_AllAnx_Stai_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Stai_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllAnx_Gm_State <- sapply(GMD_TBV_AllAnx_Stai_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Gm_Trait <- sapply(GMD_TBV_AllAnx_Stai_Gm, function(v) summary(v)$p.table[5,4])


###LOBES###
#GAM model
GMD_TBV_AllAnx_Stai_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Stai_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllAnx_Lobes_State <- sapply(GMD_TBV_AllAnx_Stai_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Lobes_Trait <- sapply(GMD_TBV_AllAnx_Stai_Lobes, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
GMD_TBV_AllAnx_Lobes_State_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_State,method="fdr")
GMD_TBV_AllAnx_Lobes_Trait_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_Trait,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllAnx_Stai_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Stai_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllAnx_ROIs_State <- sapply(GMD_TBV_AllAnx_Stai_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_ROIs_Trait <- sapply(GMD_TBV_AllAnx_Stai_ROIs, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
GMD_TBV_AllAnx_ROIs_State_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_State,method="fdr")
GMD_TBV_AllAnx_ROIs_Trait_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_Trait,method="fdr")



############################
#### TRAIT ANXIETY ONLY ####
############################

###GM###
#GAM model
GMD_TBV_AllAnx_Trait_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Trait_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllAnx_Gm_TraitOnly <- sapply(GMD_TBV_AllAnx_Trait_Gm, function(v) summary(v)$p.table[4,4])


###LOBES###
#GAM model
GMD_TBV_AllAnx_Trait_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Trait_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllAnx_Lobes_TraitOnly <- sapply(GMD_TBV_AllAnx_Trait_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllAnx_Lobes_TraitOnly_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_TraitOnly,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllAnx_Trait_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Trait_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllAnx_ROIs_TraitOnly <- sapply(GMD_TBV_AllAnx_Trait_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllAnx_ROIs_TraitOnly_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_TraitOnly,method="fdr")



##############################################
#### CORRELATED TRAITS- NOT AGE REGRESSED ####
##############################################

###GM###
#GAM model
GMD_TBV_AllAnx_CorrTrMood_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrPsych_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrExt_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrFear_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_CorrTrMood_Gm, summary)
lapply(GMD_TBV_AllAnx_CorrTrPsych_Gm, summary)
lapply(GMD_TBV_AllAnx_CorrTrExt_Gm, summary)
lapply(GMD_TBV_AllAnx_CorrTrFear_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllAnx_Gm_CorrTrMood <- sapply(GMD_TBV_AllAnx_CorrTrMood_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Gm_CorrTrPsych <- sapply(GMD_TBV_AllAnx_CorrTrPsych_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Gm_CorrTrExt <- sapply(GMD_TBV_AllAnx_CorrTrExt_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Gm_CorrTrFear <- sapply(GMD_TBV_AllAnx_CorrTrFear_Gm, function(v) summary(v)$p.table[4,4])


###LOBES###
#GAM model
GMD_TBV_AllAnx_CorrTrMood_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrPsych_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrExt_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrFear_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxSubjData)
})


#Look at the model summaries
lapply(GMD_TBV_AllAnx_CorrTrMood_Lobes, summary)
lapply(GMD_TBV_AllAnx_CorrTrPsych_Lobes, summary)
lapply(GMD_TBV_AllAnx_CorrTrExt_Lobes, summary)
lapply(GMD_TBV_AllAnx_CorrTrFear_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllAnx_Lobes_CorrTrMood <- sapply(GMD_TBV_AllAnx_CorrTrMood_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Lobes_CorrTrPsych <- sapply(GMD_TBV_AllAnx_CorrTrPsych_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Lobes_CorrTrExt <- sapply(GMD_TBV_AllAnx_CorrTrExt_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Lobes_CorrTrFear <- sapply(GMD_TBV_AllAnx_CorrTrFear_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllAnx_Lobes_CorrTrMood_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_CorrTrMood,method="fdr")
GMD_TBV_AllAnx_Lobes_CorrTrPsych_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_CorrTrPsych,method="fdr")
GMD_TBV_AllAnx_Lobes_CorrTrExt_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_CorrTrExt,method="fdr")
GMD_TBV_AllAnx_Lobes_CorrTrFear_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_CorrTrFear,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllAnx_CorrTrMood_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrPsych_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrExt_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxSubjData)
})

GMD_TBV_AllAnx_CorrTrFear_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxSubjData)
})


#Look at the model summaries
lapply(GMD_TBV_AllAnx_CorrTrMood_ROIs, summary)
lapply(GMD_TBV_AllAnx_CorrTrPsych_ROIs, summary)
lapply(GMD_TBV_AllAnx_CorrTrExt_ROIs, summary)
lapply(GMD_TBV_AllAnx_CorrTrFear_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllAnx_ROIs_CorrTrMood <- sapply(GMD_TBV_AllAnx_CorrTrMood_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_ROIs_CorrTrPsych <- sapply(GMD_TBV_AllAnx_CorrTrPsych_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_ROIs_CorrTrExt <- sapply(GMD_TBV_AllAnx_CorrTrExt_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_ROIs_CorrTrFear <- sapply(GMD_TBV_AllAnx_CorrTrFear_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllAnx_ROIs_CorrTrMood_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_CorrTrMood,method="fdr")
GMD_TBV_AllAnx_ROIs_CorrTrPsych_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_CorrTrPsych,method="fdr")
GMD_TBV_AllAnx_ROIs_CorrTrExt_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_CorrTrExt,method="fdr")
GMD_TBV_AllAnx_ROIs_CorrTrFear_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_CorrTrFear,method="fdr")



###################################
#### PSYCHOPATHOLOGY BIFACTORS ####
###################################

###GM###
#GAM model
GMD_TBV_AllAnx_Bifactors_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Bifactors_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllAnx_Gm_Mood <- sapply(GMD_TBV_AllAnx_Bifactors_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Gm_Psych <- sapply(GMD_TBV_AllAnx_Bifactors_Gm, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllAnx_Gm_Ext <- sapply(GMD_TBV_AllAnx_Bifactors_Gm, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllAnx_Gm_Phb <- sapply(GMD_TBV_AllAnx_Bifactors_Gm, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllAnx_Gm_OverallPsy <- sapply(GMD_TBV_AllAnx_Bifactors_Gm, function(v) summary(v)$p.table[8,4])


###LOBES###
#GAM model
GMD_TBV_AllAnx_Bifactors_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Bifactors_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllAnx_Lobes_Mood <- sapply(GMD_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_Lobes_Psych <- sapply(GMD_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllAnx_Lobes_Ext <- sapply(GMD_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllAnx_Lobes_Phb <- sapply(GMD_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllAnx_Lobes_OverallPsy <- sapply(GMD_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
GMD_TBV_AllAnx_Lobes_Mood_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_Mood,method="fdr")
GMD_TBV_AllAnx_Lobes_Psych_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_Psych,method="fdr")
GMD_TBV_AllAnx_Lobes_Ext_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_Ext,method="fdr")
GMD_TBV_AllAnx_Lobes_Phb_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_Phb,method="fdr")
GMD_TBV_AllAnx_Lobes_OverallPsy_fdr <- p.adjust(GMD_TBV_AllAnx_Lobes_OverallPsy,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllAnx_Bifactors_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnx_Bifactors_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllAnx_ROIs_Mood <- sapply(GMD_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnx_ROIs_Psych <- sapply(GMD_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllAnx_ROIs_Ext <- sapply(GMD_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllAnx_ROIs_Phb <- sapply(GMD_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllAnx_ROIs_OverallPsy <- sapply(GMD_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[8,4])

#FDR correct p-values 
GMD_TBV_AllAnx_ROIs_Mood_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_Mood,method="fdr")
GMD_TBV_AllAnx_ROIs_Psych_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_Psych,method="fdr")
GMD_TBV_AllAnx_ROIs_Ext_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_Ext,method="fdr")
GMD_TBV_AllAnx_ROIs_Phb_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_Phb,method="fdr")
GMD_TBV_AllAnx_ROIs_OverallPsy_fdr <- p.adjust(GMD_TBV_AllAnx_ROIs_OverallPsy,method="fdr")



#############################################
#############################################
### GRAY MATTER DENSITY ANALYSES WITH TBV ###
######## All Anx subjects and TD ############
#############################################
#############################################
#GMD data: gray matter density data from JLF: i.e., mprage_jlf_gmd_*

###########################
#### ALL ANXIETY VS TD ####
###########################

###GM###
#GAM model
GMD_TBV_AllAnxVsTd_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + AllAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxVsTd_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_Gm_AllAnxTd <- sapply(GMD_TBV_AllAnxVsTd_Gm, function(v) summary(v)$p.table[4,4])


###LOBES###
#GAM model
GMD_TBV_AllAnxVsTd_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + AllAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxVsTd_Lobes, summary)

#Create a vector p-values
GMD_TBV_Lobes_AllAnxTd <- sapply(GMD_TBV_AllAnxVsTd_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_Lobes_AllAnxTd_fdr <- p.adjust(GMD_TBV_Lobes_AllAnxTd,method="fdr")


##ROIs###
#GAM model
GMD_TBV_AllAnxVsTd_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + AllAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxVsTd_ROIs, summary)

#Create a vector p-values
GMD_TBV_ROIs_AllAnxTd <- sapply(GMD_TBV_AllAnxVsTd_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_ROIs_AllAnxTd_fdr <- p.adjust(GMD_TBV_ROIs_AllAnxTd,method="fdr")



##############################
#### COARSE ANXIETY VS TD ####
##############################

###GM###
#GAM model
GMD_TBV_CoarseAnxVsTd_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + CoarseAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_CoarseAnxVsTd_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_Gm_CoarseAnxTd <- sapply(GMD_TBV_CoarseAnxVsTd_Gm, function(v) summary(v)$p.table[4,4])


###LOBES###
#GAM model
GMD_TBV_CoarseAnxVsTd_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + CoarseAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_CoarseAnxVsTd_Lobes, summary)

#Create a vector p-values
GMD_TBV_Lobes_CoarseAnxTd <- sapply(GMD_TBV_CoarseAnxVsTd_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_Lobes_CoarseAnxTd_fdr <- p.adjust(GMD_TBV_Lobes_CoarseAnxTd,method="fdr")

##ROIs###
#GAM model
GMD_TBV_CoarseAnxVsTd_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + CoarseAnxTd, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_CoarseAnxVsTd_ROIs, summary)

#Create a vector p-values
GMD_TBV_ROIs_CoarseAnxTd <- sapply(GMD_TBV_CoarseAnxVsTd_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_ROIs_CoarseAnxTd_fdr <- p.adjust(GMD_TBV_ROIs_CoarseAnxTd,method="fdr")



#############################
#### STATE TRAIT ANXIETY ####
#############################

###GM###
#GAM model
GMD_TBV_AllAnxTd_Stai_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Stai_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllAnxTd_Gm_State <- sapply(GMD_TBV_AllAnxTd_Stai_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Gm_Trait <- sapply(GMD_TBV_AllAnxTd_Stai_Gm, function(v) summary(v)$p.table[5,4])


###LOBES###
#GAM model
GMD_TBV_AllAnxTd_Stai_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Stai_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllAnxTd_Lobes_State <- sapply(GMD_TBV_AllAnxTd_Stai_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Lobes_Trait <- sapply(GMD_TBV_AllAnxTd_Stai_Lobes, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
GMD_TBV_AllAnxTd_Lobes_State_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_State,method="fdr")
GMD_TBV_AllAnxTd_Lobes_Trait_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_Trait,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllAnxTd_Stai_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Stai_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllAnxTd_ROIs_State <- sapply(GMD_TBV_AllAnxTd_Stai_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_ROIs_Trait <- sapply(GMD_TBV_AllAnxTd_Stai_ROIs, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
GMD_TBV_AllAnxTd_ROIs_State_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_State,method="fdr")
GMD_TBV_AllAnxTd_ROIs_Trait_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_Trait,method="fdr")



############################
#### TRAIT ANXIETY ONLY ####
############################

###GM###
#GAM model
GMD_TBV_AllAnxTd_Trait_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Trait_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllAnxTd_Gm_TraitOnly <- sapply(GMD_TBV_AllAnxTd_Trait_Gm, function(v) summary(v)$p.table[4,4])


###LOBES###
#GAM model
GMD_TBV_AllAnxTd_Trait_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Trait_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllAnxTd_Lobes_TraitOnly <- sapply(GMD_TBV_AllAnxTd_Trait_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllAnxTd_Lobes_TraitOnly_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_TraitOnly,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllAnxTd_Trait_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Trait_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllAnxTd_ROIs_TraitOnly <- sapply(GMD_TBV_AllAnxTd_Trait_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllAnxTd_ROIs_TraitOnly_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_TraitOnly,method="fdr")



##############################################
#### CORRELATED TRAITS- NOT AGE REGRESSED ####
##############################################

###GM###
#GAM model
GMD_TBV_AllAnxTd_CorrTrMood_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrPsych_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrExt_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrFear_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_CorrTrMood_Gm, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrPsych_Gm, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrExt_Gm, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrFear_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllAnxTd_Gm_CorrTrMood <- sapply(GMD_TBV_AllAnxTd_CorrTrMood_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Gm_CorrTrPsych <- sapply(GMD_TBV_AllAnxTd_CorrTrPsych_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Gm_CorrTrExt <- sapply(GMD_TBV_AllAnxTd_CorrTrExt_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Gm_CorrTrFear <- sapply(GMD_TBV_AllAnxTd_CorrTrFear_Gm, function(v) summary(v)$p.table[4,4])


###LOBES###
#GAM model
GMD_TBV_AllAnxTd_CorrTrMood_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrPsych_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrExt_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrFear_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_CorrTrMood_Lobes, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrPsych_Lobes, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrExt_Lobes, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrFear_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllAnxTd_Lobes_CorrTrMood <- sapply(GMD_TBV_AllAnxTd_CorrTrMood_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Lobes_CorrTrPsych <- sapply(GMD_TBV_AllAnxTd_CorrTrPsych_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Lobes_CorrTrExt <- sapply(GMD_TBV_AllAnxTd_CorrTrExt_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Lobes_CorrTrFear <- sapply(GMD_TBV_AllAnxTd_CorrTrFear_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllAnxTd_Lobes_CorrTrMood_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_CorrTrMood,method="fdr")
GMD_TBV_AllAnxTd_Lobes_CorrTrPsych_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_CorrTrPsych,method="fdr")
GMD_TBV_AllAnxTd_Lobes_CorrTrExt_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_CorrTrExt,method="fdr")
GMD_TBV_AllAnxTd_Lobes_CorrTrFear_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_CorrTrFear,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllAnxTd_CorrTrMood_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrPsych_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrExt_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = AllAnxTdSubjData)
})

GMD_TBV_AllAnxTd_CorrTrFear_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = AllAnxTdSubjData)
})


#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_CorrTrMood_ROIs, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrPsych_ROIs, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrExt_ROIs, summary)
lapply(GMD_TBV_AllAnxTd_CorrTrFear_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllAnxTd_ROIs_CorrTrMood <- sapply(GMD_TBV_AllAnxTd_CorrTrMood_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_ROIs_CorrTrPsych <- sapply(GMD_TBV_AllAnxTd_CorrTrPsych_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_ROIs_CorrTrExt <- sapply(GMD_TBV_AllAnxTd_CorrTrExt_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_ROIs_CorrTrFear <- sapply(GMD_TBV_AllAnxTd_CorrTrFear_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllAnxTd_ROIs_CorrTrMood_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_CorrTrMood,method="fdr")
GMD_TBV_AllAnxTd_ROIs_CorrTrPsych_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_CorrTrPsych,method="fdr")
GMD_TBV_AllAnxTd_ROIs_CorrTrExt_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_CorrTrExt,method="fdr")
GMD_TBV_AllAnxTd_ROIs_CorrTrFear_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_CorrTrFear,method="fdr")



###################################
#### PSYCHOPATHOLOGY BIFACTORS ####
###################################

###GM###
#GAM model
GMD_TBV_AllAnxTd_Bifactors_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
    +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Bifactors_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllAnxTd_Gm_Mood <- sapply(GMD_TBV_AllAnxTd_Bifactors_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Gm_Psych <- sapply(GMD_TBV_AllAnxTd_Bifactors_Gm, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllAnxTd_Gm_Ext <- sapply(GMD_TBV_AllAnxTd_Bifactors_Gm, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllAnxTd_Gm_Phb <- sapply(GMD_TBV_AllAnxTd_Bifactors_Gm, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllAnxTd_Gm_OverallPsy <- sapply(GMD_TBV_AllAnxTd_Bifactors_Gm, function(v) summary(v)$p.table[8,4])


###LOBES###
#GAM model
GMD_TBV_AllAnxTd_Bifactors_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Bifactors_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllAnxTd_Lobes_Mood <- sapply(GMD_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_Lobes_Psych <- sapply(GMD_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllAnxTd_Lobes_Ext <- sapply(GMD_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllAnxTd_Lobes_Phb <- sapply(GMD_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllAnxTd_Lobes_OverallPsy <- sapply(GMD_TBV_AllAnxTd_Bifactors_Lobes, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
GMD_TBV_AllAnxTd_Lobes_Mood_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_Mood,method="fdr")
GMD_TBV_AllAnxTd_Lobes_Psych_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_Psych,method="fdr")
GMD_TBV_AllAnxTd_Lobes_Ext_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_Ext,method="fdr")
GMD_TBV_AllAnxTd_Lobes_Phb_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_Phb,method="fdr")
GMD_TBV_AllAnxTd_Lobes_OverallPsy_fdr <- p.adjust(GMD_TBV_AllAnxTd_Lobes_OverallPsy,method="fdr")


##ROIs###
#GAM model
GMD_TBV_AllAnxTd_Bifactors_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxTdSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllAnxTd_Bifactors_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllAnxTd_ROIs_Mood <- sapply(GMD_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllAnxTd_ROIs_Psych <- sapply(GMD_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllAnxTd_ROIs_Ext <- sapply(GMD_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllAnxTd_ROIs_Phb <- sapply(GMD_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllAnxTd_ROIs_OverallPsy <- sapply(GMD_TBV_AllAnxTd_Bifactors_ROIs, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
GMD_TBV_AllAnxTd_ROIs_Mood_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_Mood,method="fdr")
GMD_TBV_AllAnxTd_ROIs_Psych_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_Psych,method="fdr")
GMD_TBV_AllAnxTd_ROIs_Ext_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_Ext,method="fdr")
GMD_TBV_AllAnxTd_ROIs_Phb_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_Phb,method="fdr")
GMD_TBV_AllAnxTd_ROIs_OverallPsy_fdr <- p.adjust(GMD_TBV_AllAnxTd_ROIs_OverallPsy,method="fdr")



#############################################
#############################################
### GRAY MATTER DENSITY ANALYSES WITH TBV ###
######### Full sample (n=1601) ##############
#############################################
#############################################
#GMD data: gray matter density data from JLF: i.e., mprage_jlf_gmd_*

#############################
#### STATE TRAIT ANXIETY ####
#############################

###GM###
#GAM model
GMD_TBV_AllSubj_Stai_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Stai_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllSubj_Gm_State <- sapply(GMD_TBV_AllSubj_Stai_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Gm_Trait <- sapply(GMD_TBV_AllSubj_Stai_Gm, function(v) summary(v)$p.table[5,4])


###LOBES###
#GAM model
GMD_TBV_AllSubj_Stai_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Stai_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllSubj_Lobes_State <- sapply(GMD_TBV_AllSubj_Stai_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Lobes_Trait <- sapply(GMD_TBV_AllSubj_Stai_Lobes, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
GMD_TBV_AllSubj_Lobes_State_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_State,method="fdr")
GMD_TBV_AllSubj_Lobes_Trait_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_Trait,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllSubj_Stai_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + State_General + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Stai_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllSubj_ROIs_State <- sapply(GMD_TBV_AllSubj_Stai_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_ROIs_Trait <- sapply(GMD_TBV_AllSubj_Stai_ROIs, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
GMD_TBV_AllSubj_ROIs_State_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_State,method="fdr")
GMD_TBV_AllSubj_ROIs_Trait_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_Trait,method="fdr")



############################
#### TRAIT ANXIETY ONLY ####
############################

###GM###
#GAM model
GMD_TBV_AllSubj_Trait_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Trait_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllSubj_Gm_TraitOnly <- sapply(GMD_TBV_AllSubj_Trait_Gm, function(v) summary(v)$p.table[4,4])


###LOBES###
#GAM model
GMD_TBV_AllSubj_Trait_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Trait_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllSubj_Lobes_TraitOnly <- sapply(GMD_TBV_AllSubj_Trait_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllSubj_Lobes_TraitOnly_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_TraitOnly,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllSubj_Trait_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Trait_General, list(i = as.name(x))), data = staiSubjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Trait_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllSubj_ROIs_TraitOnly <- sapply(GMD_TBV_AllSubj_Trait_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllSubj_ROIs_TraitOnly_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_TraitOnly,method="fdr")



##############################################
#### CORRELATED TRAITS- NOT AGE REGRESSED ####
##############################################

###GM###
#GAM model
GMD_TBV_AllSubj_CorrTrMood_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrPsych_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrExt_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrFear_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_CorrTrMood_Gm, summary)
lapply(GMD_TBV_AllSubj_CorrTrPsych_Gm, summary)
lapply(GMD_TBV_AllSubj_CorrTrExt_Gm, summary)
lapply(GMD_TBV_AllSubj_CorrTrFear_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllSubj_Gm_CorrTrMood <- sapply(GMD_TBV_AllSubj_CorrTrMood_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Gm_CorrTrPsych <- sapply(GMD_TBV_AllSubj_CorrTrPsych_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Gm_CorrTrExt <- sapply(GMD_TBV_AllSubj_CorrTrExt_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Gm_CorrTrFear <- sapply(GMD_TBV_AllSubj_CorrTrFear_Gm, function(v) summary(v)$p.table[4,4])


###LOBES###
#GAM model
GMD_TBV_AllSubj_CorrTrMood_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrPsych_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrExt_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrFear_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(GMD_TBV_AllSubj_CorrTrMood_Lobes, summary)
lapply(GMD_TBV_AllSubj_CorrTrPsych_Lobes, summary)
lapply(GMD_TBV_AllSubj_CorrTrExt_Lobes, summary)
lapply(GMD_TBV_AllSubj_CorrTrFear_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllSubj_Lobes_CorrTrMood <- sapply(GMD_TBV_AllSubj_CorrTrMood_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Lobes_CorrTrPsych <- sapply(GMD_TBV_AllSubj_CorrTrPsych_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Lobes_CorrTrExt <- sapply(GMD_TBV_AllSubj_CorrTrExt_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Lobes_CorrTrFear <- sapply(GMD_TBV_AllSubj_CorrTrFear_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllSubj_Lobes_CorrTrMood_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_CorrTrMood,method="fdr")
GMD_TBV_AllSubj_Lobes_CorrTrPsych_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_CorrTrPsych,method="fdr")
GMD_TBV_AllSubj_Lobes_CorrTrExt_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_CorrTrExt,method="fdr")
GMD_TBV_AllSubj_Lobes_CorrTrFear_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_CorrTrFear,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllSubj_CorrTrMood_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Mood, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrPsych_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Psychosis, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrExt_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Externalizing, list(i = as.name(x))), data = subjData)
})

GMD_TBV_AllSubj_CorrTrFear_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + Fear, list(i = as.name(x))), data = subjData)
})


#Look at the model summaries
lapply(GMD_TBV_AllSubj_CorrTrMood_ROIs, summary)
lapply(GMD_TBV_AllSubj_CorrTrPsych_ROIs, summary)
lapply(GMD_TBV_AllSubj_CorrTrExt_ROIs, summary)
lapply(GMD_TBV_AllSubj_CorrTrFear_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllSubj_ROIs_CorrTrMood <- sapply(GMD_TBV_AllSubj_CorrTrMood_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_ROIs_CorrTrPsych <- sapply(GMD_TBV_AllSubj_CorrTrPsych_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_ROIs_CorrTrExt <- sapply(GMD_TBV_AllSubj_CorrTrExt_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_ROIs_CorrTrFear <- sapply(GMD_TBV_AllSubj_CorrTrFear_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
GMD_TBV_AllSubj_ROIs_CorrTrMood_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_CorrTrMood,method="fdr")
GMD_TBV_AllSubj_ROIs_CorrTrPsych_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_CorrTrPsych,method="fdr")
GMD_TBV_AllSubj_ROIs_CorrTrExt_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_CorrTrExt,method="fdr")
GMD_TBV_AllSubj_ROIs_CorrTrFear_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_CorrTrFear,method="fdr")



###################################
#### PSYCHOPATHOLOGY BIFACTORS ####
###################################

###GM###
#GAM model
GMD_TBV_AllSubj_Bifactors_Gm <- lapply(GMD_Gm_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Bifactors_Gm, summary)

#Create a vector p-values (fdr correction not used because only one DV was tested)
GMD_TBV_AllSubj_Gm_Mood <- sapply(GMD_TBV_AllSubj_Bifactors_Gm, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Gm_Psych <- sapply(GMD_TBV_AllSubj_Bifactors_Gm, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllSubj_Gm_Ext <- sapply(GMD_TBV_AllSubj_Bifactors_Gm, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllSubj_Gm_Phb <- sapply(GMD_TBV_AllSubj_Bifactors_Gm, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllSubj_Gm_OverallPsy <- sapply(GMD_TBV_AllSubj_Bifactors_Gm, function(v) summary(v)$p.table[8,4])


###LOBES###
#GAM model
GMD_TBV_AllSubj_Bifactors_Lobes <- lapply(GMD_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Bifactors_Lobes, summary)

#Create a vector p-values
GMD_TBV_AllSubj_Lobes_Mood <- sapply(GMD_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_Lobes_Psych <- sapply(GMD_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllSubj_Lobes_Ext <- sapply(GMD_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllSubj_Lobes_Phb <- sapply(GMD_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllSubj_Lobes_OverallPsy <- sapply(GMD_TBV_AllSubj_Bifactors_Lobes, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
GMD_TBV_AllSubj_Lobes_Mood_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_Mood,method="fdr")
GMD_TBV_AllSubj_Lobes_Psych_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_Psych,method="fdr")
GMD_TBV_AllSubj_Lobes_Ext_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_Ext,method="fdr")
GMD_TBV_AllSubj_Lobes_Phb_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_Phb,method="fdr")
GMD_TBV_AllSubj_Lobes_OverallPsy_fdr <- p.adjust(GMD_TBV_AllSubj_Lobes_OverallPsy,method="fdr")


###ROIs###
#GAM model
GMD_TBV_AllSubj_Bifactors_ROIs <- lapply(GMD_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = subjData)
})

#Look at the model summaries
lapply(GMD_TBV_AllSubj_Bifactors_ROIs, summary)

#Create a vector p-values
GMD_TBV_AllSubj_ROIs_Mood <- sapply(GMD_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[4,4])
GMD_TBV_AllSubj_ROIs_Psych <- sapply(GMD_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[5,4])
GMD_TBV_AllSubj_ROIs_Ext <- sapply(GMD_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[6,4])
GMD_TBV_AllSubj_ROIs_Phb <- sapply(GMD_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[7,4])
GMD_TBV_AllSubj_ROIs_OverallPsy <- sapply(GMD_TBV_AllSubj_Bifactors_ROIs, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
GMD_TBV_AllSubj_ROIs_Mood_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_Mood,method="fdr")
GMD_TBV_AllSubj_ROIs_Psych_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_Psych,method="fdr")
GMD_TBV_AllSubj_ROIs_Ext_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_Ext,method="fdr")
GMD_TBV_AllSubj_ROIs_Phb_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_Phb,method="fdr")
GMD_TBV_AllSubj_ROIs_OverallPsy_fdr <- p.adjust(GMD_TBV_AllSubj_ROIs_OverallPsy,method="fdr")



