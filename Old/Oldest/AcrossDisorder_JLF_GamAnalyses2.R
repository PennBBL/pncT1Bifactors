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
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + StaiPreStateBifactorGScore + StaiPreTraitBifactorGScore, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Stai_GmWmCsf, summary)

#Create a vector p-values
Vol_TBV_AllAnx_GmWmCsf_State<-sapply(Vol_TBV_AllAnx_Stai_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_Trait<-sapply(Vol_TBV_AllAnx_Stai_GmWmCsf, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnx_GmWmCsf_State_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_State,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_Trait_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_Trait,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnx_Stai_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + StaiPreStateBifactorGScore + StaiPreTraitBifactorGScore, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Stai_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnx_Lobes_State<-sapply(Vol_TBV_AllAnx_Stai_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_Trait<-sapply(Vol_TBV_AllAnx_Stai_Lobes, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnx_Lobes_State_fdr<-p.adjust(Vol_TBV_AllAnx_Lobes_State,method="fdr")
Vol_TBV_AllAnx_Lobes_Trait_fdr<-p.adjust(Vol_TBV_AllAnx_Lobes_Trait,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnx_Stai_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + StaiPreStateBifactorGScore + StaiPreTraitBifactorGScore, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Stai_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnx_ROIs_State<-sapply(Vol_TBV_AllAnx_Stai_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_Trait<-sapply(Vol_TBV_AllAnx_Stai_ROIs, function(v) summary(v)$p.table[5,4])

#FDR correct p-values
Vol_TBV_AllAnx_ROIs_State_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_State,method="fdr")
Vol_TBV_AllAnx_ROIs_Trait_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_Trait,method="fdr")



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
Vol_TBV_AllAnx_GmWmCsf_CorrTrMood<-sapply(Vol_TBV_AllAnx_CorrTrMood_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_CorrTrPsych<-sapply(Vol_TBV_AllAnx_CorrTrPsych_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_CorrTrExt<-sapply(Vol_TBV_AllAnx_CorrTrExt_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_CorrTrFear<-sapply(Vol_TBV_AllAnx_CorrTrFear_GmWmCsf, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_GmWmCsf_CorrTrMood_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_CorrTrMood,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_CorrTrPsych_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_CorrTrPsych,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_CorrTrExt_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_CorrTrExt,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_CorrTrFear_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_CorrTrFear,method="fdr")


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
Vol_TBV_AllAnx_Lobes_CorrTrMood<-sapply(Vol_TBV_AllAnx_CorrTrMood_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_CorrTrPsych<-sapply(Vol_TBV_AllAnx_CorrTrPsych_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_CorrTrExt<-sapply(Vol_TBV_AllAnx_CorrTrExt_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_CorrTrFear<-sapply(Vol_TBV_AllAnx_CorrTrFear_Lobes, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_Lobes_CorrTrMood_fdr<-p.adjust(Vol_TBV_AllAnx_Lobes_CorrTrMood,method="fdr")
Vol_TBV_AllAnx_Lobes_CorrTrPsych_fdr<-p.adjust(Vol_TBV_AllAnx_Lobes_CorrTrPsych,method="fdr")
Vol_TBV_AllAnx_Lobes_CorrTrExt_fdr<-p.adjust(Vol_TBV_AllAnx_Lobes_CorrTrExt,method="fdr")
Vol_TBV_AllAnx_Lobes_CorrTrFear_fdr<-p.adjust(Vol_TBV_AllAnx_Lobes_CorrTrFear,method="fdr")


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
Vol_TBV_AllAnx_ROIs_CorrTrMood<-sapply(Vol_TBV_AllAnx_CorrTrMood_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_CorrTrPsych<-sapply(Vol_TBV_AllAnx_CorrTrPsych_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_CorrTrExt<-sapply(Vol_TBV_AllAnx_CorrTrExt_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_CorrTrFear<-sapply(Vol_TBV_AllAnx_CorrTrFear_ROIs, function(v) summary(v)$p.table[4,4])

#FDR correct p-values
Vol_TBV_AllAnx_ROIs_CorrTrMood_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_CorrTrMood,method="fdr")
Vol_TBV_AllAnx_ROIs_CorrTrPsych_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_CorrTrPsych,method="fdr")
Vol_TBV_AllAnx_ROIs_CorrTrExt_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_CorrTrExt,method="fdr")
Vol_TBV_AllAnx_ROIs_CorrTrFear_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_CorrTrFear,method="fdr")



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
Vol_TBV_AllAnx_GmWmCsf_Mood<-sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_GmWmCsf_Psych<-sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnx_GmWmCsf_Ext<-sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnx_GmWmCsf_Phb<-sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnx_GmWmCsf_OverallPsy<-sapply(Vol_TBV_AllAnx_Bifactors_GmWmCsf, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllAnx_GmWmCsf_Mood_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_Mood,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_Psych_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_Psych,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_Ext_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_Ext,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_Phb_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_Phb,method="fdr")
Vol_TBV_AllAnx_GmWmCsf_OverallPsy_fdr<-p.adjust(Vol_TBV_AllAnx_GmWmCsf_OverallPsy,method="fdr")


###LOBES###
#GAM model
Vol_TBV_AllAnx_Bifactors_Lobes <- lapply(Vol_Lobe_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Bifactors_Lobes, summary)

#Create a vector p-values
Vol_TBV_AllAnx_Lobes_Mood<-sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_Lobes_Psych<-sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnx_Lobes_Ext<-sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnx_Lobes_Phb<-sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnx_Lobes_OverallPsy<-sapply(Vol_TBV_AllAnx_Bifactors_Lobes, function(v) summary(v)$p.table[8,4])

#FDR correct p-values
Vol_TBV_AllAnx_Lobes_Mood_fdr<-p.adjust(LobeMood_vol,method="fdr")
Vol_TBV_AllAnx_Lobes_Psych_fdr<-p.adjust(LobePsych_vol,method="fdr")
Vol_TBV_AllAnx_Lobes_Ext_fdr<-p.adjust(LobeExt_vol,method="fdr")
Vol_TBV_AllAnx_Lobes_Phb_fdr<-p.adjust(LobePhb_vol,method="fdr")
Vol_TBV_AllAnx_Lobes_OverallPsy_fdr<-p.adjust(LobeOverallPsy_vol,method="fdr")


###ROIs###
#GAM model
Vol_TBV_AllAnx_Bifactors_ROIs <- lapply(Vol_ROI_List, function(x) {
    gam(substitute(i ~ s(age) + sex + mprage_antsCT_vol_TBV + goassessItemBifactor4FactorMood +  goassessItemBifactor4FactorPsych +  goassessItemBifactor4FactorExt
        +  goassessItemBifactor4FactorPhb +  goassessItemBifactor4FactorOverallPsy, list(i = as.name(x))), data = AllAnxSubjData)
})

#Look at the model summaries
lapply(Vol_TBV_AllAnx_Bifactors_ROIs, summary)

#Create a vector p-values
Vol_TBV_AllAnx_ROIs_Mood<-sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[4,4])
Vol_TBV_AllAnx_ROIs_Psych<-sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[5,4])
Vol_TBV_AllAnx_ROIs_Ext<-sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[6,4])
Vol_TBV_AllAnx_ROIs_Phb<-sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[7,4])
Vol_TBV_AllAnx_ROIs_OverallPsy<-sapply(Vol_TBV_AllAnx_Bifactors_ROIs, function(v) summary(v)$p.table[8,4])

#FDR correct p-values 
Vol_TBV_AllAnx_ROIs_Mood_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_Mood,method="fdr")
Vol_TBV_AllAnx_ROIs_Psych_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_Psych,method="fdr")
Vol_TBV_AllAnx_ROIs_Ext_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_Ext,method="fdr")
Vol_TBV_AllAnx_ROIs_Phb_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_Phb,method="fdr")
Vol_TBV_AllAnx_ROIs_OverallPsy_fdr<-p.adjust(Vol_TBV_AllAnx_ROIs_OverallPsy,method="fdr")




