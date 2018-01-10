##Read in subject data
subjData<-readRDS("/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_ravens.rds")

##Run linear models
#Left insula (Freesurfer)
Model1<-lm(mprage_fs_vol_lh_insula~ age + sex + ageSq + white + meduCnbGo1 + mprageMassICV + averageRating + goassessItemBifactor4FactorMood + goassessItemBifactor4FactorPsych + goassessItemBifactor4FactorExt + goassessItemBifactor4FactorPhb + goassessItemBifactor4FactorOverallPsy, data=subjData)

#Left insula (MARS)
Model2<-lm(mprage_mars_vol_L_Ains~ age + sex + ageSq + white + meduCnbGo1 + mprageMassICV + averageRating + goassessItemBifactor4FactorMood + goassessItemBifactor4FactorPsych + goassessItemBifactor4FactorExt + goassessItemBifactor4FactorPhb + goassessItemBifactor4FactorOverallPsy, data=subjData)


##Make sex an ordered variable (required for GAM)
subjData$sex<-as.ordered(subjData$sex)

##Load libraries
library(ANTsR)
library(mgcv)

##Run GAMs
#Left insula (Freesurfer)
Model3<-gam(mprage_fs_vol_lh_insula~ s(age) + sex + white + meduCnbGo1 + mprageMassICV + averageRating + goassessItemBifactor4FactorMood + goassessItemBifactor4FactorPsych + goassessItemBifactor4FactorExt + goassessItemBifactor4FactorPhb + goassessItemBifactor4FactorOverallPsy, data=subjData)

#Left insula (MARS)
Model4<-gam(mprage_mars_vol_L_Ains~ s(age) + sex + white + meduCnbGo1 + mprageMassICV + averageRating + goassessItemBifactor4FactorMood + goassessItemBifactor4FactorPsych + goassessItemBifactor4FactorExt + goassessItemBifactor4FactorPhb + goassessItemBifactor4FactorOverallPsy, data=subjData)
