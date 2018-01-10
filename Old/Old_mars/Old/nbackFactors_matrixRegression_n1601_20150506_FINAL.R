library(ANTsR)
library(vegan)

##load in data
subjData<-readRDS("/import/speedy/eons/progs/frac2back/shanmuganBifactorPaper/subjectData/nbackFactors_n1601_subjData_20150506.rds")
ts<- antsImageRead("/import/speedy/eons/group_results_n1601/frac2back/voxelwise_analyses/n1129_f2b.cope.paths_ACROSS.INCLUDE/fourd_4mm.nii.gz",4)
bblids<-read.table("/import/speedy/eons/group_results_n1601/frac2back/voxelwise_analyses/n1129_f2b.cope.paths_ACROSS.INCLUDE/bblids.txt",header=FALSE)
mask<-antsImageRead("/import/speedy/eons/group_results_n1601/frac2back/voxelwise_analyses/n1129_f2b.cope.paths_ACROSS.INCLUDE/mask_4mm.nii.gz",3)
tsMat<-timeseries2matrix(ts,mask)

names(bblids)[1]<-"bblid"
subjData$race<-as.factor(subjData$race)
subjData$sex<-as.factor(subjData$sex)

dataSubset<-subjData[ subjData$bblid %in% bblids$bblid,]
dataSubset<-dataSubset[order(dataSubset$bblid),]

##calcualte distances
fracdistEuc <- vegdist(tsMat, method='euclidean')

fracModelReducedOEM<-adonis(fracdistEuc~age_at_go1_scan+ nback_epi10qa_meanrelrms + sex + overall_psychopathology_4factor + externalizing_4factor + mood_4factor,data=dataSubset)
#overall_psychopathology_4factor  0.009 **
#externalizing_4factor            0.044 *
#mood_4factor                     0.092 .

fracModelMoodFull<-adonis(fracdistEuc~age_at_go1_scan+ nback_epi10qa_meanrelrms + sex + psychosis_4factor + phobias_4factor + overall_psychopathology_4factor + externalizing_4factor + mood_4factor,data=dataSubset)
#age_at_go1_scan                  0.001 ***
#nback_epi10qa_meanrelrms         0.001 ***
#sex                              0.009 **
#psychosis_4factor                0.252
#phobias_4factor                  0.127
#overall_psychopathology_4factor  0.015 *
#externalizing_4factor            0.041 *
#mood_4factor                     0.084 .

save.image("/import/speedy/eons/progs/frac2back/shanmuganBifactorPaper/workspaces/nbackFactors_matrixRegression_n1601_20150506_FINAL.Rdata")

