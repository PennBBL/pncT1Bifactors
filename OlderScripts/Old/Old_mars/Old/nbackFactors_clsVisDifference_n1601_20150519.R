####PARAMETERS
subjDataName<-"/import/speedy/eons/progs/frac2back/shanmuganBifactorPaper/subjectData/nbackFactors_n1601_subjData_20150506.rds"
tsName<-"/import/speedy/eons/group_results_n1601/frac2back/voxelwise_analyses/n1129_f2b.cope.paths_ACROSS.INCLUDE/fourd.nii.gz"
maskName<-"/import/speedy/eons/group_results_n1601/frac2back/voxelwise_analyses/n1129_f2b.cope.paths_ACROSS.INCLUDE/mask.nii.gz"
subjName<-"/import/speedy/eons/group_results_n1601/frac2back/voxelwise_analyses/n1129_f2b.cope.paths_ACROSS.INCLUDE/bblids.txt"
yLab<-"2B - 0B activation (a.u.)"
mdlIn<-' y ~ AnxiousMisery + Psychosis + Behavior + Overall + sex  + age + nback_epi10qa_meanrelrms'  #linear model to evaluate within the cluster
clsName<-'/import/speedy/eons/group_results_n1601/frac2back/voxelwise_analyses/n1129_f2b.cope.paths_ACROSS.INCLUDE/mainEffect_fTests_dif/easythresh/cluster_mask_zfstat1_309_1.nii.gz'
clsVal<-56
outPdf<-'/import/speedy/eons/group_results_n1601/frac2back/voxelwise_analyses/n1129_f2b.cope.paths_ACROSS.INCLUDE/mainEffect_fTests_dif/easythresh/cluster_mask_zfstat1_309_cl56_acc.pdf'
####

####libraries
library(ANTsR)
library(visreg)
library(reshape2)

####Load image data
ts<-antsImageRead(tsName,4)
mask<-antsImageRead(maskName,3)

###Reshape
tsMat<-timeseries2matrix(ts,mask)

#Read in cluster
cls<-antsImageRead(clsName,3)
clsMat <- matrix(nrow =1, ncol = length(which(mask>0)))
clsMat[1,]<- as.numeric(cls[mask>0])

###Get mean activation within the cluster
y<-rowMeans(tsMat[,which(clsMat==clsVal)])

##Load and subset subject data
subjDataIn<-readRDS(subjDataName)
subj<-read.table(subjName)
subjData<-subjDataIn[subjDataIn$bblid %in% subj$V1,]
subjData<-subjData[order(subjData$bblid),]

#combine y and subject data
subjData<-cbind(subjData,y)

#get reduced datframe for melt
subjDataRed<-subjData[,c("bblid","age","sex","nback_epi10qa_meanrelrms","y","mood_4factor","psychosis_4factor","externalizing_4factor","overall_psychopathology_4factor")]

#reshape to long format
dataLong<-melt(subjDataRed,id.vars=c("bblid","sex","age","nback_epi10qa_meanrelrms","y"),variable.name="symptomDomain",value.name="symptomScore")


mdl<-lm(y~symptomDomain*symptomScore+sex+age+nback_epi10qa_meanrelrms,data=dataLong)


colPal4<-c("#0000FF","#4B0082","#DC143C","#006400")
#colPal3<-c("#4B0082","#0000FF","#006400")
#DC143C75 --red-- mood
#4B0082-- purple-- psychosis
#0000FF -blue-- behavior
#006400 -- green-- overall

#808080 -- gray


anova(mdl)

pdf(outPdf)

par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.1, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")

visreg(mdl,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,line.par=list(col=colPal4),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="2B - 0B Activation (a.u.)",legend=FALSE)


dev.off()

