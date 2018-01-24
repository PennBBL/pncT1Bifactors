###NOTES THAT **ASSUMES HAVE ALREADY RUN A LINEAR MODEL ALREADY (USUALLY A GOOD IDEA)***
###THUS ASSUMES THAT YOU HAVE IN DIRECTORY WHERE RAN THE LINEAR MODEL:
#1) A 4 merged file of images
#2) A mask image
#3) An .rds with subject data that will go in your model (in a different directory)


### VARIABLES ###
#################
subjDataName<-"/import/monstrum/Users/antoniak/PNC_asl/subject_data/pncSubjectData_dataRelease_20150730.rds"  #this is the RDS for your subject data
outDirRoot<-"/import/monstrum/Users/antoniak/PNC_asl/voxelwise_analyses/n875_cbf.paths.ants/"  #this is where it will find data from linear models
outName<-"age_sex_aslMotion_gmdCovariate_State_General_12_Trait_General_12" #what you want your ouput directory to be named-- include covariates in name-- here w/ GMD
gmdImagePath<-"/import/monstrum/Users/antoniak/PNC_asl/gmd_covariate/n875_gmdImages_downsampled.nii.gz" #full path to merged 4d GMD images

###FIND PATHS TO FILES### 
#########################
#assumes naming from flameo
aslImageName<-paste(outDirRoot,"fourd.nii.gz",sep="/")
imageIds<-paste(outDirRoot,"bblids.txt",sep="/")
maskName<-paste(outDirRoot,"mask.nii.gz",sep="/")


###LOAD SUBJECT DATA###
#######################
subjDataIn<-readRDS(subjDataName)
subjs<-read.table(imageIds)
subjDataSubset<-subjDataIn[subjDataIn$bblid %in% subjs$V1,]  #subsetting the subject data by those in the linear voxelwise analysis

#check orderring of subjects
subjData<-subjDataSubset[order(subjDataSubset$bblid),]  #assumes that images are ordered by ID

#check versus subject list
if(!identical(subjs$V1,subjData$bblid)){
	print("subjectIds not matched-- ERROR!!!")
	stop()
}

###RENAME CERTAIN VARIABLES###
##############################
subjData$ageSq<-I(scale(subjData$age, scale=FALSE, center=TRUE)^2)
subjData$aslMotion<-subjData$aslEpi10qaMeanrelrms

###MAKE OUTPUT DIRECTORIES####
##############################
outDir<-paste(outDirRoot,outName,sep="/")
logDir<-paste(outDirRoot,"logs",sep="/")
dir.create(outDir, showWarnings = FALSE)
dir.create(logDir, showWarnings = FALSE)

###ECHO ARGUMENTS###
####################
print("Arguments are:")
print(paste("subject data is:",subjDataName))
print(paste("input images are:",aslImageName))
print(paste("mask is:",maskName))
print(paste("ouput directory is:",outDir))
print(paste("log directory is:",logDir))


###cleanup logdir
system(paste('rm -f', file.path(logDir, '*')))


### LIBRARIES ###
#################
library(ANTsR)  #for ANTS -- critical for loading images
library(mgcv) #for nonlinear analyses

###LOAD DATA###
##############
#antsR webpage is helpful for this
mask<-antsImageRead(maskName,3) #3=3d
aslImageIn<-antsImageRead(aslImageName,4) #4=4d (bc this is the 4d file of asl images)
aslImageMat<-timeseries2matrix(aslImageIn,mask) #reshape to a matrix

gmdImageIn<-antsImageRead(gmdImagePath,4) #4=4d
gmdImageMat<-timeseries2matrix(gmdImageIn,mask) #reshape to a matrix

# The GMD 4d file needs to have the same order of subjects, same image dimensions (i.e. 2mm isotropic), and same space (e.g. MNI) as the asl 4d file
# can use mask you specfied for *ASL* images-- do not want to use a different mask as that will mess up image dimensions
# outcome of this is that gmImageMat is *same dimensions* as aslImageMat


###PREALLOCATE OUTPUT###
########################
pOutState<-matrix(NA,nrow=dim(aslImageMat)[2],ncol=1)
tOutState<-matrix(NA,nrow=dim(aslImageMat)[2],ncol=1) 

pOutTrait<-matrix(NA,nrow=dim(aslImageMat)[2],ncol=1)
tOutTrait<-matrix(NA,nrow=dim(aslImageMat)[2],ncol=1)

pOutGmd<-matrix(NA,nrow=dim(aslImageMat)[2],ncol=1)
tOutGmd<-matrix(NA,nrow=dim(aslImageMat)[2],ncol=1)

pOutMotion<-matrix(NA,nrow=dim(aslImageMat)[2],ncol=1)
tOutMotion<-matrix(NA,nrow=dim(aslImageMat)[2],ncol=1)


###LOOP THROUGH MODELS###
########################
timeOn<-proc.time() # keeps track of time. 

for(i in 1:dim(aslImageMat)[2]){
	print(i)
	#Run first without the pOut and tOut commands (don't need to let it finish all subjects- let it run for about a minute and the use CNTRL-C to stop)
	#Then look at lmSummary to get the correct p and t column/row numbers from the coefficent table for the pOut and tOut commands (check with lmSummary$coefficients[?,?])
	
	#Add gmd covariate and quadratic term
	gmd<-gmdImageMat[,i]
	
	#Specify model below (don't need to specify main effects if have interaction already in the model)
	lmSummary<-summary(lm(aslImageMat[,i]~age*sex + ageSq*sex + aslMotion + gmd + State_General_12 + Trait_General_12, data=subjData))

	###NOTE!!!: anytime you adjust your model order, variables, or syntax-- will need to update where in coefficient table you are pulling
	pOutState[i,1]<-lmSummary$coefficients[7,4] #pval 
        tOutState[i,1]<-lmSummary$coefficients[7,3] #tval
	
	pOutTrait[i,1]<-lmSummary$coefficients[8,4] #pval
        tOutTrait[i,1]<-lmSummary$coefficients[8,3] #tval

	##GMD relationship w/ asl-- should be strongly related
	pOutGmd[i,1]<-lmSummary$coefficients[6,4] #pval
        tOutGmd[i,1]<-lmSummary$coefficients[6,3] #tval

	##Motion relationship w/ asl
        pOutMotion[i,1]<-lmSummary$coefficients[5,4] #pval
        tOutMotion[i,1]<-lmSummary$coefficients[5,3] #tval

}
	
loopTime<-proc.time()-timeOn


###WRITE OUT IMAGE###
####################

pOutStateImage<-antsImageClone(mask)
tOutStateImage<-antsImageClone(mask)
pOutStateImage[mask==1]<-pOutState
tOutStateImage[mask==1]<-tOutState

setwd(outDir)
antsImageWrite(pOutStateImage,"lmP_State.nii.gz")
antsImageWrite(tOutStateImage,"lmT_State.nii.gz")

pOutTraitImage<-antsImageClone(mask)
tOutTraitImage<-antsImageClone(mask)
pOutTraitImage[mask==1]<-pOutTrait
tOutTraitImage[mask==1]<-tOutTrait

setwd(outDir)
antsImageWrite(pOutTraitImage,"lmP_Trait.nii.gz")
antsImageWrite(tOutTraitImage,"lmT_Trait.nii.gz")

pOutGmdImage<-antsImageClone(mask)
tOutGmdImage<-antsImageClone(mask)
pOutGmdImage[mask==1]<-pOutGmd
tOutGmdImage[mask==1]<-tOutGmd

setwd(outDir)
antsImageWrite(pOutGmdImage,"lmP_Gmd.nii.gz")
antsImageWrite(tOutGmdImage,"lmT_Gmd.nii.gz")

pOutMotionImage<-antsImageClone(mask)
tOutMotionImage<-antsImageClone(mask)
pOutMotionImage[mask==1]<-pOutMotion
tOutMotionImage[mask==1]<-tOutMotion

setwd(outDir)
antsImageWrite(pOutMotionImage,"lmP_Motion.nii.gz")
antsImageWrite(tOutMotionImage,"lmT_Motion.nii.gz")
