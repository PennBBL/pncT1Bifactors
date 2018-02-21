#############################
### DEFINE PATHS TO FILES ### 
#############################
ctImageName<-"/data/joy/BBL/projects/pncT1AcrossDisorder/results/n1360_ctPath_ACROSS.INCLUDE_smooth0/fourd.nii.gz"
maskName<-"/data/joy/BBL/studies/pnc/n1601_dataFreeze2016/neuroimaging/t1struct/n1360_antsCt_mask.nii.gz"

#################
### LIBRARIES ###
#################
library(ANTsR)  #for loading images

#################
### LOAD DATA ###
#################
mask<-antsImageRead(maskName,3) #3=3d
ctImageIn<-antsImageRead(ctImageName,4) #4=4d (bc this is the 4d file of ct images)
ctImageMat<-timeseries2matrix(ctImageIn,mask) #reshape to a matrix

#######################
### WRITE OUT IMAGE ###
#######################
save(ctImageMat, file="/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_ctData.RData")
