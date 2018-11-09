#Create nifti images: 1) with all NMF components on one brain and 2) with only FDR-significant NMF components

###############################
### LOAD DATA AND LIBRARIES ###
###############################
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load libraries
library(mgcv)
library(ANTsR)

#####################################################
### CREATE IMAGE WITH ALL COMPONENTS ON ONE BRAIN ###
#####################################################
#Load the 4d set of merged component images (NOTE: each component needs to be merged in the correct order 1-18 for this script to work).
img<-antsImageRead('/data/jux/BBL/projects/pncNmf/results/CT/n1396_18CtNmfComponentsMerged.nii.gz',4)

#Load the prior grey matter mask (warped to MNI space and binarized)
mask<-antsImageRead('/data/jux/BBL/projects/pncNmf/masks/prior_grey_thr01_2mm_MNI_bin.nii.gz',3)

#Create matrix: rows are components, columns are voxels
seed.mat<-timeseries2matrix(img, mask)

#Find, for each voxel, which component has highest loading
whichCompStr <-apply(seed.mat,2,which.max) # however, some of those are all zeros, need to remove
foo <-apply(seed.mat,2,sum) 		   # this is sum of loadings across column; if 0, entire column is 0
whichCompStr[which(foo==0)]<-0 		   # assign 0-columns to 0

#Write image with all components on one brain (every voxel is assigned to one component)
newImg<-antsImageClone(mask)               # prep for writing out image
newImg[mask==1]<-as.matrix(whichCompStr)   # put assigned values back in the image
antsImageWrite(newImg,"/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/NewJacobianNmf18Components.nii.gz") #This is identical to CT because we applied the CT components to the new Jacobian images.

##################################################################
### CREATE IMAGE WITH ONLY SIGNIFICANT COMPONENTS ON ONE BRAIN ###
################################################################## 
#Create a vector that assigns each voxel a component number
tvalMap<-antsImageClone(mask)
tvalVoxVector<-whichCompStr

#Assign all components that did not survive fdr correction to 0
#NOTE: only one component was nonsignificant for volume and mood
tvalVoxVector[which(tvalVoxVector %in% c(7))]<-0

#OPTIONAL: For the remaining components, assign the respective t-value for that component
#However, this makes it difficult to make custom color palettes in Caret later.
tvalVoxVector[which(tvalVoxVector==1)] <- 2.23
tvalVoxVector[which(tvalVoxVector==2)] <- 3.96
tvalVoxVector[which(tvalVoxVector==3)] <- 4.28
tvalVoxVector[which(tvalVoxVector==4)] <- 2.31
tvalVoxVector[which(tvalVoxVector==5)] <- 3.35
tvalVoxVector[which(tvalVoxVector==6)] <- 2.75
tvalVoxVector[which(tvalVoxVector==8)] <- 3.02
tvalVoxVector[which(tvalVoxVector==9)] <- 3.10
tvalVoxVector[which(tvalVoxVector==10)] <- 3.86
tvalVoxVector[which(tvalVoxVector==11)] <- 4.37
tvalVoxVector[which(tvalVoxVector==12)] <- 3.20
tvalVoxVector[which(tvalVoxVector==13)] <- 2.06
tvalVoxVector[which(tvalVoxVector==14)] <- 2.77
tvalVoxVector[which(tvalVoxVector==15)] <- 2.63
tvalVoxVector[which(tvalVoxVector==16)] <- 4.12
tvalVoxVector[which(tvalVoxVector==17)] <- 2.80
tvalVoxVector[which(tvalVoxVector==18)] <- 3.52

#Essentially this is a replaced image with 0s and surviving component numbers or t-values
tvalMap[mask==1]<-as.matrix(tvalVoxVector)
antsImageWrite(tvalMap,"/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/NewJacobianNmfSignificantComponents_mood_pos.nii.gz")
