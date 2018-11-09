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
#NOTE: no components were nonsignificant for volume and overall psychopathology
#tvalVoxVector[which(tvalVoxVector %in% c())]<-0

#OPTIONAL: For the remaining components, assign the respective t-value for that component
#However, this makes it difficult to make custom color palettes in Caret later.
tvalVoxVector[which(tvalVoxVector==1)] <- 3.62
tvalVoxVector[which(tvalVoxVector==2)] <- 4.91
tvalVoxVector[which(tvalVoxVector==3)] <- 4.47
tvalVoxVector[which(tvalVoxVector==4)] <- 4.93
tvalVoxVector[which(tvalVoxVector==5)] <- 4.69
tvalVoxVector[which(tvalVoxVector==6)] <- 4.87
tvalVoxVector[which(tvalVoxVector==7)] <- 3.15
tvalVoxVector[which(tvalVoxVector==8)] <- 4.19
tvalVoxVector[which(tvalVoxVector==9)] <- 4.27
tvalVoxVector[which(tvalVoxVector==10)] <- 5.13
tvalVoxVector[which(tvalVoxVector==11)] <- 4.73
tvalVoxVector[which(tvalVoxVector==12)] <- 4.76
tvalVoxVector[which(tvalVoxVector==13)] <- 4.19
tvalVoxVector[which(tvalVoxVector==14)] <- 4.32
tvalVoxVector[which(tvalVoxVector==15)] <- 3.85
tvalVoxVector[which(tvalVoxVector==16)] <- 4.71
tvalVoxVector[which(tvalVoxVector==17)] <- 4.11
tvalVoxVector[which(tvalVoxVector==18)] <- 3.58

#Essentially this is a replaced image with 0s and surviving component numbers or t-values
tvalMap[mask==1]<-as.matrix(tvalVoxVector)
antsImageWrite(tvalMap,"/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/NewJacobianNmfSignificantComponents_overall_pos.nii.gz")
