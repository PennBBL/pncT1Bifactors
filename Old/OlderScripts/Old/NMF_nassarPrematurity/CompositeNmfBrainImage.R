#Create nifti images: 1) with all 18 NMF components on one brain and 2) with only FDR-significant NMF components showing an association with gestational age

library(ANTsR)

############################
#Read in merged images and mask
img<-antsImageRead('/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/n279_18NmfComponentsMerged.nii.gz',4) #this 4d set of merged images for each comp.
mask<-antsImageRead('/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/prior_grey_thr01_2mm_MNI_bin.nii.gz',3)

#Create matrix: rows are components, columns are voxels
seed.mat<-timeseries2matrix(img, mask)

#Find, for each voxel, which component has highest loading
whichCompStr <-apply(seed.mat,2,which.max) # however, some of those are all zeros, need to remove
foo <-apply(seed.mat,2,sum) 		   # this is sum of loadings across column; if 0, entire column is 0
whichCompStr[which(foo==0)]<-0 		   # assign 0-columns to 0

#Writing that to an image where every voxel is assigned to one component
newImg<-antsImageClone(mask)               # prep for writing out image
newImg[mask==1]<-as.matrix(whichCompStr)   # put assigned values back in the image	
antsImageWrite(newImg,"/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/NMF_nassarPrematurity_all18Components.nii.gz")

#Assign t-values for components where gestational age is FDR-significantly associated with CT
tvalMap<-antsImageClone(mask)
tvalVoxVector<-whichCompStr			               # a vector that contains # of Comp for each voxel
tvalVoxVector[which(tvalVoxVector %in% c(2,6,7,10))]<-0        # assign all components that you don't want to 0
tvalVoxVector[which(tvalVoxVector==1)]<-3.57		       # for the remaining components, assign p- or t-value (we chose t-values)
tvalVoxVector[which(tvalVoxVector==3)]<-4.18
tvalVoxVector[which(tvalVoxVector==4)]<-3.97
tvalVoxVector[which(tvalVoxVector==5)]<-2.07
tvalVoxVector[which(tvalVoxVector==8)]<-2.42
tvalVoxVector[which(tvalVoxVector==9)]<-3.88
tvalVoxVector[which(tvalVoxVector==11)]<-3.57
tvalVoxVector[which(tvalVoxVector==12)]<-2.29
tvalVoxVector[which(tvalVoxVector==13)]<-2.20
tvalVoxVector[which(tvalVoxVector==14)]<-3.41
tvalVoxVector[which(tvalVoxVector==15)]<-3.43
tvalVoxVector[which(tvalVoxVector==16)]<-2.18
tvalVoxVector[which(tvalVoxVector==17)]<-3.32
tvalVoxVector[which(tvalVoxVector==18)]<-3.92

tvalMap[mask==1]<-as.matrix(tvalVoxVector)				       # essentially this is a replaced image with t-values or 0s instead of comp numbers
antsImageWrite(tvalMap,"/data/joy/BBL/projects/pncT1AcrossDisorder/images/NMF_nassarPrematurity/NMF_sge_job_output/NMF_nassarPrematurity_gaFDRresultsOnly.nii.gz")

