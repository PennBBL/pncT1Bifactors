#################
### LOAD DATA ###
#################

subjData <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")
jacobianData <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/cmpWeightedAverageNumBases_18.csv", header=FALSE)

#######################################
### REMOVE PATH TO GET SCAN ID ONLY ###
#######################################

jacobianData[] <- lapply(jacobianData, function(x) gsub("/cbica/projects/pncNmf/n1396_t1NMF/images/Jacobian_ReslicedDownsampledSmoothed2mm/", "", x))
jacobianData[] <- lapply(jacobianData, function(x) gsub("_SubjectToTemplateLogJacobian_isotropic2mm_smoothed2mm.nii.gz", "", x))

########################
### RENAME VARIABLES ###
########################

colnames(jacobianData) <- c("scanid","jacobian_Nmf18C1","jacobian_Nmf18C2","jacobian_Nmf18C3","jacobian_Nmf18C4","jacobian_Nmf18C5","jacobian_Nmf18C6","jacobian_Nmf18C7","jacobian_Nmf18C8","jacobian_Nmf18C9","jacobian_Nmf18C10","jacobian_Nmf18C11","jacobian_Nmf18C12","jacobian_Nmf18C13","jacobian_Nmf18C14","jacobian_Nmf18C15","jacobian_Nmf18C16","jacobian_Nmf18C17","jacobian_Nmf18C18")

#######################################
### MAKE JACOBIAN VARIABLES NUMERIC ###
#######################################

dataJac <- data.frame(lapply(jacobianData, function(x) as.numeric(as.character(x))))

##################
### MERGE DATA ###
##################

dataComb <-merge(subjData, dataJac, by="scanid", all=TRUE)

#################
### SAVE DATA ###
#################

saveRDS(dataComb,"/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData_Jacobian.rds")
