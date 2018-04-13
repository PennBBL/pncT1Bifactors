#################
### LOAD DATA ###
#################

subjData <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")
ravensData <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/cmpWeightedAverageNumBases_18_ravens.csv", header=FALSE)

#######################################
### REMOVE PATH TO GET SCAN ID ONLY ###
#######################################

ravensData[] <- lapply(ravensData, function(x) gsub("/cbica/projects/pncNmf/n1396_t1NMF/images/Ravens_smoothed8mm/", "", x))
ravensData[] <- lapply(ravensData, function(x) gsub("_RAVENS_2GM_2mm_smoothed8mm.nii.gz", "", x))

########################
### RENAME VARIABLES ###
########################

colnames(ravensData) <- c("scanid","Ravens_Nmf18C1","Ravens_Nmf18C2","Ravens_Nmf18C3","Ravens_Nmf18C4","Ravens_Nmf18C5","Ravens_Nmf18C6","Ravens_Nmf18C7","Ravens_Nmf18C8","Ravens_Nmf18C9","Ravens_Nmf18C10","Ravens_Nmf18C11","Ravens_Nmf18C12","Ravens_Nmf18C13","Ravens_Nmf18C14","Ravens_Nmf18C15","Ravens_Nmf18C16","Ravens_Nmf18C17","Ravens_Nmf18C18")

######################################
### MAKE RAVENS VARIABLES NUMERIC ###
######################################

dataRav <- data.frame(lapply(ravensData, function(x) as.numeric(as.character(x))))

##################
### MERGE DATA ###
##################

dataComb <-merge(subjData, dataRav, by="scanid", all=TRUE)

#################
### SAVE DATA ###
#################

saveRDS(dataComb,"/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData_NewRavens.rds")
