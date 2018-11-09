#################
### LOAD DATA ###
#################

#Jacobian volume (18 CT NMF components applied to Jacobian volume images)
data.jacobian <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/cmpWeightedAverageNumBases_18_jacobian.csv", header=FALSE)

#18 Ravens components (18 CT NMF components applied to Ravens volume images)
data.18Ravens <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/cmpWeightedAverageNumBases_18_ravens.csv", header=FALSE)

#############################
### PREPARE JACOBIAN DATA ###
#############################

#Remove path to get scan ID only
data.jacobian[] <- lapply(data.jacobian, function(x) gsub("/cbica/projects/pncNmf/n1396_t1NMF/images/Jacobian_ReslicedDownsampledSmoothed2mm/", "", x))
data.jacobian[] <- lapply(data.jacobian, function(x) gsub("_SubjectToTemplateLogJacobian_isotropic2mm_smoothed2mm.nii.gz", "", x))

#Rename variables
colnames(data.jacobian) <- c("scanid","jacobian_Nmf18C1","jacobian_Nmf18C2","jacobian_Nmf18C3","jacobian_Nmf18C4","jacobian_Nmf18C5","jacobian_Nmf18C6","jacobian_Nmf18C7","jacobian_Nmf18C8","jacobian_Nmf18C9","jacobian_Nmf18C10","jacobian_Nmf18C11","jacobian_Nmf18C12","jacobian_Nmf18C13","jacobian_Nmf18C14","jacobian_Nmf18C15","jacobian_Nmf18C16","jacobian_Nmf18C17","jacobian_Nmf18C18")

#Make Jacobian variables numeric
data.jacobian <- data.frame(lapply(data.jacobian, function(x) as.numeric(as.character(x))))

#########################################
### PREPARE 18 RAVENS COMPONENTS DATA ###
#########################################

#Remove path to get scan ID only
data.18Ravens[] <- lapply(data.18Ravens, function(x) gsub("/cbica/projects/pncNmf/n1396_t1NMF/images/Ravens_smoothed8mm/", "", x))
data.18Ravens[] <- lapply(data.18Ravens, function(x) gsub("_RAVENS_2GM_2mm_smoothed8mm.nii.gz", "", x))

#Rename variables
colnames(data.18Ravens) <- c("scanid","Ravens_Nmf18C1","Ravens_Nmf18C2","Ravens_Nmf18C3","Ravens_Nmf18C4","Ravens_Nmf18C5","Ravens_Nmf18C6","Ravens_Nmf18C7","Ravens_Nmf18C8","Ravens_Nmf18C9","Ravens_Nmf18C10","Ravens_Nmf18C11","Ravens_Nmf18C12","Ravens_Nmf18C13","Ravens_Nmf18C14","Ravens_Nmf18C15","Ravens_Nmf18C16","Ravens_Nmf18C17","Ravens_Nmf18C18")

#Make Ravens variables numeric
data.18Ravens <- data.frame(lapply(data.18Ravens, function(x) as.numeric(as.character(x))))

