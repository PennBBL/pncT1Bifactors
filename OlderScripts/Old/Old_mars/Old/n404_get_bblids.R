subjDataName <- "/home/agarza/voxelWise/testScript/n404_longitudinal_dataset_nback_dprime.rds"

data404 <- readRDS(subjDataName)

databblid <- as.data.frame(unique(data404$bblid))
names(databblid) <- NULL

write.csv(databblid, "~/voxelWise/testScript/n404_bblids.csv", row.names=F)



subjDataName <- "/data/joy/BBL/studies/exampleData/groupAnalysis/testScript/n404_longitudinal_dataset_nback_dprime.rds"

data404 <- readRDS(subjDataName)

datapaths <- read.csv("/data/joy/BBL/studies/exampleData/groupAnalysis/testScript/n404_asl_chead_path.csv", header=F)
names(datapaths) <- c("bblid", "timepoint", "asl_paths")

data404 <- data404[, -c(9,10,11,12)]


data404 <- merge(data404, datapaths, by=c("bblid", "timepoint"))

data404$asl_paths[which(data404$asl_paths == "")] <- NA

data.cov <- data404

data.cov$categorical_include <- 1
data.cov$categorical_include[which(data.cov$age < 11)] <- 0
data.cov$categorical_include[-which(data.cov$dx == "TDTD" | data.cov$dx == "PSCR")] <- 0
data.cov$categorical_include[is.na(data.cov$asl_path)] <- 0

data.cov$crosssectional_include <- 0
data.cov$crosssectional_include[which(data.cov$categorical_include == 1 & data.cov$timepoint == "go1")] <- 1

str(data.cov)

data.cov$sex <- ordered(data.cov$sex)

saveRDS(data.cov, "/data/joy/BBL/studies/exampleData/groupAnalysis/testScript/n404_longitudinal_dataset_asl_chead.rds")
