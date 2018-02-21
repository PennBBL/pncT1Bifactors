##NOTE: For some reason, the following method of excluding participants does not work for subject 102895. This subject should be excluded bc their mprageSbiaMarsQaExclude == 1, but this code does not capture this subject. Instead, define ACROSS.INCLUDE and then subset based on that variable (see script "AcrossDisorder_MarsAnalyses.R" for code).

#Exclude everone with a health problem
data.final <- data.final[which(data.final$healthExclude == 0), ]

#Exclude everyone with an average rating of zero (Bad Data Quality)
data.final <- data.final[-which(data.final$averageRating == 0), ]

#Exclude everyone using SBIAMarsQA exclusion
data.final <- data.final[-which(data.final$mprageSbiaMarsQaExclude == 1), ]