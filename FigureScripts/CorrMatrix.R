#################
### LOAD DATA ###
#################

##Read in data
subjData <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

##Subset only by rows needed for correlation tables
CorrTraits <- c("mood_corrtraitsv2","psychosis_corrtraitsv2","externalizing_corrtraitsv2","fear_corrtraitsv2")
subjData_corrTraits <- subjData[CorrTraits]

Bifactors <- c("mood_4factorv2","psychosis_4factorv2","externalizing_4factorv2","phobias_4factorv2","overall_psychopathology_4factorv2")
subjData_bifactors <- subjData[Bifactors]

#Rename variables for plotting
colnames(subjData_corrTraits) <- c("Correlated Traits Anxious-Misery", "Correlated Traits Psychosis", "Correlated Traits Behavioral", "Correlated Traits Fear")

colnames(subjData_bifactors) <- c("Bifactors Anxious-Misery", "Bifactors Psychosis", "Bifactors Behavioral", "Bifactors Fear", "Bifactors Overall")

#Load libraries
library(psych)
library(corrgram)
library(corrplot)

####################
### CORRELATIONS ###
####################

#Make correlation tables
corTable_corrTraits <- cor(subjData_corrTraits, method="pearson", use="complete.obs")
corTable_bifactors <- cor(subjData_bifactors, method="pearson", use="complete.obs")

#Round correlation values to two decimal places
corTable_corrTraits_rounded <- round(corTable_corrTraits, 2)
corTable_bifactors_rounded <- round(corTable_bifactors, 2)

#Save tables
write.csv(corTable_corrTraits_rounded,"/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Table_CorrTrait_Correlations.csv",row.names=TRUE,quote=FALSE)
write.csv(corTable_bifactors_rounded,"/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Table_Bifactor_Correlations.csv",row.names=TRUE,quote=FALSE)

#############
### PLOTS ###
#############

##Define a function to get the p-values
#mat : is a matrix of data
#... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#Matrix of the correlation p-values
p.mat_corrTraits <- cor.mtest(subjData_corrTraits)
p.mat_bifactors <- cor.mtest(subjData_bifactors)

##FDR correct across all correlations
#Correlated Traits
p.mat_corrTraits_fdr <- matrix(p.adjust(as.vector(as.matrix(p.mat_corrTraits)), method='fdr'),ncol=4)
colnames(p.mat_corrTraits_fdr) <- colnames(p.mat_corrTraits)
rownames(p.mat_corrTraits_fdr) <- rownames(p.mat_corrTraits) 

#Bifactors
p.mat_bifactors_fdr <- matrix(p.adjust(as.vector(as.matrix(p.mat_bifactors)), method='fdr'),ncol=5)
colnames(p.mat_bifactors_fdr) <- colnames(p.mat_bifactors)
rownames(p.mat_bifactors_fdr) <- rownames(p.mat_bifactors)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

png(height=1200, width=1200, pointsize=25, file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Corrplot_CorrTrait_Correlations_fdr.png")
corrplot(corTable_corrTraits, method="color", col=col(200),  
         type="upper", title="Correlations between Correlated Traits",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat_corrTraits_fdr, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
	 #move title
	 mar=c(0,0,1,5) 
         )
dev.off()

png(height=1200, width=1200, pointsize=25, file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Corrplot_Bifactor_Correlations_fdr.png")
corrplot(corTable_bifactors, method="color", col=col(200),
         type="upper", title="Correlations between Bifactor Scores",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat_bifactors_fdr, sig.level = 0.05, insig = "blank",
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
         #move title
         mar=c(0,0,1,5)
         )
dev.off()


png(height=1200, width=1200, pointsize=25, file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Corrplot_CorrTrait_Correlations.png")
corrplot(corTable_corrTraits, method="color", title="Correlations between Correlated Traits", mar=c(0,0,1,0))
dev.off()

png(height=1200, width=1200, pointsize=25, file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Corrplot_Bifactor_Correlations.png")
corrplot(corTable_bifactors, method="color", title="Correlations between Bifactor Scores", mar=c(0,0,1,0))
dev.off()




#jpeg("/data/joy/BBL/projects/pncAslAcrossDisorder/TablesFigures/Corrplot3.jpg")
#corrplot(M, method="number",type="lower",tl.cex = .6, title="Correlations between Correlated Traits and New Bifactor Scores", mar=c(0,0,1,0))
#corrplot.mixed(M,tl.cex=.5)
#dev.off()

#corrgram plot
#jpeg("/data/joy/BBL/projects/pncAslAcrossDisorder/TablesFigures/Corrplot4.jpg")
#Correlations<-corrgram(subjData_short, order=TRUE, lower.panel=panel.shade,
#  upper.panel=NULL, text.panel=panel.txt,
#  main="Correlations between Correlated Traits and Bifactors")
#dev.off()

#Mood vs OverallPsy
#jpeg("/data/joy/BBL/projects/pncAslAcrossDisorder/TablesFigures/MoodByOverall.jpg")
#MoodByOverall <- plot(subjData$Mood, subjData$goassessItemBifactor4FactorOverallPsy)
#dev.off()
