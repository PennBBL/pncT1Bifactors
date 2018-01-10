#Define paths
subjDataName<-"/data/joy/BBL/projects/pncT1AcrossDisorder/subjectData/n1360_JLF_volCtGmd_subjData.rds"
outPdf_frontal<-'/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure3_frontalCT.pdf'
outPdf_occipit<-'/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure3_occipitCT.pdf'
outPdf_parietal<-'/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure3_parietalCT.pdf'
outPdf_temporal<-'/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure3_temporalCT.pdf'
outPdf_totalGrey<-'/data/joy/BBL/projects/pncT1AcrossDisorder/TablesFigures/Figure3_totalGreyCT.pdf'

#Load libraries
library(visreg)
library(reshape2)

#Load and subset subject data
subjData<-readRDS(subjDataName)

#Get reduced datframe for melt
subjData_frontal<-subjData[,c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmFrontalTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_occipit<-subjData[,c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmOccipitalTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_parietal<-subjData[,c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmParietalTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_temporal<-subjData[,c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmTemporalTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_totalGrey<-subjData[,c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]


#Reshape to long format
dataLong_frontal<-melt(subjData_frontal,id.vars=c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmFrontalTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_occipit<-melt(subjData_occipit,id.vars=c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmOccipitalTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_parietal<-melt(subjData_parietal,id.vars=c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmParietalTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_temporal<-melt(subjData_temporal,id.vars=c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmTemporalTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_totalGrey<-melt(subjData_totalGrey,id.vars=c("bblid","age","sex","ageSq","mprage_antsCT_vol_TBV","CT_gmTotal"),variable.name="symptomDomain",value.name="symptomScore")


#Run models
mdl_frontal<-lm(CT_gmFrontalTotal ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + symptomDomain*symptomScore, data=dataLong_frontal)
mdl_occipit<-lm(CT_gmOccipitalTotal ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + symptomDomain*symptomScore, data=dataLong_occipit)
mdl_parietal<-lm(CT_gmParietalTotal ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + symptomDomain*symptomScore, data=dataLong_parietal)
mdl_temporal<-lm(CT_gmTemporalTotal ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + symptomDomain*symptomScore, data=dataLong_temporal)
mdl_totalGrey<-lm(CT_gmTotal ~ age + sex + I(scale(age, scale=FALSE, center=TRUE)^2) + mprage_antsCT_vol_TBV + symptomDomain*symptomScore, data=dataLong_totalGrey)

#Define figure colors
#Colors	used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
#808080 = gray (confidence intervals)
colors<-c("#325194","#943282","#B3141C","#F58311","#329444")

#Run anova on models
anova(mdl_frontal)
anova(mdl_occipit)
anova(mdl_parietal)
anova(mdl_temporal)
anova(mdl_totalGrey)

#Graph figure and save as a pdf
pdf(outPdf_frontal)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_frontal,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Frontal Lobe Cortical Thickness",legend=FALSE)
dev.off()

pdf(outPdf_occipit)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_occipit,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Occipital Lobe Cortical Thickness",legend=FALSE)
dev.off()

pdf(outPdf_parietal)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_parietal,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Parietal Lobe Cortical Thickness",legend=FALSE)
dev.off()

pdf(outPdf_temporal)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_temporal,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Temporal Lobe Cortical Thickness",legend=FALSE)
dev.off()

pdf(outPdf_totalGrey)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_totalGrey,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Total Grey Matter Cortical Thickness",legend=FALSE)
dev.off()


##Helpful plotting tips
#mgp – A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
#mar – A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1)
#oma - A vector of the form c(bottom, left, top, right) giving the size of the outer margins in lines of text.
#lwd - The line width, a positive number, defaulting to 1. 
#lend - The line end style. This can be specified as an integer or string: when left blank or specfied as "round", this command produces rounded line caps [default]; 1 or "butt" produce butt line caps; 2 or "square" produce square line caps.
#cex.lab - The magnification to be used for x and y labels relative to the current setting of cex.
#cex.axis - The magnification to be used for axis annotation relative to the current setting of cex.
#bty - A character string which determined the type of box which is drawn about plots. If bty is one or "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box.
#line.par - colors of the lines (order determined by order of factors in symptomDomain
#fill.par - color for the confidence interval bands
#band=FALSE - remove confidence bands