#Define paths
subjDataName<-"/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds"
outPdf_frontal<-'/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/LineGraph_frontalCT.pdf'
outPdf_occipit<-'/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/LineGraph_occipitCT.pdf'
outPdf_parietal<-'/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/LineGraph_parietalCT.pdf'
outPdf_temporal<-'/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/LineGraph_temporalCT.pdf'
outPdf_totalGrey<-'/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/LineGraph_totalGreyCT.pdf'

#Load libraries
library(visreg)
library(reshape2)

#Load and subset subject data
subjData<-readRDS(subjDataName)

#Get reduced datframe for melt
subjData_frontal<-subjData[,c("bblid","age","ageSq","sex","averageManualRating","mprage_jlfLobe_ct_Frontal_Lobe","mood_4factorv2","psychosis_4factorv2","externalizing_4factorv2","phobias_4factorv2","overall_psychopathology_4factorv2")]

subjData_occipit<-subjData[,c("bblid","age","ageSq","sex","averageManualRating","mprage_jlfLobe_ct_Occipital_Lobe","mood_4factorv2","psychosis_4factorv2","externalizing_4factorv2","phobias_4factorv2","overall_psychopathology_4factorv2")]

subjData_parietal<-subjData[,c("bblid","age","ageSq","sex","averageManualRating","mprage_jlfLobe_ct_Parietal_Lobe","mood_4factorv2","psychosis_4factorv2","externalizing_4factorv2","phobias_4factorv2","overall_psychopathology_4factorv2")]

subjData_temporal<-subjData[,c("bblid","age","ageSq","sex","averageManualRating","mprage_jlfLobe_ct_Temporal_Lobe","mood_4factorv2","psychosis_4factorv2","externalizing_4factorv2","phobias_4factorv2","overall_psychopathology_4factorv2")]


#Reshape to long format
dataLong_frontal<-melt(subjData_frontal,id.vars=c("bblid","age","ageSq","sex","averageManualRating","mprage_jlfLobe_ct_Frontal_Lobe"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_occipit<-melt(subjData_occipit,id.vars=c("bblid","age","ageSq","sex","averageManualRating","mprage_jlfLobe_ct_Occipital_Lobe"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_parietal<-melt(subjData_parietal,id.vars=c("bblid","age","ageSq","sex","averageManualRating","mprage_jlfLobe_ct_Parietal_Lobe"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_temporal<-melt(subjData_temporal,id.vars=c("bblid","age","ageSq","sex","averageManualRating","mprage_jlfLobe_ct_Temporal_Lobe"),variable.name="symptomDomain",value.name="symptomScore")


#Run models
#NOTE: sometimes visreg has a problem with ageSq and needs I(scale(age, scale=FALSE, center=TRUE)^2), and sometimes the opposite is true- dependent on version?
mdl_frontal<-lm(mprage_jlfLobe_ct_Frontal_Lobe ~ age + ageSq + sex + averageManualRating + symptomDomain*symptomScore, data=dataLong_frontal)
mdl_occipit<-lm(mprage_jlfLobe_ct_Occipital_Lobe ~ age + ageSq + sex + averageManualRating + symptomDomain*symptomScore, data=dataLong_occipit)
mdl_parietal<-lm(mprage_jlfLobe_ct_Parietal_Lobe ~ age + ageSq + sex + averageManualRating + symptomDomain*symptomScore, data=dataLong_parietal)
mdl_temporal<-lm(mprage_jlfLobe_ct_Temporal_Lobe ~ age + ageSq + sex + averageManualRating + symptomDomain*symptomScore, data=dataLong_temporal)

#Define figure colors
#Colors	used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
#808080 = gray (confidence intervals)
colors<-c("#325194","#943282","#B3141C","#F58311","#329444")

#Run anova on models
anova(mdl_frontal)
anova(mdl_occipit)
anova(mdl_parietal)
anova(mdl_temporal)

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