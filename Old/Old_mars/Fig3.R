#Define paths
subjDataName<-"/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/subjectData/n1385_MARS_datarel_020716_ravens.rds"
outPdf_subcort<-'/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/TablesFigures/Figure3_subcort.pdf'
outPdf_frontal<-'/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/TablesFigures/Figure3_frontal.pdf'
outPdf_occipit<-'/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/TablesFigures/Figure3_occipit.pdf'
outPdf_parietal<-'/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/TablesFigures/Figure3_parietal.pdf'
outPdf_temporal<-'/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/TablesFigures/Figure3_temporal.pdf'
outPdf_totalGrey<-'/data/joy/BBL/projects/pncT1AcrossDisorder_Kaczkurkin/TablesFigures/Figure3_totalGrey.pdf'

#Load libraries
library(visreg)
library(reshape2)

#Load and subset subject data
subjData<-readRDS(subjDataName)

#Get reduced datframe for melt
subjData_subcort<-subjData[,c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmSubcortTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_frontal<-subjData[,c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmFrontalTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_occipit<-subjData[,c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmOccipitalTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_parietal<-subjData[,c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmParietalTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_temporal<-subjData[,c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmTemporalTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]

subjData_totalGrey<-subjData[,c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmTotal","goassessItemBifactor4FactorMood","goassessItemBifactor4FactorPsych","goassessItemBifactor4FactorExt","goassessItemBifactor4FactorPhb","goassessItemBifactor4FactorOverallPsy")]


#Reshape to long format
dataLong_subcort<-melt(subjData_subcort,id.vars=c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmSubcortTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_frontal<-melt(subjData_frontal,id.vars=c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmFrontalTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_occipit<-melt(subjData_occipit,id.vars=c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmOccipitalTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_parietal<-melt(subjData_parietal,id.vars=c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmParietalTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_temporal<-melt(subjData_temporal,id.vars=c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmTemporalTotal"),variable.name="symptomDomain",value.name="symptomScore")
dataLong_totalGrey<-melt(subjData_totalGrey,id.vars=c("bblid","age","sex","ageSq","white","meduCnbGo1","mprageMassICV","averageRating","Vol_gmTotal"),variable.name="symptomDomain",value.name="symptomScore")


#Run models
mdl_subcort<-lm(Vol_gmSubcortTotal ~ age + sex + ageSq + white + meduCnbGo1 + mprageMassICV + averageRating + symptomDomain*symptomScore, data=dataLong_subcort)
mdl_frontal<-lm(Vol_gmFrontalTotal ~ age + sex + ageSq + white + meduCnbGo1 + mprageMassICV + averageRating + symptomDomain*symptomScore, data=dataLong_frontal)
mdl_occipit<-lm(Vol_gmOccipitalTotal ~ age + sex + ageSq + white + meduCnbGo1 + mprageMassICV + averageRating + symptomDomain*symptomScore, data=dataLong_occipit)
mdl_parietal<-lm(Vol_gmParietalTotal ~ age + sex + ageSq + white + meduCnbGo1 + mprageMassICV + averageRating + symptomDomain*symptomScore, data=dataLong_parietal)
mdl_temporal<-lm(Vol_gmTemporalTotal ~ age + sex + ageSq + white + meduCnbGo1 + mprageMassICV + averageRating + symptomDomain*symptomScore, data=dataLong_temporal)
mdl_totalGrey<-lm(Vol_gmTotal ~ age + sex + ageSq + white + meduCnbGo1 + mprageMassICV + averageRating + symptomDomain*symptomScore, data=dataLong_totalGrey)

#Define figure colors
#Colors	used: #329444 = green (OverallPsych), #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear)
#808080 = gray (confidence intervals)
colors<-c("#325194","#943282","#B3141C","#F58311","#329444")

#Run anova on models
anova(mdl_subcort)
anova(mdl_frontal)
anova(mdl_occipit)
anova(mdl_parietal)
anova(mdl_temporal)
anova(mdl_totalGrey)

#Graph figure and save as a pdf
pdf(outPdf_subcort)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_subcort,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Subcortical Volume (cc)",legend=FALSE)
dev.off()

pdf(outPdf_frontal)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_frontal,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Frontal Lobe Volume (cc)",legend=FALSE)
dev.off()

pdf(outPdf_occipit)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_occipit,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Occipital Lobe Volume (cc)",legend=FALSE)
dev.off()

pdf(outPdf_parietal)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_parietal,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Parietal Lobe Volume (cc)",legend=FALSE)
dev.off()

pdf(outPdf_temporal)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_temporal,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Temporal Lobe Volume (cc)",legend=FALSE)
dev.off()

pdf(outPdf_totalGrey)
par(mgp=c(2.5,.65,0), lwd=2, lend=2, cex.lab=1.5, cex.axis=1.25, mar=c(4,4,1,.7), oma=c(0,0,2,0), bty="l")
visreg(mdl_totalGrey,"symptomScore",by="symptomDomain",partial=FALSE,rug=FALSE,overlay=TRUE,band=FALSE,line.par=list(col=colors),fill.par=list(col="#80808050"), xlab="Symptom Score (z)", ylab="Total Grey Matter Volume (cc)",legend=FALSE)
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