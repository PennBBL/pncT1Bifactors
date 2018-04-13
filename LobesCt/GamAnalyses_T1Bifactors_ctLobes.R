##########################################
#### GAM MODELS FOR T1 BIFACTOR STUDY ####
##########################################

#Load data
data.Lobes <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load library
library(mgcv)
library(visreg)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

#Get lobe variable names
lobes <- c("mprage_jlfLobe_ct_Limbic_Lobe","mprage_jlfLobe_ct_Insular_Lobe","mprage_jlfLobe_ct_Frontal_Lobe","mprage_jlfLobe_ct_Parietal_Lobe","mprage_jlfLobe_ct_Occipital_Lobe","mprage_jlfLobe_ct_Temporal_Lobe")

#Run gam models (GAM without TBV)
LobeModels <- lapply(lobes, function(x) {
  gam(substitute(i ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, list(i = as.name(x))), method="REML", data = data.Lobes)
})

#Look at model summaries
models <- lapply(LobeModels, summary)

######################
#### MOOD RESULTS ####
######################

#Pull p-values
p_mood <- sapply(LobeModels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_mood <- as.data.frame(p_mood)

#Print original p-values to three decimal places
p_mood_round <- round(p_mood,3)

#FDR correct p-values
p_mood_fdr <- p.adjust(p_mood[,1],method="fdr")

#Convert to data frame
p_mood_fdr <- as.data.frame(p_mood_fdr)

#To print fdr-corrected p-values to three decimal places
p_mood_fdr_round <- round(p_mood_fdr,3)

#List the lobes that survive FDR correction
Lobe_mood_fdr <- row.names(p_mood_fdr)[p_mood_fdr<0.05]

#Name of the lobes that survive FDR correction
Lobe_mood_fdr_names <- lobes[as.numeric(Lobe_mood_fdr)]

#To check direction of coefficient estimates
mood_coeff <- models[as.numeric(Lobe_mood_fdr)]

###########################
#### PSYCHOSIS RESULTS ####
###########################

#Pull p-values
p_psy <- sapply(LobeModels, function(v) summary(v)$p.table[5,4])

#Convert to data frame
p_psy <- as.data.frame(p_psy)

#Print original p-values to three decimal places
p_psy_round <- round(p_psy,3)

#FDR correct p-values
p_psy_fdr <- p.adjust(p_psy[,1],method="fdr")

#Convert to data frame
p_psy_fdr <- as.data.frame(p_psy_fdr)

#To print fdr-corrected p-values to three decimal places
p_psy_fdr_round <- round(p_psy_fdr,3)

#List the lobes that survive FDR correction
Lobe_psy_fdr <- row.names(p_psy_fdr)[p_psy_fdr<0.05]

#Name of the lobes that survive FDR correction
Lobe_psy_fdr_names <- lobes[as.numeric(Lobe_psy_fdr)]

#To check direction of coefficient estimates
psy_coeff <- models[as.numeric(Lobe_psy_fdr)]

########################################
#### EXTERNALIZING BEHAVIOR RESULTS ####
########################################

#Pull p-values
p_ext <- sapply(LobeModels, function(v) summary(v)$p.table[6,4])

#Convert to data frame
p_ext <- as.data.frame(p_ext)

#Print original p-values to three decimal places
p_ext_round <- round(p_ext,3)

#FDR correct p-values
p_ext_fdr <- p.adjust(p_ext[,1],method="fdr")

#Convert to data frame
p_ext_fdr <- as.data.frame(p_ext_fdr)

#To print fdr-corrected p-values to three decimal places
p_ext_fdr_round <- round(p_ext_fdr,3)

#List the lobes that survive FDR correction
Lobe_ext_fdr <- row.names(p_ext_fdr)[p_ext_fdr<0.05]

#Name of the lobes that survive FDR correction
Lobe_ext_fdr_names <- lobes[as.numeric(Lobe_ext_fdr)]

#To check direction of coefficient estimates
ext_coeff <- models[as.numeric(Lobe_ext_fdr)]

##############################
#### PHOBIA(FEAR) RESULTS ####
##############################

#Pull p-values
p_fear <- sapply(LobeModels, function(v) summary(v)$p.table[7,4])

#Convert to data frame
p_fear <- as.data.frame(p_fear)

#Print original p-values to three decimal places
p_fear_round <- round(p_fear,3)

#FDR correct p-values
p_fear_fdr <- p.adjust(p_fear[,1],method="fdr")

#Convert to data frame
p_fear_fdr <- as.data.frame(p_fear_fdr)

#To print fdr-corrected p-values to three decimal places
p_fear_fdr_round <- round(p_fear_fdr,3)

#List the lobes that survive FDR correction
Lobe_fear_fdr <- row.names(p_fear_fdr)[p_fear_fdr<0.05]

#Name of the lobes that survive FDR correction
Lobe_fear_fdr_names <- lobes[as.numeric(Lobe_fear_fdr)]

#To check direction of coefficient estimates
fear_coeff <- models[as.numeric(Lobe_fear_fdr)]

#########################################
#### OVERALL PSYCHOPATHOLOGY RESULTS ####
#########################################

#Pull p-values
p_overall <- sapply(LobeModels, function(v) summary(v)$p.table[8,4])

#Convert to data frame
p_overall <- as.data.frame(p_overall)

#Print original p-values to three decimal places
p_overall_round <- round(p_overall,3)

#FDR correct p-values
p_overall_fdr <- p.adjust(p_overall[,1],method="fdr")

#Convert to data frame
p_overall_fdr <- as.data.frame(p_overall_fdr)

#To print fdr-corrected p-values to three decimal places
p_overall_fdr_round <- round(p_overall_fdr,3)

#List the lobes that survive FDR correction
Lobe_overall_fdr <- row.names(p_overall_fdr)[p_overall_fdr<0.05]

#Name of the lobes that survive FDR correction
Lobe_overall_fdr_names <- lobes[as.numeric(Lobe_overall_fdr)]

#To check direction of coefficient estimates
overall_coeff <- models[as.numeric(Lobe_overall_fdr)]

######################################
#### PLOT FEAR BY CT IN EACH LOBE ####
######################################

#Scatter plots for each lobe

##############
### Limbic ###
##############
plotdata <- visreg(LobeModels[[1]],'phobias_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$phobias_4factorv2,
                       y=plotdata$res$visregRes)

##To make data points have color gradients, use colpal and colkey
##Colors are defined as follows: colpal[1]=Purples, colpal[2]=Blues, colpal[3]=Oranges, colpal[4]=Reds
#colpal <- c("Purples","Blues","Oranges","Reds")
#colkey <- brewer.pal(8,colpal[3])[c(3:8)]

##Or for solid color data points, use this command:
#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#F58311"
lineColor<- "#F55d11"
p_text <- paste("p == .003")
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Fear (z)", y = "Limbic CT (mm)") +
  theme(axis.title=element_text(size=30,face="bold"), axis.text=element_text(size=20), axis.title.x=element_text(color = "black"))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_ctLimbic_fear.png")


###############
### Insular ###
###############
plotdata <- visreg(LobeModels[[2]],'phobias_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$phobias_4factorv2,
                       y=plotdata$res$visregRes)

##To make data points have color gradients, use colpal and colkey
##Colors are defined as follows: colpal[1]=Purples, colpal[2]=Blues, colpal[3]=Oranges, colpal[4]=Reds
#colpal <- c("Purples","Blues","Oranges","Reds")
#colkey <- brewer.pal(8,colpal[3])[c(3:8)]

##Or for solid color data points, use this command:
#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#F58311"
lineColor<- "#F55d11"
p_text <- paste("p == .017")
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Fear (z)", y = "Insular CT (mm)") +
  theme(axis.title=element_text(size=30,face="bold"), axis.text=element_text(size=20), axis.title.x=element_text(color = "black"))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_ctInsular_fear.png")

###############
### Frontal ###
###############
plotdata <- visreg(LobeModels[[3]],'phobias_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$phobias_4factorv2,
                       y=plotdata$res$visregRes)

##To make data points have color gradients, use colpal and colkey
##Colors are defined as follows: colpal[1]=Purples, colpal[2]=Blues, colpal[3]=Oranges, colpal[4]=Reds
#colpal <- c("Purples","Blues","Oranges","Reds")
#colkey <- brewer.pal(8,colpal[3])[c(3:8)]

##Or for solid color data points, use this command:
#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#B3141C"
lineColor<- "#6e0c11"
p_text <- paste("p == .004")
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Fear (z)", y = "Frontal CT (mm)") +
  theme(axis.title=element_text(size=30,face="bold"), axis.text=element_text(size=20), axis.title.x=element_text(color = "black"), axis.title.y=element_text(color = "#B3141C"))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_ctFrontal_fear.png")

#################
### Occipital ###
#################
plotdata <- visreg(LobeModels[[5]],'phobias_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$phobias_4factorv2,
                       y=plotdata$res$visregRes)

##To make data points have color gradients, use colpal and colkey
##Colors are defined as follows: colpal[1]=Purples, colpal[2]=Blues, colpal[3]=Oranges, colpal[4]=Reds
#colpal <- c("Purples","Blues","Oranges","Reds")
#colkey <- brewer.pal(8,colpal[3])[c(3:8)]

##Or for solid color data points, use this command:
#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#329444"
lineColor<- "#1f5b2a"
p_text <- paste("p < .001")
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Fear (z)", y = "Occipital CT (mm)") +
  theme(axis.title=element_text(size=30,face="bold"), axis.text=element_text(size=20), axis.title.x=element_text(color = "black"), axis.title.y=element_text(color = "#329444"))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_ctOccipital_fear.png")

################
### Temporal ###
################
plotdata <- visreg(LobeModels[[6]],'phobias_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$phobias_4factorv2,
                       y=plotdata$res$visregRes)

##To make data points have color gradients, use colpal and colkey
##Colors are defined as follows: colpal[1]=Purples, colpal[2]=Blues, colpal[3]=Oranges, colpal[4]=Reds
#colpal <- c("Purples","Blues","Oranges","Reds")
#colkey <- brewer.pal(8,colpal[3])[c(3:8)]

##Or for solid color data points, use this command:
#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#14b3ab"
lineColor<- "#0c6e69"
p_text <- paste("p < .001")
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Fear (z)", y = "Temporal CT (mm)") +
  theme(axis.title=element_text(size=30,face="bold"), axis.text=element_text(size=20), axis.title.x=element_text(color = "black"), axis.title.y=element_text(color = "#14b3ab"))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_ctTemporal_fear.png")

##################################
### Parietal - not significant ###
##################################
plotdata <- visreg(LobeModels[[4]],'phobias_4factorv2',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x,
                      x=plotdata$fit[[plotdata$meta$x]],
                      smooth=plotdata$fit$visregFit,
                      lower=plotdata$fit$visregLwr,
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1",
                       x=plotdata$res$phobias_4factorv2,
                       y=plotdata$res$visregRes)

##To make data points have color gradients, use colpal and colkey
##Colors are defined as follows: colpal[1]=Purples, colpal[2]=Blues, colpal[3]=Oranges, colpal[4]=Reds
#colpal <- c("Purples","Blues","Oranges","Reds")
#colkey <- brewer.pal(8,colpal[3])[c(3:8)]

##Or for solid color data points, use this command:
#Colors used: #325194 = blue (Anxious-Misery), #943282 = purple (Psychosis), #B3141C = red (Behavioral), #F58311 = orange (Fear), #329444 = green (OverallPsych))
colkey <- "#e6aa1d"
lineColor<- "#e6891d"
p_text <- paste("p == .063")
Limbic<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = lineColor,size=2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = lineColor, alpha = 0.9, size = 1.5) +
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = lineColor, alpha = 0.9, size = 1.5) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 10, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Fear (z)", y = "Parietal CT (mm)") +
  theme(axis.title=element_text(size=30,face="bold"), axis.text=element_text(size=20), axis.title.x=element_text(color = "black"), axis.title.y=element_text(color = "#e6aa1d"))

ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Scatterplot_ctParietal_fear.png")



#######################################################################

limbic_fear <- gam(mprage_jlfLobe_ct_Limbic_Lobe ~ s(age) + sex + averageManualRating + mood_4factorv2 + psychosis_4factorv2 + externalizing_4factorv2 + phobias_4factorv2 + overall_psychopathology_4factorv2, method="REML", data = data.Lobes)

#LobeModels
fig <- visreg(limbic_fear, "phobias_4factorv2", gg=TRUE, 
    ylab="Limbic Lobe Cortical Thickness", xlab = "Fear (z)",
       cex.lab=1.75, font.lab=2, cex.axis=1.5, fill= list(col="#FFF9C4"), line=list(col="#F58311"))

vis <- visreg(limbic_fear, "phobias_4factorv2", plot=FALSE)
fig <- plot(vis, gg=TRUE,  ylab="Limbic Lobe Cortical Thickness", xlab = "Fear (z)") + 
    theme(text = element_text(size=20)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave(file="/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/TEST_ct_LimbicFear.png")
