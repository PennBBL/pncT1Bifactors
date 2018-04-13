NMF component ~ s(age) + sex + medu1 + ga

##########################################
#### GAM MODELS FOR PREMATURITY STUDY ####
##########################################

#Load data
data.NMF <- read.csv("./Google Drive/CEDRIC/Toni's Figures/n282_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Load library
library(mgcv)
library(visreg)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf26",names(data.NMF))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + ga, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

#Pull p-values
p <- sapply(NmfModels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p <- as.data.frame(p)

#Print original p-values to three decimal places
p_round <- round(p,3)

#FDR correct p-values
pfdr <- p.adjust(p[,1],method="fdr")

#Convert to data frame
pfdr <- as.data.frame(pfdr)

#To print fdr-corrected p-values to three decimal places
pfdr_round <- round(pfdr,3)

#List the NMF components that survive FDR correction
Nmf_fdr <- row.names(pfdr)[pfdr<0.05]

#Scatter plots for NMF4

plotdata <- visreg(NmfModels[[4]],'ga',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x, 
                      x=plotdata$fit[[plotdata$meta$x]], 
                      smooth=plotdata$fit$visregFit, 
                      lower=plotdata$fit$visregLwr, 
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1", 
                       x=plotdata$res$ga,
                       y=plotdata$res$visregRes)

colpal <- c("Purples","Blues","Oranges","Reds")
colkey <- brewer.pal(8,colpal[4])[c(3:8)]
p_text <- paste("p < ", pfdr_round[[1]][[4]],sep="")
NMF4<-ggplot() +
  geom_point(data = predicts, aes(x, y, colour = x), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = colkey[6],size=1.2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = colkey[6], alpha = 0.9, size = 0.9) + 
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = colkey[6], alpha = 0.9, size = 0.9) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 5, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Gestational Age (Weeks)", y = "Volume of Network 4") 

#Scatter plots for NMF2

plotdata <- visreg(NmfModels[[2]],'ga',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x, 
                      x=plotdata$fit[[plotdata$meta$x]], 
                      smooth=plotdata$fit$visregFit, 
                      lower=plotdata$fit$visregLwr, 
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1", 
                       x=plotdata$res$ga,
                       y=plotdata$res$visregRes)

colpal <- c("Purples","Blues","Oranges","Reds")
colkey <- brewer.pal(8,colpal[3])[c(3:8)]
p_text <- paste("p < ", pfdr_round[[1]][[2]],sep="")
NMF2<-ggplot() +
  geom_point(data = predicts, aes(x, y), alpha= 1  ) +
  scale_colour_gradientn(colours = colkey,  name = "") +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = colkey[6],size=1.2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = colkey[6], alpha = 0.9, size = 0.9) + 
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = colkey[6], alpha = 0.9, size = 0.9) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 5, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Gestational Age (Weeks)", y = "Volume of Network 2") 


#Scatter GA vs. Exex function with covariates of age, sex, medu1
exemodel <- gam(F1_Exec_Comp_Res_Accuracy ~ s(age) + sex + medu1 + ga, method = "REML", data = data.NMF)
plotdata <- visreg(exemodel,'ga',type = "conditional",scale = "linear", plot = FALSE)
smooths <- data.frame(Variable = plotdata$meta$x, 
                      x=plotdata$fit[[plotdata$meta$x]], 
                      smooth=plotdata$fit$visregFit, 
                      lower=plotdata$fit$visregLwr, 
                      upper=plotdata$fit$visregUpr)
predicts <- data.frame(Variable = "dim1", 
                       x=plotdata$res$ga,
                       y=plotdata$res$visregRes)
colpal <- c("Purples","Blues","Oranges","Reds")
colkey <- brewer.pal(8,colpal[3])[c(3:8)]
p_text <- paste("p < 0.05")
exe_age<-ggplot() +
  geom_point(data = predicts, aes(x, y), color = colkey[2],alpha= 1  ) +
  geom_line(data = smooths, aes(x = x, y = smooth), colour = colkey[5],size=1.2) +
  geom_line(data = smooths, aes(x = x, y=lower), linetype="dashed", colour = colkey[4], alpha = 0.9, size = 0.9) + 
  geom_line(data = smooths, aes(x = x, y=upper), linetype="dashed",colour = colkey[4], alpha = 0.9, size = 0.9) +
  annotate("text",x = -Inf, y = Inf, hjust = -0.1,vjust = 1,label = p_text, parse=TRUE,size = 5, colour = "black",fontface ="italic" ) +
  theme(legend.position = "none") +
  labs(x = "Gestational Age (Weeks)", y = "Executive Function Score (z)") 
