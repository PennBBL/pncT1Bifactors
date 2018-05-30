############################################################
#### Canonical Correlation MODELS FOR T1 BIFACTOR STUDY ####
############################################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#install packages 
#install.packages("CCA")
#install.packages("GGally")

#Load library
library(ggplot2)
library(GGally)
library(CCA)

#Get NMF variable data
nmfComponents <- data.NMF[grep("Ct_Nmf18",names(data.NMF))]

#Get Bifactor data
bifactors <- data.NMF[grep("4factorv2",names(data.NMF))]

#Canonical correlation
cc1 <- cc(nmfComponents, bifactors)

#Display the canonical correlations 
cc1$cor

#Raw canonical coefficients
cc1[3:4]

#Compute canonical loadings (these can be interpreted like correlations)
cc2 <- comput(nmfComponents, bifactors, cc1)

#Display canonical loadings
cc2[3:6]

####################################################################################
#Tests of canonical dimensions
#Written by (https://stats.idre.ucla.edu/r/dae/canonical-correlation-analysis/)
####################################################################################

ev <- (1 - cc1$cor^2)

n <- dim(nmfComponents)[1]
p <- length(nmfComponents)
q <- length(bifactors)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2

w <- rev(cumprod(rev(ev)))

# initialize
d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}

pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))

######################################################################

#Standardized nmfComponent canonical coefficients diagonal matrix of nmfComponent sd's
s1 <- diag(sqrt(diag(cov(nmfComponents))))
s1 %*% cc1$xcoef

#Standardized bifactors canonical coefficients diagonal matrix of bifactors sd's
s2 <- diag(sqrt(diag(cov(bifactors,use="complete.obs"))))
s2 %*% cc1$ycoef
##what do we do with NA in bifactors?
#length(which(is.na(bifactors))) --> 10

######################################################################






#using Cancor function

#install.packages("candisc")
#library(candisc) --> having trouble at this step
#cc<-cancor(nmfComponents,bifactors,na.rm=TRUE)

#coef(cc,type="both",standardize=TRUE)
#plot(cc,smooth=TRUE)
#heplot(cc,xpd=TRUE)
