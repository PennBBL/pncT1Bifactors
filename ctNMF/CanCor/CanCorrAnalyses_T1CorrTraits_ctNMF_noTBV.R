############################################################
#### Canonical Correlation MODELS FOR T1 BIFACTOR STUDY ####
############################################################


#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1396_T1_subjData.rds")

#install packages 
#install.packages("CCA")
#install.packages("GGally")

#Load library
library(ggplot2)
library(GGally)
library(CCA)

#Get NMF variable data {LOOK AT JLF SCRIPT FOR HOW TO PULL DATA NOT NAMES}
nmfComponents <- data.NMF[grep("Nmf18",names(data.NMF))]

#Get Bifactor data
corrtraits <- data.NMF[grep("corrtraitsv2",names(data.NMF))]

#Canonical correlation
cc1 <- cc(nmfComponents, corrtraits)

#display the canonical correlations 
cc1$cor

#raw canonical coefficient
cc1[3:4]

# compute canonical loadings
cc2 <- comput(nmfComponents, corrtraits, cc1)
# display canonical loadings
cc2[3:6]

####################################################################################
# tests of canonical dimensions
# Written by (https://stats.idre.ucla.edu/r/dae/canonical-correlation-analysis/)
####################################################################################

ev <- (1 - cc1$cor^2)

n <- dim(nmfComponents)[1]
p <- length(nmfComponents)
q <- length(corrtraits)
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

# standardized nmfComponent canonical coefficients diagonal matrix of nmfComponent sd's
s1 <- diag(sqrt(diag(cov(nmfComponents))))
s1 %*% cc1$xcoef

# standardized corrtraits canonical coefficients diagonal matrix of corrtraits sd's
s2 <- diag(sqrt(diag(cov(corrtraits,use="complete.obs"))))
s2 %*% cc1$ycoef
##what do we do with NA in corrtraits?
#length(which(is.na(corrtraits))) --> 10

######################################################################






#using Cancor function

#library("candisc") --> having trouble at this step
#cc<-cancor(nmfComponents,corrtraits,na.rm=TRUE)

#coef(cc,type="both",standardize=TRUE)
#plot(cc,smooth=TRUE)
#heplot(cc,xpd=TRUE)
