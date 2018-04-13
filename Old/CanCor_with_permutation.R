num.perm <- 1000 #number of permutations to run
behavior.perm <- rlply(num.perm,data$behavior[sample(nrow(data$behavior)),]) #permute the clinical matrix by row
scca.perm.cca<-sapply(behavior.perm, function(y_perm){ out<-ccaDWpermorder(data$brain,y_perm,0.8,0.4,candnum,scca.cand)} ) #run scca again with permuted clinical but with original connectivity
#load("~/Desktop/BBL/projects/xiaNetworkCca/sCCA/aim1/result/201701/pwr_perm_cca.RData")
perm.cor <- simplify2array(scca.perm.cca['cors',]) #extract the correlations
perm.pval <- sapply(seq_along(cor.df$cor),function(x) (length(which(perm.cor[x,] >= cor.df$cor[x])) ) / length(which(is.na(perm.cor[x,]) == FALSE))) #calcualte the empirical p-val

ccaDWpermorder <- function(X,Y,pen_x,pen_y,rank,cca_org){
  perm.mode<-PMA::CCA(x=X, z=Y, typex=c("standard"),typez=c("standard"), penaltyx=pen_x, penaltyz=pen_y, K=rank, niter=20, trace=FALSE)
  perm.mode.reorder<-reorderCCA(perm.mode,cca_org,rank)
}

