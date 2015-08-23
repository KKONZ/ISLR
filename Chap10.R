# Chapter 10 Lab 1: Principal Components Analysis

states=row.names(USArrests)
states

names(USArrests)

#Check vairables mean (2 indicates checking columns, 1 would be rows)
apply(USArrests, 2, mean)

#Check variables variation
apply(USArrests, 2, var)

# variance are quite different, so we will standardize the data
pr.out=prcomp(USArrests, scale=TRUE)
# The scale option indicates that we would like a standard deviation of 1

names(pr.out)
#Center and scale components correspond to the means and standard deciations of the variables that were used
# for scaling prior to implementing PCA
pr.out$center
pr.out$scale

# The rotation matrix provides the principal compenent loandings
pr.out$rotation

# x has its own PC score vectors
dim(pr.out$x)
biplot(pr.out, scale=0)

# The scale=0 argument ensures that the arrows are scaled to represent the loadings
# PC are only unique up to a sign change
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

# sd for each principal component
pr.out$sdev

# var explained by each principal componenet obtained by squaring these:
pr.var=pr.out$sdev^2
pr.var

# prop of var explained by each PC, divide the var by total var explained by all four PCs
pve=pr.var/sum(pr.var)
pve
# see that 62% of the variance in the data is explained by the first PC

# plot PVE
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
a=c(1,2,8,-3)

# computes the cumulative sum of the elements of a numeric vector:
cumsum(a)


# Chapter 10 Lab 2: Clustering
# K-Means Clustering

set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# perform K-means cluster with k=2
km.out=kmeans(x,2,nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

# If there were more than 2 variables then we could have used PCA instead and plot the first 2 
# PC scores

# Because we generated the data we know there are 2 clusters
# however, with real data we wouldn't know, this is what would happen if we choose K=3 instead


set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

# the nstart argument is greater than 1 than the 1st step of the algorithm will be perfomed 
# using multiple random assignements
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

# Hierarchical Clustering

# dist() in this example is used to comppute the 50x50 inter-observation Euclidean distance matrix
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

# plot heir cluster, numbers at bottom of plot identify each observation
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

# determine the cluster labels for each observation for a given cut of the dendogram use cutree()
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
# single linkage identifies one point as belonging to its own cluster. 

# A more sensilbe answer is obtained when four clusters are selected:
cutree(hc.single, 4)

# Scale variables
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")


# as.dist() function converts an arbitrary square symmetric matrix into a form that hclust() 
# function recognizes as a distance matrix. This only makes sense for data with at least three features
# since the absolute correlation between any 2 obsvs with measurements on 2 features is always 1
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")


# Chapter 10 Lab 3: NCI60 Data Example

# The NCI60 data
# genomic data
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
# 64 cancer cell lines and 6830 gene expression measurements
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# PCA on the NCI60 Data

pr.out=prcomp(nci.data, scale=TRUE)

# plot PC score vectors in order to visualize
# assigns a distinct color to each of the 64 cell lines
# (rainbow function takes as its argument a positive integer, returns vector of number of distinct colros)
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")

# get the PVE
summary(pr.out)

# plot PVE of first few PCs
plot(pr.out)
# height of each bar is obtained by squaring the corresponding element of pr.out$sdev

# more informative to plot PVE of each PC (scree plot) and cumulative PVE 
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")

# Clustering the Observations of the NCI60 Data

sd.data=scale(nci.data)

# hier clustering using complete, single, and average linkage
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")
# can see that linkage highly affects results

# single linkage will tend to yield 'trailing' clusters, individual obsvers attached 1 by 1
# complete and average linkage tend to yeild more balanced and attractive clusters


# Can cut the dendogram at the height that will yeild a particular number of clusters, say four
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)
# clear pattern, notice that all of lukemia falls into cluster 3

par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")
# Gives a brief summary of the object
hc.out

# compare heir clustering with K-means clustering with K=4
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)
# we can see that cluster 2 is identical to both methods, however the rest of the clusters are different


# Instead of creating hier clusters on the entire data, we can instead use the first few PCs
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)
