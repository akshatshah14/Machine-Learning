

# NCI60 cancer cell line microarray data, which consists of 6,830 gene expression 
# measurements on 64 cancer cell lines. Each cell line is labeled with a cancer type.
# The data has 64 rows and 6,830 columns.

library (ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

dim(nci.data)

# examining the cancer types for the cell lines
table(nci.labs)
#==============================================================================================
# We first perform PCA on the data after scaling the variables (genes) to
# have standard deviation one
pr.out =prcomp (nci.data , scale=TRUE)

# # The function will be used to assign a color to each of
# the 64 cell lines, based on the cancer type to which it corresponds.

Cols=function (vec ){
  cols=rainbow (length (unique (vec )));
  return (cols[as.numeric (as.factor (vec))]);
}

# plot the principal component score vectors
par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col =Cols(nci .labs), pch =19,
       xlab ="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3) ], col =Cols(nci.labs), pch =19,
       xlab ="Z1",ylab="Z3")

# obtain a summary of the proportion of variance explained (PVE)
# of the first few principal components using the summary() method for a
# prcomp object
summary (pr.out)

# Using the plot() function, we can also plot the variance explained by the
# first few principal components
plot(pr.out) # Note that the height of each bar in the bar plot is given by squaring the
             # corresponding element of pr.out$sdev
#===================================================================================================
# Clustering the Observations of the NCI60 Data
sd.data=scale(nci.data)

par(mfrow =c(1,3))
data.dist=dist(sd.data)

# performing hierarchical clustering
# Complete linkage
plot(hclust (data.dist), labels =nci.labs , main=" Complete
Linkage ", xlab ="", sub ="", ylab ="")

#Average Linkage
plot(hclust (data.dist , method ="average"), labels =nci.labs ,
       main=" Average Linkage ", xlab ="", sub ="", ylab ="")

#Single Linkage
plot(hclust (data.dist , method ="single"), labels =nci.labs ,
       main=" Single Linkage ", xlab="", sub ="", ylab ="")

# NOTE: the choice of linkage certainly does affect the results obtained. 
# Typically, single linkage will tend to yield trailing clusters: very large 
# clusters onto which individual observations attach one-by-one. On the other hand,
# complete and average linkage tend to yield more balanced, attractive clusters. 
# For this reason, complete and average linkage are generally preferred to single linkage.

# Inference: Clearly cell lines within a single cancer type do tend to cluster together, 
# although the clustering is not perfect.


# We can cut the dendrogram at the height that will yield a particular number of clusters, k=4 
hc.out =hclust (dist(sd.data))
hc.clusters =cutree (hc.out ,4)
table(hc.clusters ,nci.labs)

# There are some clear patterns. All the leukemia cell lines fall in cluster 3,
# while the breast cancer cell lines are spread out over three different clusters.

par(mfrow =c(1,1))
plot(hc.out , labels =nci.labs)
abline (h=139, col =" red ")

# How do these NCI60 hierarchical clustering results compare to what we get if we 
# perform K-means clustering with K = 4??

set.seed (2)
km.out =kmeans (sd.data , 4, nstart =20)
km.clusters =km.out$cluster
table(km.clusters ,hc.clusters )

# We see that the four clusters obtained using hierarchical clustering and Kmeans
# clustering are somewhat different. Cluster 2 in K-means clustering is
# identical to cluster 3 in hierarchical clustering. However, the other clusters differ.
#=================================================================================================

# Rather than performing hierarchical clustering on the entire data matrix,
# we can simply perform hierarchical clustering on the first few principal
# component score vectors, as follows:

hc.out =hclust (dist(pr.out$x [ ,1:5]) )
plot(hc.out , labels =nci.labs , main=" Hier. Clust . on First Five Score Vectors ")
table(cutree (hc.out ,4) , nci.labs)
#==================================================================================================