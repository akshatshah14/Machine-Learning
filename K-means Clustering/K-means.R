

# preparing the data
# simulated data set containing 50 observations, in which there are two clusters;
# the first 25 observations have a mean shift relative to the next 25 observations
set.seed(123)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] = x[1:25,1] + 3
x[1:25, 2] = x[1:25,2] - 4

#================================================================================================

# performing k-means clustering with k = 2
km.out = kmeans(x,2,nstart = 20)
km.out$cluster
km.out$withinss
km.out$tot.withinss
#81.01995
plot(x,col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=2",
     xlab = "", ylab = "", pch = 20, cex = 2)


# The k-means clustering perfectly separated the observations into two clusters.
# In this example, we knew there were really two clusters because we generated the data.
# However, in real data we do not know the true number of clusters.
# So how about performing k-means clustering with k = 3?
set.seed(456)
km.out = kmeans(x,3,nstart = 20)
km.out$cluster
km.out$withinss
km.out$tot.withinss
#62.43309
#======================================================================================================
# kmeans() function can run with multiple initial configurations. If the value of nstart >1
# k-means will be performed using multiple random assignments. It is strongly recommended running
# k-means with large value of nstart, otherwise undesirable optimum will be obtained

set.seed(789)
km.out = kmeans(x,3,nstart = 1)
km.out$tot.withinss

km.out = kmeans(x,3,nstart = 20)
km.out$tot.withinss

km.out = kmeans(x,3,nstart = 50)
km.out$tot.withinss


#======================================================================================================
# Within Sum of Squares (WSS)

# Compute clustering algorithm (e.g., k-means clustering) for different values of k.
# For instance, by varying k from 1 to 10 clusters, for each k, calculate the total 
# within-cluster sum of square (wss). # Plot the curve of wss according to the number
# of clusters k.# The location of a bend (knee) in the plot is generally considered as 
# an indicator of the appropriate number of clusters.

# Compute and plot wss for k = 2 to k = 10

k.max <- 10 # Maximal number of clusters
data <- x
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 2, lty =2)


#==================================================================================================
#applying Silhoutte statistics

# Compute clustering algorithm (e.g., k-means clustering) for different values of k. 
# For instance, by varying k from 1 to 15 clusters
# For each k, calculate the average silhouette of observations (avg.sil)
# Plot the curve of avg.sil according to the number of clusters k.
# The location of the maximum is considered as the appropriate number of clusters.

# Compute the average silhouette width for 
# k = 2 to k = 15
library(cluster)
k.max = 10
sil <- rep(0, k.max)

for(i in 2:k.max){
  km.res <- kmeans(data, i, nstart = 50)
  
  ss <- silhouette(km.res$cluster, dist(data))
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)

#================================================================================================