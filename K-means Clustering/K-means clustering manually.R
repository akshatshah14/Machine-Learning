#2. Perform K-means clustering manually, with K = 2, on a small example with n = 6 observations and p =
#  2 features. The observations are as follows.
#Obs. X1 X2
#1    1   4
#2    1   3
#3    0   4
#4    5   1
#5    6   2
#6    4   0  
#(a) Plot the observations.
a <- cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
plot(a[,1], a[,2])

#(b) Randomly assign a cluster label to each observation. You can use the sample() command in R to do
#this. Report the cluster labels for each observation.
set.seed(174007425)
labels <- sample(2, nrow(a), replace = T)
labels
plot(a[, 1], a[, 2], col = (labels + 1), pch = 20, cex = 2)

#(c) Compute the centroid for each cluster.
#compute for the green cluster

a11=(0+1+5)/3
a12=(3+4+1)/3
a21=(1+6+4)/3
a22=(4+2+0)/3

#compute for the red cluster

c1 <- c(mean(a[labels == 1, 1]), mean(a[labels == 1, 2]))
c2 <- c(mean(a[labels == 2, 1]), mean(a[labels == 2, 2]))
plot(a[,1], a[,2], col=(labels + 1), pch = 20, cex = 2)
points(c1[1], c1[2], col = 2, pch = 4)
points(c2[1], c2[2], col = 3, pch = 4)

#(d) Assign each observation to the centroid to which it is closest, in terms of Euclidean distance. Report
#the cluster labels for each observation.
labels <- c(1, 1, 1, 2, 2, 2)
plot(a[, 1], a[, 2], col = (labels + 1), pch = 20, cex = 2)
points(c1[1], c1[2], col = 2, pch = 4)
points(c2[1], c2[2], col = 3, pch = 4)
#(e) Repeat (c) and (d) until the answers obtained stop changing.
c1 <- c(mean(a[labels == 1, 1]), mean(a[labels == 1, 2]))
c2 <- c(mean(a[labels == 2, 1]), mean(a[labels == 2, 2]))
plot(a[,1], a[,2], col=(labels + 1), pch = 20, cex = 2)
points(c1[1], c1[2], col = 2, pch = 4)
points(c2[1], c2[2], col = 3, pch = 4)
#(f) In your plot from (a), color the observations according to the cluster labels obtained.
plot(a[, 1], a[, 2], col=(labels + 1), pch = 20, cex = 2)
