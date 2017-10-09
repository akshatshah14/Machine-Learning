#===============================================================================================================================
#Introduction to R Programming
#Machine Learning for Data Science--spring 2017
#Instructor: Nabil R. Adam (adam@adam.rutgers.edu)
#Author: debopriya.ghosh@rutgers.edu
#Last Modified On:
#===============================================================================================================================
#initialization
require('e1071')
data(iris)
dim(iris)


set.seed(500)

#sample 50%, 75%, and 85% of the data for training. 
idxTrain <- sample(nrow(iris),as.integer(nrow(iris)*0.50))
train.iris = iris[idxTrain,]
test.iris = iris[-idxTrain,]

colnames(iris)

start <- proc.time()
model = svm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width , data = train.iris)
pred = predict(model,test.iris[,-5])
proc.time()-start

accuracy = sum(diag(table(test.iris$Species,pred)))/nrow(test.iris)

#==========================================================================================================================