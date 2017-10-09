#ML Assignment :2
#Akshat Shah

#Mushroom dataset

require(faraway)
require('e1071')
mushrooms <- read.csv("C:/Users/Akshat/Downloads/mushrooms.csv")
dim(mushrooms)

set.seed(174007425)
summary(mushrooms)
#Part 1
#sample 50% of the data for training.
idxTrain <- sample(nrow(mushrooms),as.integer(nrow(mushrooms)*0.50))
train.mushrooms = mushrooms[idxTrain,]
test.mushrooms  = mushrooms[-idxTrain,]
colnames(mushrooms)
start <- proc.time()
model = svm(class ~ cap.shape + cap.surface + cap.color+ bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.color + ring.number + ring.type + spore.print.color + population + habitat , data = train.mushrooms)
pred = predict(model,test.mushrooms[,-1])
proc.time()-start
accuracy_50 = sum(diag(table(test.mushrooms$class,pred)))/nrow(test.mushrooms)
accuracy_50

#sample 60% of the data for training.
set.seed(174007425)
idxTrain <- sample(nrow(mushrooms),as.integer(nrow(mushrooms)*0.60))
train.mushrooms = mushrooms[idxTrain,]
test.mushrooms  = mushrooms[-idxTrain,]
colnames(mushrooms)
start <- proc.time()
model = svm(class ~ cap.shape + cap.surface + cap.color+ bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.color + ring.number + ring.type + spore.print.color + population + habitat , data = train.mushrooms)
pred = predict(model,test.mushrooms[,-1])
proc.time()-start
accuracy_60 = sum(diag(table(test.mushrooms$class,pred)))/nrow(test.mushrooms)

#sample 80% of the data for training.
set.seed(174007425)
idxTrain <- sample(nrow(mushrooms),as.integer(nrow(mushrooms)*0.80))
train.mushrooms = mushrooms[idxTrain,]
test.mushrooms  = mushrooms[-idxTrain,]
colnames(mushrooms)
start <- proc.time()
model = svm(class ~ cap.shape + cap.surface + cap.color+ bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.color + ring.number + ring.type + spore.print.color + population + habitat , data = train.mushrooms)
pred = predict(model,test.mushrooms[,-1])
proc.time()-start
accuracy_80 = sum(diag(table(test.mushrooms$class,pred)))/nrow(test.mushrooms)


#-----------------------------------------------------------------------------
#Length of Each training set	|Time taken for the classifier |	Accuracy      |
#-----------------------------|------------------------------|----------------|
#     50	                    |   1.77                       |	 0.9921221    |
#     60	                    |   2.05	                     |   0.9963077    |
#     80	                    |   2.63		                   |   0.9981538    |
#-----------------------------|------------------------------|----------------|

# When the length of training set is 50% the time taken by the system is 1.77 and accuracy is 0.9921221
# When the length of training set is 60% the time taken by the system is 2.05 and accuracy is 0.9963077
# When the length of training set is 80% the time taken by the system is 2.63 and accuracy is 0.9981538
# We see that as the length of the training set is increasing the time taken increases also the accuracy is seen increasing 



#SVM on a different dataset
#Pima dataset

#50%
set.seed(174007425)
data(pima)
summary(pima)
pima$test = as.factor(pima$test)
summary(pima)
idxTrain <- sample(nrow(pima),as.integer(nrow(pima)*0.50))
train.pima = pima[idxTrain,]
test.pima  = pima[-idxTrain,]

colnames(pima)

start <- proc.time()
model = svm(test ~ pregnant + glucose + diastolic + triceps + insulin + bmi + diabetes + age , data = train.pima)
pred = predict(model,test.pima[,-9])
proc.time()-start

accuracy50 = sum(diag(table(test.pima$test,pred)))/nrow(test.pima)
accuracy50

#60%
set.seed(174007425)
summary(pima)
pima$test = as.factor(pima$test)
idxTrain <- sample(nrow(pima),as.integer(nrow(pima)*0.60))
train.pima = pima[idxTrain,]
test.pima  = pima[-idxTrain,]
colnames(pima)

start <- proc.time()
model = svm(test ~ pregnant + glucose + diastolic + triceps + insulin + bmi + diabetes + age , data = train.pima)
pred = predict(model,test.pima[,-9])
proc.time()-start

accuracy60 = sum(diag(table(test.pima$test,pred)))/nrow(test.pima)
accuracy60
#80%
set.seed(174007425)
summary(pima)
pima$test = as.factor(pima$test)
idxTrain <- sample(nrow(pima),as.integer(nrow(pima)*0.80))
train.pima = pima[idxTrain,]
test.pima  = pima[-idxTrain,]
colnames(pima)

start <- proc.time()
model = svm(test ~ pregnant + glucose + diastolic + triceps + insulin + bmi + diabetes + age , data = train.pima)
pred = predict(model,test.pima[,-9])
proc.time()-start

accuracy80 = sum(diag(table(test.pima$test,pred)))/nrow(test.pima)
accuracy80


#------------------------------------------------------------------------------
#Length of Each training set	|Time taken for the classifier |	Accuracy      |
#-----------------------------|------------------------------|----------------|
#     50	                    |   0.06                       |	 0.7682292    |
#     60	                    |   0.07	                     |   0.7857143    |
#     80	                    |   0.09		                   |   0.8246753    |
#-----------------------------|------------------------------|----------------|


# When the length of training set is 50% the time taken by the system is 0.06 and accuracy is 0.7682292
# When the length of training set is 60% the time taken by the system is 0.07 and accuracy is 0.7857143
# When the length of training set is 80% the time taken by the system is 0.09 and accuracy is 0.8246753
# We see that as the length of the training set is increasing the time taken increases also the accuracy is seen increasing.