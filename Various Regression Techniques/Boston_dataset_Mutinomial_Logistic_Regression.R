#Boston dataset
require(MASS)
head(Boston)
boston=Boston
summary(boston)

#converting chas to a factor variable as it contains values 0 and 1
boston$chas = factor(boston$chas)
#converting crim to 0 and 1 on basis of median
boston$crim = ifelse(boston$crim < median(boston$crim) , 0, 1)
#converting crim to a factor variable
boston$crim = factor(boston$crim)
summary(boston)

#plotting multinomial logistic for crim against all the variables
glm.fit = glm(crim ~ .,data = boston, family = "binomial")
summary(glm.fit)

set.seed(5000000)
idxTrain <- sample(nrow(boston),as.integer(nrow(boston)*0.75))
train.boston = boston[idxTrain,]
test.boston  = boston[-idxTrain,]

#Null deviance: 701.46  on 505  degrees of freedom
#Residual deviance: 211.93  on 492  degrees of freedom
#AIC: 239.93

#calculating accuracy for the model trained for all attributes
model = glm(crim ~ ., family="binomial",data = train.boston)
summary(model)


pred1 = predict(model,test.boston[,-1],type="response")
pred1 = ifelse(pred1 < median(Boston$crim) , 0, 1)
accuracy_75 = sum(diag(table(test.boston$crim,pred1)))/nrow(test.boston)
accuracy_75


table(pred1, test.boston$crim)

mean(pred1!= test.boston$crim)

#The accuracy that we get while using all the attribues is 81.88 and error rate is 18.11024

#Trying to predict the accuracy by using only the significant factors
model2 = glm(crim ~ zn + nox + dis + rad + tax + ptratio + black + medv, family="binomial",data = train.boston)
summary(model2)


pred2 = predict(model2,test.boston[,-1], type="response")
pred2 = ifelse(pred2 < median(Boston$crim) , 0, 1)
model2_accuracy_75 = sum(diag(table(test.boston$crim,pred2)))/nrow(test.boston)
model2_accuracy_75

table(pred2, test.boston$crim)
mean(pred2 != test.boston$crim)

