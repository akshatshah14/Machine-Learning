require(MASS)
#Attribute Information:
 # Binary outcome variable: default payment (Yes = 1, No = 0), as the response variable.
#Explanatory variables:
#  X1: Amount of the given credit (dollar)
#X2: Gender (1 = male; 2 = female).
#X3: Education (1 = graduate school; 2 = university; 3 = high school; 4 = others).
#X4: Marital status (1 = married; 2 = single; 3 = others).
#X5: Age (year).
#X6 - X11: History of past payment (from April to September, 2005). The measurement scale for the
#repayment status is: -1 = pay duly; 1 = payment delay for one month; 2 = payment delay for two months; .
#. .; 8 = payment delay for eight months; 9 = payment delay for nine months and above.
#X12-X17: Amount of bill statement (dollar) (from April to September, 2005).
#X18-X23: Amount of previous payment (dollar) (from April to September, 2005).

DefaultCredit <- read.csv("C:/Users/Akshat/Desktop/Machine Learning/DefaultCredit.csv",na.strings=c("NA","NaN", " "), header = TRUE)
summary(DefaultCredit)

DefaultCredit = DefaultCredit[-1,]
summary(DefaultCredit)

#dropping levels with 0 factor variables
cols.num <- c("X","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X21","X22","X23","Y")
DefaultCredit[cols.num] <- lapply(DefaultCredit[cols.num],droplevels)

#Dropping column 1
DefaultCredit=DefaultCredit[,2:25]
summary(DefaultCredit)
#X3 only takes 1,2,3,4 anything apart from that is converted to 4 (others)
levels(DefaultCredit$X3)[levels(DefaultCredit$X3) == "0"] = "4"
levels(DefaultCredit$X3)[levels(DefaultCredit$X3) == "5"] = "4"
levels(DefaultCredit$X3)[levels(DefaultCredit$X3) == "6"] = "4"

#converting values of 0,-2 from X6-X11 to NA and then removing them
levels(DefaultCredit$X6)[levels(DefaultCredit$X6)== "0"] = NA
levels(DefaultCredit$X7)[levels(DefaultCredit$X7)== "0"] = NA
levels(DefaultCredit$X8)[levels(DefaultCredit$X8)== "0"] = NA
levels(DefaultCredit$X9)[levels(DefaultCredit$X9)== "0"] = NA
levels(DefaultCredit$X10)[levels(DefaultCredit$X10)== "0"] = NA
levels(DefaultCredit$X11)[levels(DefaultCredit$X11)== "0"] = NA


levels(DefaultCredit$X6)[levels(DefaultCredit$X6)== "-2"] = NA
levels(DefaultCredit$X7)[levels(DefaultCredit$X7)== "-2"] = NA
levels(DefaultCredit$X8)[levels(DefaultCredit$X8)== "-2"] = NA
levels(DefaultCredit$X9)[levels(DefaultCredit$X9)== "-2"] = NA
levels(DefaultCredit$X10)[levels(DefaultCredit$X10)== "-2"] =NA
levels(DefaultCredit$X11)[levels(DefaultCredit$X11)== "-2"] = NA

#Converting 0 to 3(others)
levels(DefaultCredit$X4)[levels(DefaultCredit$X4)=="0"] ="3"
summary(DefaultCredit)
DefaultCredit = na.omit(DefaultCredit)
dim(DefaultCredit)

#converting all the factor variables to numeric 
DefaultCredit$X1 = as.numeric(as.character(DefaultCredit$X1))
DefaultCredit$X5 = as.numeric(as.character(DefaultCredit$X5))
DefaultCredit$X12 = as.numeric(as.character(DefaultCredit$X12))
DefaultCredit$X13 = as.numeric(as.character(DefaultCredit$X13))
DefaultCredit$X14 = as.numeric(as.character(DefaultCredit$X14))
DefaultCredit$X15 = as.numeric(as.character(DefaultCredit$X15))
DefaultCredit$X16 = as.numeric(as.character(DefaultCredit$X16))
DefaultCredit$X17 = as.numeric(as.character(DefaultCredit$X17))
DefaultCredit$X18 = as.numeric(as.character(DefaultCredit$X18))
DefaultCredit$X19 = as.numeric(as.character(DefaultCredit$X19))
DefaultCredit$X20 = as.numeric(as.character(DefaultCredit$X20))
DefaultCredit$X21 = as.numeric(as.character(DefaultCredit$X21))
DefaultCredit$X22 = as.numeric(as.character(DefaultCredit$X22))
DefaultCredit$X23 = as.numeric(as.character(DefaultCredit$X23))
summary(DefaultCredit)


set.seed(5000)
idxTrain <- sample(nrow(DefaultCredit),as.integer(nrow(DefaultCredit)*0.75))
train.DefaultCredit = DefaultCredit[idxTrain,]
test.DefaultCredit  = DefaultCredit[-idxTrain,]


glm_null= glm(Y ~ 1 , data = train.DefaultCredit,family = "binomial" )
glm_model = glm(Y ~ ., data = train.DefaultCredit,family = "binomial")

#Forward model
forward_model = stepAIC(glm_null, direction='forward', scope=list(lower=glm_null,upper=glm_model))
forward_model_aic = forward_model$aic
forward_model_aic
forward_model_r2=1- (forward_model$deviance/forward_model$null.deviance)
forward_model_r2

#Error rate of testing data in forward model 
predict_forward_test = predict(forward_model,test.DefaultCredit[,-24],type="response",data= train.DefaultCredit)
predict_forward_test = ifelse(predict_forward_test < 0.5 , 0, 1)
table(predict_forward_test, test.DefaultCredit$Y)
forward_test_error_rate = mean(predict_forward_test!=test.DefaultCredit$Y)
forward_test_error_rate

#Error Rate of training data in forward model
predict_forward_train = predict(forward_model,train.DefaultCredit[,-24],type="response")
predict_forward_train = ifelse(predict_forward_train<0.5,0,1)
table(predict_forward_train, train.DefaultCredit$Y)
forward_train_error_rate = mean(predict_forward_train!=train.DefaultCredit$Y)
forward_train_error_rate

#Backward model
backward_model = stepAIC(glm_model,scope=list(lower=glm_null) ,direction='backward', data= train.DefaultCredit)
backward_model_aic=backward_model$aic
backward_model_aic
backward_model_r2 = 1 - (backward_model$deviance/backward_model$null.deviance)
backward_model_r2

#Backward model test error rate
predict_backward = predict(backward_model,test.DefaultCredit,type = "response")
predict_backward = ifelse(predict_backward <0.5,0,1)
table(predict_backward,test.DefaultCredit$Y)
backward_test_error_rate = mean(predict_backward!=test.DefaultCredit$Y)
backward_test_error_rate

#Backward model train error rate
predict_backward_train = predict(backward_model,train.DefaultCredit[,-24],type = "response")
predict_backward_train = ifelse(predict_backward_train <0.5,0,1)
table(predict_backward_train,train.DefaultCredit$Y)
backward_train_error_rate=mean(predict_backward_train!=train.DefaultCredit$Y)
backward_train_error_rate

#Both model

Both_model = stepAIC(glm_null,scope= list(upper=glm_model), direction='both', data = train.DefaultCredit)
Both_model_aic = Both_model$aic
Both_model_aic
Both_model_r2 = 1 - (Both_model$deviance/Both_model$null.deviance)
Both_model_r2
#both model test error rate
predict_both_test = predict(Both_model,test.DefaultCredit[,-24],type="response")
predict_both_test = ifelse(predict_both_test < 0.5 , 0, 1)
table(predict_both_test, test.DefaultCredit$Y)
both_model_test_error_rate = mean(predict_both_test!=test.DefaultCredit$Y)
both_model_test_error_rate
#both mode train test error rate
predict_both_train = predict(Both_model,train.DefaultCredit[,-24],type="response")
predict_both_train = ifelse(predict_both_train < 0.5 , 0, 1)
table(predict_both_train, train.DefaultCredit$Y)
both_model_train_error_rate = mean(predict_both_train!=train.DefaultCredit$Y)
both_model_train_error_rate

#subset selection
#install.packages("leaps")
require(leaps)
sub_model = regsubsets(Y~ ., data =train.DefaultCredit, nbest = 10, really.big = T)
plot(sub_model)


#Test error rate 
forward_test_error_rate
backward_test_error_rate
both_model_test_error_rate

#the test error rate for forward is 0.2175197 , for backward = 0.2116142 and for both =0.2175197

#Train error rate
forward_train_error_rate
backward_train_error_rate
both_model_train_error_rate
#The train error rate for forward = 0.2144499 , backward= 0.216092, both = 0.2144499

#AIC
forward_model_aic
backward_model_aic
Both_model_aic
#The AIC for forward= 2984.49, backward= 2985.204, both = 2984.49 

#r-square value
forward_model_r2
backward_model_r2
Both_model_r2
#the R^2 value for forward=0.2630634 , backward=0.2643982, both = 0.2630634.

#_______________________________________________________________________#
#Direction | Test error rate | Train error rate | AIC       | R2        |
#_______________________________________________________________________#
# Forward  |  0.2175197      |  0.2144499       | 2984.49   | 0.2630634 |
# Backward |  0.2116142      |  0.216092        | 2985.204  | 0.2643982 |
# Both     |  0.2175197      |  0.2144499       | 2984.49   | 0.2630634 |
#_______________________________________________________________________#

#The final model that we have the follwoing variables while using the forward, backward and Both approach
#Forward
#Y ~ X6 + X10 + X19 + X18 + X5 + X3 + X22 + X7 + X1 + X13 

#Backward
#Y ~ X1 + X3 + X5 + X6 + X7 + X9 + X15 + X16 + X18 + X19 + X21 +  X22

#Both
#Y ~ X6 + X10 + X19 + X18 + X5 + X3 + X22 + X7 + X1 + X13

#Thus we conclude that when direction="forward" and when direction ="both" we get the same AIC values of 2984.49  which 
#is less than the AIC value of backward which is 2985.204 . Also the values are similar and there is not much significant difference between them.
#Overall when direction = forward and direction = Both would perform better.