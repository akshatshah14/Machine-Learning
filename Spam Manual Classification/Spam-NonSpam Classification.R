
#SPAM/NON-SPAM CLASSIFICATION

#initialization
#building the model
model = function(df)
{
  return(1+x1+x2^2+x1^3+x2^3)
}
# specifying threshold
threshold = 4



# genertating train data
x1 = c(1,0,1,0,0,1,1,1,0,1)
x2 = c(1,1,1,0,0,0,1,1,0,0)
df=data.frame(cbind(x1,x2))
df


df = cbind(df,z=model(df))

#defining the label: spam(0/1)
df$spam = 0


df[df$z > threshold,]$spam = 1
# this completes training data generation

df

#generate test data
x1.t= sample(c(0,1), 20 , replace = TRUE)
x2.t = sample(c(0,1), 20 , replace = TRUE)
test = data.frame(cbind(x1.t,x2.t))
test
#predict on test data 
predict = function(model,test)
{
  ytest = rep(0,nrow(test))
  ztest = model(test)
    ytest[ztest>threshold] = 1
  return(ytest)
  
}

predict(model,test)

#======================================================================================================================================================================
