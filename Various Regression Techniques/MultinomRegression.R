
# loading the data
Diabetes = read.csv("C:\\Users\\Debopriya\\Dropbox\\Spring 2017- ML for Data Science\\Datasets\\dataset_diabetes\\dataset_diabetes\\diabetic_data.csv", na.strings = "?");

dim(Diabetes)
colnames(Diabetes)
summary(Diabetes)

# check which columns have missing values , is.na returns 1 if TRUE else 0
cols = colnames(Diabetes)[colSums(is.na(Diabetes)) > 0]

#exclude columns with missing values
Diabetes = Diabetes[,!(colnames(Diabetes) %in% cols)]

#===============================================================================================================================
#Multinomial Regression
#Machine Learning for Data Science--spring 2017
#Instructor: Nabil R. Adam (adam@adam.rutgers.edu)
#Author: debopriya.ghosh@rutgers.edu
#Last Modified On:
#===============================================================================================================================
Diabetes =read.csv("C:\\Users\\Debopriya\\Dropbox\\Spring 2017- ML for Data Science\\Datasets\\dataset_diabetes\\dataset_diabetes\\diabetic_data.csv",na.strings = '?')
dim(Diabetes)
summary(Diabetes)
colnames(Diabetes)

cols = colnames(Diabetes)[colSums(is.na(Diabetes))>0]

Diabetes = Diabetes[,!(colnames(Diabetes)%in% cols)]
dim(Diabetes)

#checks for complete cases to verify no missing values present
Diabetes.complete = Diabetes[complete.cases(Diabetes),]
dim(Diabetes.complete)



#building multinomial model
install.packages("foreign")
install.packages("nnet")
install.packages("ggplot2")
install.packages("reshape2")


require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


multinom.fit = multinom(readmitted ~ . ,data = Diabetes.complete[,-c(1,2,33,34)],family = "multinomial")
summary(multinom.fit)


# computing coeff, std error, z valu, and p-values and tabulating the results
z=  summary(multinom.fit)$coefficients/summary(multinom.fit)$standard.errors
p = (1-pnorm(abs(z),0,1))*2
coeff = summary(multinom.fit)$coefficients
std.err = summary(multinom.fit)$standard.errors
t = cbind(t(coeff),t(std.err),t(z),t(p))
t= unique(t)
write.csv(t,"C:\\Users\\Debopriya\\Dropbox\\Spring 2017- ML for Data Science\\Datasets\\dataset_diabetes\\dataset_diabetes\\results_multinom.csv")
