#===============================================================================================================================
#Multivariate Regression
#Machine Learning for Data Science--spring 2017
#Instructor: Nabil R. Adam (adam@adam.rutgers.edu)
#Author: debopriya.ghosh@rutgers.edu
#Last Modified On:
#===============================================================================================================================
install.packages("mlbench")
library(mlbench)
data(BreastCancer)

dim(BreastCancer)

colnames(BreastCancer)

levels(BreastCancer$Class)

summary(BreastCancer)

BreastCancer = BreastCancer[complete.cases(BreastCancer),]
BreastCancer[,2:10] = apply(BreastCancer[,2:10],2, as.numeric)


# build multivariate logistic regression model

glm.fit = glm(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + 
           Epith.c.size + Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses,
           data = BreastCancer, family = "binomial");

summary(glm.fit)
