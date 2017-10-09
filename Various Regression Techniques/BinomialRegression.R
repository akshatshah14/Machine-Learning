
#===============================================================================================================================
#Binomial Regression
#Machine Learning for Data Science--spring 2017
#Instructor: Nabil R. Adam (adam@adam.rutgers.edu)
#Author: debopriya.ghosh@rutgers.edu
#Last Modified On:
#===============================================================================================================================



install.packages("ISLR");
library(ISLR)

# binding the data
data("Default")
dim(Default)

colnames(Default)

summary(Default)

# building the glm model
glm.fit = glm(default ~ income, data = Default, family = "binomial")
summary(glm.fit)

#Call:
#glm(formula = default ~ income, family = "binomial", data = Default)

#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-0.2968  -0.2723  -0.2576  -0.2478   2.7111  

#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -3.094e+00  1.463e-01 -21.156   <2e-16 ***
#income      -8.353e-06  4.207e-06  -1.985   0.0471 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 2920.6  on 9999  degrees of freedom
#Residual deviance: 2916.7  on 9998  degrees of freedom
#AIC: 2920.7

#Number of Fisher Scoring iterations: 6

# finding the correlation between income and balance
cor.test(Default$balance, Default$income)
plot( Default$balance, Default$income, col = Default$default)

