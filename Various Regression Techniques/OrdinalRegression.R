birthweight = read.csv("C:\\Users\\Akshat\\Dropbox\\Spring 2017- ML for Data Science\\Datasets\\lowbwt.csv");
colnames(birthweight)
birthweight$WT = 0

birthweight[birthweight$BWT >0 & birthweight$BWT <2500,]$WT = 4
birthweight[birthweight$BWT >=2500 & birthweight$BWT <3000,]$WT = 3
birthweight[birthweight$BWT >=3000 & birthweight$BWT <3500,]$WT = 2
birthweight[birthweight$BWT >=3500,]$WT = 1

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

polr.fit = polr(as.factor(WT) ~ AGE + LWT + as.factor(RACE) + as.factor(SMOKE) + as.factor(PTL) + as.factor(HT) + as.factor(UI) + as.factor(FTV), data = birthweight, Hess=TRUE)
summary(polr.fit)

(ctable <- coef(summary(polr.fit)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))

ci = confint.default(polr.fit)

## odds ratios
exp(coef(polr.fit))

## OR and CI
exp(cbind(OR = coef(polr.fit), ci))

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s <- with(birthweight, summary(as.numeric(WT) ~ AGE + LWT + as.factor(RACE) + as.factor(SMOKE) + as.factor(PTL) + as.factor(HT) + as.factor(UI) + as.factor(FTV), fun=sf)))


#evaluate the parallel slopes assumption 
glm(I(as.numeric(WT) >= 2) ~ AGE + LWT + as.factor(RACE) + as.factor(SMOKE) + as.factor(PTL) + as.factor(HT) + as.factor(UI) + as.factor(FTV), family="binomial", data = birthweight)
glm(I(as.numeric(WT) >= 3) ~ AGE + LWT + as.factor(RACE) + as.factor(SMOKE) + as.factor(PTL) + as.factor(HT) + as.factor(UI) + as.factor(FTV), family="binomial", data = birthweight)

glm(I(as.numeric(WT) >= 4) ~ AGE + LWT + as.factor(RACE) + as.factor(SMOKE) + as.factor(PTL) + as.factor(HT) + as.factor(UI) + as.factor(FTV), family="binomial", data = birthweight)


s[,5] = s[,5]-s[,4]
s[,4] = s[,4]-s[,3]
s[,3] = s[,3]-s[,3]
s
