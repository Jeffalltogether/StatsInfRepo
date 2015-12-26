##Homework 4
library(datasets)

#problem 1
MPGmu <- mean(mtcars[,1])
MPGmu + c(-1) * qnorm(0.05)*sd(DT[,1])/sqrt(length(DT[,1]))


#problem 2
cyl6 <- mtcars$mpg[mtcars$cyl==6]
cyl4 <- mtcars$mpg[mtcars$cyl==4]
t.test(cyl4, cyl6, paried=FALSE, alternative="two.sided", var.equal=FALSE)$p.value

#problem 3
n=100
PSA=3.0
sd=1.1

PSA + c(-1,1) * qnorm(0.025)*sd/sqrt(n)

#problem 4
n=100
x=55
alpha=0.05
#using `binom.test`
binom.test(55,100,0.5,alternative="greater")
#OR
#Using `pbinom`
##Note you have to start at 54 as it lower.tail = FALSE gives the strictly greater than probabilities
round(pbinom(54, prob = .5, size = 100, lower.tail = FALSE),4)
