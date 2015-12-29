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

#problem 5: poisson test
x = 15800
t = 30
r = 520

poisson.test(x,t,r,alternative = "greater", conf.level = 0.95)

#problem 6: two sample Z-test
mu0 = 10
mu1 = 11
n = 100
sigma = 4
se <- sigma*sqrt((1/n)+(1/n))

zeta = (mu1 - mu0) / se
2 * pnorm(-abs(zeta))

#problem 8: power of Z-test
z <- qnorm(1-0.05)
pnorm(mu0 + z * sigma/sqrt(n), mean = mu1, sd = sigma/sqrt(n), lower.tail=FALSE)

#problem 9: sample size of Z-test
delta = 0.01
sigma = 0.04
power = 0.80
sigLevel = 0.95

n <- (qnorm(sigLevel) + qnorm(power)) ^ 2 * sigma ^ 2 / delta^2

#problem 11
cyl6 <- mtcars$mpg[mtcars$cyl==6]
cyl8 <- mtcars$mpg[mtcars$cyl==8]
t.test(cyl8, cyl6, paried=FALSE, alternative="two.sided", var.equal=FALSE)$p.value

mu1 = mean(cyl6)
mu2 = mean(cyl8)
n1 = length(cyl6)
n2 = length(cyl8)
s1 = sd(cyl6)
s2 = sd(cyl8)

sigmaPooled <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
sePooled = sigmaPooled*sqrt((1/n1)+(1/n2))

zeta = (mu2 - mu1) / sePooled
2 * pnorm(-abs(zeta))


##Quiz Quetions
#q1

baseline <- c(140, 138, 150, 148, 135)
week2    <- c(132, 135, 151, 146, 130)
t.test(week2, baseline, paried=TRUE, alternative="less", conf.level=0.95, var.equal=TRUE)

#q2
n=9
mu = 1100
sigma=30

mu + c(-1,1) * qt(0.025,n-1)*sigma/sqrt(n)

#q3
n=4
x=3
binom.test(x,n,0.5,alternative="greater")

#q4
x = 10
t = 1787
r = 1/100

poisson.test(x,t,r,alternative = "less", conf.level = 0.95)

#q5
n1=9
n2=9
mu1=-3
mu2=1
s1=1.5
s2=1.8

sigmaPooled <- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
sePooled = sigmaPooled*sqrt((1/n1)+(1/n2))

zeta = (mu2 - mu1) / sePooled
2 * pt(-abs(zeta),(n1+n2-2))

#q7
n=100
mu0=0
mu1=0.01
sigma=0.04

z <- qnorm(1-0.05)
pnorm(mu0 + z * sigma/sqrt(n), mean = mu1, sd = sigma/sqrt(n), lower.tail=FALSE)

#q8
delta = 0.01
sigma = 0.04
power = 0.90
sigLevel = 0.95

n <- (qnorm(sigLevel) + qnorm(power)) ^ 2 * sigma ^ 2 / delta^2
