### Statistical Inference Week 1
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
set.seed(777)

a = rnorm(25,3, 1)
b = rnorm(25,3, 1.1)
c = rnorm(25,5, 1.6)
d = rnorm(25,7, 1.7)
e = rnorm(25,8, 1.5)
f = rnorm(25,4, 1.5)

Class1 <- data.frame(Group=rep("Class1",25), x1 = a, x2 = b)
Class2 <- data.frame(Group=rep("Class2",25), x1 = c, x2 = d)
Class3 <- data.frame(Group=rep("Class3",25), x1 = e, x2 = f)

DF <- rbind(Class1, Class2, Class3)

ggplot(DF, aes(x1, x2), colour = Group) + geom_point()

points.matrix <- cbind(x1 = DF$x1, x2 = DF$x2)
kclust <- kmeans(points.matrix, 3)

ggdata <- data.frame(points.matrix, Cluster=kclust$cluster, Species=DF$Group)

p = ggplot(ggdata) +
        stat_ellipse(aes(x=x1,y=x2,fill=factor(Cluster)), geom="polygon", level=0.95, alpha=0.6) +
        scale_fill_manual(values=c("red","yellow","deepskyblue"), labels=c("Deep Burn", "Shallow Burn", "Healthy Skin")) +
        guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster")) +
        geom_point(aes(x=5, y=8), size=9, shape=20) +
        scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10))+ xlab(expression(X[1])) +
        scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10))+ ylab(expression(X[2])) +
        theme(text = element_text(size=35), axis.text.x = element_text(angle=60, hjust=1, size=25),
              axis.text.y = element_text(hjust=1, size=25))

p + geom_line(y_intercept)
