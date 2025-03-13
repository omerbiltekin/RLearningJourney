#Standard Normal Distribution
#Z~N(0,1)

#P(Z<z):0.75?
qnorm(0.75) #0.6744898
pnorm(0.6744898) #0.75

#P(Z>z):0.9?
qnorm(1-0.9)
qnorm(0.9, lower.tail = F)

#P(a < z < 1.5):0.7 --> a:?
pnorm(1.5)
0.9331928 - 0.7
a<-qnorm(0.2331928) #a:-0.7283725
pnorm(1.5)-pnorm(-0.7283725)

#P(μ-a < z <μ+a):0.8 --> a:?
qnorm(0.5-0.8/2) #a:1.281552
qnorm(0.8+(1-0.8)/2)

#P(Z<z):0.025 --> z:?
qnorm(0.025) #z:-1.959964
pnorm(-1.959964)

#rnorm
rnorm(4,mean=0,sd=1)
x<- rnorm(400,50,10)
hist(x)

hist(rnorm(1E5,50,10))

set.seed(13)
rnorm(10)
rnorm(10)

x<-rnorm(30,100,16)
hist(x)
z<-(x-100)/16
x
z

scale(x)
(x-mean(x))/sd(x)
