#Confidence Interval

alfa <- c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25)
qnorm(1-alfa/2) 
round(qnorm(1-alfa/2),3)

#Example
xbar <- 2.20
sigma <- 0.35
n <- 11
alpha <- 0.05
z <- qnorm(1-alpha/2)
ME <- z*sigma/sqrt(n)
xbar - ME
xbar + ME
CI <- c(xbar - ME, xbar + ME)
CI


alpha<-0.1
n<-3
round(qt(1-alpha/2, df=n-1),3)


#t CI
n <- 25
xbar <- 50
s <- 8
alpha <- 0.05
t <- qt(1-alpha/2,df=n-1)
t
ME <- t * s / sqrt(n)
CI <- c(xbar-ME, xbar + ME)
CI

xbar <- 747.4
s <- 192.4
n <- 18
alpha <- 0.05

t <- qt(1-alpha/2, df = n-1)
ME <- t * s /sqrt(n)
t_CI <- c(xbar - ME, xbar + ME)
t_CI

sigma <- 188
z <- qnorm(1-alpha/2)
ME <- z * sigma / sqrt(n)
z_CI <- c(xbar - ME, xbar + ME)
z_CI

#Confidence Interval for Population Proportion

lower <- 79.14
upper <- 82.86
n <- 13
alpha1 <- 0.10
t_90 <- qt(1-alpha1/2,df=n-1)
t_90
xbar <- (lower+upper)/2
ME <- upper - xbar
ME
s <- (ME/t_90)*sqrt(n)
s

alpha2 <- 0.05 
t_95 <- qt(1-alpha2/2,df=n-1)
int1 <- xbar - (t_95*s/sqrt(n))
int2 <- xbar + (t_95*s/sqrt(n))
c(int1,int2)

phat <-25/100
n <-100
alpha <-0.05
z <- qnorm(1-alpha/2)
lower <- phat - z*(sqrt(phat*(1-phat)/n))
upper <- phat + z*(sqrt(phat*(1-phat)/n)) 
c(lower,upper)


n <-500
phat <-49/n
alpha <-0.05
z <- qnorm(1-alpha/2)
lower <- phat - z*(sqrt(phat*(1-phat)/n))
upper <- phat + z*(sqrt(phat*(1-phat)/n)) 
c(lower,upper)



#Confidence Interval for Population Variance

x <- c(67.4,67.8,68.2,69.3,69.5,67.0,68.1,68.6,67.9,67.2)
x
skare <- var(x)
n <- length(x)
alpha <- 0.05
kikare1 <- qchisq(alpha/2,df=n-1,lower.tail = F)
kikare2 <- qchisq(1-alpha/2, df=n-1, lower.tail = F)
lower <- (n-1)*skare / kikare1
upper <- (n-1)*skare / kikare2
c(lower,upper)
skare

skare <- 0.0025
n <- 20
alpha <- 0.05
kikare1 <- qchisq(alpha/2,df=n-1,lower.tail = F)
kikare2 <- qchisq(1-alpha/2, df=n-1,lower.tail = F)
lower <- (n-1)*skare / kikare1
upper <- (n-1)*skare / kikare2
c(lower,upper)

round(lower,4)
round(c(lower,upper),4)
