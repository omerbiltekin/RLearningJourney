#Hypothesis Testing

mu <- 10000
sigma <- 125
n <- 30
alpha <- 0.05
xbar <- 9900
z <- (xbar-mu)/(sigma/sqrt(n))
z_alpha <- qnorm(alpha)
z < z_alpha
#We can reject the null hypothesis

p <- pnorm(z)
p
p < alpha
#We can reject the null hypothesis

install.packages("TeachingDemos")
library(TeachingDemos)
?TeachingDemos
?z.test
z.test(9900, mu=10000, stdev=125, alt="less",
       n=30, conf.level=0.95)


mu <- 2
sigma <- 0.25
n <- 35
alpha <- 0.05
xbar <- 2.1
z <- (xbar-mu)/(sigma/sqrt(n))
z_alpha <- qnorm(1-alpha)
z > z_alpha
#We can reject the null hypothesis

p <- 1-pnorm(z)
p

z.test(2.1, mu=2, stdev=0.25,n=35,
       alt="greater",conf.level=0.95)

mu <- 15.4
sigma <- 2.5
n <- 35
alpha <- 0.05
xbar <- 14.6
z <- (xbar-mu)/(sigma/sqrt(n))
z_l <- qnorm(alpha/2)
z_u <- qnorm(1-alpha/2)
(z < z_l) | (z > z_u)
# We can not reject the null hypothesis

p <- pnorm(z) * 2
p
p<alpha
#We can not reject the null hypothesis


z.test(14.6, mu=15.4, stdev=2.5,n=35,
       alt="two.sided",conf.level=0.95)



