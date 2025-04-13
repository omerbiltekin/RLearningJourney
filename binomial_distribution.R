?Distributions

x <- seq(-10,30,len=100)
fx <- dnorm(x, mean=10, sd=5)
plot(x, fx, 
     main="Plot of a Normal dist.")
plot(x, fx, 
     main="Plot of a Normal dist.",
     type="l",lwd=3)


x <- rnorm(100, mean=10, sd=5)
x <- sort(x)
fx <- dnorm(x, mean=10, sd=5)
plot(x, fx, 
     main="Plot of a Normal dist.")
plot(x, fx, 
     main="Plot of a Normal dist.",
     type="l")

x <- rnorm(1000, mean=10, sd=5)
x <- sort(x)
fx <- dnorm(x, mean=10, sd=5)
plot(x, fx, 
     main="Plot of a Normal dist.")
plot(x, fx, 
     main="Plot of a Normal dist.",
     type="l")

x <- rnorm(10000, mean=0, sd=1)
x <- sort(x)
fx <- dnorm(x, mean=0, sd=1)
plot(x, fx, 
     main="Plot of the Standard Normal dist.")
plot(x, fx, 
     main="Plot of the Standard Normal dist.",
     type="l")


x <- seq(-10,30,len=100)
Fx <- pnorm(x, mean=10, sd=5)
plot(x, Fx, 
     main="CDF of a Normal dist.")
plot(x, Fx, 
     main="CDF of a Normal dist.",
     type="l",lwd=3)


x <- rnorm(100,mean=10,sd=5)
x <- sort(x)
Fx <- pnorm(x, mean=10, sd=5)
plot(x, Fx, 
     main="CDF of a Normal dist.")
plot(x, Fx, 
     main="CDF of a Normal dist.",
     type="l",lwd=3)

x <- rnorm(1000,mean=10,sd=5)
x <- sort(x)
Fx <- pnorm(x, mean=10, sd=5)
plot(x, Fx, 
     main="CDF of a Normal dist.")
plot(x, Fx, 
     main="CDF of a Normal dist.",
     type="l",lwd=3)


x <- rnorm(1000,mean=0,sd=1)
x <- sort(x)
Fx <- pnorm(x, mean=0, sd=1)
plot(x, Fx, 
     main="CDF of the Standard Normal dist.")
plot(x, Fx, 
     main="CDF of the Standard Normal dist.",
     type="l",lwd=3)

#Binomial distribution
#P(X=2)=?
(factorial(5)/(factorial(2)*factorial(3))*0.3^2*0.7^3)
choose(5,2)* 0.3^2 * (1-0.3)^(5-2)
dbinom(2, size=5,prob=0.3)

#Display the probability distribution table 
x <- 0:5
px <- dbinom(x, size=5, prob=0.3)
x
px
sum(px)

# Display the probability distribution table of the number of black balls drawn.
dbinom(x=0:5, size=5, prob=0.3)

# Display the probability distribution table of the number of red balls drawn.
dbinom(x=0:5, size=5, prob=0.7)

# What is the probability of drawing max. 3 black balls?
# P(X<= 3)=?
dbinom(0, size=5, prob=0.3)+
  dbinom(1, size=5, prob=0.3)+
  dbinom(2, size=5, prob=0.3)+
  dbinom(3, size=5, prob=0.3)

sum(dbinom(0:3,size=5,prob=0.3))

pbinom(3,size=5, prob=0.3)

# What is the probability of drawing max. 4 black balls?
pbinom(4, size=5, prob=0.3)

1-dbinom(5,size=5,prob=0.3)  

x <- 0:5
x
pbinom(x, size=5, prob=0.3)

cumsum(dbinom(x, size=5, prob=0.3))







