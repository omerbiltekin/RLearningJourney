?Distributions



#Binomial distribution
# X~Binomial(5,0.3)

#P(X=2)=?
dbinom(x=2, size=5, prob=0.3)
dbinom(2, 5, 0.3)

# X~Binomial(n=5, p=0.3)
# What is the probability of drawing max. 3 black balls?
# P(X<=3)=?
# P(X=0) + P(X=1) + P(X=2) + P(X=3)
pbinom(3, 5, 0.3)


#rbinom
# X~Binom(n=5, p=0.3)
rbinom(n=10, size=5, prob=0.3)

# X~Binom(n=1, p=0.5)
rbinom(10, 1, 0.5)

# X~Binom(n=100, p=0.5)
rbinom(10, 100, 0.5)

# X~Binom(n=150, p=0.05)
rbinom(7, 150, 0.05)



#Plots for the pmf of Binomial Distribution
# X~Binom(n=5, p=0.3)
x <- 0:5   #x ekseeni
x
fx <- dbinom(x, 5, 0.3)  #y ekseni
fx
plot(x, fx, type="h" ,
     main="Pmf of Binomial Distribution",
     xlab="Number of Successes",
     ylab="Probability")

plot(0:5, dbinom(x=0:5, size=5, prob=0.3), type="h",main="Pmf of Binomial Distribution",
     xlab="Number of Successes",
     ylab="Probability")


# X~Binom(n=50, p=0.3)
plot(0:50, dbinom(x=0:50, size=50, prob=0.3), type="p")



# Normal approximation to Binomial
# X~Binom(n=50, p=0.5)
plot(0:50, dbinom(x=0:50, size=50, prob=0.5), type="p")

# Plots for the cdf of Binomial Distribution
# X~Binom(n=5, p=0.3)
x <- 0:5
Fx <- pbinom(x, 5, 0.3)
plot(x,Fx, type="p", pch=8)




plot(0:5, pbinom(0:5, size=5, prob=0.3), type="p", pch=19)
plot(0:5, pbinom(0:5, size=5, prob=0.3), type="p", pch=19,
     main="CDF of Binomial Distribution",
     xlab="Number of Successes",
     ylab="Probability")

# X~Binom(n=50, p=0.3)
plot(0:50, pbinom(0:50, size=50, prob=0.3), type="p", pch=19)




# X~Binom(n=50, p=0.5)
plot(0:50, pbinom(0:50, size=50, prob=0.5), type="p", pch=19)

plot(0:5, dbinom(0:5, size=5, prob=0.3), type="h")
plot(0:5, pbinom(0:5, size=5, prob=0.3), type="b")

plot(0:50, pbinom(0:50, size=50, prob=0.3), type="b")
plot(0:50, pbinom(0:50, size=50, prob=0.5), type="b")

#Sampling distribution of sample mean
x <- 1:6
px <- rep(1/6, each=6)
x
px
mu <- sum(x*px)
variance <- sum(((x-mu)^2)*px)

y <- sample(1:6, size=100, rep=T)
mean(y)
var(y)

y <- sample(1:6, size=1000*100, rep=T)
m <- matrix(y,nrow=100,ncol=1000)
xbar <- colMeans(m)
hist(xbar)
mean(xbar)
var(xbar)# ~ 2.916667 / 100

outcomes <- seq(1,6,0.5)
ps <- c(1:6, 5:1) / 36 # Vector of probabilities
sampling_distribution <- data.frame(Outcomes = outcomes, Probabilities = ps)
sampling_distribution

sample_mean <- sum(outcomes*ps)
sample_variance <- sum(((outcomes -sample_mean)^2)*ps)
sample_variance
variance/2

barplot(px,  names.arg =x, main = "Probability Distribution of a Fair Die",xlab = "Outcome", ylab = "Probability")
barplot(ps,  names.arg =outcomes, main = "Sampling Distribution of X_bar",xlab = "X_Bar", ylab = "Probability")

#simulate
x<-1:6
px<-rep(1/6,6)
barplot(px,  names.arg =x, main = "Probability Distribution of a Fair Die",xlab = "Outcome", ylab = "Probability")

draws <- sample(x, size = 500, replace = TRUE, prob = px)
freq_table <- table(draws)
probabilities <- freq_table / sum(freq_table)
barplot(probabilities, main = "500 draws", xlab = "Outcomes", ylab = "Probabilities")


draws <- sample(x, size = 4 * 500, replace = TRUE, prob = px)
draws <- matrix(draws, ncol = 4)
drawmeans <- colMeans(draws)
freq_table <- table(drawmeans)
probabilities <- freq_table / sum(freq_table)
barplot(probabilities, main = "500 means of 4 draws", xlab = "Outcomes", ylab = "Probabilities")


#simulate chi-squared (df=3) random variates using rnorm() function.
x1<-rnorm(1000) 
hist(x1)
x2<-rnorm(1000)
hist(x2)
x3<-rnorm(1000)
hist(x3)

chi <- x1^2 + x2^2 + x3^2
hist(chi)

x <- rchisq(1000, df=3)
hist(x)

#X~chi-square(df=5)
#P(X < 3)=?
pchisq(3,df=5)

# P(1<X<3)=?
pchisq(3,df=5)-pchisq(1,df=5)

#P(X > 10)=?
1-pchisq(10,df=5)
pchisq(10,df=5,lower.tail = F)

#P(X<k)=0.05 --> k=?
qchisq(0.05,df=5)

#P(X>k)=0.05 --> k=?
qchisq(1-0.05,df=5)

x <- rchisq(1000,df=5)
x <- sort(x)
px <- dchisq(x,df=5)
plot(x,px)

plot(x,px,type="l")

Px <- pchisq(x,df=5)
plot(x,Px)

x <- rchisq(1000,df=30)
x <- sort(x)
px <- dchisq(x,df=30)
plot(x,px)

#pdf of the Chi-Squared Dist.
y <- sort(rchisq(1000, df=5))

fy <- dchisq(y,df=5)
plot(y,fy)
plot(y,fy, type="l", 
     main="Plot of the Chi-squared dist. (df=5)",
     xlab="X", ylab="f(x)")

y <- sort(rchisq(1000,df=15))
fy <- dchisq(y,df=15)
plot(y,fy,type="l", main="Plot of the Chis-squared dist. (df=15)",
     xlab="X",ylab="f(x)")

y <- sort(rchisq(1000,df=30))
fy <- dchisq(y,df=30)
plot(y,fy,type="l", main="Plot of the Chis-squared dist. (df=30)",
     xlab="X",ylab="f(x)")