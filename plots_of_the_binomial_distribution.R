?rbinom
rbinom(10, size=5, prob=0.3)

rbinom(10, 1, 0.5)

rbinom(10, 100, 0.5)

z <- rbinom(1000, 100, 0.5)
mean(z)

z <- rbinom(10000, 100, 0.5)
mean(z)

rbinom(7, 150, 0.05)
rbinom(30, 150, 0.05)


x <- 0:5
px <- dbinom(x, size=5, prob=0.3)
x
px
plot(x, px, pch=19)
plot(x, px, pch=19,type="h")

x <- 0:50
px <- dbinom(x, size=50, prob=0.3)
plot(x, px, pch=19)
plot(x, px, pch=19,type="h")

x <- 0:50
px <- dbinom(x, size=50, prob=0.5)
plot(x, px, pch=19)
plot(x, px, pch=19,type="h")


x <- 0:5
Px <- pbinom(x, size=5, prob=0.3)
plot(x, Px, pch=19)

plot(x, Px, pch=19, type="b")

x <- 0:50
Px <- pbinom(x, size=50, prob=0.3)
plot(x, Px, pch=19)

plot(x, Px, pch=19, type="b")

x <- 0:50
Px <- pbinom(x, size=50, prob=0.7)
plot(x, Px, pch=19)
plot(x, Px, pch=19, type="b")

x <- 0:50
Px <- pbinom(x, size=50, prob=0.5)
plot(x, Px, pch=19)
plot(x, Px, pch=19, type="b")


# X~binom(n=5, p=0.3)
x <- rbinom(1000, size=5, prob=0.3)
x
px <- dbinom(x, size=5, prob=0.3)
plot(x,px)

# X~binom(n=5, p=0.3)
x <- rbinom(10000, size=5, prob=0.3)
x
px <- dbinom(x, size=5, prob=0.3)
plot(x,px, type="h",
     main="Plot of a binomial dist.")


