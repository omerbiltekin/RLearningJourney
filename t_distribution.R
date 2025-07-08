?Distributions
?dt

#The Student's t Distribution
#dt(x, df, ncp, log = FALSE)
#pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
#qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
#rt(n, df, ncp)

# Let X~t(5) (df=5) 

# P(X<3)=?
pt(3,df=5)

#P(1<X<3)=?
pt(3,df=5)-pt(1,df=5) 

#P(X>3)=?
1-pt(3,df=5)
#or
pt(3,df=5,lower.tail = F)

# P(X<k)=0.05 --> k=?
qt(0.05,df=5)

# P(X>k)=0.05 --> k=?
qt(1-0.05,df=5)
# or
qt(0.05,df=5,lower.tail = F)

# P(X>k)=0.025 --> k=?
qt(1-0.025,df=5)

#Random t values
y <- rt(1000, df=5)
hist(y)
mean(y)
var(y)
5/3

y <- sort(rt(1000,df=5))
fy <- dt(y, df=5)
plot(y,fy, type="l", 
     main="Plot of a t dist. (df=5)",
     xlab="X", ylab="f(x)")

