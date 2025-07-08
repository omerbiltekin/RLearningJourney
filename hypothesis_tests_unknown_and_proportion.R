#Hypothesis Tests of a Single Population (Unknown Variance)

mu <- 10000
xbar <- 9900
s <- 125
alpha <- 0.05
n <- 30
t_test <- (xbar-mu)/(s/sqrt(n))
t_alpha <- qt(alpha,df=n-1)
t_test < t_alpha
# we reject the claim that mean lifetime of a light bulb is above 10,000 hours.
p_value <- pt(t_test,df=n-1)
p_value < alpha
#We can reject the null hypothesis

#if we have sample data
x <- c(9795, 10073,  9742,  9916, 10135,  9825,  9841,  9821,  9864,  9917, 10053,  9800,  9766,  9880,  9766,  9876, 9825,  9606,  9930,  9868, 10013, 10018, 10084,  9988, 10002,  9863, 10076, 10046,  9818,  9793)
x
t.test(x, mu=10000, alt="less",
       conf.level =1-alpha )


mu <- 2
xbar <- 2.1
s <- 0.3
alpha <- 0.05
n <- 35
t_test <- (xbar-mu)/(s/sqrt(n))
t_alpha <- qt(alpha,df=n-1,lower.tail = F)
t_test > t_alpha
# t_test > t_alpha, so H0 is rejected. We reject the claim that there is
# at most 2 grams of saturated fat in a cookie.
p_value <- 1-pt(t_test,df=n-1)
p_value < alpha
#We can reject the null hypothesis

mu <- 15.4
xbar <- 14.6
s <- 2.5
alpha <- 0.05
n <- 35
t_test <- (xbar-mu)/(s/sqrt(n))
t_alpha <- qt(alpha/2,df=n-1)
t_test < t_alpha
# We cannot reject the null hypothesis
# We failed to reject the null hypothesis
p_value <- pt(t_test,df=n-1)*2
p_value < alpha
# We failed to reject the null hypothesis


#Hypothesis Tests of a Single Population Proportion  

#One Sample Proportion Test
p <- 0.6
phat <- 85/148
alpha <- 0.05
n <- 148
z_test <- (phat-p) / sqrt(p*(1-p)/n)
z_alpha <- qnorm(alpha)
z_test < z_alpha
#We cannot reject the null hypothesis

p_value <- pnorm(z_test)
p_value < alpha
# we do not reject the null hypothesis that the proportion of voters in the population is above 60%.

?prop.test
prop.test(x=85,n=148, p=0.6,
          alt="less",
          conf.level = 1-alpha,
          correct=F)

z_test^2

#Lower Tail Test of Population Proportion
#H0: p>0.6		H1: p < 0.6
p0 <- 0.6
pbar <- 85/148
n <- 148
alpha <- 0.05
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) # -0.6375983
z_alpha <- qnorm(alpha) 		 # -1.644854
# z is not in critical region, so H0 is NOT rejected.
# we do not reject the null hypothesis that the proportion of voters in the population is above 60%.

#alternative solution-1
#H0: p>0.6    H1: p < 0.6
p0 <- 0.6
pbar <- 85/148
n <- 148
alpha <- 0.05
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) # -0.6375983
pnorm(z)	 # p_value = 0.2618676

# p is not less than alpha, so H0 is NOT rejected.
# we do not reject the null hypothesis that the proportion of voters in the population is above 60%.

#alternative solution-2
alpha <- 0.05
prop.test(x=85,n=148, p=0.6, alt="less", correct=FALSE,
          conf.level=1-alpha)


#Upper Tail Test of Population Proportion
#H0 p<0.12	 H1: p > 0.12
p0 <-0.12
pbar <- 30/214
n <- 214
alpha <- 0.05
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) # 0.908751
z_alpha <- qnorm(alpha , lower.tail = F) # 1.644854
# z is not in critical region, so H0 is NOT rejected.
# we do not reject the null hypothesis that the  
# proportion of rotten apples in harvest stays below 12% 


#alternative solution-1
p0 <- 0.12
pbar <- 30/214
n <- 214
alpha <- 0.05
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) # 0.908751
pnorm(z, lower.tail=FALSE) 	 # p-value = 0.1817408
# p is not less than alpha, so H0 is NOT rejected.
# we do not reject the null hypothesis that the 
# proportion of rotten apples in harvest stays below 12% 


#alternative solution-2
prop.test(x=30,n=214, p=0.12, alt="greater", correct=FALSE,
          conf.level=1-0.05)
# p is not less than alpha(0.05), so H0 is NOT rejected.
# we do not reject the null hypothesis that the proportion of rotten apples in harvest stays below 12% 



#Two-Tailed Test of Population Proportion
#H0: p = 0.5	   H0: p ??? 0.5
p0 <- 0.5
pbar <- 12/20
n <- 20
alpha <- 0.05
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) # 0.8944272
z_alpha <- qnorm(1-alpha/2)	 # 1.959964
# z is not in critical region, so H0 is NOT rejected.
# we do not reject the null hypothesis that 
# the coin toss is fair. 


#alternative solution-1
p0 <- 0.5
pbar <- 12/20
n <- 20
alpha <- 0.05
z <- (pbar-p0)/sqrt(p0*(1-p0)/n) # 0.8944272
2*pnorm(z, lower.tail = FALSE)	 # p-value   0.3710934
# p is not less than alpha, so H0 is NOT rejected.
# we do not reject the null hypothesis that 
# the coin toss is fair. 


#alternative solution-2
alpha <- 0.05
prop.test(x=12,n=20, p=0.5, alt="two.sided",correct=FALSE, conf.level = 1-alpha)
