#Hypothesis Tests of a Two Population Means

#HT for Dependent Samples
#Two Tailed
A  <-  c(9.8,9.8,10.1,10.1,10.2)
B  <-  c(10.1,10,10.2,9.9,10.1)
d  <-  c(B - A)
n  <-  length(d)
xbar_d  <-  mean(d)
v_d  <-  sd(d)
alpha  <-  0.05
t_table_1 <-  qt(alpha/2, df= n - 1) # -2.776445
t_table_2 <-  qt((1 - alpha/2), df  =  n - 1) # 2.776445
t_test <-  xbar_d/(v_d /sqrt(n))   # 0.6469966
t_test < t_table_1  #FALSE
t_test > t_table_2  #FALSE
#H0 is not rejected. 
p_value <- 2*pt(-abs(t_test), df = n - 1) # 0.553
#or
p_value <-2*pt(t_test, df = n - 1,lower.tail = F) # 0.553

#alternative solution
A  <-  c(9.8,9.8,10.1,10.1,10.2)
B  <-  c(10.1,10,10.2,9.9,10.1)
t.test(A,B, paired = TRUE, mu = 0, alternative = "two.sided", 
       conf.level = 0.95 )



#HT for Dependent Samples
#Upper Tail
before <- c(6 ,20 ,3, 0, 4)
after <- c(4, 6, 2, 0, 0)
d  <-  c(before - after)
n  <-  length(d)
xbar_d  <-  mean(d)
v_d  <-  sd(d)
alpha  <-  0.10
t_table <-  qt(1- alpha, df  =  n - 1) # 1.533206
#or t_table <-  qt(alpha, df  =  n - 1, lower.tail=F) # 1.533206
t <-  xbar_d/(v_d /sqrt(n))   #1.655032 
t > t_table  #TRUE
p_value <- 1 - pt(t, df = n - 1) # 0.08663016
p_value < alpha #TRUE

#alternative solution
before <- c(6 ,20 ,3, 0, 4)
after <- c(4, 6, 2, 0, 0)
t.test(before,after, paired = TRUE, mu = 0, alternative = "greater",conf.level = 0.90 )



#HT for Dependent Samples 
#Lower Tail
before <- c(127, 98, 110, 105, 122, 109, 111, 95)
after <- c(130, 104, 108, 108, 122, 117, 105, 98)
d <- before-after
x_bar <-  mean(d)
v_d  <-  sd(d)
n  <-  length(d)
alpha  <-  0.01
t_table <-  qt(alpha, df  =  n - 1) # -2.997952
t <-  x_bar/(v_d /sqrt(n))   # -1.190648
t < t_table  #FALSE
p_value <- pt(t, df = n - 1) # 0.1362988
p_value < alpha #FALSE

#alternative solution
t.test(before,after, paired = TRUE, mu = 0, alternative = "less", conf.level = 0.99 )



#HT for Two Population Means (Known Variance)
x1_bar  <-  14.06
x2_bar  <-  13.9
n1  <-  20
n2  <-  18
sigma1_square  <-  0.2
sigma2_square  <-  0.23
alpha  <-  0.1
z_table  <-  qnorm(1-alpha/2)
error  <-  sqrt(sigma1_square/n1 + sigma2_square/n2)
z  <-  (x1_bar - x2_bar)/ error

#alternative solution
p <- 2*pnorm(z, lower.tail = F)
p

#Hypothesis Tests of a Two Population Means (Independent Samples)

x1_bar <- 14.06
x2_bar <- 13.9
n1 <- 20
n2 <- 18
sigma1_square <- 0.2
sigma2_square <- 0.23
alpha <- 0.1
error <- sqrt(sigma1_square/n1 + 
                sigma2_square/n2)
z <- (x1_bar - x2_bar) / error
z
z_table1 <- qnorm(alpha/2)
z_table2 <- qnorm(1-alpha/2)
z < z_table1
z > z_table2

p <- 2*pnorm(z,lower.tail = F)
p
p < alpha
# Ho can not be rejected
# We can't reject null hypothesis
# Failed to reject Ho

#Ho: mu1-mu2 <=0
#H1: mu1-mu2 >0
x1_bar <- 14.06
x2_bar <- 13.9
n1 <- 20
n2 <- 18
sigma1_square <- 0.2
sigma2_square <- 0.23
alpha <- 0.1
error <- sqrt(sigma1_square/n1 + 
                sigma2_square/n2)
z <- (x1_bar - x2_bar) / error
z
z_table <- (qnorm(1-alpha))
z_table
z > z_table        

p <- pnorm(z,lower.tail = F)
p < alpha #FALSE
# Ho can not be rejected
# We can't reject null hypothesis
# Failed to reject Ho

#t.test
A <- c(20,24,15,21,20,18,21)
B <- c(20,26,28,27,20)
t.test(A,B,alt="two.sided",
       var.equal = T,
       conf.level = 1-0.01,
       mu=0)
#p-value > alpha
# Ho can not be rejected
# We can't reject null hypothesis
# Failed to reject Ho

A <- c(20,24,15,21,20,18,21)
B <- c(20,26,28,27,20)
t.test(A,B,alt="less",
       var.equal = T,
       conf.level = 1-0.01,
       mu=0)

A <- c(20,24,15,21,20,18,21)
B <- c(20,26,28,27,20)
t.test(A,B,alt="greater",
       var.equal = T,
       conf.level = 1-0.01,
       mu=0)

#Variances are Unknown and Not Equal
C1 <- c(207, 208, 208, 207, 205, 206, 206, 208, 208, 203)
C2 <- c(203, 204, 205, 206, 205, 205, 204, 205, 204,204)	
t.test(C1,C2, mu = 0, alternative = "two.sided", 
       var.equal = FALSE, conf.level = 0.95 )


C1 <- c(207, 208, 208, 207, 205, 206, 206, 208, 208, 203)
C2 <- c(203, 204, 205, 206, 205, 205, 204, 205, 204,204)	
t.test(C1,C2, mu = 0, alternative = "greater", 
       var.equal = 	FALSE, conf.level = 0.95 )


C1 <- c(207, 208, 208, 207, 205, 206, 206, 208, 208, 203)
C2 <- c(203, 204, 205, 206, 205, 205, 204, 205, 204,204)	
t.test(C1,C2, mu = 0, alternative = "less", var.equal = FALSE, 	conf.level = 0.95 )

#Hypothesis Test of a Two Population Proportions

x1 <- 176
x2 <- 168
n1 <- n2 <- 200
p1_est <- x1/n1
p2_est <- x2/n2
p1_est
p2_est
alpha <- 0.01
z_table_1 <- qnorm(alpha/2)
z_table_2 <- qnorm(1-alpha/2)
z_table_1
z_table_2
z <- (p1_est-p2_est)/sqrt((p1_est*(1-p1_est)/n1)+(p2_est*(1-p2_est)/n2))
z
z < z_table_1
z > z_table_2
#Ho can not be rejected
# We failed to reject null hypothesis

#Calculate p-value
p_value <- 2*pnorm(z,lower.tail = F)
p_value < alpha
#Ho can not be rejected
# We failed to reject null hypothesis

#Using prop.test()
?prop.test

prop.test(x=c(176,168),
          n=c(200,200),
          alt="two.sided",
          conf.level = 1-0.01,
          correct = F)

z^2


