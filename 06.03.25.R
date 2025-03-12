?Distributions

#d... pdf f(x)
#p... cdf F(x)
#q... inverse cdf
#r... random

#p(x <= 9):? mean 10, variance 4
pnorm(9, mean = 10, sd = 2) #0.3085375
pnorm(9,10,2)
qnorm(0.3085375 , mean =10, sd=2) #9

#p(9 <= x <= 10):? 
pnorm(10,10,2) - pnorm(9,10,2)

#p(8 < x < 12):?
pnorm(12,10,2) - pnorm(8,10,2)
#p(6 < x < 14):?
pnorm(14,10,2) - pnorm(6,10,2)
#p(4 < x < 16):?
pnorm(16,10,2) - pnorm(4,10,2)


#p(x > 9)=?
1-pnorm(9,10,2)
#p(8.5 < x < 13):?
pnorm(13,10,2) - pnorm(8.5,10,2)
#p(8 < x < 18):?
pnorm(18,10,2) - pnorm(8,10,2)

#p(X<x):0.75 -->x:?
qnorm(0.75,10.2)
#p(X>x):0.90 -->x:?
qnorm(1-0.9,10,2)
qnorm(0.9,10,2,lower.tail = F)

#p(a<x<13):0.7 -->a:?
p<-pnorm(13,10,2)
0.9331928 - 0.7
a=qnorm(0.2331928,10,2)

#P(b <x< 13)=0.8 b=?
pnorm(13,10,2) - pnorm(a,10,2)
b=qnorm(0.1,10,2)
b=10-q #2.5631
pnorm(10+a,10,2)-pnorm(10-a,10,2)

#P(µ-a <X<µ+a)=0.8 a=?
(1-0.8)/2
mu_a<-qnorm(0.1,10,2)
10-mu_a
pnorm(10+2.563103,10,2)-pnorm(10-2.563103,10,2)
