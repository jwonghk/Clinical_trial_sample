## Usig simulated annealing to find the optial alpha split (assuming only 1 sub-population) which
## will maximizes the power in Cong Chen paper: Hypothesis Testing in a Confirmatory Phase III Trial With a Possible Subset Effect

#Objective Function

library(mvtnorm)


r=0.5
It0 = (qnorm(0.025)+qnorm(0.1))^2/(log(0.75))^2
It1 = sqrt(It0)

mean1=c(0,0)
Sigma1=rbind(c(1,sqrt(r)),c(sqrt(r),1))

s1 = 1/sqrt(20)
s2 = 1/sqrt(10)
theta1 = -log(0.75) #0.29
mean2=c(theta1, theta1)

Sigma2 = rbind(c(s1^2,s1*s2*sqrt(r)),c(s1*s2*sqrt(r), s2^2))

N0 = 10


#Power Function
power1 = function(N, alpha1, alpha2, It=It1, r2=sqrt(0.5), m1=mean1, m2=mean2,
                  S1=Sigma1, S2=Sigma2)
  
{ sample = rep(0,N)

for (i in c(1:N))
{
  x = rmvnorm(1, mean=m2, sigma=S2)
  b1 = qnorm(1-alpha1) - It*x[1]
  b2 = qnorm(1-alpha2) - It*r2*x[2]    
  sample[i] = pmvnorm(lower=c(-Inf,-Inf), upper=c(b1,b2), mean=m1, sigma=S1)
  
}
1-mean(sample)

}


