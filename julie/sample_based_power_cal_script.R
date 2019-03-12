##Sampling Method to evalulate power
## March 8, 2019

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
power1 = function(N, a1=qnorm(1-alpha1), a2=qnorm(1-alpha2), It=It1, r2=sqrt(0.5), m1=mean1, m2=mean2,
S1=Sigma1, S2=Sigma2)

  { sample = rep(0,N)
  
    for (i in c(1:N))
    {
      x = rmvnorm(1, mean=m2, sigma=S2)
      b1 = a1 - It*x[1]
      b2 = a2 - It*r2*x[2]    
      sample[i] = pmvnorm(lower=c(-Inf,-Inf), upper=c(b1,b2), mean=m1, sigma=S1)
    
    }
  1-mean(sample)
  
  }

# Constraint
constraint = function(alpha0, alpha=0.025, r=0.5)
{
  corre = rbind(c(1,sqrt(r)),c(sqrt(r),1))
  
  (1 - pmvnorm(lower=c(-Inf, -Inf), upper=c(qnorm(1-alpha0[1]),qnorm(1-alpha0[2])), mean = c(0,0),
               corr = corre)[1]-alpha)
  
}


N0= 5000
alpha1 = 0.0125
alpha2 = 0.0125
power1(N=N0)
constraint(c(alpha1, alpha2))



