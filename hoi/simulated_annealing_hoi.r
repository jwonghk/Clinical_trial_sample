## Usig simulated annealing to find the optimal alpha split (assuming only 1 sub-population, in this case it will 
## mean that there are only two alpha, i.e. alpha1, alph2) which
## will maximizes the power in Cong Chen paper while
## keeping the type 1 error rate to be less than alpha: 
## Cong Chen paper: Hypothesis Testing in a Confirmatory Phase III Trial With a Possible Subset Effect

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


## Constraint
constraint = function(alpha0, alpha=0.025, r=0.5)
{
  corre = rbind(c(1,sqrt(r)),c(sqrt(r),1))
  
  (1 - pmvnorm(lower=c(-Inf, -Inf), upper=c(qnorm(1-alpha0[1]),qnorm(1-alpha0[2])), mean = c(0,0),
               corr = corre)[1]-alpha)
  
}



## Maximize the above power function subject to the constraint that
## the sum of the two alphas has to be equal to 0.025
## i,e. to allow for a type 1 error of 2.5% in the overall trial

n=2000
k=100
T0=0.2
N=n*k
d=1.0

set.seed(20190312)
alpha1<- runif(1, 0, 0.025)
alpha2<- 0.025-alpha1
alpha <- c(alpha1,alpha2)
power_initial <- power1(3000,alpha1,alpha2)

cons <- constraint(alpha)


result <- c(alpha1, alpha2, power_initial, cons)
for (i in c(1:k))
{
    for (j in c(1:m))
    {
          
        
        al <-runif(1,0,0.025)
        a2 <- 0.025-a1
        
        p_update <- power(3500, a1,a2)
        if (p_update > p0)
        {    alpha1 <- a1
             alpha2 <- a2
             result = rbind(result, c(alpha1,alpha2, p_update))
        }
        
        
        if (p_update < p0)
        {    p = runif(1,0,1)
             if ( p < exp(-(p0-p_update)/T0))
             {
               
             }
             
        }
        
    }
}    

