# HW 3 solutions
# Code written by Choo Liu (cuining.liu@ucdenver.edu), updates by Alex Kaizer (alex.kaizer@ucdenver.edu)
# 22 Aug 2017, 09 Sep 2018

# Air Quality Question
# via https://www.epa.gov/outdoor-air-quality-data/air-quality-index-daily-values-report
# retrieved 22 August 2017
# ================================

####Part A, Ozone Status####
aqi <- read.csv("H:/Teaching/BIOS 6611/Fall 2018/Homework/Week 3/ozone.csv")

#Question 1, estimating daily probability of "good" ozone levels
unique(aqi$AQI.Category) #identify different AQI categories included in the data
prob_aqi <- sum(aqi$AQI.Category == "Good") / length(aqi$AQI.Category) #estimate daily probability from data
prob_aqi

#Question 2, calculate exact binomial probability that at least 5 of next 7 days will have "good" ozone
1 - pbinom(q = 4, size = 7, prob = prob_aqi)

#Question 3, calculate normal approximation to binomial that at least 5 of next 7 days will have "good" ozone
approx_mean <- 7*prob_aqi #estimate mean from binomial: n*p
approx_var <- 7*prob_aqi*(1-prob_aqi) #estimate variance from binomial: n*p*(1-p)

1 - pnorm(q = 4.5, mean = approx_mean, sd = sqrt(approx_var)) #Get the probability using the normal with continuity correction



####Part B, Estimating Hospital Budgets#####

#Part B1, reproducible summary table
df <- read.csv("H:/Teaching/BIOS 6611/Fall 2018/Homework/Week 3/ProcedureCost.csv")

#Create new column with indicator variable if cost is non-zero
df$nonzero[df$Cost == 0] <- 0
df$nonzero[df$Cost != 0] <- 1

table(df$Procedure, df$nonzero)


#Part B2, reproducible calculation of proportion of non-zero costs, mean non-zero cost, and variance in non-zero costs by Procedure
p <- aggregate(nonzero ~ Procedure, data = df, FUN = function(x) {sum(x != 0) / length(x)})[ , 2]
m <- aggregate(Cost ~ Procedure, data = df[df$Cost != 0, ], FUN = mean)[ , 2]
v <- aggregate(Cost ~ Procedure, data = df[df$Cost != 0, ], FUN = var)[ , 2]


#Part B4, derive total cost for budget calculation using random variables with correct notation
n1 <- 120
n2 <- 200
p1 <- p[1]
p2 <- p[2]
m1 <- m[1]
m2 <- m[2]
v1 <- v[1]
v2 <- v[2]

EY1 <- p1*m1
VY1 <- p1*v1 + p1*(1-p1)*m1^2
EY2 <- p2*m2
VY2 <- p2*v2 + p2*(1-p2)*m2^2
ETot <- n1*EY1 + n2*EY2
VTot <- n1*VY1 + n2*VY2

qnorm( 0.80, mean=ETot, sd=sqrt(VTot) )

#Part B5, derive total cost for budget calculation using simulation
n.iter <- 100000
Tot <- rep(NA, n.iter)
set.seed(515) 

for( i in 1:n.iter ){
  alpha1 <- m1^2/v1  
  beta1  <- v1/m1
  Z1 <- rgamma(n1, shape=alpha1, scale=beta1)
  R1 <- rbinom(n1, 1, p1)
  Y1 <- R1*Z1
  alpha2 <- m2^2/v2 
  beta2  <- v2/m2
  Z2 <- rgamma( n2, shape=alpha2, scale=beta2 )
  R2 <- rbinom(n2, 1, p2)
  Y2 <- R2*Z2
  Tot[i] <- sum(Y1) + sum(Y2)
}
quantile(Tot, 0.8)

