# Original code by Sarah Ryan, tweaks by Alex Kaizer

### Exercise 1-Discrete Distributions: Binomial and Poisson

## 1a-Probability of 2.5% of Patagonians having disease

# Set up
n = 120  # sample size
p = 0.01  # probability
k = 0.025*n  # number of patagonians with the disease
lambda = n*p  # Use approximation for lambda

# probability of patagonians with the disease
prob_bin = choose(n,k)*p^k*(1-p)^(n-k)  # exact binomial probability
prob_pois = (lambda^k*exp(-lambda))/factorial(k) # Poisson approximation to binomial probability

# Compare
prob_pois-prob_bin


## 1b-Varying sample size and prevalence

# Libraries needed
library(ggplot2)

# Set up
n=seq(80,400,by=40) # varying sample size
p=seq(0.0025,.025,by=.0025)  # varying probability
np<-expand.grid(n=n,p=p)  # find every combination
np$k<-.025*np$n   # find the number in your sample with sarcoidosis 
np$lambda <- np$n*np$p  # find lambda using the approximation equation

# Calculate the probabilities
np$prob_bin<-choose(np$n,np$k)*np$p^np$k*(1-np$p)^(np$n-np$k)  # exact binomial probability
np$prob_pois = (np$lambda^np$k)*exp(-np$lambda)/factorial(np$k)  # Poisson approximation to binomial probability
np$diff <- np$prob_pois-np$prob_bin

# Plot the results
np$p<-factor(np$p)
ggplot(data=np,aes(x=n,y=diff,group=p,color=p))+geom_line()



### Exercise 2-Expected Value and Variance

## 2c-simulate exponential distribution and calculate mean and variance to compare to theoretical results from a/b

set.seed(0202)
sim_exp<-rexp(100000,3)
mean(sim_exp)
var(sim_exp)



### Exercise 3-Properties of Estimators

## 3a-Bias of median as estmiator
set.seed(0203) # Set seed for reproducibility
sim_norm <- rnorm(n=100,mean=70,sd=sqrt(15)) # Simulate a normal distribution
med <- median(sim_norm)  # Calculate the median
bias <- med-70  # Calculate the bias (Estimate - population mean)
bias

## 3b-Consistency of median as estimator
set.seed(0203) # Set seed for reproducibility

# Increasing sample size
ns<-seq(100,100000,by=100)
bias_median<-sapply(ns,function(x){
  sim<-rnorm(n=x,mean=70,sd=sqrt(15)) #Simulate normal distributions for each n
  medians<-median(sim) # Calculate the median of the samples
  bias<-medians-70 # Calculate the bias of the samples
  return(bias)
})

# Plot the results
plot(x=ns,y=bias_median,type='l')

## 3c-how variance changes with sample size
set.seed(0203) # Set seed the same as before

# Increasing sample size
ns<-seq(100,100000,by=100)
var_median<-sapply(ns,function(x){
  sim<-rnorm(n=x,mean=70,sd=sqrt(15))
  med<-median(sim)
  var_median<-sum((sim-med)^2)/length(sim)
  return(var_median)
})

# Plot the results
plot(x=ns,y=var_median,type='l')

## 3d-efficiency of estimators
set.seed(0203) # Set seed the same as before

# Increasing sample size
ns<-rep(1000,100000)
medians<-sapply(ns,function(x){
  sim<-rnorm(n=x,mean=70,sd=sqrt(15))
  med<-median(sim)
  return(med)
})


set.seed(0203) # Set seed the same as before

# Increasing sample size
ns<-rep(1000,100000)
means<-sapply(ns,function(x){
  sim<-rnorm(n=x,mean=70,sd=sqrt(15))
  m<-mean(sim)
  return(m)
})

var(medians)

var(means)

var(means)/var(medians) # Relative efficiency
