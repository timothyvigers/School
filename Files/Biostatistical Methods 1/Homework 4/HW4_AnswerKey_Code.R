#####BIOS6611-2018 Homework 4 code#####

#Question 2a
power.t.test(n=5,delta=100,sd=75,sig.level=0.05,type="one.sample",alternative="two.sided")

#Question 2b
power.t.test(delta=100,sd=75,sig.level=0.05,power=0.90,type="one.sample",alternative="two.sided")

#Question 2c-90% power
power.t.test(n=5,sd=75,sig.level=0.05,power=0.90,type="one.sample",alternative="two.sided")

#Question 2c-80% power
power.t.test(n=5,sd=75,sig.level=0.05,power=0.80,type="one.sample",alternative="two.sided")


#Question 3ia
set.seed(2345)
n <- 5
mean <- 0
sd <- 75
numTrials <-10000
alpha <- 0.05 
  
count<- 0
for( i in 1:numTrials){
  y <- rnorm(n,mean,sd)
  t <- t.test(y, mu = 0, alternative = "two.sided")
  if(t$p.value < 0.05) count <- count + 1 else count <- count
}

power <- count/numTrials
power 

#Question 3ib
set.seed(1796)
n <- 5
mean <- 100
sd <- 75
numTrials <-10000
alpha <- 0.05 

count<- 0
for( i in 1:numTrials){
  y <- rnorm(n,mean,sd)
  t <- t.test(y, mu = 0, alternative = "two.sided")
  if(t$p.value < 0.05) count <- count + 1 else count <- count
}

power <- count/numTrials
power 

#Question 3iia
#### Using a function in R ####
# The Function below will use sample size, mean, SD, number of trials
# and alpha as inputs
compute_power = function(n, mean, sigma, numTrials, alpha){
	sample <- matrix(rnorm(n*numTrials, mean, sigma), ncol=numTrials)
	xbar <- apply(sample, 2, mean)
	variance <- apply(sample, 2, var)
	df.num = n-1
	test.stat <- (xbar-0)/sqrt(variance/n)
	return (mean(abs(test.stat) >= qt((1-(alpha/2)), df.num))) 
}

set.seed(2345)
compute_power(5,0,75,10000,0.05)

#Question 3iib
set.seed(1796)
compute_power(5,100,75,10000,0.05)
  
  


  
