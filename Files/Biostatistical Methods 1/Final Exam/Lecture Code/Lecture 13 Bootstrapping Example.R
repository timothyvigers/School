###########################################################
### Recreate figure 5.2 from Chihara and Hesterberg

# Replicating Figure 5.2 from C & H text 
set.seed(515)
par(mfrow=c(2,2))
u <- 23; sd<-7; n<-50; L<-u-3*sd; U<-u+3*sd          
X <- seq(L,U,.1) 
dat <- rnorm(n=n,mean=u,sd=sd) 

B <- 1000
my.boot <- numeric(B)
for (i in 1:B){
	x <- sample(dat, size=50, replace=TRUE)#draw resample 
	my.boot[i] <- mean(x) #compute mean, store in my.boot
}

plot(X,dnorm(X,mean=u,sd=sd), type="l", lwd=2, col="red",
	ylab="Density", main="Population, N(23,49)",xlim=c(L,U))
plot(X,dnorm(X,mean=u,sd=sd/sqrt(n)), type="l", lwd=2, col=1, 
	ylab="Density", main="Population, N(23,(49/50)^2)",xlim=c(L,U), xlab=expression(bar(X)))
lines(c(u,u), c(0,1), col="blue", lwd=2, lty=2)

hist(dat,xlab="X",ylab="Density", xlim=c(L,U), lwd=2, main="Sample, n=50")
hist(my.boot,xlab=expression(bar(X)),ylab="Density", xlim=c(L,U), lwd=2, main="Bootstrap distribution")

boot.mean <- mean(my.boot)
lines(c(boot.mean, boot.mean), c(0,250), col="blue", lwd=2, lty=2)


###########################################################
### Example 5.3 from Chihara and Hesterberg for bootstrap 

library(resampledata)
set.seed(53)

Arsenic <- resampledata::Bangladesh$Arsenic

# Part 1: Describe the distribution of arsenic in groundwater along with the sample mean and standard deviation
hist(Arsenic) 
qqnorm(Arsenic)											 
qqline(Arsenic) 

mean(Arsenic)
sd(Arsenic)

# Part 2: Obtain and describe a bootstrap sample for the mean
n <- length(Arsenic)
N <- 10^4
arsenic.mean <- numeric(N)
for (i in 1:N){ 
	x <- sample(Arsenic, n, replace = TRUE) 
	arsenic.mean[i] <- mean(x)
}

hist(arsenic.mean, main = "Bootstrap distribution of means")
abline(v = mean(Arsenic), col = "blue", lty = 2) # observed mean

qqnorm(arsenic.mean)   
qqline(arsenic.mean)

mean(arsenic.mean) # bootstrap mean
mean(arsenic.mean)-mean(Arsenic) # bias
sd(arsenic.mean) # bootstrap SE

# Part 3: Obtain Normal percentile 95% CI
LL <- mean(arsenic.mean)-1.96*sd(arsenic.mean) #Lower limit of 95% Normal CI
LL
UL <- mean(arsenic.mean)+1.96*sd(arsenic.mean) #Upper limit of 95% Normal CI
UL

sum(arsenic.mean < LL)/N  # Coverage of CI at lower end
sum(arsenic.mean > UL)/N  # Coverage of CI at upper end 

# Part 4: Obtain bootstrap percentile 95% CI
quantile(arsenic.mean, c(0.025, 0.975))

###########################################################
### Example 5.4 from Chihara and Hesterberg for bootstrap 

times.Basic <- c(7,10,10.6,10.2,8.6,7.6,8.2,10.4,11.0,8.5)
times.Ext <- c(3.4,7.8,9.4,4.7,5.4,7.6,5.0,8.0,7.8,9.6)

# Summarize distributions
mean(times.Basic); sd(times.Basic); length(times.Basic)

mean(times.Ext); sd(times.Ext); length(times.Ext)

par(mfrow=c(2,2))
hist(times.Basic, main='Basic Cable', xlab='Time in Minutes')
hist(times.Ext, main='Extended Cable', xlab='Time in Minutes')
boxplot(times.Basic, times.Ext, names=c('Basic','Extended'))


# Bootstrap with Independent Samples	
set.seed(54)
n.Basic <- length(times.Basic)
n.Ext <- length(times.Ext)
B <- 10^4
times.diff.mean <- numeric(B)

for (i in 1:B){
	Basic.boot <- sample(times.Basic, n.Basic, replace=TRUE)
	Ext.boot <- sample(times.Ext, n.Ext, replace=TRUE)
	times.diff.mean[i] <- mean(Basic.boot)-mean(Ext.boot)
}

par(mfrow=c(2,1))
hist(times.diff.mean, main=expression(paste('Bootstrap distribution of ', bar(X)[1] - bar(X)[2])), xlab=expression(bar(X)[1] - bar(X)[2]) )
abline(v=mean(times.diff.mean), col='purple', lwd=2)
qqnorm(times.diff.mean)
qqline(times.diff.mean)

# Compare sample and bootstrap
mean(times.Basic)-mean(times.Ext) #sample difference

mean(times.diff.mean) #bootstrap estimated difference

mean(times.diff.mean)-(mean(times.Basic)-mean(times.Ext))  #bias

sd(times.diff.mean) #bootstrap SE

quantile(times.diff.mean,c(0.025,0.975)) #bootstrap CI


# Bootstrap Sampling with Paired/Matched Samples
set.seed(54)
n <- 10
B <- 10^4
times.diffpair <- times.Basic - times.Ext
times.diffpair.mean <- numeric(B)

for (i in 1:B){
	diff.boot <- sample(times.diffpair, n, replace=T)
	times.diffpair.mean[i] <- mean(diff.boot)
}

par(mfrow=c(2,1))
hist(times.diffpair.mean, main=expression(paste('Bootstrap distribution of ', bar(X)[1] - bar(X)[2],' WITH matching')), xlab=expression(bar(X)[1] - bar(X)[2]) )
abline(v=mean(times.diffpair.mean), col='purple', lwd=2)
qqnorm(times.diffpair.mean)
qqline(times.diffpair.mean)

mean(times.diffpair.mean)

quantile(times.diffpair.mean,c(0.025,0.975))







