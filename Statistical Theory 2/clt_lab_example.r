
N<-1000  	# samples
n<-100	# observations per sample
lambda<-6	#poisson
p<-.8		# binomial

# POISSON
xbar<-c(length=N)
for(i in 1:N) {
	x<-rpois(n,lambda=lambda)
	xbar[i]<-mean(x)
}
hist(xbar,prob=T)
print(mean(xbar))
print(var(xbar))
xx<-range(xbar)
xx<-seq(xx[1],xx[2],by=.01)
dx<-dnorm(xx,mean=lambda,sd=sqrt(lambda/n))
lines(xx,dx)


# BINOMIAL
xbar<-c(length=N)
for(i in 1:N) {
	x<-rbinom(n,size=15,prob=p)
	xbar[i]<-mean(x)
}
hist(xbar,prob=T)
print(mean(xbar))
print(var(xbar))
xx<-range(xbar)
xx<-seq(xx[1],xx[2],by=.01)
dx<-dnorm(xx,mean=15*p,sd=sqrt((15*p*(1-p)/n)))
lines(xx,dx)

