alpha<-0.05
Zalpha<-qnorm(1-alpha)
#Zalpha<-1.645	#this would also work

cat("\nZalpha\n")
print(Zalpha)

sigma<-1		#could be anything - not specified
mu<-seq(-4,4,by=0.1)	#define a range of mu around null value
N<-c(1,4,16,64,100)

beta<-matrix(nrow=length(mu),ncol=length(N))	#define a matrix - each column will be the power function for one of the values of N

#-----------------------------------------------------
# beta=power=P(Z>Zalpha - sqrt(N)*(mu/sigma))

for(i in 1:5)	{		#we have five values of N

	ZZ<-Zalpha-(sqrt(N[i])*(mu/sigma))
	beta[,i]<-1-pnorm(ZZ)
}

xrange<-range(mu)	#range for the x-axis
yrange<-range(beta)	#range for the y-axis

#this plots the first column of beta (beta[,1]), i.e., for N=1

plot(mu,beta[,1],lty=1,type="l",ylab="Beta(mu) or P(X is in R)",
main="Power curves for H0:mu<=0 vs. H1:mu>0 \n sigma=1, alpha=0.05",
xlim=xrange,ylim=yrange)

#this plots the other four values of N

for(i in 2:5)	{
	lines(mu,beta[,i],lty=i)
}

lgd<-c("N=1","N=4","N=16","N=64","N=100")
legend(-3,.9,legend=lgd,lty=c(1:5))


