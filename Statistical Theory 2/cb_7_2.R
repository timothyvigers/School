# -----------------------------------------------------------------------------
# cb_7_2.R 
# last updated 2/26/03; 3/2/06 (R compatible) 2/28/19--dropbox
# for: Casella & Berger Problem 7.2

# -----------------------------------------------------------------------------
# Goal: Numerical Maximization
# 
# -----------------------------------------------------------------------------
# clean out directory
#	remove(ls())
# -----------------------------------------------------------------------------
# shut off graphics
	graphics.off()
# -----------------------------------------------------------------------------

#dir<-"C:/sam/teaching/splus_theory_06/"
dir<-"C:/Users/mawhinns/Dropbox/teaching/Rcode/"

file.out<-paste(dir,"cb_7_2_out.txt",sep="")
file.run<-paste(dir,"cb_7_2.R",sep="")
file.pdf<-paste(dir,"cb_7_2.pdf",sep="")

sink(file.out)

# -----------------------------------------------------------------------------
# program / date / time
#
        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program: Casella and Berger Problem 7.2b\n\n")
        print(date())
        cat("\n\n")

#-----------------------------------------------------------------------------
# enter data

pdf(file.pdf)

x<-c(22.,23.9,20.9,23.8,25.,24.,21.7,23.8,22.8,23.1,23.1,23.5,23.,23.)
n<-length(x)

#-----------------------------------------------------------------------------
# 	compute sample mean and variance

xbar<-mean(x)
cat("\nxbar:  ")
print(xbar)

xvar<-var(x)
cat("\nxvar:  ")
print(xvar)

#-----------------------------------------------------------------------------
# compute method of moments estimates --> initial estimates

x2sum<-sum(x**2)
alpha<-(xbar)**2/((x2sum/n) - xbar**2)
beta<-xbar/alpha

cat("\n\nmethod of moments: alpha = ")
print(alpha)
cat("method of moments: beta = ")
print(beta)

cat("check MoM Estimates: E[X] = alpha*beta = ")
print(alpha*beta)
cat("check MoM Estimates: Var[X] = alpha*beta^2 = ")
print(alpha*beta*beta)


#-------------------------------------------------

cat("\n\n")
cat("maximize wrt alpha, beta=f(alpha)\n\n")

#---------------------------------------------------------------
# MAXIMIZE likelihood 

sumx<-sum(x)
alpha<-seq(1,1000,by=.1)	# use large range of alpha's to make pretty graph
			# more efficient approach use values near MoM's

cat("\n STUDENTS SHOULD USE SMALLER RANGE OF ALPHA'S (Near MoM Estimates) TO AVOID LONG RUN TIME....\n")

NN<-length(alpha)	
like<-c(length=NN)
beta<-c(length=NN)

for(i in c(1:NN)) {
	beta[i]<-sumx/(n*alpha[i])
	like[i]<-prod(dgamma(x,shape=alpha[i],scale=beta[i]))
}

maxlike<-max(like)
ind.maxL<-like==maxlike
mlealpha<-alpha[ind.maxL==T]
mlebeta<-beta[ind.maxL==T]

mytxt<-paste("MLE for Alpha=",mlealpha,"\n", "MLE for Beta=",round(mlebeta,3))
cat("\nMLE's obtained maximizing Likelihood\n")
cat(mytxt)
cat("\n\n")
plot(alpha,like,main="Maximize Likelihoood wrt alpha, beta=f(alpha)",
	xlab="ALPHA",ylab="likelihood")
text(300,maxlike,mytxt)	# R
#text(1,maxlike,mytxt,adj=-1) 	# Splus


#---------------------------------------------------------------
# MAXIMIZE LOG(Likelihood) 

sumx<-sum(x)
alpha<-seq(1,1000,by=.1)	# use large range of alpha's to make pretty graph
			# more efficient approach use values near MoM's

cat("\n STUDENTS SHOULD USE SMALLER RANGE OF ALPHA'S (Near MoM Estimates) TO AVOID LONG RUN TIME....\n")

NN<-length(alpha)
loglike<-c(length=NN)
beta<-c(length=NN)

for(i in c(1:NN)) {
	beta[i]<-sumx/(n*alpha[i])
	loglike[i]<-sum(log(dgamma(x,shape=alpha[i],scale=beta[i])))
}

maxlike<-max(loglike)
ind.maxL<-loglike==maxlike
mlealpha<-alpha[ind.maxL==T]
mlebeta<-beta[ind.maxL==T]
mytxt<-paste("MLE for Alpha=",mlealpha,"\n","MLE for Beta=",round(mlebeta,3))
cat("\n\nMLE's obtained maximizing LOG Likelihood\n")
cat(mytxt)
cat("\n\n")
plot(alpha,loglike,main="Maximize LOG Likelihood wrt alpha, beta=f(alpha)",
	xlab="ALPHA",ylab="LOG likelihood")
text(200,maxlike*2,mytxt,adj=-1)


cat("check MLE Estimates: E[X] = alpha*beta = ")
print(mlealpha*mlebeta)
cat("check MLE Estimates: Var[X] = alpha*beta^2 = ")
print(mlealpha*mlebeta*mlebeta)

sink()
graphics.off()
