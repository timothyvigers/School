# -----------------------------------------------------------------------------
# program:	binom_boundary.R
# original:	2/28/02
# updated:  	3/2/03 (dbinom), 2/27/04 (ind.maxL), 2/2/19 dropbox
# for: 		master's theory iii lecture-1
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: plot LogLikelihoods: binomial 0, n, n/2 successes
# -----------------------------------------------------------------------------
# clean out directory
#	remove(ls())
# -----------------------------------------------------------------------------
# shut off graphics
	graphics.off()
# -----------------------------------------------------------------------------
# Libraries 
#	library(Hmisc,T)
#	library(Design,T)
# -----------------------------------------------------------------------------
# files
	
	# use paste() command to attach file name to current directory
	# paste() creates a character string by combining text and/or numeric values

	#dir<-"C:/sam/teaching/splus_theory_07/"
	dir<-"C:/Users/mawhinns/Dropbox/teaching/Rcode/"

	file.out<-paste(dir,"binom_boundary_out.txt",sep="")
	file.run<-paste(dir,"binom_boundary.R",sep="")
	file.pdf<-paste(dir,"binom_boundary.pdf",sep="")

	pdf(file.pdf)
	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to plot Binomial Log Likelihoods for 0, n/2 and n successes: Masters Theory\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program

	# allow 4 graphs on one page
	par(mfrow=c(2,2),oma=c(0,0,4,0))

	# create vector of p=Pr(success): 0,.05, .1, ... 1
	p<-seq(0,1,by=.05)

	# define n
	n<-50

	# ---------------------------------------------------------------------
	#sum xi = 25

	cat("\n\n----------------------------------------------------------------")
	cat("\n-----------------------  n/2 Successes -------------------------\n")
	logL<-log(dbinom(25,50,p))
	plot(p,logL,main="\nLog Likelihood with n/2 successes (n=50)",
	xlab="p = Pr(Success)",ylab="Log L",cex=.7)
	cat("\nLog Likelihood for p=0,.05,...1, with n/2 successes\n")
	print(logL)

	#-----------------------
	# find the value of p that maximizes the log(like)
	ind.maxL<-logL==max(logL)	# indicator vector for maximum
	mlep<-p[ind.maxL==T]		# choose p that corresponds to logL==max(logL)
	cat("\nLogL,  p,  indicator of maximum T=TRUE, F=FALSE (numeric 1=TRUE, 0=FALSE) for n/2 successes\n")
	print(cbind(logL, p, ind.maxL))	# cbind converts ind.maxL to a numeric
					# indicator T=1, F=0

		#------------------------
		# another method to find value of p that maximizes logL

		# maxlike<--1e99999	# large negative to begin
		# NN<-length(p)
		# for (i in 1:NN) {
		#	if(logL[i]>maxlike) {
		#		maxlike<-logL[i]
		#		mlep<-p[i]
		#	}
		#}
		#------------------------

	cat("\n Value of p that maximized logL with n/2 successes\n")
	print(mlep)

	mytxt<-paste("p Hat = ",mlep)
	text(.5,median(logL)*2,mytxt,cex=.7)	# add text to graph

	# ---------------------------------------------------------------------
	#sum xi =0
	cat("\n\n----------------------------------------------------------------")
	cat("\n-----------------------  0   Successes -------------------------\n")
	logL<-log(dbinom(0,50,p))

	plot(p,logL,main="\nLog Likelihood with 0 successes (n=50)",
	xlab="p = Pr(Success)",ylab="Log L",cex=.7)
	cat("\n\nLog Likelihood for p=0,.05,...1, with 0 successes\n")
	print(logL)
	
	#-----------------------
	# find the value of p that maximizes the log(like)
	ind.maxL<-logL==max(logL)	# indicator vector for maximum
	mlep<-p[ind.maxL==T]		# choose p that corresponds to logL==max(logL)
	cat("\nLogL,  p,  indicator of maximum T=TRUE, F=FALSE (numeric 1=TRUE, 0=FALSE) for 0 successes\n")
	print(cbind(logL, p, ind.maxL))

	cat("\n Value of p that maximized logL with 0 successes\n")
	print(mlep)

	mytxt<-paste("p Hat = ",mlep)
	text(.5,median(logL)*2,mytxt,cex=.7)	# add text to graph

	# ---------------------------------------------------------------------
	# sum xi =n
	cat("\n\n----------------------------------------------------------------")
	cat("\n-----------------------  n   Successes -------------------------\n")
	logL<-log(dbinom(50,50,p))
	plot(p,logL,main="\nLog Likelihood with n successes (n=50)",
	xlab="p = Pr(Success)",cex=.7)
	cat("\n\nLog Likelihood for p=0,.05,...1, with n successes\n")
	print(logL)
	
	#-----------------------
	# find the value of p that maximizes the log(like)
	ind.maxL<-logL==max(logL)	# indicator vector for maximum
	mlep<-p[ind.maxL==T]		# choose p that corresponds to logL==max(logL)
	cat("\nLogL,  p,  indicator of maximum T=TRUE, F=FALSE (numeric 1=TRUE, 0=FALSE) for n successes\n")
	print(cbind(logL, p, ind.maxL))

	cat("\n Value of p that maximized logL with n successes\n")
	print(mlep)

	mytxt<-paste("p Hat = ",mlep)
	text(.5,median(logL)*2,mytxt,cex=.7)	# add text to graph

	# ---------------------------------------------------------------------
	# compare loglikelihoods for n=50, n=500, n=5000, number successes = n/2

	logL1<-log(dbinom(25,50,p))
	logL2<-log(dbinom(250,500,p))
	logL3<-log(dbinom(2500,5000,p))
	yrange<-c(-400,0)
	plot(p,logL1,main="Log Likelihood with n/2 successes \n(n=50 / n=500 / n=5000)",
	xlab="p = Pr(Success)",ylab="Log L",cex=.7,type="l",ylim=yrange)
	lines(p,logL2,lty=2)
	lines(p,logL3,lty=3)
	lgd<-c("n=50","n=500","n=5000")
	legend(.72,-300,lgd,lty=c(1:3),cex=.6)
	
	# overall label on graph
	mtext("Binomial Log Likelihoods",outer=T,cex=1.25)

	sink()				# close output file
	graphics.off()
