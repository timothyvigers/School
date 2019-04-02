# -----------------------------------------------------------------------------
# program:	hw1_exponential<-mean.s
# original:	1/17/06
# updated:  	
# for: 		master's theory ii lecture-1
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: generate N random exp(2) samples of size n, calculate mean for each 
#	 1...N, sample
#	 compare distribution of observed vs expected (theoretical)
#------------------------------------------------------------
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

	dir<-"C:/sam/teaching/splus_theory_10/hw-1/"

	file.out<-paste(dir,"hw1_exponential_mean_out.txt",sep="")
	file.run<-paste(dir,"hw1_exponential_mean.r",sep="")

	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to generate random N exp(2) samples of size nn, and compare \n  mean and variance of N sample means to their theoretical distribution\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program

	set.seed(16)

	N<-100
	nn<-30
	beta<-2
	
	y<-c(length=N)
	for(i in 1:N) {
		# x<-rexp(n=nn,scale=beta) # R doesn't have scale, only rate
		x<-rexp(n=nn,rate=1/beta)
		y[i]<-mean(x)
	}
	
	mean.xbar<-mean(y)
	var.xbar<-var(y)
	cat("sample mean")
	print(mean.xbar)
	cat("sample var")
	print(var.xbar)


	# should be gamma(alpha=nn, beta=beta/nn)
	# with E[y]=beta and Var[y]=beta^2/nn
	cat("\n\nexpected mean")
	print(beta)
	cat("expected variance")
	print(beta^2/nn)

	sink()				# close output file
