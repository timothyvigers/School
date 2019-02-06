# -----------------------------------------------------------------------------
# program:	hw3_exponential.s
# original:	2/1/06
# updated:  	
# for: 		master's theory ii hw-3
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: generate N random exp(2) from U(0,1)
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

	dir<-"C:/sam/teaching/splus_theory_10/hw-2/"

	file.out<-paste(dir,"hw2_exponential_out.txt",sep="")
	file.run<-paste(dir,"hw2_exponential.r",sep="")

	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to generate random exp(2) samples from U(0,1) distribution\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program

	# ---------------------------------------------------------------------
	# exponential(2)

	set.seed(16)

	N<-1000
	beta<-2
	
	u<- runif(N,0,1)
	y<- -beta*log(1-u)
	
	mean.y<-mean(y)
	var.y<-var(y)
	cat("mean of 'exponential(2)' RVs")
	print(mean.y)
	cat("sample var of 'exponential(2)' RVs")
	print(var.y)
	mytxt<-paste("Distribution of",N,"RVs from an exponential(",beta,")\nline=true distribution")
	hist(y,prob=T,main=mytxt)

	x<-seq(0.5,15,by=.1)
	dx<-dexp(x,rate=1/beta)
	lines(x,dx,type="l")

	sink()				# close output file
