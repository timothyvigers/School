# -----------------------------------------------------------------------------
# program:	hw3_discrete.s
# original:	2/9/06
# updated:  	
# for: 		master's theory ii hw-2
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: generate random samples with discrete dist'n, calculate number 
#		with X(5)>5
#	 compare observed vs expected (theoretical)
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

	dir<-"/Users/timvigers/Documents/GitHub/School/Statistical Theory 2/"

	file.out<-paste(dir,"hw2_discrete_out.txt",sep="")
	file.run<-paste(dir,"hw2_discrete_order.r",sep="")

	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to generate N random sample of size nn=20 from a discrete distribution \n and determine the proportion with X_(5)>5, compare to expected under order stat dist'n \n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program

	# allow 4 graphs on one page
	#par(mfrow=c(2,2),oma=c(0,0,4,0))

	set.seed(1017)
  # Set number of random samples
	N<-10000
	# Set number of order statistics/sample size
	nn<-20
	
	Y<-c(length=N) 	# create vector of length N
	
	# For each random sample, generate a random u(0,1) of size 20. For each value
	# above 0.2, assign value 3 (in a new vector), for any above 0.5, assign 5, etc.
	# Order the new vector, and for each value >= 5 set a value in vector Y to 1.
	for(i in 1:N) {
		Y[i]<-0
		x<-runif(nn,0,1)
		z<-rep(1,nn)	# Pr(Y=1)=.2
		z[x>.2]<-3	# Pr(Y=3)=.3
		z[x>.5]<-5	# Pr(Y=5)=.3
		z[x>.8]<-7	# Pr(Y=7)=.2
		
		zz<-z[order(z)]
		if(zz[5]>=5) {
			Y[i]<-1
		}
	}
  # Calculate proportion of 1s in Y.
	cat("Proportion of samples of size nn with X_(5)>5\n")
	print(sum(Y)/N)
	cat("EXPECTED Proportion of samples of size nn with X_(5)>5\n")
	print(1-sum(dbinom(size=nn,prob=.5,x=c(5:nn))))
 	print(sum(dbinom(size=nn,prob=.5,x=c(16:nn))))

	sink()				# close output file
