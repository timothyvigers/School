# -----------------------------------------------------------------------------
# program:	hw1_5_3.s
# original:	1/17/06
# updated:  	
# for: 		master's theory ii hw-1
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: generate random normal(3,1) samples, calculate number > 3.5
#	 compare observed vs expected (theoretical)
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

	dir<-"C:/sam/teaching/splus_theory_10/hw-1/"

	file.out<-paste(dir,"hw1_5_3_out.txt",sep="")
	file.run<-paste(dir,"hw1_5_3.r",sep="")

	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to generate random N(3,1), and distribution of X<-i> 3.5\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program

	# allow 4 graphs on one page
	par(mfrow=c(2,2),oma=c(0,0,4,0))

	set.seed(16)

	N<-100
	mn<-3
	sd<-1
	
	mu<-3.5
	x<-rnorm(N,mean=mn,sd=sd)

	y<-x>mu		# creates T/F (True/False) indicator vector of x_i>mu 
	sumy<-sum(y)	# number > mu; summing indicators, numerically T=1 and F=0
	py<-sumy/N	# estimate proportion > mu

	# paste() creates a character string by combining text and/or numeric values
	# "\n" puts a return into the string

	mytxt<-paste("Randomly generated",N,"iid Normal RVs with mean=",mn,"and sd=",sd,"\n")
	cat(mytxt)
	cat("\nX\n")
	print(x)
	hist(x)

	cat("\n\n")
	mytxt<-paste("Computed RV Y_i, indicator X_i >",mu,"\n")
	cat(mytxt)
	cat("\nY\n")
	print(y)

	mytxt<-paste("\nNumber and Proportion of Y_i >",mu,"\n")
	cat(mytxt)
	print(sumy)
	print(py)

	cat("\n----------------------------------\n")
	pp<-1-pnorm(3.5,mean=3,sd=1)
	ee<-N*pp
	mytxt<-paste("Expected number of X_i >", mu,"\n")
	cat(mytxt)
	mytxt<-paste("Y_i is binomial, N=",N,", p=",pp,"\n")
	cat(mytxt)
	mytxt<-paste("N*p=",ee)
	cat(mytxt)

	sink()				# close output file
