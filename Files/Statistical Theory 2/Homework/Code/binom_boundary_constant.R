# -----------------------------------------------------------------------------
# program:	binom_boundary_constant.R
# original:	2/28/02
# updated:  	3/2/03 (dbinom), 2/27/04 (ind.maxL); 2/21/13 (multiple * like); 2/28/19 dropbox;
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

	dir<-"Z:/teaching/theory/theory_13/code/"
	dir<-"C:/Users/mawhinns/Dropbox/teaching/Rcode/"

	file.pdf<-paste(dir,"binom_boundary_constant_out.pdf",sep="")
	file.run<-paste(dir,"binom_boundary_constant.R",sep="")
	pdf(file.pdf)

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

	par()
	# allow 4 graphs on one page
	#par(mfrow=c(1,1))

	# create vector of p=Pr(success): 0,.05, .1, ... 1
	p<-seq(0,1,by=.05)

	# define n
	n<-50

	C1<-.5
	C2<-1.2

	#PLOT TO SHOW CONSTANT TIME LOGLIKE HAS SAME MAX
	logL<-log(dbinom(25,50,p))
	plot(p,logL,main="\nLog Likelihood with n/2 successes (n=50)",
	xlab="p = Pr(Success)",ylab="Log L",cex=.7,type="l",lty=1)
	cat("\n\nLog Likelihood for p=0,.05,...1, with 0 successes\n")
	print(logL)
	lines(p,logL*C1,lty=2)
	lines(p,logL*C2,lty=3)
	lgd<-c("LogLike",paste("LogLike*",C1,sep=""),paste("LogLike*",C2,sep=""))
	legend(.4,-30,lgd,lty=c(1,2,3))

graphics.off()
