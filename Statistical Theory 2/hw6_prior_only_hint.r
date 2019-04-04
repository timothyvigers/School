# -----------------------------------------------------------------------------
# hw2<-prior_only.s  HINT
# original:	 3/18/02; updated 3/11/03; updated 3/16/04
#		3/9/06
# for: masters theory iii homework-2
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: plot Normal prior and posterior (N=5, N=100) distributions 
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
	dir<-"C:/sam/teaching/splus_theory_06/"
	file.out<-paste(dir,"hw6_prior_only_out.txt",sep="")
	file.run<-paste(dir,"hw6_prior_only_hint.s",sep="")
#	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to plot Normal prior and posterior(N=5, N=100): Masters Theory\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program
	# allow 4 graphs on one page
	par(mfrow=c(2,2),oma=c(0,0,5,0))

	# ---------------------------------------------------------------------
	# parameters

		nu<-4			# prior mean
		tau2<-1			# prior variance
		tau<-sqrt(tau2)		# prior sd
	
		xbar<-2			# sample mean
		sig2<-1			# sample variance (known)
		sig<-sqrt(sig2)		# sample sd

	# ---------------------------------------------------------------------
	# prior N(nu, tau^2)  nu=4, tau^2=1

		# -------------------------------------------------------------
		# compute x limits  mean +- 4SD

		xmin<-nu-4*tau
		xmax<-nu+4*tau

		xx<-seq(xmin,xmax,by=.01)

		# -------------------------------------------------------------
		# compute prior for all xx

		prior<-dnorm(xx,mean=nu,sd=sqrt(tau2))

		# -------------------------------------------------------------
		# plot
		
		rnu<-round(nu,2)
		rtau2<-round(tau2,2)
		maintxt<-paste("Prior Distribution: \nNormal (",rnu,",",rtau2,") ",sep="")
		plot(xx,prior,type="l",lty=1,xlab="x",ylab="pdf",main=maintxt)

		priorx<-xx		# save x-values for plot 
					# with all 3 distributions
#sink()

