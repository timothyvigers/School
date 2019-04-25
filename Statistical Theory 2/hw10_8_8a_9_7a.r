# -----------------------------------------------------------------------------
# final<-8_8a_9_7a.r  
# original:	 5/12/04
# for: masters theory iii final
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: LRT and CI for N(\theta a*\theta) H0:a=1, H1:a!=1
# -----------------------------------------------------------------------------
# clean out directory
#
# -----------------------------------------------------------------------------
# shut off graphics
	graphics.off()
# -----------------------------------------------------------------------------
# Libraries 
#	library(Hmisc,T)
#	library(Design,T)
# -----------------------------------------------------------------------------
# files
	dir<-"C:/sam/teaching/splus_theory_04/"
	file.out<-paste(dir,"hw10_8_8a_9_7a_out.txt",sep="")
	file.run<-paste(dir,"hw10_8_8a_9_7a.r",sep="")

#	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program FINAL : Masters Theory\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program

	# ---------------------------------------------------------------------
	# generate data N(1,2)

		set.seed(32)
		n<-100
		x<-rnorm(n,mean=1,sd=sqrt(2))
		cat("\nmean(x):")
		print(mean(x))
		cat("\nvar(x):")
		print(var(x))

	# ---------------------------------------------------------------------
	# mles, fixed x 
	# are data consistent with H0: a=1?
	#
	# 8b. (Calculate in splus.  Do you reject?)
	#

		a0<-1
		theta0<-(-a0 + sqrt(a0^2+4*sum(x^2)/n))/2
	
		thetahat<-mean(x)
		ahat<-(var(x)*(n-1)/n)/mean(x)	
			#ahat=sigmahat/mean(x), var(x) = S^2 =sum(xi-xbar)^2/(n-1)
		#fix x
		tmp<-(ahat*thetahat/(a0*theta0))^(n/2)
		tmp1<-sum(((x-theta0)^2))/(-2*a0*theta0)
		tmp2<-sum(((x-thetahat)^2))/(-2*ahat*thetahat)
		lrt<-tmp*exp(tmp1)/exp(tmp2)

		cat("\nLRT < C<-alpha=.1465: Reject\n")
		cat("\nLRT:")
		print(lrt)

	# ---------------------------------------------------------------------
	# LRT
	# assume calpha=0.1465, what values of a0 are consistent with the data
	#
	
		byy<-0.01
		amin<-0.5
		amax<-4.0
		a0<-seq(amin,amax,by=byy)

		nn<-length(a0)
		lrt<-c(length=nn)

		i<-1
		for(a in a0) {
			theta0<-(-a + sqrt(a^2+4*sum(x^2)/n))/2
			tmp<-(ahat*thetahat/(a*theta0))^(n/2)
			tmp1<-sum(((x-theta0)^2))/(-2*a*theta0)
			tmp2<-sum(((x-thetahat)^2))/(-2*ahat*thetahat)
			lrt[i]<-tmp*exp(tmp1)/exp(tmp2)
			i<-i+1
		}

		calpha<-0.1465 	# exp(3.841459/-2)
				# based on asymptotic LRT -2*log(lrt)>3.841459

				# what values are consistent with data?

		ind.c<-lrt>calpha	# Acceptance Region
		mat<-cbind(a0,lrt,ind.c)
		mat<-mat[ind.c==1,]
		rr<-nrow(mat)
		aL<-mat[1,1]
		aU<-mat[rr,1]

		plot(a0,lrt,xlab="a",ylab="LRT(a)",type="l",
		main="LRT(a) vs 'a'\nValues of 'a' consistent with data")
		lines(c(amin,amax),rep(calpha,2))
		mytxt<-paste("Calpha=",calpha)
		text(min(a0),calpha+.03,mytxt,adj=-1)
		lines(c(aL,aL),c(0,calpha),lty=2)
		lines(c(aU,aU),c(0,calpha),lty=2)
		mytxt<-paste("aL=",aL)
		text(aL+.09,calpha+.03,mytxt,adj=-1)
		mytxt<-paste("aU=",aU)
		text(aU+.07,calpha+.03,mytxt,adj=-1)
		lines(c(1,1),c(-.1,.05),lty=1)
		mytxt<-paste("Reject: a0=",1)
		text(.9,.09,mytxt)
		lines(c(ahat,ahat),c(0,1),lty=2)
		mytxt<-paste("ahat=",round(ahat,3))
		text(ahat+.07,1,mytxt,adj=-1)

	# ---------------------------------------------------------------------
	# Asymptotic LRT (chisq,1)
	
		asylrt<--2*log(lrt)
		calpha<-qchisq(.95,1)
		ind.c<-asylrt<calpha	# Acceptance Region
		mat<-cbind(a0,asylrt,ind.c)
		mat<-mat[ind.c==1,]
		rr<-nrow(mat)
		aL<-mat[1,1]
		aU<-mat[rr,1]

		plot(a0,asylrt,xlab="a",ylab="-2Log(LRT(a))",type="l",
		main="-2Log(LRT(a)) vs 'a'\nValues of 'a' consistent with data")
		lines(c(amin,amax),rep(calpha,2))
		mytxt<-paste("Calpha=",round(calpha,3))
		text(min(a0),calpha+2,mytxt,adj=-1)
		lines(c(aL,aL),c(0,20),lty=2)
		lines(c(aU,aU),c(0,20),lty=2)
		mytxt<-paste("aL=",aL)
		text(aL+.05,calpha+2,mytxt,adj=-1)
		mytxt<-paste("aU=",aU)
		text(aU-.09,calpha+2,mytxt,adj=1)


#sink()

