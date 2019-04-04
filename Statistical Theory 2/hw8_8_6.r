# hw4<-8_6.s
# original:	 4/2/03
# updated: 4/5/04
# for: masters theory iii homework-4
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: plot lambda as a function of T
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

	dir<-"C:/sam/teaching/splus_theory_09/"

	file.out<-paste(dir,"hw8_8_6_out.txt",sep="")
	file.run<-paste(dir,"hw8_8_6.r",sep="")

#	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to plot lambda vs T (C&B 8.6): Masters Theory\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------

n<-10
m<-5

# given data (x,y) TT<-sum(x)/(sum(x)+sum(y))
# 0<=TT<=1

seqq<-0.001
TT<-seq(0,1,by=seqq)
lambda<-(n+m)^(n+m)
lambda<-lambda/(n^n*m^m)
lambda<-lambda*TT^n*(1-TT)^m

maintxt<-paste("C&B 8.6:   T vs lambda\n","n=",n,", m=",m)
plot(TT,lambda,main=maintxt,xlab="T",ylab="lambda",type="l")

# find TT that maximizes lambda
	lmax<-max(lambda)
	indmax<-lambda==lmax
	Tmax<-TT[indmax==T]
	print(Tmax)		# should be n/(m+n)
	
maxtxt<-paste("lambda max at T=",Tmax)
text(Tmax-.2,lmax,maxtxt)

# BEYOND CLASS ASSIGNMENT
# assume: reject lambda < c=.1
	text(0,.14,"Reject lambda < .1 (alpha=?)",adj=-1)
	lines(range(TT),c(.1,.1))

	indR<-lambda<=.1
	print(cbind(indR,TT))

	dffx<-diff(indR)	# see help(diff)
	z<-TT[dffx!=0]	# going from T to F and F to T
	xlo<-z[1]
	xup<-z[2]+seqq

	lines(c(xlo,xlo),c(0,.25))
	lines(c(xup,xup),c(0,.25))

	mytxt<-paste("Reject T <=",xlo)
	text(xlo-.05,.32,mytxt)

	mytxt<-paste("Reject T >=",xup)
	text(xup+.05,.32,mytxt)

