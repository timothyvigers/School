# -----------------------------------------------------------------------------
# hw8
# original:	
# for: 2003 masters theory iii final
# for: 2006 masters theory ii final (semester)
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: plot pdf, lrt and determine cutoff values
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

	file.out<-paste(dir,"hw8_bernoulli_lrt_out.txt",sep="")
	file.run<-paste(dir,"hw8_bernoulli_lrt.r",sep="")

#	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to pdf and lrt: Masters Theory\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program

	#par(mfrow=c(2,2))

	# ---------------------------------------------------------------------
	# set parameters

		n<-200
		p0<-0.5
		yobs<-75
		y<-c(0:n)

	# ---------------------------------------------------------------------
	# plot pdf

		pdf<-dbinom(y,200,.5)
		plot(y,pdf,pch="*",main="PDF of Y for p=0.5")

	# ---------------------------------------------------------------------
	# plot lrt

		lrt<-(n*p0/y)^y*((1-p0)/(1-(y/n)))^(n-y)
		plot(y,lrt,main="Likelihood Ratio Test Statistic by Number Successes",pch="*")
		if(1) {
			points(75,.4,pch=10)
			lines(c(75,75),c(min(lrt),.4),lty=2)
			text(75,.43,"Y=75 (observed)",cex=.7)
		}

		if(0) {
			text(125,1,"LRT(100) = 1, \nphat(100) = 0.5 = p0",cex=.7)
			lines(c(100,100),c(min(lrt),1),lty=2)
		}


	# ---------------------------------------------------------------------
	# cutoffs

		# based on graph we will reject H0 if Y=>b or Y<=a
		# under H0, Y is binomial(200,0.5)  
		# Pr(reject H0 | H0 true)=0.05
		# use pdf (plotted above) and assume <=0.025 in each tail
				
		a<-qbinom(.025,200,.5)-1		# goes past 0.25, put <=0.25 in tail
		b<-qbinom(.975,200,.5)		# goes past 0.975, put <=0.25 in tail
		
		a.c<-lrt[a+1]					# lrt defined for values between 0 and 200
		b.c<-lrt[b+1]					# lrt[1] is lrt at y=0, lrt[2] is lrt at y=1, etc

		#  a.c = b.c  ? area in tails should be equal (pdf symmetric), but a.c and b.c might not be!
		print(a.c)
		print(b.c)
		cc<-mean(c(a.c,b.c))
		
		# check area in tails 
		print(pbinom(a,200,.5))
		print(1-pbinom(b,200,.5))

if(1) {
		plot(y,lrt,main="Likelihood Ratio Test Statistic by Number Successes\nValues of Y that are consistent with H0",pch="*")
		points(75,.2,pch=10)
		lines(c(75,75),c(min(lrt),.2),lty=2)
		text(70,.23,"Y=75 (observed)",cex=.7)
		text(70,.26,"Reject H0",cex=.7)

		text(125,1,"LRT(100) = 1, \nphat(100) = 0.5 = p0",cex=.7)
		lines(c(100,100),c(min(lrt),1),lty=2)

		lines(c(y[1],y[201]),c(cc,cc))
		points(a,.4,pch=15)
		points(b,.4,pch=15)
		lines(c(a,a),c(min(lrt),.4),lty=3)
		lines(c(b,b),c(min(lrt),.4),lty=3)
		mytxt<-paste("Reject H0 for Y<=a","\n\na =",a)
		text(a,.48,mytxt,cex=.7,adj=1)
		mytxt<-paste("Reject H0 for Y>=b","\n\nb =",b)
		text(b,.48,mytxt,cex=.7,adj=0)
		mytxt<-paste("C=",round(cc,4))
		text(10,(cc+.02),mytxt,cex=.7)
		
		alpha<-pbinom(a,200,.5)+1-pbinom(b,200,.5)
		
		mytxt<-paste("Level alpha = 0.05\nTrue (size) alpha =",round(alpha,4))
		
		text(25,.97,mytxt,cex=.8)
}

		
#	sink()				# close output file
