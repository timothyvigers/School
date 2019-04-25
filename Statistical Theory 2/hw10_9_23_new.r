# -----------------------------------------------------------------------------
# hw10_9_23.s
# original:	 4/30/02
# new:		 4/28/04, 4/28/05
# for: masters theory iii homework #7
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: plot pdf and lrt
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

	dir<-"C:/sam/teaching/splus_theory_07/"

	file.out<-paste(dir,"hw10_9_23_out.txt",sep="")
	file.run<-paste(dir,"hw10_9_23_new.s",sep="")

#	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to pdf, lrt and CI: Masters Theory\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# par (for notes)
#	par(mfrow=c(2,1), oma=c(0,0,0,0),mar=c(3,5,3,5))
# -----------------------------------------------------------------------------
# Program


	# ---------------------------------------------------------------------
	# set parameters

		y0<-(155+104+66+50+36+40+30+35+42)
		print(y0)
		n<-9
		lamhat<-y0/n
		alpha<-0.10
		alpha2<-alpha/2
		lam0<-60		#arbitrary
		yy<-c(1:1000)

		

	# ---------------------------------------------------------------------
	# plot pdf of y

		pdf<-dpois(yy,lam0*n)
		plot(yy,pdf,xlab="Sum Xi", ylab="pdf",
			main="PDF of Poisson (n*lam0 = 9*60)")
		
	# ---------------------------------------------------------------------
	# plot lrt

		lamhat<-yy/n

		lrt1<-exp(-n*(lam0-lamhat))
		lrt2<-(lam0/lamhat)^yy
		lrt<-lrt1*lrt2
		plot(yy,lrt,main="Likelihood Ratio Test Statistic by Sum Xi\n n*lam0=9*60=540",pch="*")
		points(y0,.4,pch=10)
		lines(c(y0,y0),c(min(lrt),.4),lty=2)
		text(y0+50,.43,"Y=558 (observed)",cex=.7,adj=-1)

		text(600,1,"LRT(540) = 1, \nlamhat=lam0=60",cex=.7,adj=-1)
		lines(c(540,540),c(min(lrt),1),lty=2)


	# ---------------------------------------------------------------------
	# cutoffs

		# based on graph we will reject H0 if Y=>b or Y<=a
		# under H0, Y is poisson(lam0)
		# Pr(reject H0 | H0 true)=0.05
		# use pdf (plotted above) and assume alpha/2 in each tail
		# note: under H0 the distribution is NOT symmetric
		#	but it is close with lam0=60

		a<-qpois(alpha2,n*lam0)-1
		a.c<-lrt[a]

		b<-qpois((1-alpha2),n*lam0)
		b.c<-lrt[b]
	
		# check a.c != b.c  distribution is not symmetric
		print(a.c)
		print(b.c)

		# check alp.a != alp.b  distribution is not symmetric
		
		alp.a<-ppois(a,n*lam0)
		alp.b<-1-ppois(b,n*lam0)
		print(alp.a)
		print(alp.b)
	
		alpha.t<-alp.a+alp.b

		plot(yy,lrt,
		main="Likelihood Ratio Test Statistic by Number Successes\n Values of Y that are consistent with H0",pch="*")
		points(558,.2,pch=10)
		lines(c(558,558),c(min(lrt),.2),lty=2)
		text(610,.20,"Y=558 (observed)",cex=.7,adj=-1)
		text(610,.23,"Accept H0",cex=.7,adj=-1)

		text(585,1,"LRT(540) = 1, \nlamhat(540) = 60 = lam0",cex=.7,adj=-1)
		lines(c(540,540),c(min(lrt),1),lty=2)

		lines(c(yy[1],yy[1000]),c(a.c,a.c))
		lines(c(yy[1],yy[1000]),c(b.c,b.c))
		points(a,.4,pch=15)
		points(b,.4,pch=15)
		lines(c(a,a),c(min(lrt),.4),lty=3)
		lines(c(b,b),c(min(lrt),.4),lty=3)
		mytxt<-paste("Reject H0 for Y<=a","\n\na =",a)
		text(a,.48,mytxt,cex=.7,adj=1)
		mytxt<-paste("Reject H0 for Y>=b","\n\nb =",b)
		text(b,.48,mytxt,cex=.7,adj=0)
		mytxt<-paste("A.C=",round(a.c,3))
		text(10,(a.c+.035),mytxt,cex=.7)
		mytxt<-paste("B.C=",round(b.c,3))
		text(10,(b.c-.035),mytxt,cex=.7)
		
		mytxt<-paste("Level alpha =",alpha,"\nTrue (size) alpha =",round(alpha.t,4))
		text(25,.97,mytxt,cex=.8,adj=-1)

		# since a.c != b.c use mean of the 2 to choose the cutoff....
		C<-mean(c(a.c,b.c))
	
		
	# ---------------------------------------------------------------------
	# find CI Inverting LRT (approximately alpha/2 in each tail)
		
	# given C  # mean of a.c and b.c 
	# what values of lambda are consistent with our data?
	# fix lambda hat at observed value.  let lambda vary.

		by<-.1
		lambda<-seq(45,80,by=by)
		lambdahat<-y0/n

		lrt1<-exp(-n*(lambda-lambdahat))
		lrt2<-(lambda/lambdahat)^y0
		lrt<-lrt1*lrt2

		lrtC<-lrt>C
		tmp<-cbind(lambda,lrtC,lrt)
		tmp<-tmp[lrtC==T,]
		rr<-nrow(tmp)
		lammin<-tmp[1,1]-by	#-by as algorithm choses first value lrt>C
		lammax<-tmp[rr,1]+by	#+by as algorithm choses last  value lrt>C

		plot(lambda,lrt,xlab="lambda",ylab="LRT(lambda)",
			main="LRT CI for lambda\n Which values of lambda are consistent with y=558",
			type="l")
		lines(c(min(lambda),max(lambda)),c(C,C))
		lines(c(lammin,lammin),c(min(lrt),C))
		lines(c(lammax,lammax),c(min(lrt),C))
		text(45,C+.02,paste("C=",round(C,3)),adj=-1,cex=.7)

		mytxt<-paste("lam_L=",lammin)
		text(lammin-3,C+.05,mytxt)
		mytxt<-paste("lam_U=",lammax)
		text(lammax+3,C+.05,mytxt)
		
	
# -----------------------------------------------------------------------------
# Program

	# ---------------------------------------------------------------------
	# find CI pivot CDF (guaranteeing an interval) 
	# (approximately alpha/2 in each tail)

	# logic:
	# search  --don't use gamma/poisson relationship
	# lambda.up satisfies ppois(y0,n*lambda.up) = alpha/2
	# lambda.low satisfies 1-ppois(y0,n*lambda.low) = alpha/2

	#----------------------------
	# NUMERIC APPROACH lambda.up
		lambdaup<-seq(62,70,by=.01) #possible lambda (mle=62)
		a2<-ppois(y0,n*lambdaup)   # area left of y0, based on Pois(n*lam)
		ind<-a2<=alpha2		   # indicator of a2 values <alpha/2
		la2<-cbind(lambdaup,a2,ind) # combine lambdaup,a2,ind into matrix
		la2<-la2[ind==T,]	   # reduce matrix to only lambda's wth a2<=alpha2
		lambda.up<-la2[1,]		# choose first lambda with
						# ppois(y0,n*lambdaup)<=alpha/2
						# closest to alpha/2
						# lambda increases-> a2 decreases 
		cat("\n\nNumeric Search: Lambda.up, true alpha/2, indicator\n")
		print(lambda.up)
		lambda.up<-lambda.up[1]

	#----------------------------
	# NUMERIC APPROACH lambda.lo
		lambdalow<-seq(52,62,by=.01)  # possible values for lambda.low (mle=62)
		a2<-1-ppois(y0-1,n*lambdalow)
		ind<-a2<=alpha2
		la2<-cbind(lambdalow,a2,ind)
		la2<-la2[ind==T,]
		rr<-nrow(la2)
		lambda.low<-la2[rr,] 		# choose last lambda with
						# 1-ppois(y0,n*lambdaup)<=alpha/2
						# closest to alpha/2
						# lambda increases-> a2 increases 
		cat("\n\nNumeric Search: Lambda.low, true alpha/2, indicator\n")
		print(lambda.low)
		lambda.low<-lambda.low[1]

	# plot distributions under lambda.low and lambda.up
		yy<-c(300:800)
		poi.up<-dpois(yy,(n*lambda.up))
		poi.low<-dpois(yy,(n*lambda.low))
		yrange<-range(poi.up,poi.low)
		plot(yy,poi.up,xlab="Y= SumXi",ylab="PDF",
		main="Pivoting the CDF: PDF's under lambda.low and lambda.up \nObtained using Numeric Search",
		ylim=yrange,type="l")
		lines(yy,poi.low,lty=2)
		lines(c(y0,y0),c(yrange[1],yrange[2]),lty=1)
		mytext<-paste("Observed Y =",y0)
		text(y0+10,yrange[2],mytext,adj=-1)

		lgd.low<-paste("pdf with lambda.low =",lambda.low)
		lgd.up<-paste("pdf with lambda.up =",lambda.up)
		alp<-paste("alpha=",alpha)
		lgd<-c(lgd.low,lgd.up)
		legend(300,yrange[2],lgd,cex=.65,lty=c(2,1))
		text(300,yrange[2]*.75,alp,cex=.75,adj=-1)

	# ---------------------------------------------------------------------
	# find CI pivot CDF (guaranteeing an interval) 
	# (approximately alpha/2 in each tail)

	# logic:
	# USE gamma/poisson relationship
	# lambda.low = qchisq(alpha/2,df=2*(y0))/(2*n)
	# lambda.up = qchisq(1-alpha/2,df=2*(y0+1))/(2*n)


	lambda.low<-qchisq(alpha2,df=2*(y0))/(2*n)
	lambda.up<-qchisq((1-alpha2),df=2*(y0+1))/(2*n)

	yy<-c(300:800)
	poi.up<-dpois(yy,n*lambda.up)
	poi.low<-dpois(yy,n*lambda.low)
	yrange<-range(poi.up,poi.low)
	plot(yy,poi.up,xlab="Y= SumXi",ylab="PDF",
		main="Pivoting the CDF: PDF's under lambda.low and lambda.up \n Obtained using Gamma(Chi-square)/ Poisson Relationship",
		ylim=yrange,type="l")
	lines(yy,poi.low,lty=2)
	lines(c(y0,y0),c(yrange[1],yrange[2]),lty=1)
	mytext<-paste("Observed Y =",y0)
	text(y0+10,yrange[2],mytext,adj=-1)

	lgd.low<-paste("pdf with lambda.low =",round(lambda.low,2))
	lgd.up<-paste("pdf with lambda.up =",round(lambda.up,2))
	alp<-paste("alpha",alpha)
	lgd<-c(lgd.low,lgd.up)
	legend(300,yrange[2],lgd,cex=.65,lty=c(2,1))
	text(300,yrange[2]*.75,alp,cex=.75,adj=-1)
	

#	sink()				# close output file
	


#	sink()				# close output file
