# -----------------------------------------------------------------------------
# exact<-binom.s
# original:	 6/23/04
# new:		 
# for: bangkok (epimmune trial)
# -----------------------------------------------------------------------------
#  Data Input file: NONE
# -----------------------------------------------------------------------------
#  Goal: get exact binom confidence interval --guaranteeing an interval
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

	dir<-"C:/sam/teaching/splus_theory_10/"

	file.out<-paste(dir,"exact_binom_out.txt",sep="")
	file.run<-paste(dir,"exact_binom.r",sep="")

#	sink(file.out)		# sink sends the output to file.out
# -----------------------------------------------------------------------------
# program / date / time

        cat("\n\n\n")
        cat("Program:")
        cat(file.run)
        cat("\n\n")
        cat("Program to compute exact binom confidence interval (Rosner, 5, page 194)\n\n")
        print(date())
        cat("\n\n")
# -----------------------------------------------------------------------------
# Program

	# ---------------------------------------------------------------------
	# set parameters

		y0<-6
		#print(y0)
		n<-9
		phat<-y0/n
		alpha<-0.05
		alpha2<-alpha/2

	# ---------------------------------------------------------------------
	# find CI pivot CDF (guaranteeing an interval) 
	# (approximately alpha/2 in each tail)

	# p.up satisfies pbinom(y0,n,p.up) = alpha/2
	# p.low satisfies 1-pbinom((y0-1),n,p.low) = alpha/2

	#----------------------------
	# NUMERIC APPROACH p.up
		p.up<-1
		if(y0<n) {
			pup<-seq(round(phat,3),1,by=.001) 	#possible p 
			a2<-pbinom(y0,n,pup)	   	# area left of y0, based on Binom(n,p)
							# Pr(Y<=y0)
			ind<-a2<=alpha2		   	# indicator of a2 values <alpha/2
			la2<-cbind(pup,a2,ind) 		# combine pup,a2,ind into matrix
			la2<-matrix((la2[ind==T,]),ncol=3)
			#la2<-la2[ind==T,]	   	# reduce matrix to only p's wth a2<=alpha2
			p.up<-la2[1,]			# choose first p with
						# pbinom(y0,n*pup)<=alpha/2
						# closest to alpha/2
						# p increases-> a2 decreases 
		}
		#cat("\n\nNumeric Search: p.up, true alpha/2, indicator\n")
		#print(p.up)
		p.up<-p.up[1]

	#----------------------------
	# NUMERIC APPROACH p.lo
		p.low<-0
		if (y0>0) {
			plow<-seq(0,round(phat,3),by=.001)  	# possible values for p.low 
			a2<-1-pbinom((y0-1),n,plow)	# Pr(Y>=y0) #y0>0
			ind<-a2<=alpha2
			la2<-cbind(plow,a2,ind)
			la2<-matrix((la2[ind==T,]),ncol=3)
			rr<-nrow(la2)
			p.low<-la2[rr,] 		# choose last p with
						# 1-pbinom(y0,n,pup)<=alpha/2
						# closest to alpha/2
						# p increases-> a2 increases 
		}
		#cat("\n\nNumeric Search: p.low, true alpha/2, indicator\n")
		#print(p.low)
		p.low<-p.low[1]

	mytxt<-paste("\nN=",n,"x=",y0,"phat=",round(phat,3),"(",p.low,",",p.up,")\n")
	cat(mytxt)



#	sink()				# close output file
