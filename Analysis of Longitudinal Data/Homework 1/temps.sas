proc mixed data=temps method=ml;
 		model temp=year / solution outp=tempout;
		estimate p=1;
		repeated / type=ar(1) subject=intercept; run;

proc autoreg data = temps;
model temp=year / nlag=1 method=ml;
run;
