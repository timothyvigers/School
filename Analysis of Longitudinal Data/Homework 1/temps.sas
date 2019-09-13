proc mixed data=temps method=ml;
 		model temp=year / solution outp=tempout;
		repeated / type=ar(1) subject=intercept; run;


DATA temps; set temps;
year2 = year**2;
year3 = year**3;
year4 = year**4;
RUN;

proc mixed data=temps method=ml;
 		model temp=year year2 year3 / solution outp=tempout;
		repeated / type=ar(1) subject=intercept; run;

proc autoreg data = temps;
model temp=year year2 year3 / nlag=1 method=ml;
run;

proc loess data=temps;
 		ods output scoreresults=scoreout
            outputstatistics=statout;
 		model temp = year / smooth= 0.3 residual clm degree=1;
score data=tempout / clm; run;
