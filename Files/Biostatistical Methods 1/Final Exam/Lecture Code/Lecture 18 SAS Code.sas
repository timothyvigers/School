*----------------------------------------------------------------------
Lecture 18 SAS Code
*----------------------------------------------------------------------;
      
*----------------------------------------------------------------------
* VO2max data set
*----------------------------------------------------------------------
Create vo2max;

PROC FORMAT;
 VALUE sex 0 = "Female" 1 = "Male";
RUN;

DATA vo2max;
INPUT sex vo2max minutes;
/* Permanently assign format */
FORMAT sex sex.;  

LABEL sex = "Males"
	vo2max = "VO2 Max (ml/kg/min)"
	minutes = "2-mile Run Time (min)";

DATALINES;
0	33.40	17.53
0	32.61	17.08
0	33.68	17.08
0	35.53	16.55
0	39.37 	15.75
0	39.73	16.12
0	42.53	16.13
0	43.18	14.41
0	47.40	14.80
0	47.75	15.01
1	36.23	17.42
1	41.49	15.45
1	42.33	15.30
1	43.21	14.33
1	47.80	14.07
1	49.66	13.50
1	53.10	13.42
1	53.29	12.38
1	53.69	11.67
1	60.62	12.47
.     50.0    .
;
/* NOTE: the final data point is prsent for use later on in providing CIs and PIs for an estimate VO2max of 50 */

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Carry out simple linear regression; 
proc reg data=vo2max;
	model minutes = vo2max / clb cli clm;
run;

*----------------------------------------------------------------------
Create scatterplot with regression line, CI, and PI; 
PROC GPLOT;
	PLOT minutes*vo2max minutes*vo2max /overlay VAXIS=axis1 HAXIS=axis2;
	SYMBOL1 INTERPOL=rlcli COLOR=black VALUE=dot LINE=2;
	SYMBOL2 INTERPOL=rlclm COLOR=black VALUE=dot LINE=3;
	AXIS1 LABEL = (FONT=ARIAL HEIGHT= 1.8 ANGLE=90);
	AXIS2 LABEL = (FONT=ARIAL HEIGHT= 1.8);
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Categorical predictor example with sex;

*----------------------------------------------------------------------
Two-sample t-test for minutes by sex;
proc ttest data=vo2max;
	class sex;
	var minutes;
run; 

*----------------------------------------------------------------------
Linear regression with sex as covariate;
proc reg data=vo2max;
	model minutes = sex;
run;

*----------------------------------------------------------------------
Reverse coding for sex;
data vo2max;
	set vo2max;
	if sex=1 then sex_reverse=0; /* female=1; male=0 */
	else if sex=0 then sex_reverse =1;
run;

*----------------------------------------------------------------------
Linear regression with gender (i.e., reversed coding of sex) as covariate;
proc reg data=vo2max;
	model minutes = sex_reverse;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Introduction to multiple linear regression;
proc reg data=vo2max;
	model minutes = sex vo2max;
run;

