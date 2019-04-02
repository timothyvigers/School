*----------------------------------------------------------------------
Lecture 25 SAS Code - Polynomial Regression
*----------------------------------------------------------------------;

*----------------------------------------------------------------------
* Observed weight gain at different doses (Table 15.8, pg. 415, KKNR)
*----------------------------------------------------------------------;
DATA wtgain;
	input observation wgtgain dose;
	CARDS;
1	0.9	1
2	1.1	2
3	1.6	3
4	2.3	4
5	3.5	5
6	5.0	6
7	6.6	7
8	8.7	8
9	0.9	1
10	1.1	2
11	1.6	3
12	2.1	4
13	3.4	5
14	4.5	6
15	6.7	7
16	8.6	8
17	0.8	1
18	1.2	2
19	1.4	3
20	2.2	4
21	3.2	5
22	4.8	6
23	6.7	7
24	8.8	8
;
RUN;

/* calculate different polynomial terms of dose */
DATA wtgain;
	set wtgain;
	dosesq = dose**2; /* "**" indicates power, in this case squared */

	/* create dummy variables */
	dose1 = (dose=1);
	dose2 = (dose=2);
	dose3 = (dose=3);
	dose4 = (dose=4);
	dose5 = (dose=5);
	dose6 = (dose=6);
	dose7 = (dose=7);
	dose8 = (dose=8);
RUN;

proc print data=wtgain; run;

*----------------------------------------------------------------------
* FEV Data Set
*----------------------------------------------------------------------
Read in FEV data set;

proc import datafile="H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/FEV_rosner.csv"
     out=fev /* name for data set for SAS to reference */
     dbms=csv /* identify file as csv */
     replace; /* overwrite fev if already present */
     getnames=yes; /* take first row as column names from data */
run;

data fev;
	set fev;
	
	agesq = age**2;
	agecu = age**3;
	agequ = age**4;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
*----------------------------------------------------------------------;
ods graphics off;
ods rtf file = "L25.rtf";
run;

/* simple linear regression, slide 4 */
PROC REG DATA=wtgain;
	MODEL wgtgain = dose;
RUN;

/* model with order 2 term, slide 7 */
PROC REG DATA=wtgain;
	MODEL wgtgain = dose dosesq / covb;
RUN;

/* fit saturated model with dummy variables to estimate pure error, slide 12 */
PROC REG DATA=wtgain;
	MODEL wgtgain = dose2 dose3 dose4 dose5 dose6 dose7 dose8;
RUN;

/* use table A7 for KKNR to determine orthogonal polynomial coefficients */
/* orthogonal polynomial contrasts using data step, approx slide 48 */
data wtgain;
	set wtgain;
	IF dose = 1 THEN DO;
		odose1 = -7;
		odose2 = 7;
		odose3 = -7;
		odose4 = 7;
		odose5 = -7;
		odose6 = 1;
		odose7 = -1;
	END;
	IF dose = 2 THEN DO;
		odose1 = -5;
		odose2 = 1;
		odose3 = 5;
		odose4 = -13;
		odose5 = 23;
		odose6 = -5;
		odose7 = 7;
	END;
	IF dose = 3 THEN DO;
		odose1 = -3;
		odose2 = -3;
		odose3 = 7;
		odose4 = -3;
		odose5 = -17;
		odose6 = 9;
		odose7 = -21;
	END;
	IF dose = 4 THEN DO;
		odose1 = -1;
		odose2 = -5;
		odose3 = 3;
		odose4 = 9;
		odose5 = -15;
		odose6 = -5;
		odose7 = 35;
	END;
	IF dose = 5 THEN DO;
		odose1 = 1;
		odose2 = -5;
		odose3 = -3;
		odose4 = 9;
		odose5 = 15;
		odose6 = -5;
		odose7 = -35;
	END;
	IF dose = 6 THEN DO;
		odose1 = 3;
		odose2 = -3;
		odose3 = -7;
		odose4 = -3;
		odose5 = 17;
		odose6 = 9;
		odose7 = 21;
	END;
	IF dose = 7 THEN DO;
		odose1 = 5;
		odose2 = 1;
		odose3 = -5;
		odose4 = -13;
		odose5 = -23;
		odose6 = -5;
		odose7 = -7;
	END;
	IF dose = 8 THEN DO;
		odose1 = 7;
		odose2 = 7;
		odose3 = 7;
		odose4 = 7;
		odose5 = 7;
		odose6 = 1;
		odose7 = 1;
	END;

	/* standardize orthogonal polynomials by sqrt of sum of squared values of coefficients */
	odose1_s = odose1/SQRT(168);
	odose2_s = odose2/SQRT(168);
	odose3_s = odose3/SQRT(264);
	odose4_s = odose4/SQRT(616);
	odose5_s = odose5/SQRT(2184);
	odose6_s = odose6/SQRT(264);
	odose7_s = odose7/SQRT(3432);

RUN;


/* fit orthogonal polynomial model, slide 18 */
PROC REG DATA=wtgain;
	MODEL wgtgain = odose1 odose2 odose3 odose4 odose5 odose6 odose7 / covb;
	LinearLOF: TEST odose2, odose3, odose4, odose5, odose6, odose7;
	QuadLOF:   TEST odose3, odose4, odose5, odose6, odose7;
RUN;

/* fit lower-order polynomial model with just odose1 and odose2, slide 20 */
PROC REG DATA=wtgain;
	MODEL wgtgain = odose1 odose2;
RUN;


/* fit standardized orthogonal polynomial example, slide 22 */
PROC REG DATA=wtgain;
	MODEL wgtgain = odose1_s odose2_s odose3_s odose4_s odose5_s odose6_s odose7_s;
RUN;


/* fit cell means model, slide 23 */
PROC REG DATA=wtgain;
	MODEL wgtgain = dose1 dose2 dose3 dose4 dose5 dose6 dose7 dose8 / noint;
	LINEAR: TEST -7*dose1-5*dose2-3*dose3-1*dose4+1*dose5+3*dose6+ 5*dose7+7*dose8;
RUN;


*----------------------------------------------------------------------
* FEV data with polynomial regression, slides 29-32
*----------------------------------------------------------------------;

/* straight line model */
PROC REG DATA=fev;
	MODEL fev = age;
RUN;

/* quadratic model */
PROC REG DATA=fev;
	MODEL fev = age agesq;
RUN;

/* cubic model */
PROC REG DATA=fev;
	MODEL fev = age agesq agecu;
RUN;

/* quartic model */
PROC REG DATA=fev;
	MODEL fev = age agesq agecu agequ;
RUN;

/* quadratic model with 1st order polynomial removed */
PROC REG DATA=fev;
	MODEL fev = agesq;
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* FYI-other remedies for non-linearity
*----------------------------------------------------------------------

*** Data Step Code for fitting piecewise polynomial models ***;
*** Allowing for a knot at age=14 ***;
data fev;
	set fev;

	I14 = (age ge 14);
	age14 = age-14;
	age14_I14 = I14*age14;

	LABEL 	I14 	= 'I(age>=14)'
			age14 	= '(Age-14)'
			age14_I14 = '(Age-14)*I(age>=14)';
run;

/* Discontinuity at the knot (age=14) */
PROC REG DATA=fev;
	MODEL fev = age I14 age14_I14;
	OUTPUT out = pred2 predicted=p;
RUN;

PROC SORT DATA=pred2;
	BY age;
RUN;

PROC GPLOT DATA=pred2;
	plot p*age / VAXIS=axis1 HAXIS=axis2;
	SYMBOL INTERPOL=join VALUE=dot COLOR=black;
RUN;

/* Continuity at the knot (age=14) */
PROC REG DATA=fev;
	MODEL fev = age age14_I14;
	OUTPUT out = pred3 predicted=p;
RUN;

PROC SORT DATA=pred3;
	BY age;
RUN;

PROC GPLOT DATA=pred3;
	PLOT p*age / VAXIS=axis1 HAXIS=axis2;
	SYMBOL INTERPOL=join VALUE=dot COLOR=black;
RUN;

/* Loess Regression */
PROC LOESS DATA=fev;
	MODEL fev=age / smooth=.99;
	ODS OUTPUT outputstatistics=stats;
RUN;

PROC SORT DATA=stats;
	BY age;

PROC GPLOT DATA=stats;
	PLOT (depvar pred)*age /overlay;
	symbol1 c=black i=rl value=dot width=2;
	symbol2 c=red i=join value=none width=2;
RUN;


ods graphics off;
ods rtf close;
run;

