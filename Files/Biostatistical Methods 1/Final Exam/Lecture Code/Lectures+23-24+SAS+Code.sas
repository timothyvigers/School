*----------------------------------------------------------------------
Lectures 23-24 SAS Code - Categorical Variables and General Linear Hypotheses
*----------------------------------------------------------------------;

*----------------------------------------------------------------------
* Birthweight Data Set with unequal observations per group, and all data processing done throughout lecture to bwt5 data set (below)
*----------------------------------------------------------------------;

proc import datafile="H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/birthweight_smoking_dataset.csv"
     out=bwt /* name for data set for SAS to reference */
     dbms=csv /* identify file as csv */
     replace; /* overwrite BWT if already present */
     getnames=yes; /* take first row as column names from data */
run;

data bwt;
	set bwt;

	*** Create dummy variables ****;
	IF momsmoke = 'Non' THEN Never = 1; ELSE Never = 0;
	IF momsmoke = 'Former' THEN Former = 1; ELSE Former = 0;
	IF momsmoke = 'Light' THEN Light = 1; ELSE Light = 0;
	IF momsmoke = 'Heavy' THEN Heavy = 1; ELSE Heavy = 0;

	*** Create variable for two groups with current status ****;
	IF momsmoke = 'Non' THEN group = 0;
	IF momsmoke = 'Former' THEN group = 1;
	IF momsmoke = 'Light' THEN group = 2;
	IF momsmoke = 'Heavy' THEN group = 3;

	non = (group = 0 or group = 1);
	smoke = (group = 2 or group =3);

	IF group = 0 THEN DO;
		linear = -3;
		quad   =  1;
		cubic  = -1;
	END;
	IF group = 1 THEN DO;
		linear = -1;
		quad   = -1;
		cubic  =  3;
	END;
	IF group = 2 THEN DO;
		linear =  1;
		quad   = -1;
		cubic  = -3;
	END;
	IF group = 3 THEN DO;
		linear =  3;
		quad   =  1; 
		cubic  =  1;
	END;
run;

*----------------------------------------------------------------------
* Birthweight Data Set with 5 observations per group
*----------------------------------------------------------------------;

proc import datafile="H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/birthweight_smoking_5pergroup_dataset.csv"
     out=bwt5 /* name for data set for SAS to reference */
     dbms=csv /* identify file as csv */
     replace; /* overwrite BWT if already present */
     getnames=yes; /* take first row as column names from data */
run;

/* create dummy variables */
data bwt5;
	set bwt5;

	*** Create dummy variables ****;
	IF momsmoke = 'Never' THEN Never = 1; ELSE Never = 0;
	IF momsmoke = 'Former' THEN Former = 1; ELSE Former = 0;
	IF momsmoke = 'Light' THEN Light = 1; ELSE Light = 0;
	IF momsmoke = 'Heavy' THEN Heavy = 1; ELSE Heavy = 0;

	*** Create variable for two groups with current status ****;
	IF momsmoke = 'Never' THEN group = 0;
	IF momsmoke = 'Former' THEN group = 1;
	IF momsmoke = 'Light' THEN group = 2;
	IF momsmoke = 'Heavy' THEN group = 3;

	non = (group = 0 or group = 1);
	smoke = (group = 2 or group =3);
run;

ods graphics off;
ods rtf file = "L2324_v2.rtf";
run;

/* PROC REG to fit REFERENCE CELL MODEL (reference group: never smokers) */
PROC REG DATA=bwt5;
	MODEL birthwt = former light heavy / covb;
RUN;

/* PROC REG to fit reduced model for birthwt */
PROC REG DATA=bwt5;
	MODEL birthwt = ;
RUN;

/* PROC REG to fit REFERENCE CELL MODEL (reference group: heavy smokers) */
PROC REG DATA=bwt5;
	MODEL birthwt = never former light;
RUN;

/* PROC REG treating group as a continuous variable */
PROC REG DATA=bwt5;
	MODEL birthwt = group;
RUN;

/* General Linear Hypothesis Test Example with Matrices (approx slide 23) */
PROC IML;
	beta = {7.44000, -0.20000,  -1.26000,  -1.48000};

	sigma ={0.2148         -0.2148         -0.2148         -0.2148,
    	   -0.2148          0.4296          0.2148          0.2148,
    	   -0.2148          0.2148          0.4296          0.2148,
    	   -0.2148          0.2148          0.2148          0.4296};

PRINT "t statistic for b(heavy)=b(light)";
c = {0 0 -1 1};
t = (c*beta)*INV(SQRT(c*sigma*c`));
PRINT t;

PRINT "F statistic for b(former)=b(heavy)=b(light)= 0";
c = {0 1 0 0, 0 0 1 0, 0 0 0 1};
F = (c*beta)`*INV(c*sigma*c`)*(c*beta)/NROW(c);
PRINT F;


/* PROC REG for birthwt with smoking group and testing generalized linear hypotheses */
PROC REG DATA=bwt5;
	MODEL birthwt = former light heavy;

	/* these 3 statement request the equivalent test */
	TEST light = heavy;
	TEST light-heavy;
	TEST light-heavy=0; 

	/* these 3 statement request the equivalent test */
	TEST former=light=heavy=0;
	TEST former,light,heavy;
	test former=0, light=0, heavy=0;
RUN;


/* PROC REG to fit CELL MEANS MODEL (no intercept) */
PROC REG DATA=bwt5;
	MODEL birthwt = never former light heavy / noint;
RUN;


/* orthogonal contrast example 1 (not orthogonal), approximately slide 33 */
PROC REG DATA=bwt5;
	MODEL birthwt=never former light heavy/noint;
	TEST never-former; * row 1 ;
	TEST never-light;	* row 2 ;
	TEST never-heavy;	* row 3 ;
	TEST never-former, never-light, never-heavy;
RUN;


/* orthogonal contrast example 2 (actually orthogonal), approximately slide 36 */
PROC REG DATA=bwt5;
	MODEL birthwt=never former light heavy/noint;
	TEST never-former; * row 1 ;
	TEST light-heavy;	* row 2 ;
	TEST never+former-light-heavy;	* row 3 ;
	TEST never-former, light-heavy, never+former-light-heavy;
RUN;


/* orthogonal contrast with different weights for different groups, approx. slide 41 */
PROC REG DATA=bwt5;
	MODEL birthwt=never former light heavy/noint;
	TEST .75*never + .25*former - .5*light - .5*heavy;
RUN;


/* cell means model with non vs. smoker, approx. slide 43 */
PROC REG DATA=bwt5;
	MODEL birthwt=non;
RUN;


/* Orthogonal polynomial contrast example with cell means model, approx. slide 48 */
PROC REG DATA=bwt5;
	MODEL birthwt=never former light heavy/noint;
	Overall:	TEST never=former=light=heavy; 
	Linear: 	TEST -3*never -1*former +1*light +3*heavy=0; 
	Quadratic: 	TEST  1*never -1*former -1*light +1*heavy=0; 
	Cubic: 		TEST -1*never +3*former -3*light +1*heavy=0; 
RUN;

/* orthogonal polynomial contrasts using data step, approx slide 51 */
data bwt5;
	set bwt5;
	IF group = 0 THEN DO;
		linear = -3;
		quad   =  1;
		cubic  = -1;
	END;
	IF group = 1 THEN DO;
		linear = -1;
		quad   = -1;
		cubic  =  3;
	END;
	IF group = 2 THEN DO;
		linear =  1;
		quad   = -1;
		cubic  = -3;
	END;
	IF group = 3 THEN DO;
		linear =  3;
		quad   =  1; 
		cubic  =  1;
	END;
RUN;

PROC REG data=bwt5;
	MODEL birthwt = linear quad cubic;
RUN;


/* Orthogonal polynomial constrasts with unequal sample sizes, approx. slide 53 */
/* NOTE: BE CAREFUL USING FORMATS WITH CONTRASTS !!! */
/* Note these contrasts are for equal n's !!! */

PROC REG DATA=bwt;
	MODEL birthwt=never former light heavy/noint;
 	Overall:   TEST never=former=light=heavy;
	Linear:    TEST -3*never -1*former +1*light +3*heavy=0; 
	Quadratic: TEST  1*never -1*former -1*light +1*heavy=0; 
	Cubic:     TEST -1*never +3*former -3*light +1*heavy=0;
RUN;

/* Obtain polynomial coefficients corrected for unequal sample sizes with PROC IML */
PROC IML;
	N = {7,5,7,8};
	X = {0,1,2,3};
	op = ORPOL(X,3,N);
PRINT op;

DATA bwt;
	set bwt;
	IF group = 0 THEN DO;
		o1 = -0.263541;
		o2 = 0.1740137;
		o3 = -0.07801;
	END;
	IF group = 1 THEN DO;
		o1 = -0.098062;
		o2 = -0.214473;
		o3 = 0.3276404;
	END;
	IF group = 2 THEN DO;
		o1 = 0.0674175;
		o2 = -0.215651;
		o3 = -0.234029;
	END;
	IF group = 3 THEN DO;
		o1 =  0.2328967;
		o2 = 0.1704784;
		o3 = 0.0682584;
	END;
RUN;

PROC REG DATA=bwt;
	MODEL birthwt = o1 o2 o3;
 	Linear: 	TEST o1;
  	Quadratic: 	TEST o2;
  	Cubic:	 	TEST o3;
RUN;



*----------------------------------------------------------------------
*----------------------------------------------------------------------
* Addendum analyses demonstrating equivalence of reference cell coding and cell means coding models
*----------------------------------------------------------------------
*----------------------------------------------------------------------;

PROC REG DATA=bwt5;/* Reference Cell Coding Model*/
	MODEL birthwt = former light heavy;
	/*Algebraic Translation of Orthogonal Contrast Matrix*/
 	REFortha: TEST Intercept- Intercept-former = 0, Intercept+light - Intercept-heavy = 0, Intercept + Intercept+former - Intercept-light - Intercept-heavy = 0;
 	REForths: TEST -former=0, light-heavy=0, former-light- heavy=0; /*Simplified Algebraic*/
 
 	REForth1: TEST -former=0; /* Never vs. Former */
 	REForth2: TEST light-heavy=0;
 	REForth3: TEST former-light-heavy=0; /* Never+Former - (Light+Heavy) */
RUN;

PROC REG DATA= bwt5; /* Cell Means Coding Model */
	MODEL birthwt = never former light heavy / noint;
	/* Orthogonal Contrast Matrix, 3 rows */
	CMortha: TEST never-former=0, light-heavy=0, never+former-light-heavy=0;

	CMorth1: TEST never-former=0;
 	CMorth2: TEST light-heavy=0;
 	CMorth3: TEST never+former-light-heavy=0;
RUN;



PROC REG DATA= bwt5;/*Reference Cell Coding Model*/
 	MODEL birthwt = linear quad cubic;
	OverOrth: TEST linear=0, quad=0, cubic=0;
RUN;

PROC REG DATA= bwt5; /* Cell Means Coding Model */
 	MODEL birthwt = never former light heavy/noint;
 	Overall:   TEST never=former=light=heavy;
 	Linear:    TEST -3*never -1*former +1*light +3*heavy=0; 
 	Quadratic: TEST  1*never -1*former -1*light +1*heavy=0; 
 	Cubic:     TEST -1*never +3*former -3*light +1*heavy=0;
 	OverOrth:  TEST -3*never -1*former +1*light +3*heavy=0,  
    	              1*never -1*former -1*light +1*heavy=0, 
        	         -1*never +3*former -3*light +1*heavy=0;
RUN;


ods graphics off;
ods rtf close;
run;

