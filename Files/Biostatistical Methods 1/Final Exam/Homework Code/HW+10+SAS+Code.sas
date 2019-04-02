************************************;
***  BIOS 6611                   ***;
***  Assignment #10 SAS code 	 ***;
************************************;

ods graphics off;
ods rtf file = "HW10_output.rtf";
run;

DATA carotenoids;
	INFILE "H:\Teaching\BIOS 6611\Fall 2018\Homework\Week 13\carotenoids.dat"; 
	INPUT age sex smoke bmi vitamins calories fat fiber alcohol chol betadiet retdiet betaplas retplas;
RUN;

/* create dummy variables */
data carotenoids;
	set carotenoids;

	*** Create dummy variables ****;
	IF smoke = 1 THEN smk_never = 1; ELSE smk_never = 0;
	IF smoke = 2 THEN smk_former = 1; ELSE smk_former = 0;
	IF smoke = 3 THEN smk_current = 1; ELSE smk_current = 0;

	*** Create variable for two groups with current status ****;
	IF smoke = 1 THEN group = 0;
	IF smoke = 2 THEN group = 1;
	IF smoke = 3 THEN group = 2;

	smk_nongrp = (group = 0 or group = 1); /* not currently smokers */
	smk_curgrp = (group = 2); /* current smokers */
run;

PROC FORMAT;
	value sex	1 = 'Male'
				2 = 'Female';
	value smoke	1 = 'Never'
				2 = 'Former'
				3 = 'Current';
	value vitamins	1 = 'Yes, regularly'
					2 = 'Yes, irregularly'
					3 = 'No';
RUN;

/* Question 1A */
PROC MEANS N MEAN STD STDERR data=carotenoids;
	VAR  betaplas;
	CLASS smoke;
	FORMAT smoke smoke.;
RUN;

/* Questions 1B-1F */
PROC REG DATA=carotenoids;
	MODEL betaplas = smk_former smk_current/COVB CLB;
	TEST smk_former-smk_current;
RUN;

/* Questions 2A-2C */
PROC REG DATA=carotenoids;
	MODEL betaplas = smk_never smk_former smk_current /NOINT COVB;
	TEST smk_never=smk_former=smk_current;                 /*2B*/
	TEST smk_never=smk_former,smk_never=smk_current;       /*Alternate 2B*/
	TEST smk_former-smk_current;                           /*2C */
	TEST .5*smk_never + .5*smk_former - smk_current;       /*2D*/
	TEST .5709*smk_never + .4291*smk_former - smk_current; /*2D, weighting by n*/
RUN;

/* Question 3 */
PROC TTEST DATA=carotenoids;
	CLASS smoke;
	VAR betaplas;
	WHERE smoke in (2,3); /* specify groups to compare */
	FORMAT smoke smoke.;
RUN;

ods graphics off;
ods rtf close;
run;
