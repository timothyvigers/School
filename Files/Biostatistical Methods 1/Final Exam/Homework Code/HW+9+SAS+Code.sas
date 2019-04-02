************************************;
***  BIOS 6611                   ***;
***  Assignment #9 SAS code 	 ***;
************************************;

ods graphics on;
ods rtf file = "HW9_output_v2.rtf";
run;

DATA lead;
	SET "H:\Teaching\BIOS 6611\Fall 2018\Homework\Week 12\lead2.sas7bdat"; /* lead2 is the name of the SAS data set */
RUN;

/* Question 1A */
PROC REG DATA=lead;
	MODEL iq = expose / CLB;
RUN;
 /* Question 1B */
PROC REG DATA=lead;
	MODEL iq = expose race/ CLB;
RUN;
 /* Question 1C */
PROC FREQ DATA=lead;
	TABLES race*expose/ CHISQ;
RUN;

PROC REG DATA=lead;
	MODEL race = expose/ CLB;
RUN;

/* Begin Question 2*/ 
/* New DATA step to create new variables */
DATA lead2;
	SET lead;
	IF first2y = 1 THEN notfirst2y = 0;
	IF first2y = 0 THEN notfirst2y = 1;

	milesf2y = miles*first2y;
	milesNf2y = miles*notfirst2y;

	LABEL notfirst2y = 'Not exposed first 2years'
		milesf2y = 'miles*first2y'
		milesNf2y = 'miles*notfirst2y';
RUN;

/* Questions 2a, 2b, and 2c (unless you choose to recode and run two models, see below) */
/* NOT exposed during first 2 years as reference group*/
PROC REG DATA=lead2;
	MODEL iq = miles first2y milesf2y / CLB;
RUN;

/* Question 2c if you choose to recode and fit two models */
/* Two Models */
PROC SORT DATA=lead2;
	BY first2y;
RUN;

PROC REG DATA=lead2;
	MODEL iq = miles  / CLB;
	BY first2y ;
RUN;

/* Exposed during first 2 years as reference group*/
PROC REG DATA=lead2;
	MODEL iq = miles notfirst2y milesNf2y / CLB;
RUN;

/* Question 2d figures */
/* SGPlot Code for 2D */
PROC SGPLOT DATA=lead2; 
	REG Y=iq X=miles / GROUP=first2y;
	FORMAT first2y;
RUN;

/* Plot for 2D */
PROC GPLOT DATA=lead2;
	PLOT iq*miles = first2y / VAXIS=axis1 HAXIS=axis2 LEGEND=legend1;
	SYMBOL1 I=rl VALUE=circle COLOR=black LINE=3 WIDTH=2;
	SYMBOL2 I=rl VALUE=dot COLOR=black LINE=1  WIDTH=2;
	AXIS1 LABEL = (FONT=ARIAL HEIGHT=2.5 ANGLE=90 POSITION=center  ) 
       VALUE=(FONT=ARIAL HEIGHT=2);
	AXIS2 LABEL = (FONT=ARIAL HEIGHT= 2.5 POSITION=center )
       VALUE=(FONT=ARIAL HEIGHT=2);
	LEGEND1 FRAME LABEL=(FONT=ARIAL HEIGHT= 1.5) VALUE=(FONT=ARIAL HEIGHT=1.5) 
         POSITION=(bottom inside right) ACROSS=1
         ;
	FORMAT first2y;
RUN;

/* full model for 3 (from 2a) */
PROC REG DATA=lead2;
	MODEL iq = miles first2y milesf2y / CLB;
RUN;

/* reduced model for 3 */
PROC REG DATA=lead2;
	MODEL iq = miles / CLB;
RUN;

ods graphics off;
ods rtf close;
run;
