*----------------------------------------------------------------------
Lecture 22 SAS Code - Interactions
*----------------------------------------------------------------------;

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

/* PROC REG cannot handle categorical variables as text, need to dummy code smoking status */
data fev;
	set fev;
	if smoke='nonsmoker' then csmoke=0; /* smoker=1; nonsmoker=0 */
	else if smoke='smoker' then csmoke=1;

	if sex='male' then male=1; /* male=1; female=0 */
	else if sex='female' then male=0;
run;

ods graphics off;
ods rtf file = "L22.rtf";
run;


*----------------------------------------------------------------------
*----------------------------------------------------------------------
regression with GPLOT figure of predictions; 

PROC REG data=fev;
	MODEL fev = csmoke age;        /* csmoke: 0=Non-smoker; 1=Smoker */
	OUTPUT OUT=pred1 p=yhat;
RUN;

PROC GPLOT DATA=pred1;
 PLOT yhat*age=csmoke / VAXIS=axis1 HAXIS=axis2 LEGEND=legend1;
 SYMBOL1 I=rl VALUE=none COLOR=black LINE=1 WIDTH=2;
 SYMBOL2 I=rl VALUE=none COLOR=black LINE=3  WIDTH=2;
 AXIS1 LABEL = (FONT=ARIAL HEIGHT= 1.75 ANGLE=90 POSITION=center 'FEV (Liters)' ) 
       VALUE=(FONT=ARIAL HEIGHT=1.75);
 AXIS2 LABEL = (FONT=ARIAL HEIGHT= 1.5  POSITION=center 'Age (years)')
       VALUE=(FONT=ARIAL HEIGHT=1.5);
 LEGEND1 FRAME LABEL=(FONT=ARIAL HEIGHT= 1.5) VALUE=(FONT=ARIAL HEIGHT=1.5) 
         POSITION=(top inside left) ACROSS=1;
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
create interactions and fit model; 

DATA fev;
	SET fev;

	agesmk = age*csmoke;
	sexsmk = sex*csmoke;

	LABEL 	agesmk = "Age x smoke"
			sexsmk = "Sex x smoke:";
RUN;


PROC REG data=fev;
	MODEL fev = csmoke age agesmk / COVB;
RUN;


PROC GPLOT DATA=fev;
 PLOT fev*age = csmoke / VAXIS=axis1 HAXIS=axis2 LEGEND=legend1;
 SYMBOL1 I=rl VALUE=circle COLOR=black LINE=1 WIDTH=2;
 SYMBOL2 I=rl VALUE=dot COLOR=black LINE=3  WIDTH=2;
 AXIS1 LABEL = (FONT=ARIAL HEIGHT=2.5 ANGLE=90 POSITION=center 'FEV (Liters)' ) 
       VALUE=(FONT=ARIAL HEIGHT=2);
 AXIS2 LABEL = (FONT=ARIAL HEIGHT= 2.5 POSITION=center 'Age (years)')
       VALUE=(FONT=ARIAL HEIGHT=2);
 LEGEND1 FRAME LABEL=(FONT=ARIAL HEIGHT= 2) VALUE=(FONT=ARIAL HEIGHT=2) 
         POSITION=(top inside left) ACROSS=1
         OFFSET=(1.5,-.8);
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
recoding smoking group; 

DATA fev;
	SET fev;

	agesmk = age*csmoke;
	sexsmk = sex*csmoke;

	nsmoke = (csmoke = 0);
	agensmk = nsmoke*age;

	LABEL 	id = "ID Number"
			age = "Age (years)"
			fev = "FEV (Liters)"
			height = "Height"
			sex = "Sex"
			csmoke = "Current Smoker"
			agesmk = "Age x smoke"
			sexsmk = "Sex x smoke"
			nsmoke = "Non Smoker"
			agensmk = "Age x Non-smoke";
RUN;

** Run Recoded model to make smokers reference group **;
PROC REG DATA=fev;
	MODEL fev = age nsmoke agensmk / CLB;
RUN;


*----------------------------------------------------------------------
*----------------------------------------------------------------------
Use PROC GLM and ESTIMATE statements for the parameter of interest;

PROC GLM;
	MODEL fev = csmoke age agesmk /CLPARM;
	ESTIMATE 'Age Effect: Non-Smokers' age 1;  /* Estimate 1ßage */
	ESTIMATE 'Age Effect: Smokers' age 1 agesmk 1; /* Estimate 1ßage + 1ßagesmk */
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
*----------------------------------------------------------------------
Create weight gain data set for example;

DATA wtgain;
INPUT wgtgain dose dosesq;
DATALINES;
1	1.0	1.0
1.2	2.0	4.0
1.8	3.0	9.0
2.5	4.0	16.0
3.6	5.0	25.0
4.7	6.0	36.0
6.6	7.0	49.0
9.1	8.0	64.0
;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Fit models with no predictor, just dose, and dose+dose^2;

PROC REG DATA=wtgain;
	MODEL wgtgain =;
	MODEL wgtgain = dose; 
	MODEL wgtgain = dose dosesq;
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Fit mixed model for wtgain;

PROC MIXED DATA=wtgain METHOD=ML;
	MODEL wgtgain = /SOLUTION;
RUN;

PROC MIXED DATA=wtgain METHOD=ML;
	MODEL wgtgain = dose /SOLUTION;
RUN;

PROC MIXED DATA=wtgain METHOD=ML;
 MODEL wgtgain = dose dosesq /SOLUTION;
RUN;


ods graphics off;
ods rtf close;
run;

