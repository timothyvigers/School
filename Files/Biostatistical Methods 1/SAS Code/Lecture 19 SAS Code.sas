*----------------------------------------------------------------------
Lecture 19 SAS Code - Multiple Linear Regression
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

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Simple linear regression of FEV ~ smoking status; 

proc reg data=fev;
	model fev = csmoke;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Multiple linear regression of FEV ~ smoking status + age; 

proc reg data=fev;
	model fev = csmoke age;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Regression on FEV with intercept only; 

proc reg data=fev;
	model fev = ;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Simple linear regression of FEV ~ age for reduced model; 

proc reg data=fev;
	model fev = age;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Multiple linear regression of FEV ~ age + smoke + height + sex for reduced model; 

proc reg data=fev;
	model fev = csmoke age height male;
	Sex_Height: TEST male, height; /* This line of code implements the partial F test in SAS */
run;
