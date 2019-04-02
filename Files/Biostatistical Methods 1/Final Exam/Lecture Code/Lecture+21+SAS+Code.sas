*----------------------------------------------------------------------
Lecture 21 SAS Code - Confounders and Mediation
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
ods rtf file = "L21b.rtf";
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
t-test of FEV ~ smoking status; 

proc ttest data=fev;
	class csmoke;
	var age;
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
correlations of FEV, smoking status, and age; 

proc corr data=fev;
	var age csmoke fev;
run;

proc corr data=fev;
	var age fev;
	partial csmoke;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
*----------------------------------------------------------------------
mediation example;

/* read in the data for Topiramate drug pilot study */ 
proc import datafile="H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/pilot_top.csv"
     out=top /* name for data set for SAS to reference */
     dbms=csv /* identify file as csv */
     replace; /* overwrite fev if already present */
     getnames=yes; /* take first row as column names from data */
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Model 1 (total effect of intervention: crude model); 

proc reg data=top;
	model csbp = trtgrp;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Model 2 (direct effect of intervention: adjusted model); 

proc reg data=top;
	model csbp = trtgrp cbmi;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Model 3 (covariate model); 

proc reg data=top;
	model cbmi = trtgrp;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Model 4 (adjust for effect of diet as well); 

proc reg data=top;
	model csbp = trtgrp cbmi diet_exclude;
run;


ods graphics off;
ods rtf close;
run;
