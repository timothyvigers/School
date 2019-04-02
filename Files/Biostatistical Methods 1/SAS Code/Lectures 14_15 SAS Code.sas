*----------------------------------------------------------------------
Lectures 14-15 SAS Code
*----------------------------------------------------------------------;


*----------------------------------------------------------------------
*----------------------------------------------------------------------
Calculating the F-Statistic Value for the 95th Percentile with df1 and df2;

data probsqs;
/* X ~ F(df1=3, df2=23) */
	df1 = 3;
	df2 = 23;
	prob = probf(x, df1, df2);
/* Computing x so that Pr(X <= x) = 0.95 */
prob = 0.95;
df1 = 3;
df2 = 23;
x = finv(prob, df1, df2);
run; 

proc print data=probsqs;
var prob x;
run;

      
*----------------------------------------------------------------------
* Birthweight and Smoking Example
*----------------------------------------------------------------------
Read in birthweight and smoking data;

proc import datafile="H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/birthweight_smoking_dataset.csv"
     out=BWT /* name for data set for SAS to reference */
     dbms=csv /* identify file as csv */
     replace; /* overwrite BWT if already present */
     getnames=yes; /* take first row as column names from data */
run;


*----------------------------------------------------------------------
*----------------------------------------------------------------------
Carry out one-way ANOVA; 

proc anova data=BWT;
	class momsmoke;
	model birthwt = momsmoke;
	means momsmoke;
run;


*----------------------------------------------------------------------
*----------------------------------------------------------------------
Fit GLM model with diagnostic plots to carry out ANOVA on BWT data ;

ODS GRAPHICS ON;
PROC GLM DATA = BWT ORDER = internal PLOT = diagnostics;
	CLASS momsmoke;
	MODEL birthwt = momsmoke/solution;
RUN;
ODS GRAPHICS OFF;


*----------------------------------------------------------------------
*----------------------------------------------------------------------
Fit Welch's ANOVA on BWT data;

PROC GLM DATA = BWT;
	CLASS momsmoke;
	MODEL birthwt = momsmoke;
	MEANS momsmoke/ hovtest=levene(type=abs) hovtest=bartlett welch;
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Calculate LSD multiple comparisons;

proc anova data=BWT;
	class momsmoke;
	model birthwt = momsmoke;
	means momsmoke / LSD;
run;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Calculate Dunnett, Bonferroni, and Tukey HSD multiple comparisons;
PROC GLM DATA = BWT ORDER = internal;
	CLASS momsmoke;
	MODEL birthwt = momsmoke/noint solution;
	MEANS momsmoke/ dunnett('Non') bon tukey;
RUN;

*----------------------------------------------------------------------
*----------------------------------------------------------------------
Calculate Kruskal-Wallis nonparametric ANOVA and Dunn's posthoc macro;
PROC NPAR1WAY DATA = BWT WILCOXON ANOVA;
	CLASS momsmoke;
	VAR birthwt;
RUN;


FILENAME DUNN 'H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/dunn macro.sas';
%INCLUDE DUNN;

%DUNN(BWT,momsmoke,birthwt,0.05);
RUN;




*----------------------------------------------------------------------
* HOX Example
*----------------------------------------------------------------------
Read in HOX genes data for FDR example;

proc import datafile="H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/HOXgenes.csv"
     out=hox /* name for data set for SAS to reference */
     dbms=csv /* identify file as csv */
     replace; /* overwrite BWT if already present */
     getnames=yes; /* take first row as column names from data */
run;

/* Create histograms between groups for example */
title "A3 Gene by Group";
proc sgpanel data=hox;
	panelby group / rows=4 layout=rowlattice;
	histogram A3;
run;

title "FAKE Gene by Group";
proc sgpanel data=hox;
	panelby group / rows=4 layout=rowlattice;
	histogram FAKE;
run;

/* Conduct Welch's ANOVA over all genes */
ods output welch=welchtab ; /* tell SAS to save the Welch's ANOVA table as an object we can manipulate */
PROC glm DATA = hox NOPRINT ; /* specify NOPRINT to save us from producing lots of tables/plots when we really only want the Welch's ANOVA p-value */
	CLASS group;
	MODEL A3 A4 HOXA5 A7 HOXA9 A10 B3 B6 B9 MEIS1 MEIS2 PBX2 PBX3 FAKE = group; /* tell SAS we want to generate an ANOVA for each gene */
	MEANS group / welch; /* Specify that we want Welch's ANVOA */
RUN;
ods output close; /* tells SAS to stop writing to the function */

/* Print the welchtab data set to see what we have and identify what we need to extract in the next data step */
proc print data=welchtab; run; 

/* Extract p-values from Welch's ANOVAs using a data step */
data one;
	set welchtab;
	if Source="group" ;
	keep ProbF;
	rename ProbF=Raw_P; /* Rename ProbF to Raw_P to match formatting expected in PROC MULTTEST */
run;
 
/* Check that p-values were extracted and renamed to Raw_P from ProbF */
proc print data=one; run;

/* Apply FDR correction */
PROC MULTTEST INPVALUES=one FDR;
RUN;
