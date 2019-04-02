************************************;
***  BIOS 6611                   ***;
***  Assignment #11 SAS code 	 ***;
************************************;

DATA amniotic;
	INPUT cells temp;
	temp2=temp**2;
	temp3=temp**3;

	IF temp=40 THEN
		DO;
			lin=-3;
			quad=1;
			cubic=-1;
		END;

	IF temp=60 THEN
		DO;
			lin=-1;
			quad=-1;
			cubic=3;
		END;

	IF temp=80 THEN
		DO;
			lin=1;
			quad=-1;
			cubic=-3;
		END;

	IF temp=100 THEN
		DO;
			lin=3;
			quad=1;
			cubic=1;
		END;
	lncells=log(cells);
	cellsn=cells*10**6;
	DATALINES;
 1.13 40
 1.20 40
 1.00 40
 0.91 40
 1.05 40
 1.75 60
 1.45 60
 1.55 60
 1.64 60
 1.60 60
 2.30 80
 2.15 80
 2.25 80
 2.40 80
 2.49 80
 3.18 100
 3.10 100
 3.28 100
 3.35 100
 3.12 100
;

/* QUESTION 1A */
PROC REG DATA=amniotic;
	MODEL cells=temp /clb;
	OUTPUT OUT=resids1 PREDICTED=pred RSTUDENT=jackknife;
RUN;

/* QUESTION 1B */
/* Y-X scatterplot with LINEAR regression line */
PROC GPLOT DATA=amniotic;
	PLOT cells*temp;
	SYMBOL INTERPOL=rl VALUE=dot COLOR=black;
RUN;

/* -OR- */
PROC SGPLOT DATA=amniotic;
	REG Y=cells X=temp;
RUN;

/* Jackknife Residual Plot versus Predictor, versus Predicted */
PROC GPLOT DATA=resids1;
	PLOT jackknife*(temp pred);
	SYMBOL VALUE=dot INTERPOL=rl COLOR=black;
RUN;
/* -OR- */
PROC SGPLOT DATA=resids1;
	REG Y=jackknife X=temp;
RUN;

PROC SGPLOT DATA=resids1;
	REG Y=jackknife X=pred;
RUN;

/* Histogram of Jackknife Residuals */
PROC GCHART DATA=resids1;
	VBAR jackknife;
RUN;
/* -OR- */
PROC SGPLOT DATA=resids1;
	histogram jackknife;
	density jackknife;
RUN;

/* Normal Probability Plot of Jackknife Residuals */
PROC UNIVARIATE NORMAL PLOT DATA=resids1;
	VAR jackknife;
RUN;

/* QUESTION 1C & 1D */
PROC REG DATA=amniotic;
	MODEL cells=temp temp2 temp3;
	LOF_linear: TEST temp2, temp3;	/* 1c */
	LOF_quad: TEST temp3; /* 1d */
RUN;

/* QUESTION 1D */
PROC SORT DATA=amniotic;
	BY temp cells;
RUN;

PROC REG DATA=amniotic;
	MODEL cells=temp temp2;
	OUTPUT OUT=resids2 PREDICTED=pred RSTUDENT=jackknife;
RUN;

***ALL;
PROC REG DATA=amniotic;
	MODEL cells=temp;
	MODEL cells=lin;
	MODEL cells=temp temp2;
	MODEL cells=lin quad;
	MODEL cells=temp temp2 temp3;
	MODEL cells=lin quad cubic;
RUN;

/* QUESTION 1E */
/* Y-X scatterplot with QUADRATIC regression line */
PROC GPLOT DATA=resids2;
	PLOT cells*temp /overlay;
	SYMBOL INTERPOL=rq VALUE=dot COLOR=black;
RUN;
/* -OR- */
PROC SGPLOT DATA=resids2;
	REG Y=cells X=temp / degree=2;
RUN;

/* Jackknife Residual Plot versus Predicted */
PROC GPLOT DATA=resids2;
	PLOT jackknife*(pred);
	SYMBOL VALUE=dot INTERPOL=rl COLOR=black;
RUN;
/* -OR- */
PROC SGPLOT DATA=resids2;
	REG Y=jackknife X=pred;
RUN;

/* Histogram of Jackknife Residuals */
PROC GCHART DATA=resids2;
	VBAR jackknife;
RUN;
/* -OR- */
PROC SGPLOT DATA=resids2;
	histogram jackknife;
	density jackknife;
RUN;

/* Normal Probability Plot of Jackknife Residuals */
PROC UNIVARIATE NORMAL PLOT DATA=resids2;
	VAR jackknife;
RUN;

/* QUESTION 1F */
PROC REG DATA=amniotic;
	MODEL cells=lin quad cubic;
RUN;

/* QUESTION 2A */
PROC REG DATA=amniotic;
	MODEL lncells=temp /clb;
	OUTPUT OUT=resids3 PREDICTED=pred RSTUDENT=jackknife;
RUN;

/* QUESTION 2B */
/* Y-X scatterplot with LINEAR regression line */
PROC GPLOT DATA=amniotic;
	PLOT lncells*temp;
	SYMBOL INTERPOL=rl VALUE=dot COLOR=black;
RUN;
/* -OR- */
PROC SGPLOT DATA=amniotic;
	REG Y=lncells X=temp;
RUN;

/* Jackknife Residual Plot versus Predictor, versus Predicted */
PROC GPLOT DATA=resids3;
	PLOT jackknife*(temp pred);
	SYMBOL VALUE=dot INTERPOL=rl COLOR=black;
RUN;
/* -OR- */
PROC SGPLOT DATA=resids3;
	REG Y=jackknife X=temp;
RUN;

PROC SGPLOT DATA=resids3;
	REG Y=jackknife X=pred;
RUN;

/* Histogram of Jackknife Residuals */
PROC GCHART DATA=resids3;
	VBAR jackknife;
RUN;
/* -OR- */
PROC SGPLOT DATA=resids3;
	histogram jackknife;
	density jackknife;
RUN;

/* Normal Probability Plot of Jackknife Residuals */
PROC UNIVARIATE NORMAL PLOT DATA=resids3;
	VAR jackknife;
RUN;

/* QUESTION 2D */
PROC REG DATA=amniotic;
	MODEL lncells=temp temp2 temp3;
	LOF_linear: TEST temp2, temp3;
	LOR_quad: TEST temp3;
RUN;

/*Compare with*/
PROC REG DATA=amniotic;
	MODEL lncells=lin quad cubic;
	LOF_linear: TEST quad, cubic;
	LOF_quad: TEST cubic;
RUN;