proc import datafile="/folders/myfolders/FEV_rosner.csv"
     out=fev /* name for data set for SAS to reference */
     dbms=csv /* identify file as csv */
     replace; /* overwrite fev if already present */
     getnames=yes; /* take first row as column names from data */
run;

/* FEV ~ age, not adjusted for height, slide 17 */
PROC REG DATA=fev;
	MODEL fev = age;
RUN;

/* FEV ~ age + height, slide 18 */
PROC REG DATA=fev;
	MODEL fev = age height / PARTIAL;
RUN;


/* Fit FEV ~ Age without transformation and create diagnostic plots, approiximately slide 23 */
PROC REG DATA = fev;
	MODEL fev = age;
	OUTPUT OUT= outfev RSTUDENT = jackknife;
RUN;

/* Y-X scatterplot with linear regression line */
PROC GPLOT DATA = fev;
	PLOT fev*age;
	SYMBOL INTERPOL=rl VALUE=dot COLOR=black;
RUN;

/* Jackknife Residual Plot */
PROC GPLOT DATA = outfev;
	PLOT jackknife*age;
	SYMBOL VALUE=dot INTERPOL=rl COLOR=black;
RUN;

/* Histogram of Jackknife Residuals */
PROC GCHART DATA= outfev;
	VBAR jackknife;
RUN;

/* Normal Probability Plot of Jackknife Residuals */
PROC UNIVARIATE NORMAL PLOT DATA= outfev;
 	VAR jackknife;
RUN;



/* Fit FEV ~ Age without transformation and create diagnostic plots, approiximately slide 24 */
DATA fev; /* calculate log(FEV) and create csmoke */
	SET fev;
	logfev = log(fev);
	
	if smoke='nonsmoker' then csmoke=0; /* smoker=1; nonsmoker=0 */
	else if smoke='smoker' then csmoke=1;

	if sex='male' then male=1; /* male=1; female=0 */
	else if sex='female' then male=0;
RUN;

PROC REG DATA = fev;
	MODEL fev = age;
	OUTPUT OUT= outfev RSTUDENT = jackknife;
RUN;

/* Y-X scatterplot with linear regression line */
PROC GPLOT DATA = fev;
	PLOT fev*age;
	SYMBOL INTERPOL=rl VALUE=dot COLOR=black;
RUN;

/* Jackknife Residual Plot */
PROC GPLOT DATA = outfev;
	PLOT jackknife*age;
	SYMBOL VALUE=dot INTERPOL=rl COLOR=black;
RUN;

/* Histogram of Jackknife Residuals */
PROC GCHART DATA= outfev;
	VBAR jackknife;
RUN;

/* Normal Probability Plot of Jackknife Residuals */
PROC UNIVARIATE NORMAL PLOT DATA= outfev;
 	VAR jackknife;
RUN;


/* Model for log(FEV) and smoking status, approximately slides 25-26 */
PROC REG;
	MODEL logfev=csmoke /*0=Non-smoker;1=smoker*/;
	WHERE age ge 14;
RUN;

/* Model for log(FEV) and age, approximately slide 27 */
PROC REG;
	MODEL logfev=age /*0=Non-smoker;1=smoker*/;
RUN;

