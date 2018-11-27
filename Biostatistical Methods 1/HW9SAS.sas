libname homework "\\Mac\Home\Documents\School\UC Denver\Biostatistics\Biostatistical Methods 1\Homework 9";

data lead;
set homework.lead2;
run;

proc reg data = lead;
model iq = expose / clb;
run;

proc reg data = lead;
model iq = expose race / clb;
run;

data lead;
	set lead;
	milesxfirst2y = miles*first2y;
run; 

DATA lead;
SET lead;
IF first2y = 1 THEN notfirst2y = 0; 
IF first2y = 0 THEN notfirst2y = 1;
milesf2y = miles*first2y;
milesNf2y = miles*notfirst2y;
LABEL notfirst2y = 'Not exposed first 2years'
milesf2y = 'miles*first2y'
milesNf2y = 'miles*notfirst2y';
RUN;

proc reg data = lead;
model iq = miles first2y milesf2y / clb;
run;

proc reg data = lead;
model iq = miles notfirst2y milesNf2y / clb;
run;

PROC GPLOT DATA=lead;
	PLOT iq*miles = first2y / VAXIS=axis1 HAXIS=axis2 LEGEND=legend1;
	SYMBOL1 I=rl VALUE=circle COLOR=black LINE=3 WIDTH=2;
	SYMBOL2 I=rl VALUE=dot COLOR=black LINE=1 WIDTH=2;
	AXIS1 LABEL = (FONT=ARIAL HEIGHT=2.5 ANGLE=90 POSITION=center)
	VALUE=(FONT=ARIAL HEIGHT=2);
	AXIS2 LABEL = (FONT=ARIAL HEIGHT= 2.5 POSITION=center )
	VALUE=(FONT=ARIAL HEIGHT=2);
	LEGEND1 FRAME LABEL=(FONT=ARIAL HEIGHT= 1.5) VALUE=(FONT=ARIAL
	HEIGHT=1.5)
	POSITION=(bottom inside right) ACROSS=1;
	FORMAT first2y;
RUN;

proc reg data = lead;
model iq = miles first2y milesf2y / clb; 
model iq = miles / clb;
run;

proc reg data = lead;
model iq = miles first2y milesf2y;
first_miles: TEST first2y,milesf2y;
run;
