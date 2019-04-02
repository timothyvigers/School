DATA hw1;
	INPUT id gender chol wtkg age;
	chol2 = chol*chol;
	wt2 = wtkg*wtkg;
	cholwt = chol*wtkg;

LABEL wtkg = 'Weight (kg)'
		chol = 'Cholesterol (mg/100mL)'
		;

DATALINES;
1	0	254	57	23
2	1	402	79	57
3	0	288	63	28
4	1	354	84	46
5	0	220	30	34
6	1	451	76	57
7	0	405	65	52
;
RUN;

PROC REG;
	MODEL chol=wtkg/CLB;
RUN;

PROC GPLOT data=hw1;
   PLOT chol*wtkg=1 chol*wtkg=2 / OVERLAY VAXIS=axis1;
   SYMBOL1 INTERPOL=rlcli COLOR=black VALUE=dot;
   SYMBOL2 INTERPOL=rlclm COLOR=black VALUE=dot;
   AXIS1 LABEL = (FONT=ARIAL HEIGHT= 1.5 ANGLE=90 POSITION=center );
RUN; 
