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

PROC REG DATA=amniotic;
MODEL cells=temp /clb;
OUTPUT OUT=resids1 PREDICTED=pred RSTUDENT=jackknife;
RUN;
