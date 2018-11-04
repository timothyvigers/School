 DATA hw1;
INPUT id gender chol wtkg age;
 chol2 = chol*chol;
 wt2 = wtkg*wtkg;
 cholwt = chol*wtkg;
 LABEL wtkg = 'Weight (kg)'
 chol = 'Cholesterol (mg/100mL)'
 ;
 DATALINES;
 1 0 254 57 23
 2 1 402 79 57
 3 0 288 63 28
 4 1 354 84 46
 5 0 220 30 34
 6 1 451 76 57
 7 0 405 65 52
 ;
RUN;

PROC REG;
MODEL chol=wtkg/CLB;
RUN;
