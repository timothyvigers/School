*----------------------------------------------------------------------
Lecture 20 SAS Code - Matrix/Linear Algebra
*----------------------------------------------------------------------;

*----------------------------------------------------------------------
* Examples of matrix operations in SAS
*----------------------------------------------------------------------;

ods graphics on;
ods rtf file = "L20.rtf";
run;

/* Adding matrices */
proc iml;
	A1 = {3 4 , 2 2}; /* row1, row2 */
	B1 = {1 3 , 2 4};
	C1 = A1 + B1;
	print C1;
quit;

/* Multiplying matrices */
proc iml;
	A2 = {3 4 , 2 2};
	B2 = {2 1 -5 , 4 -3 1};
	A2B2 = A2 * B2;
	print A2B2;
quit;

/* Transpose */
proc iml;
	A3 = {1 2 4 , 3 7 8 , 9 5 6};
	A3T = A3`;
	print A3T;
quit;

/* Trace */
proc iml;
	A4 = {3 2 4 , 2 7 5 , 4 5 6};
	trA4 = TRACE(A4);
	print trA4;
quit;

/* Determinant */
proc iml;
	A4 = {3 2 4 , 2 7 5 , 4 5 6};
	detA4 = det(A4);
	print detA4;
quit;

/* Determinant and inverse */
proc iml;
	A5 = {1 2 3 , 2 3 1 , 0 1 1};
	detA5 = det(A5);
	print detA5;
	invA5 = inv(A5);
	print invA5;
quit;


*******************************************
* Example of MLR in SAS using matrices    *
* Birthweight & SBP Example Rosner        *
*******************************************;

DATA birth;
INPUT sbp weight age;
LABEL 	sbp 	 = "SBP (mmHg)"
		weight = "Weight (oz)"
		age    = "Age (days)";
DATALINES;
89 135 3
90 120 4
83 100 3
77 105 2
92 130 4
98 125 5
82 125 2
85 105 3
96 120 5
95  90 4 
80 120 2
79  95 3
86 120 3
97 150 4
92 160 3
88 125 3
;
RUN;

PROC REG DATA=birth;
	MODEL sbp = weight age / clb covb xpx;
RUN;

/* Now with matrix notation and PROC IML */
PROC IML;
X={
1 135 3,
1 120 4,
1 100 3,
1 105 2,
1 130 4,
1 125 5,
1 125 2,
1 105 3,
1 120 5,
1  90 4,
1 120 2,
1  95 3,
1 120 3,
1 150 4,
1 160 3,
1 125 3};
Y = {89,90,83,77,92,98,82,85,96,95,80,79,86,97,92,88};
xpx = x`*x;
xpy = x`*y;
ypy = y`*y;
xpxi = INV(xpx); 
betahat = xpxi*xpy;
ssm=betahat`*x`*y-SUM(Y)**2/NROW(y); 
sse=(y-x*betahat)`*(y-x*betahat);
sst=ssm+sse;
sighat2 = sse/(NROW(y)-NROW(betahat));
varb = xpxi*sighat2; 
cb1 = {0 1 0};
b1 = cb1*betahat; 
varb1=cb1*xpxi*cb1`*sighat2;
seb1=SQRT(varb1);
tb1 = cb1*betahat/seb1;
pb1 = 2*PROBT(-ABS(tb1),(NROW(y)-NROW(betahat)));
cb2 = {0 0 1};
b2 = cb2*betahat;
varb2=cb2*xpxi*cb2`*sighat2;
seb2=SQRT(varb2);
tb2 = cb2*betahat/seb2;
pb2 = 2*PROBT(-ABS(tb2),(NROW(y)-NROW(betahat)));

PRINT y x;
PRINT xpx xpy ypy;
PRINT xpxi;
PRINT betahat;
PRINT ssm;
PRINT sse;
PRINT sst;
PRINT sighat2;
PRINT varb;
PRINT b1 seb1 tb1 pb1;
PRINT b2 seb2 tb2 pb2; 
/* NOTE: You can change the field width for printing numeric values with the RESET command */
RESET fw=12;
PRINT xpxi;
RESET fw=4;
PRINT b2 seb2 tb2 pb2;


ods graphics off;
ods rtf close;
run;
