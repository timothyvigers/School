DATA carotenoids;
INFILE "\\Mac\Home\Documents\School\UC Denver\Biostatistics\Biostatistical Methods 1\Homework 10\carotenoids.dat";
INPUT age sex smoke bmi vitamins calories fat fiber alcohol chol betadiet retdiet betaplas retplas;
RUN;

PROC FORMAT;
value sex 1 = 'Male' 2 = 'Female';
value smoke 1 = 'Never' 2 = 'Former' 3 = 'Current';
value vitamins 1='Yes,regularly' 2 = 'Yes, irregularly' 3 = 'No';
RUN;

data carotenoids;
set carotenoids;
if smoke = 1 THEN never = 1; ELSE never = 0;
IF smoke = 2 THEN former = 1; ELSE former = 0;
IF smoke = 3 THEN current = 1; ELSE current = 0;
RUN;

proc means data = carotenoids mean std stderr;
class smoke;
var betaplas;
run;

proc reg data = carotenoids;
model betaplas = former current / covb;
model betaplas = never former current / noint;
test former-current;
test 0.58*never + 0.42*former - current;
run;

proc ttest data = carotenoids;
where smoke in (2,3);
class smoke;
var betaplas;
run;
