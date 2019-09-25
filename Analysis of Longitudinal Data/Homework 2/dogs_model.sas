1     /**********************************************************************
2     *   PRODUCT:   SAS
3     *   VERSION:   9.4
4     *   CREATOR:   External File Interface
5     *   DATE:      24SEP19
6     *   DESC:      Generated SAS Datastep Code
7     *   TEMPLATE SOURCE:  (None Specified.)
8     ***********************************************************************/
9        data WORK.dogs    ;
10       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
11       infile '\\Mac\Home\Documents\GitHub\School\Analysis of Longitudinal Data\Homework 2\dog_data.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
12          informat group $2. ;
13          informat id best32. ;
14          informat time best32. ;
15          informat y best32. ;
16          format group $2. ;
17          format id best12. ;
18          format time best12. ;
19          format y best12. ;
20       input
21                   group  $
22                   id
23                   time
24                   y
25       ;
26       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
27       run;


proc mixed data=dogs;
class id group time;
model y = group time group*time / solution;
estimate 'group comp' group 1 -1 0/E;
estimate 'time comp' time 1 0 0 0 -1 /E;
random intercept / subject=id(group); run;
