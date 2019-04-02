1     /**********************************************************************
2     *   PRODUCT:   SAS
3     *   VERSION:   9.4
4     *   CREATOR:   External File Interface
5     *   DATE:      02APR19
6     *   DESC:      Generated SAS Datastep Code
7     *   TEMPLATE SOURCE:  (None Specified.)
8     ***********************************************************************/
9        data WORK.formixed    ;
10       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
11       infile 'C:\Users\Tim\Documents\School\UC Denver\Biostatistics\Biostatistical Methods 2\Homeworks\Homework 6\file2_FEV1.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
12          informat ID best32. ;
13          informat FEV1 best32. ;
14          informat trt best32. ;
15          informat pack_years best32. ;
16          informat current_smoker best32. ;
17          informat emphysema best32. ;
18          informat race best32. ;
19          informat height best32. ;
20          informat bmi best32. ;
21          format ID best12. ;
22          format FEV1 best12. ;
23          format trt best12. ;
24          format pack_years best12. ;
25          format current_smoker best12. ;
26          format emphysema best12. ;
27          format race best12. ;
28          format height best12. ;
29          format bmi best12. ;
30       input
31                   ID
32                   FEV1
33                   trt
34                   pack_years
35                   current_smoker
36                   emphysema
37                   race
38                   height
39                   bmi
40       ;
41       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
42       run;

PROC MIXED DATA=formixed; 
CLASS id; 
MODEL FEV1 = pack_years current_smoker emphysema race height bmi trt / SOLUTION; 
RANDOM INT trt /TYPE=UN SUBJECT=ID G GCORR; 
RUN;

PROC MIXED DATA=formixed; 
CLASS id; 
MODEL FEV1 = pack_years current_smoker emphysema race height bmi trt / SOLUTION; 
RANDOM INT /TYPE=UN SUBJECT=ID G GCORR; 
RUN;
