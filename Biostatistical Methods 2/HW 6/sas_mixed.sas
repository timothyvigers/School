/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      09APR19
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
data WORK.inv3    ;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile '\\Mac\Home\Documents\School\Biostatistical Methods 2\Homeworks\Homework 6\file2_FEV1.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
   informat ID best32. ;
   informat FEV1 best32. ;
   informat trt best32. ;
   informat pack_years best32. ;
   informat current_smoker best32. ;
   informat emphysema best32. ;
   informat race best32. ;
   informat height best32. ;
   informat bmi best32. ;
   format ID best12. ;
   format FEV1 best12. ;
   format trt best12. ;
   format pack_years best12. ;
   format current_smoker best12. ;
   format emphysema best12. ;
   format race best12. ;
   format height best12. ;
   format bmi best12. ;
input
            ID
            FEV1
            trt
            pack_years
            current_smoker
            emphysema
            race
            height
            bmi
;
if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;

/* Random intercept only */

PROC MIXED DATA=inv3; 
CLASS id; 
MODEL FEV1 = pack_years current_smoker emphysema race height bmi trt /s; 
RANDOM INT /TYPE=UN SUBJECT=ID; 
RUN;

/* Random intercept and random slope */

PROC MIXED DATA=inv3; 
CLASS id; 
MODEL FEV1 = pack_years current_smoker emphysema race height bmi trt /s; 
RANDOM INT trt /TYPE=UN SUBJECT=ID G GCORR; 
RUN;

