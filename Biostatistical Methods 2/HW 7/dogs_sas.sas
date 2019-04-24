/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      23APR19
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
data WORK.dogs    ;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile '\\Mac\Home\Documents\School\Biostatistical Methods 2\Homeworks\Homework 7\dogdata_long.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
   informat VAR1 $4. ;
   informat X best32. ;
   informat dog best32. ;
   informat trt $18. ;
   informat id best32. ;
   informat minutes best32. ;
   informat GBV best32. ;
   informat time_cat $4. ;
   informat sas_trt $18. ;
   format VAR1 $4. ;
   format X best12. ;
   format dog best12. ;
   format trt $18. ;
   format id best12. ;
   format minutes best12. ;
   format GBV best12. ;
   format time_cat $4. ;
   format sas_trt $18. ;
input
            VAR1  $
            X
            dog
            trt  $
            id
            minutes
            GBV
            time_cat  $
            sas_trt  $
;
if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;

proc mixed data=dogs;
class id sas_trt time_cat;
model gbv = sas_trt*time_cat / s;
random intercept / subject = id;
CONTRAST "Chol. vs Can." 
     sas_trt*time_cat -1 0 0 0 0 1 0 0 0 0 0 0 0 0 0,
     sas_trt*time_cat 0 -1 0 0 0 0 1 0 0 0 0 0 0 0 0,
     sas_trt*time_cat 0 0 -1 0 0 0 0 1 0 0 0 0 0 0 0,
     sas_trt*time_cat 0 0 0 -1 0 0 0 0 1 0 0 0 0 0 0,
     sas_trt*time_cat 0 0 0 0 -1 0 0 0 0 1 0 0 0 0 0/E;
run;
