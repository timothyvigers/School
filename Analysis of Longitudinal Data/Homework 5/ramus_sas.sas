/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      22OCT19
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
   data WORK.ramus_uni    ;
   %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
   infile '\\Mac\Home\Documents\GitHub\School\Analysis of Longitudinal Data\Homework 5\ramus_uni.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
      informat boy best32. ;
      informat age best32. ;
      informat height best32. ;
      format boy best12. ;
      format age best12. ;
      format height best12. ;
   input
               boy
               age
               height
   ;
   if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
   run;

proc mixed data = ramus_uni;
class age;
model height = age / solution;
estimate "linear" age -3 -1 1 3 / e;
random intercept / subject = boy;
repeated / type = AR(1);
run;
