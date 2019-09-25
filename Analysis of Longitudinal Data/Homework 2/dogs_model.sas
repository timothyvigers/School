/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      24SEP19
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
   data WORK.dogs    ;
   %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
   infile '\\Mac\Home\Documents\GitHub\School\Analysis of Longitudinal Data\Homework 2\dog_data.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
      informat group $2. ;
      informat id best32. ;
      informat time best32. ;
      informat y best32. ;
      format group $2. ;
      format id best12. ;
      format time best12. ;
      format y best12. ;
   input
               group  $
               id
               time
               y
   ;
   if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
   run;


proc mixed data=dogs;
class id group time;
model y = group time group*time / solution;
estimate 'group comp' group 1 -1 0/E;
estimate 'time comp' time 1 0 0 0 -1 /E;
contrast 'ch0 vs. co60 comp' group 1 0 -1 time 1 0 -1 0 0 group*time 1 0 0 0 0 0 0 0 0 0 0 0 -1 0 0/E;
contrast 'ch0 vs. cl60 comp' group 1 -1 0 time 1 0 -1 0 0 group*time 1 0 0 0 0 0 0 -1 0 0 0 0 0 0 0/E;
random intercept / subject=id(group); run;
