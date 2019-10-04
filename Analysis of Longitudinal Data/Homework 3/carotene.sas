/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      03OCT19
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
   data WORK.carotene    ;
   %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
   infile '\\Mac\Home\Documents\GitHub\School\Analysis of Longitudinal Data\Homework 3\long.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
      informat Id best32. ;
      informat Prepar $3. ;
      informat variable $10. ;
      informat value best32. ;
      informat time best32. ;
      format Id best12. ;
      format Prepar $3. ;
      format variable $10. ;
      format value best12. ;
      format time best12. ;
   input
               Id
               Prepar  $
               variable  $
               value
               time
   ;
   if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */

data carotene;
set carotene;
timesq = time * time;
timecu = time * time * time;
run;

proc mixed data = carotene;
class Prepar;
model value = time timesq timecu Prepar Prepar*time Prepar*timesq Prepar*timecu / solution;
estimate 'group comp' Prepar 1 0 0 -1 Prepar*time 1 0 0 -1 Prepar*timesq 1 0 0 -1 Prepar*timecu 1 0 0 -1/E;
random intercept / subject=Id; 
repeated / type=UN subject=Id; 
run;
