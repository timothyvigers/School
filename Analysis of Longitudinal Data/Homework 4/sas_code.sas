/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      17OCT19
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
   data WORK.cereal    ;
   %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
   infile '\\Mac\Home\Documents\GitHub\School\Analysis of Longitudinal Data\Homework 4\Cereal2.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
      informat Cond best32. ;
      informat FamIDNO best32. ;
      informat FamMem best32. ;
      informat C1 best32. ;
      informat C2 best32. ;
      informat C3 best32. ;
      informat C4 best32. ;
      informat C5 best32. ;
      informat C6 best32. ;
      informat C7 best32. ;
      informat C8 best32. ;
      informat C9 best32. ;
      informat C10 best32. ;
      informat C11 best32. ;
      informat C12 best32. ;
      informat C13 best32. ;
      informat C14 best32. ;
      informat Sex best32. ;
      informat Age1 best32. ;
      informat Ht1 best32. ;
      informat Wt1 best32. ;
      format Cond best12. ;
      format FamIDNO best12. ;
      format FamMem best12. ;
      format C1 best12. ;
      format C2 best12. ;
      format C3 best12. ;
      format C4 best12. ;
      format C5 best12. ;
      format C6 best12. ;
      format C7 best12. ;
      format C8 best12. ;
      format C9 best12. ;
      format C10 best12. ;
      format C11 best12. ;
      format C12 best12. ;
      format C13 best12. ;
      format C14 best12. ;
      format Sex best12. ;
      format Age1 best12. ;
      format Ht1 best12. ;
      format Wt1 best12. ;
   input
               Cond
               FamIDNO
               FamMem
               C1
               C2
               C3
               C4
               C5
               C6
               C7
               C8
               C9
               C10
               C11
               C12
               C13
               C14
               Sex
               Age1
               Ht1
               Wt1
   ;
   if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
   run;

proc genmod data=cereal;
model cond = c1 sex wt1/dist=poisson link=log;
run;

proc genmod data=cereal;
model cond = c1 sex wt1/dist=poisson link=log pscale;
run;

proc nlmixed data=cereal;
  model cond ~ Poisson(exp(b0 + b1 * sex + b2 * wt1 + eps) );
  random eps ~ normal( 0, sig*sig ) subject=famidno;
run;
