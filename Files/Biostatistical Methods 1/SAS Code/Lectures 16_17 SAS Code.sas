*----------------------------------------------------------------------
Lectures 16-17 SAS Code
*----------------------------------------------------------------------;

      
*----------------------------------------------------------------------
* FEV Data Set
*----------------------------------------------------------------------
Read in FEV data set;

proc import datafile="H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/FEV_rosner.csv"
     out=fev /* name for data set for SAS to reference */
     dbms=csv /* identify file as csv */
     replace; /* overwrite BWT if already present */
     getnames=yes; /* take first row as column names from data */
run;


*----------------------------------------------------------------------
*----------------------------------------------------------------------
Carry out simple linear regression; 

proc reg data=fev;
	model fev = age;
run;

