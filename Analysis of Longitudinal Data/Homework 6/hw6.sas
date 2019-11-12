proc import datafile = '\\Mac\Home\Documents\GitHub\School\Analysis of Longitudinal Data\Homework 6\albuterol.csv'
 out = work.albuterol
 dbms = CSV;
run;

proc glimmix data=albuterol;
model albuterol_use = 
  friday ln_mmax_pm25 temperature pressure humidity / solution distribution=poisson;
random intercept / subject=id;
random _residual_ / subject=id type=ar(1); 
run;
