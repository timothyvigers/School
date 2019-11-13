proc import datafile = '\\Mac\Home\Documents\GitHub\School\Analysis of Longitudinal Data\Homework 6\albuterol.csv'
 out = work.albuterol
 dbms = CSV;
run;


proc genmod data = albuterol;
class id friday;
model albuterol_use = date friday ln_mmax_pm25 temperature pressure humidity / dist=poisson;
repeated subject = id / type = AR(1) modelse; 
run;


proc glimmix data=albuterol pconv=0.000001;
class id friday;
model albuterol_use = date friday ln_mmax_pm25 temperature pressure humidity / dist=poisson solution;
random intercept / subject=id;
random _residual_ / subject=id type=sp(exp)(date); 
run;


proc means data=albuterol;
run;

