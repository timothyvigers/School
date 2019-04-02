#-------------------------------------------------------------------------------
#
# Biostatistical Methods Exam 1
# Tim Vigers 10/25/16
#
#-------------------------------------------------------------------------------
library(epiR)

sensitivity <- 31/186
specificity <- 1- (150/2677)
ppv <- (0.17*0.2)/((0.17*0.2)+(0.06*0.8))
npv <- (0.94*0.8)/((0.83*0.2)+(0.94*0.8))
table <- as.table(matrix(c(31,150,155,2527) ,ncol=2,byrow=T))
epi.tests(table)

p1 <- 38/659
p2 <- 61/1359
rr<-p1/p2
se <- sqrt((621/(38*659))+(1298/(61*1359)))
exp(log(rr)+(1.96*se))
exp(log(rr)-(1.96*se))
table <- as.table(matrix(c(38,621,61,1298) ,ncol=2,byrow=T))
epi.2by2(table)