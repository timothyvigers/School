#Load required libraries
library(Epi)
library(ggplot2)

#Read in data set HOXgenes.txt
hox <- read.table(header=T,'C:\\Users\\timbv\\Documents\\School\\UC Denver\\Biostatistics\\Biostatistical Methods 1\\Homework 5\\HOXgenes.txt')

#Create new variable for defined response
hox$nevent <- hox$response
hox$nevent[hox$response == 0] <- "CR"
hox$nevent[hox$response == 1] <- 'Relapse/Death'

#Create hypothetical variables for shifts of 30 and 50 in CR group
hox$HOXA9 <- hox$HOXA9 - 0.001 #add noise to make histograms more easily plotted
hox$HOXA9_30 <- hox$HOXA9_50 <- hox$HOXA9
hox$HOXA9_30[which(hox$response==0)] <- ( hox$HOXA9 + 30 )[which(hox$response==0)]
hox$HOXA9_50[which(hox$response==0)] <- ( hox$HOXA9 + 50 )[which(hox$response==0)] 

###Create histogram comparing those who had complete response vs. those who relapsed or died
qplot(HOXA9, data=hox, facets=nevent ~ ., geom='histogram', ylab='Count', breaks=seq(0,300,25), xlim=c(0,300)) + labs(title='HOX A9')
qplot(HOXA9_30, data=hox, facets=nevent ~ ., geom='histogram', ylab='Count', ylim=c(0,10), breaks=seq(0,300,25), xlim=c(0,300)) + labs(title='HOX A9 Hypothetical Shift 30')
qplot(HOXA9_50, data=hox, facets=nevent ~ ., geom='histogram', ylab='Count', ylim=c(0,10), breaks=seq(0,300,25), xlim=c(0,300)) + labs(title='HOX A9 Hypothetical Shift 50')

###Create ROC plot
roc1 <- ROC(form = response ~ HOXA9, data=hox, plot="ROC", main='HOX A9 Example ROC Curve from Epi Package')
roc2 <- ROC(form = response ~ HOXA9_30, data=hox, plot="ROC")
roc3 <- ROC(form = response ~ HOXA9_50, data=hox, plot="ROC")

#Overlay all 3
# overlaying the ROC curves
par(col = "blue", lty = 2)
ROC(form = response ~ HOXA9, data = hox, plot = "ROC", PV = T, MI = F, grid = F, AUC = F)
par(new = T, col = "red", lty = 1)
ROC(form = response ~ HOXA9_50, data = hox, plot = "ROC", PV = T, MI = F, grid = F, AUC = F)
par(new = T, col = "black", lty = 3)
ROC(form = response ~ HOXA9_30, data = hox, plot = "ROC", PV = T, MI = F, grid = F, AUC = F, main='Comparing All 3 Scenarios')
legend('bottomright', col=c('blue','black','red'), lty=c(2,3,1), legend=c('HOX A9','+30','+50'), bty='n')
