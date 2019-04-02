# HW 6 solutions
# Code written by Anna Baron and Alex Kaizer (alex.kaizer@ucdenver.edu)
# 2017/2018

###################
# Question 1 - Bootstrap with the Procedure Cost Data Set
# Read in the Cost Data

df <- read.csv("H:/Teaching/BIOS 6611/Fall 2018/Homework/Week 7/ProcedureCost.csv")
df

### Part 1.i - bootstrap of new procedure mean
## 1.i.a - plots of observed data

par(mfrow=c(1,3)) #create plotting area for 3 figures in one row

hist(df$Cost[df$Procedure==2], main='New Procedure Cost', xlab='Cost in 1000s') #Histogram
qqnorm(df$Cost[df$Procedure==2]); qqline(df$Cost[df$Procedure==2]) #Normal Q-Q Plot
boxplot(df$Cost[df$Procedure==2], main='New Procedure Cost', ylab='Cost in 1000s') #Box Plot


## 1.i.c - provide summary statistics for observed cost
m <- aggregate(Cost ~ Procedure, data = df, FUN = mean)[,2] #calculate mean for each procedure
sd <- aggregate(Cost ~ Procedure, data = df, FUN = sd)[,2] #calculate standard deviation for each procedure

m[2] #mean for new procedures
sd[2] #sd for new procedures


## 1.i.d/e/f/g - Bootstrap sampling
nN <- length(df$Procedure[df$Procedure==2]) #identify sample size with procedure 2
nN

B <- 10^4 #set number of bootstrap iterations

boot.mean <- numeric(B) #initialize vector to store bootstrap mean estimates in
set.seed(515) #set seed for reproducibility

for (i in 1:B){
	xB <- sample(df$Cost[df$Procedure==2], nN, replace = TRUE)
	boot.mean[i] <- mean(xB)
}


## 1.i.d - bootstrap plots

par(mfrow=c(1,2)) #create plotting area for 2 figures in one row

hist(boot.mean, main='Bootstrap Dist. of Means - New', xlab='Mean Cost') #histogram of mean estimates from bootstrap
abline(v = m[2], col = "blue", lty = 2) #observed mean from procedure cost data
qqnorm(boot.mean); qqline(boot.mean) #Q-Q plot


## 1.i.f - mean, SE, and bias of bootstrap distribution

mean(boot.mean) # bootstrap mean
mean(boot.mean)-mean(df$Cost[df$Procedure==2]) # bias for New procedure
sd(boot.mean) # bootstrap SE


## 1.i.g - 95% normal percentile and 95% bootstrap percentile confidence intervals

# Obtain Normal percentile 95% CI and estimate of coverage
LLN <- mean(boot.mean)-1.96*sd(boot.mean) # Lower limit of 95% Normal CI
LLN
ULN <- mean(boot.mean)+1.96*sd(boot.mean) # Upper limit of 95% Normal CI
ULN
sum(boot.mean > ULN)/B # Coverage of CI at upper end
sum(boot.mean < LLN)/B # Coverage of CI at lower end

# Obtain bootstrap percentile 95% CI and estimate of accuracy
quantile(boot.mean, c(0.025, 0.975))
(mean(boot.mean)-mean(df$Cost[df$Procedure==2])) / sd(boot.mean) #bias/bootstrap SE for potential accuracy of bootstrap CI, values exceeding +/-0.10 indicate worse accuracy


### Part 1.ii - Sampling distribution of Ratio of Means New to Standard
## 1.ii.a/b - Bootstrap sampling
B <- 10^5 #set number of bootstraps
cost.ratio.mean <- numeric(B) #initialize vector to store results in

nS <- length(df$Procedure[df$Procedure==1]) #determine sample size of standard procedure
nS

set.seed(515) #set seed for reproducibility

for (i in 1:B){
	Standard.boot <- sample(df$Cost[df$Procedure==1], nS, replace = TRUE)
	New.boot <- sample(df$Cost[df$Procedure==2], nN, replace = TRUE)
	cost.ratio.mean[i] <- mean(New.boot)/mean(Standard.boot)
}


## 1.ii.a - bootstrap plots, calculation of mean, SE, bias

par(mfrow=c(1,2)) #create plotting area for 2 figures in one row

hist(cost.ratio.mean, main='Bootstrap Dist. of New/St', xlab='New/Standard Cost Ratio')
qqnorm(cost.ratio.mean); qqline(cost.ratio.mean)

mean(cost.ratio.mean) # bootstrap mean
mean(cost.ratio.mean)-(mean(df$Cost[df$Procedure==2])/mean(df$Cost[df$Procedure==1])) # bias for ratio
sd(cost.ratio.mean) # bootstrap SE


## 1.ii.b - 95% normal percentile and 95% bootstrap percentile confidence intervals

# Obtain Normal percentile 95% CI and estimate of coverage
LL <- mean(cost.ratio.mean)-1.96* sd(cost.ratio.mean) # Lower limit of 95% Normal CI
LL
UL <- mean(cost.ratio.mean)+1.96* sd(cost.ratio.mean) # Upper limit of 95% Normal CI
UL
sum(cost.ratio.mean < LL)/B # Coverage of CI at lower end
sum(cost.ratio.mean > UL)/B # Coverage of CI at upper end

# Obtain bootstrap percentile 95% CI and estimate of accuracy
quantile(cost.ratio.mean, c(0.025, 0.975))
( mean(cost.ratio.mean)-(mean(df$Cost[df$Procedure==2])/mean(df$Cost[df$Procedure==1])) ) / sd(cost.ratio.mean) # bootstrap CI accuracy



###################
# Question 2 - False Discovery Rate correction for multiple SNP testing

# create vector of p-values

pvec2 <- c(0.04,0.1,0.4,0.55,0.34,0.62,0.001,0.01,0.8,0.005)
fdr.vals <- p.adjust( pvec2, method='fdr') #calculate FDR adjusted p-values

# create matrix to summarize SNP and FDR value to identify which are still significant
matrix( c( 1:10, round(fdr.vals, 3)), nrow=10, byrow=F, dimnames=list( 1:10, c('SNP','FDR') ) )  #7, 8, and 10 are sig still



###################
# Question 3 - Lung Function and ANOVAs

# Create data set from table

lung <- data.frame( group=c( rep('A',5), rep('B',12), rep('C',5) ), 
	react=c(20.8,4.1,30,24.7,13.8, 7.5,7.5,11.9,4.5,3.1,8,4.7,28.1,10.3,10,5.1,2.2, 9.2,2,2.5,6.1,7.5) )

## 3.i - ANOVA with equal variances assumed
aov.lung <- aov( react ~ group, data=lung)
anova(aov.lung)

## 3.ii - Tukey's HSD post-hoc testing
TukeyHSD(aov.lung)

## 3.iii - ANOVA without equal variance assumption
oneway.test( react ~ group, data=lung, var.equal=FALSE)




