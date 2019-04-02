#################################################################
###Permutation chi-square example

X <- matrix(nrow=3, byrow=T, c(172,52,313,103,258,119),
            dimnames=list(c("18-29","30-49",">50"),c("for","against")))

prop.table(X, margin=1)

# first run install.packages("epitools") - only have to do this once
# then attach the libary using library(epitools)
# The expand.table() function will expand the contingency table
# to individual level data on which we can run permutations

library(epitools)
dat <- expand.table(X)

# define a function to do the chisquare test

chisq<-function(Obs)
{ #Obs is the observed contingency table
  Expected <- outer(rowSums(Obs),colSums(Obs))/sum(Obs)
  sum((Obs-Expected)^2/Expected)
}

# do a permutation test

# first compute observed statistic

agegrp <- dat[,1]
response <- dat[,2]

observed <- chisq(table(agegrp, response))
observed

B <- 10^5-1  #set number of times to repeat this process
result <- numeric(B) # space to save the random differences

for(i in 1:B)
{
  agegrp.permuted <- sample(agegrp)
  perm.table <- table(agegrp.permuted, response)
  result[i] <- chisq(perm.table)
}

##Plot

hist(result, freq=FALSE, xlab = expression(Chi^2), main="Permutation distribution for chi-square statistic")
abline(v = observed, col = "blue", lty=5)
curve(dchisq(x, 2), add=TRUE, col="green", lwd=2)

#Compute P-value from the permutation distribution
(sum(result >= observed)+1)/(B + 1)  #P-value

# compute p-value from chi-square distribution
1-pchisq(observed, df=2)


#################################################################
###Wilcoxon rank sum example

Y <- matrix(nrow=8, byrow=T, c(5,1,9,5,6,4,3,4,2,8,0,5,0,2,0,1),
            dimnames=list(c(20,25,seq(30,80,10)),c("dom","sexlink")))

eye.test <- expand.table(Y) #from epi.tools package used in previous example
colnames(eye.test) <- c('acuity','grp')
eye.test$acuity <- as.numeric(eye.test$acuity)
eye.test$d_sl <- as.numeric(eye.test$grp)

library(exactRankTests)

# Two-sided exact test
wilcox.exact(acuity ~ d_sl, eye.test)

# Two-sided asymptotic test
wilcox.exact(acuity ~ d_sl, eye.test, exact=F)


