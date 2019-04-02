#BIOS 6611 Homework 6 Example R Code

###################
# Part A
library(epiR) # See use of this in Lecture 10

gvhd <- read.table("H:/Teaching/BIOS 6611/Fall 2018/Homework/Week 6/gvhd.txt", header = T)
gvhd <- gvhd[gvhd$hla.matched.sibling == 1, ]

summary <- table(gvhd$treatment, gvhd$outcome)


###################
# Part B - proportion with GvHD in each Treatment group
summary["A", "1"] / sum(summary["A", ])
summary["B", "1"] / sum(summary["B", ])


###################
# Part C - association between Treatment and Incidence of GvHD
# From Lecture 12: Define a function to do the chisquare test

chisq<-function(Obs) #uncorrected chi-square statistic
{ #Obs is the observed contingency table
  Expected <- outer(rowSums(Obs),colSums(Obs))/sum(Obs)
  sum((Obs-Expected)^2/Expected)
}

# Do a permutation test
# First compute observed statistic
observed <- chisq(table(gvhd$treatment,gvhd$outcome))
observed

B <- 10^4-1  #set number of times to repeat this process
result <- numeric(B) # space to save the random differences

for(i in 1:B)
{
  treat.permuted <- sample(gvhd$treatment)
  perm.table <- table(treat.permuted, gvhd$outcome)
  result[i] <- chisq(perm.table)
}

# Compute p-value from the permutation distribution
(sum(result >= observed)+1)/(B + 1)  #P-value

# Compute p-value from chi-square distribution
1-pchisq(observed, df=1) 


###################
# Part D - create a grid
p_grid <- seq(from = 0, to = 1, length.out = 30)


###################
# Part E - calculate likelihood for treatment A
# Treatment A
likelihood <- dbinom(x = sum(summary["A", "1"]),
                     size = sum(summary["A", ]),
                     prob = p_grid)

###################
# Part F - define prior
prior_MRD <- ifelse( p_grid > 0.1 & p_grid  < 0.4, 0.3, 0)


###################
# Part G - calculate posterior distribution
posterior <- likelihood * prior_MRD / sum(likelihood * prior_MRD)


###################
# Part H - calculate mean of prior and posterior distribution
# Prior mean
sum(p_grid * prior_MRD/sum(prior_MRD) )  # Note that the denominator is needed in order to obtain a properly weighted mean based on the prior

# Posterior mean
sum(p_grid  * posterior) # This is already a proper distribution from G above


###################
# Part I - Plots overlaid
plot(p_grid, prior_MRD,
     xlab = "Posterior Probability of GvHD\n(Treatment A)",
     ylab = "Density", type = 'b', col = "black", ylim=c(0,0.31))
lines(p_grid , posterior, type = 'b', col = "red")
lines(p_grid , likelihood, type = 'b', col = "blue")
legend('topright', col=c('black','red','blue'), legend=c('Prior','Posterior','Likelihood'), lty=1 )

###################
# Part J - Extra Credit
# Treatment B
# ------------------------------
#Part E for EC
likelihoodB <- dbinom(x = sum(summary["B", "1"]),
                      size = sum(summary["B", ]),
                      prob = p_grid)

#Part G for EC (note Part F is same for prior_MRD for treatments A and B)
posteriorB <- likelihoodB * prior_MRD / sum(likelihoodB * prior_MRD)

#Part H for EC
sum(p_grid  * prior_MRD/sum(prior_MRD) ) 
sum(p_grid * posteriorB)

#Part I for EC
plot(p_grid, prior_MRD,
     xlab = "Posterior Probability of GvHD\n(Treatment B)",
     ylab = "Density", type = 'b', col = "black", ylim=c(0,0.34))
lines(p_grid , posteriorB, type = 'b', col = "red")
lines(p_grid , likelihoodB, type = 'b', col = "blue")
lines(p_grid, posterior, type='b', col='gray65')
legend('topright', col=c('black','red','blue','gray65'), legend=c('Prior','Posterior','Likelihood','Posterior for Trt A'), lty=1 )


###################
# Part K - Extra Credit
# Small subsample - Treatment A
set.seed(1234) # For reproducibility!
gvhda <- gvhd[which(gvhd$treatment=='A'),]
gvhdsub <- gvhda[sample(1:nrow(gvhda), 10, replace=FALSE), ]

summary_sub <- table(gvhdsub$treatment,gvhdsub$outcome)

likelihood_As <- dbinom(x = sum(summary_sub["A", "1"]),
                     size = sum(summary_sub["A", ]),
                     prob = p_grid)

posterior_As <- likelihood_As * prior_MRD / sum(likelihood_As * prior_MRD)

plot(p_grid, prior_MRD,
     xlab = "Posterior Probability of GvHD\n(Treatment A)",
     ylab = "Density", type = 'b', col = "black")
lines(p_grid , posterior_As, type = 'b', col = "red")
lines(p_grid , likelihood_As, type = 'b', col = "blue")
legend('topright', col=c('black','red','blue'), legend=c('Prior','Posterior','Likelihood'), lty=1 )

# Mean based on prior - same as above
sum(p_grid  * prior_MRD)/sum(prior_MRD)

# Posterior mean based on smaller sample - closer to prior than larger set of data
sum(p_grid  * posterior_As)










