# HW 5 solutions
# Choo Liu (cuining.liu@ucdenver.edu), modified by Alex Kaizer (alex.kaizer@ucdenver.edu)
# ================================================

###Parts A and B 

##Create dataframe of information from table
cmms <- data.frame(
	d = c(2, 1, 4, 5, 3, 1),
	nd = c(0, 0, 3, 9, 16, 18),
	score = c(5, 10, 15, 20, 25, 30)
)

cmms #print data frame to check values


##Create function to summarize the false positives, true positives, false negatives, and true negatives to calculate sensitivity and specificity

test_CMMS <- function(cmms, threshold) {
#cmms: feed in cmms data (or for future use, any similarly structured data set)
#threshold: threshold score to use

	fp <- sum( cmms[cmms$score<=threshold, "nd"] )
	tp <- sum( cmms[cmms$score<=threshold, "d"] )
  
	tn <- sum( cmms[cmms$score>threshold, "nd"] )
	fn <- sum( cmms[cmms$score>threshold, "d"] )
  
	res <- c(sensitivity = tp / (tp + fn), specificity = tn / (tn + fp))

	return(res)
}

##Calculate sens and spec for all possible score thresholds

roc <- rbind(cutoff= seq(0,30,by=5), sapply(seq(0,30,by=5), function(x) test_CMMS(cmms=cmms, threshold=x)) )

roc #print table of results



###Part E - Plot ROC curve and obtain AUC

## First, expand the dataframe above separtely for those w/ and w/o dementia 
# For those with dementia:
dem_expand <- data.frame(score=rep(cmms$score, cmms$d))
r_dem1 <- rep(1, length(dem_expand))

# Merge the two columns for those with dementia 
dem_r <- cbind(dem_expand, r_dem1)

# For those without dementia:
nodem_expand <- data.frame(score=rep(cmms$score, cmms$nd))
r_dem1 <- rep(0, length(nodem_expand))

# Merge the two columns for those without dementia 
dem_nr <- cbind(nodem_expand, r_dem1)

# Now merge/bind the rows of the two data frames together into one data frame
cmms_exp <- rbind(dem_r, dem_nr)

# Now obtain the ROC plot with AUC using the Epi package function - Lecture 9
library(Epi)
ROC(form = r_dem1 ~ score, data = cmms_exp, plot = "ROC", main = "ROC for CMMS Score"  )



###Part F: extra credit-plot ROC curve based on answers from part B and implement the trapezoidal rule to obtain AUC

plot(1 - roc["specificity", ], roc["sensitivity", ], type = "o", xlab='1-Specificity', ylab='Sensitivity')

##Create function for trapezoidal rule calculation
auc_trap_calculation <- function(index){
  
	x1 <- 1 - roc["specificity", index]
	y1 <- roc["sensitivity", index]
  
	x2 <- 1 - roc["specificity", index + 1]
	y2 <- roc["sensitivity", index + 1]
  
	abs( (y1 + y2) * (x2 - x1) /2 )
}

sum( sapply(1:(ncol(roc) - 1), auc_trap_calculation) )



###Part G: calculuate PPV and NPV for given prevalence values based on a cutoff of <=20
Se <- as.numeric( roc['sensitivity', which(roc['cutoff',]==20)] )
Sp <- as.numeric( roc['specificity', which(roc['cutoff',]==20)] )

##10% prevalence
p <- 0.10
PPV_10 <- Se*p/(Se*p + (1-Sp)*(1-p))
PPV_10

NPV_10 <- Sp*(1-p)/((1-Se)*p + Sp*(1-p))
NPV_10

##40% prevalence
p <- 0.40
PPV_40 <- Se*p/(Se*p + (1-Sp)*(1-p))
PPV_40

NPV_40 <- Sp*(1-p)/((1-Se)*p + Sp*(1-p))
NPV_40



###Part H: Extra credit
##H.i-calculuate LR+ and LR- values

LR_POS <- roc["sensitivity", ] / (1 - roc["specificity", ])
LR_POS

LR_NEG <- (1 - roc["sensitivity", ]) / roc["specificity", ] 
LR_NEG

LR <- rbind(roc, LR_POS, LR_NEG) #append LR values to roc
LR


##H.ii-posterior odds
Pr_Odds_Dem <- 0.3
Pr_Odds_Dem

Pr_Odds_NoDem <- 1/Pr_Odds_Dem
Pr_Odds_NoDem

Post_Odds_Dem <- Pr_Odds_Dem*LR_POS
Post_Odds_NoDem <- Pr_Odds_NoDem*(1/LR_NEG)

Posterior <- rbind(LR, Pr_Odds_Dem, Post_Odds_Dem, Pr_Odds_NoDem, Post_Odds_NoDem)    
Posterior



