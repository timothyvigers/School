#Load required libraries
library(ggplot2)
library(reshape2)

#################
#a) sigma = 10 (known); N=15, 20, 25; Detectable Difference between null and alternative means = -15 to 15; alpha = 0.01, 0.05, 0.10 (two-sided). Find Power. 
#For known sigma we need to program our own function

findPowerZ <- function(diff = 5, sd = 1, n = 10, alpha = 0.05) { 
	z.alpha <- qnorm(1 - (alpha/2))
	power <- pnorm(diff/(sd/sqrt(n)) - z.alpha)
	return(power)
}

findPowerZ(diff=abs(5), sd=10, n=20, alpha=0.05)
findPowerZ(diff=abs(-5), sd=10, n=20, alpha=0.05)

#Enter parameters to calculate power for
sd <- 10
n <- rep(c(15, 20, 25), 3)
alpha <- c(rep(0.01, 3), rep(0.05, 3), rep(0.1, 3))
diff <- c(-15:15)
powers <- matrix(rep(NA, 9 * 31), ncol = 9, nrow = 31)
for (i in 1:9) { 
	for (d in 1:31) { 
		p <- findPowerZ(n = n[i], alpha = alpha[i], diff = abs(diff[d]), sd = 10)
		powers[d, i] <- p
	}
}

datanames <- paste(n, alpha)
powers <- as.data.frame(powers)
names(powers) <- datanames

#transform data and plot
p.melt <- melt(powers)
p.melt[, 1] <- as.character(p.melt[, 1])
p.melt[, 3:4] <- matrix(unlist(strsplit(p.melt[, 1], " ")), ncol = 2, byrow = T)
p.melt[, 5] <- rep(diff, 9)
names(p.melt) <- c("label", "power", "n", "alpha", "diff")
qplot(diff, power, data = p.melt, linetype = n, color = alpha, geom = "line",shape = n) +
	geom_point() + theme_bw() + xlab("Difference") + ylab("Power") +
	ggtitle("Achievable Power vs. Difference of Means by Sample Size (N) and Alpha")

#################
#b) s = 10 (sigma unknown); N=15, 20, 25; Detectable Difference between null and alternative means = -15 to 15; alpha= 0.01, 0.05, 0.10 (two-sided). Find Power. 
#R has base functions for unknown sigma

#Example of function
pwrt_b <- power.t.test(n = 15, sd = 10, sig.level = 0.01, delta = -15, type = "one.sample", alternative = "two.sided")
pwrt_b
pwrt_b$power

#Enter parameters to calculate power for
sd <- 10
n <- rep(c(15, 20, 25), 3)
alpha <- c(rep(0.01, 3), rep(0.05, 3), rep(0.1, 3))
diff <- c(-15:15)
powers <- matrix(rep(NA, 9 * 31), ncol = 9, nrow = 31)
for (i in 1:9) { 
	for (d in 1:31) { 
		p <- power.t.test(n = n[i], sig.level = alpha[i], delta = diff[d], sd = 10, type = "one.sample", alternative = "two.sided")
		powers[d, i] <- p$power
	}
}

datanames <- paste(n, alpha)
powers <- as.data.frame(powers)
names(powers) <- datanames

#transform data and plot
p.melt <- melt(powers)
p.melt[, 1] <- as.character(p.melt[, 1])
p.melt[, 3:4] <- matrix(unlist(strsplit(p.melt[, 1], " ")), ncol = 2, byrow = T)
p.melt[, 5] <- rep(diff, 9)
names(p.melt) <- c("label", "power", "n", "alpha", "diff")
qplot(diff, power, data = p.melt, linetype = n, color = alpha, geom = "line",shape = n) +
	geom_point() + theme_bw() + xlab("Difference") + ylab("Power") +
	ggtitle("Achievable Power vs. Difference of Means by Sample Size (N) and Alpha")

#################
#c) s = 10 (sigma unknown); Detectable Difference between null and alternative means = 5 to 10; alpha= 0.01, 0.05, 0.10 (two-sided); Power = 0.80, 0.90, 0.95. Find N.

#Example of function
pwrt_c <- power.t.test(power=0.8, sd = 10, sig.level = 0.01, delta = 5, type = "one.sample", alternative = "two.sided")
pwrt_c
ceiling(pwrt_c$n)

#Enter parameters to calculate sample size for
delta <- c(50:100)/10
power <- rep(c(0.8, 0.9, 0.95), 3)
alpha <- rep(c(0.01, 0.05, 0.1), each = 3)
ns <- matrix(rep(NA, 9 * 51), ncol = 9, nrow = 51)
for (i in 1:9) { 
	for (j in 1:51) { 
		p <- power.t.test(power = power[i], sig.level = alpha[i], delta = delta[j], sd = 10, type = "one.sample", alternative = "two.sided")
		ns[j, i] <- ceiling(p$n)
	}
}

ns <- as.data.frame(ns)
names(ns) <- paste(power, alpha)

#transform data and plot
n.melt <- melt(ns)
n.melt[, 1] <- as.character(n.melt[, 1])
n.melt[, 3:4] <- matrix(unlist(strsplit(n.melt[, 1], " ")), ncol = 2, byrow = T)
n.melt[, 5] <- rep(delta, 9)
names(n.melt) <- c("label", "n", "power", "alpha", "delta")
qplot(delta, n, data = n.melt, linetype = power, color = alpha, geom = "line",shape = power) + 
	geom_point() + theme_bw() + xlab("Difference") + ylab("True Sample Size") +
	ggtitle("Required Sample Size (N) vs. Difference of Means by Power and Alpha")


#################
#d) s = 10 (sigma unknown); N = 15, 20, 25; alpha= 0.01, 0.05, 0.10 (two-sided); Power = 0.80, 0.9, 0.95. Find detectable difference between null and alternative means.

#Example of function
pwrt_d <- power.t.test(power=0.8, sd = 10, sig.level = 0.01, n=15, type = "one.sample", alternative = "two.sided")
pwrt_d
pwrt_d$delta

#Enter parameters to calculate sample size for
alpha <- c(rep(0.01, 9), rep(0.05, 9), rep(0.1, 9))
n <- rep(c(rep(15, 3), rep(20, 3), rep(25, 3)), 3)
power2 <- rep(power, 3)
diffs <- rep(NA, 9 * 3)
sd <- 10

for (i in 1:27) {
	p <- power.t.test(power = power2[i], sig.level = alpha[i], n = n[i], sd = 10,type = "one.sample", alternative = "two.sided")
	diffs[i] <- p$delta
}

diffs <- as.data.frame(diffs)
diffs[, 2] <- alpha
diffs[, 3] <- power
diffs[, 4] <- n
names(diffs) <- c("diffs", "alpha", "power", "ntotal")
diffs$alpha <- factor(diffs$alpha)
diffs$power <- factor(diffs$power)

#plot results
qplot(ntotal, diffs, data = diffs, color = alpha, geom = "line", shape = power,
	linetype = power) + geom_point() + xlab("True Sample Size") + ylab("Difference in Means") +
	ggtitle("Detectable Mean Difference vs. Sample Size (N) by Power and Alpha") +
	theme_bw()


#################
#e) Confidence interval s = 10 (sigma unknown); alpha= 0.01, 0.05, 0.10 (two-sided); halfwidth = 3 6 9 12; prob (halfwidth) = 0.80, 0.9, 0.95. Find N.




findNfromCI <- function(h = 3, pw = 0.95, s = 10) { 
	alpha <- 1 - pw
	z <- qnorm(1 - (alpha/2))
	n <- (z * s/h)^2
	out <- ceiling(n)
	return(out)
}

findNfromCI(3)

halfwidth <- rep(c(3, 6, 9, 12), each = 3)
power <- rep(c(0.8, 0.9, 0.95), 4)
ns <- matrix(rep(NA, 1 * 12), ncol = 1, nrow = 12)
sd <- 10
for (i in 1:12) { 
	nn <- findNfromCI(h = halfwidth[i], pw = power[i], s = sd)
	ns[i] <- ceiling(nn)
}

ns <- data.frame(cbind(ns, power, halfwidth))
names(ns)[1] <- "N"

#plot results
qplot(halfwidth, N, data = ns, linetype = factor(power), color = factor(power),
	geom = "line", shape = factor(power)) + geom_point() + theme_bw() + xlab("Halfwidth") +
	ylab("N needed (estimate)") + ggtitle("Required Sample Size (N) vs. Confidence Interval by Power")


