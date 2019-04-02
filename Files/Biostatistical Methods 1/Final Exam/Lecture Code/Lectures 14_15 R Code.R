################################################################
### Companion R Code for Lectures 14-15: One-Way ANOVA and Multiple Testing
### Also see Companion SAS Code for examples of implementation in SAS
################################################################

### Load libraries
library(car) #for ANOVA diagnostic plots, Levene's test
library(MASS)
library(DescTools) #for some types of post-hoc testing
library(ggplot2)

### Read in the two data sets we will be using for examples in Lectures 14/15

## Birth weight and mother's smoking status
BWT <- read.csv("H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/birthweight_smoking_dataset.csv", header=T)

# Tell R the "ordering" for mother's smoking status
BWT$momsmoke <- factor(BWT$momsmoke, c('Non','Former','Light','Heavy'))

## HOX genes csv file
hox <- read.csv("H://Teaching/BIOS 6611/Fall 2018/Lectures/Figure and Example Code for Lectures/HOXgenes.csv", header=T)


################################################################
### Birth weight and smoking related analyses/figures
################################################################

### Create boxplot of birth weights by group (slides 2 and 16)
boxplot( birthwt ~ momsmoke, data=BWT, xlab='Group', ylab='Birth Weight (pounds)')


### Based on our data, determine 95th percentile for the F-dist. using qf function (slide 8)
qf(p=0.95, df1=3, df2=23) 


### One-Way ANOVA (slide 10)
aov.pg10 <- aov( birthwt ~ momsmoke , data=BWT)
summary(aov.pg10) #notice that this summary ANOVA table does not include the "Total" row ("Corrected Total" in SAS-terms)


### One-Way ANOVA as GLM (slides 13-16)
smoke.fit <- lm( birthwt ~ momsmoke, data=BWT )

## Page 14, ANOVA table and GLM regression table
anova(smoke.fit) #the ANOVA table
summary(smoke.fit) #the regression coefficient estimates

## Page 15, diagnostic-type plots to try and represent what SAS provides
# QQ plot to assess normality
qqPlot(smoke.fit)

# Cook's distance plot
cutoff <- 4/((nrow(BWT) - length(smoke.fit$coefficients) - 2))
plot(smoke.fit, which=4, cook.levels=cutoff)

# Studentized residuals plot, similar to lower-left "Residual" plot in SAS output on slide 15 of lecture notes
sresid <- studres(smoke.fit)
hist(sresid, freq = FALSE, main = "Distribution of Studentized Residuals")
xfit <- seq(min(sresid), max(sresid), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)

# Spread-Level Plot, similar to top row's first two plots in SAS output on slide 15
spreadLevelPlot(smoke.fit)


### Welch's ANOVA and testing equality of variances (slides 19-20)

leveneTest( birthwt ~ momsmoke, data=BWT, center=mean) #center=mean chosen to match SAS output, leveneTest documentation notes using center=median is more robust

bartlett.test( birthwt ~ momsmoke, data=BWT)

oneway.test( birthwt ~ momsmoke, data=BWT, var.equal=FALSE) #Welch's ANOVA


### Post-hoc testing

## LSD-related functions
PostHocTest( aov.pg10, method=c('lsd') ) #Least Significant Differences (slides 24-25)
pairwise.t.test( x = BWT$birthwt, g = BWT$momsmoke, p.adjust.method='none') #LSD p-value comparison table on slide 26

## Dunnett test function
DunnettTest( x = BWT$birthwt, g = BWT$momsmoke ) #Dunnett's test (slide 31)

## Bonferroni correction functions
PostHocTest( aov.pg10, method=c('bonferroni') ) #Bonferroni correction (slide 32)
pairwise.t.test( x = BWT$birthwt, g = BWT$momsmoke, p.adjust.method='bonferroni') #alternatively you can use pairwise.t.test and specify the correction

## Tukey HSD functions
PostHocTest( aov.pg10, method=c('hsd') ) #Tukey's HSD (slide 33)
TukeyHSD( aov.pg10 ) #alternatively you can use TukeyHSD function in the 'stat' package which is automatically loaded with R


### Kruskal-Wallis non-parametric ANOVA (slide 42)

kruskal.test( birthwt ~ momsmoke, data=BWT) #compared to SAS< it only produces the Kruskal-Wallis nonparametric ANVOA results (i.e. no parametric ANOVA table or Wilcoxon scores table)

## Dunn's post-hoc comparison testing (slide 43)

DunnTest(birthwt ~ momsmoke, data=BWT)



################################################################
### HOX Genes and False Discovery Rate Example (slides 37-40)
################################################################

## Histograms by grouping of age/response for A3 and FAKE genes

ggplot(hox, aes(x=A3)) + geom_histogram(breaks=seq(0,200,length=6)) + facet_grid(group~.)
ggplot(hox, aes(x=FAKE)) + geom_histogram(breaks=seq(2,12,length=6)) + facet_grid(group~.)

## Calculate p-value for each gene based on Welch's one-way ANOVA 

p.vec <- NULL #initialize object to append p-values to
gene.vec <- c('A3','A4','HOXA5','A7','HOXA9','A10','B3','B6','B9','MEIS1','MEIS2','PBX2','PBX3','FAKE') #vector of genes to test

for(i in gene.vec){
	waov <- oneway.test( as.formula( paste0( i, ' ~ group')), data=hox, var.equal=FALSE) #Welch's ANOVA
	p.vec <- c(p.vec, waov$p.value)
}

p.adjust( p.vec, method='fdr' ) #FDR adjustment for p-values

cbind( gene.vec, round(p.adjust(p.vec, method='fdr' ),4) ) #matrix showing genes and FDR p-values


