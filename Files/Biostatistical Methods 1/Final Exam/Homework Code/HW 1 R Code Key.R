####BIOS 6611 HW 1####

#Exercise 1-RLab1
#
set.seed(123) #in order to reproducibly simulate data, need to set a seed

#1a
n <- 10000
norm <- rnorm(n, mean = 125, sd = 8)
pois <- rpois(n, lambda = 1.5)
binom <- rbinom(n, size = 5, prob = 0.15)

#1b
#normal: should be mean = 125, sd = 8
mean(norm)
sd(norm)
#poisson: should be mean = 1.5, sd = 1.22 (sqrt(1.5))
mean(pois)
sd(pois)
#binomal: should be mean = 0.75 (5*0.15), sd = 0.7984 (sqrt(5*0.15*0.85))   
mean(binom)
sd(binom)

#1c
par(mfrow=c(2,3)) #use this to make 1 figure with all 6 plots

hist(norm)
hist(pois)
hist(binom)

boxplot(norm, main='Box plot of norm')
boxplot(pois, main='Box plot of pois')
boxplot(binom, main='Box plot of binom')

#############################################################################
#Exercise 2-Rlab1

#2a
set.seed(2)
nsim <- 1000
n <- 10
meanV <- rep(NA, 1000)
medianV <- rep(NA, 1000)
varV <- rep(NA, 1000)

for(i in 1:nsim){
  random <- rnorm(n, mean = 40, sd = 10)
  meanV[i] <- mean(random)
  medianV[i] <- median(random)
  varV[i] <- var(random)
}

par( mfrow=c(1,3) )

hist(meanV)
hist(medianV)
hist(varV)

#2b
#Normally distributed

#2c
varVshift <- varV*(9/10^2)
hist(varVshift)
curve(dchisq(x, df = 9), col="green", xlim = c(0,25),
      ylim = c(0,0.12))

plot(dchisq(0:25, df = 9), col="green", xlim = c(0,25),
      ylim = c(0,0.12))


#############################################################################
#Exercise 3-Rlab1

#3a and 3b
set.seed(3)
nsim <- 500
sizeVec <- c(10,20,30,40,50)
meanMatrix <- matrix(NA, nrow = 500, ncol = 5)

for(j in 1:5){
	for(i in 1:nsim){
		binomData <- rbinom(sizeVec[j],size=1, prob=0.15)
		meanMatrix[i,j] <- mean(binomData)
	}
}

#3c
apply(meanMatrix,2,mean)
apply(meanMatrix,2,sd)

#3d
par( mfrow=c(2,3) )
sapply(1:5, function(x) hist(meanMatrix[,x], xlab='Mean', main=paste0('n=',sizeVec[x]) ) )

#3e---around 40 or so


#############################################################################
#Exercise 4---Rlab1
set.seed(4)
nsim <- 500
sizeVec <- c(10,50,100,1000)
meanMatrix <- matrix(NA, nrow = 500, ncol = 4)

for(j in 1:4){
  for(i in 1:nsim){
    cauchyData <- rcauchy(sizeVec[j])
    meanMatrix[i,j] <- mean(cauchyData)
  }
}

#They don't look normal
par( mfrow=c(2,2) )
sapply(1:4, function(x) hist(meanMatrix[,x], xlab='Mean', main=paste0('Cauchy, n=',sizeVec[x]) ) )


#############################################################################
#Exercise 5---Rlab1


#5a, sampling from IDs
id.vec <- seq(from=9001, to=9250, by=1) #creates vector of record IDs

set.seed(5) #set seed to reproducibly sample from id.vec
id.sample <- sample(id.vec, 30, replace=F) #randomly samples 30 records for audit


#5b, manipulating names data frame
#5b.i
names <- read.table( 'filepath/names.txt')

#5b.ii (lots of ways one could do this)
names$team <- 'blue' #add column where everyone is on the "blue" team, we will write this over with our sample below
set.seed(52)
names$team[sample(1:nrow(names), size=nrow(names)/2, replace=F)] <- 'red'

#5b.iii
head(names, n=10)

#5b.iv
table(names$team) #should have 15 in each group


#5c, creating data frame and subsetting the data
set.seed(515)
df.5c <- data.frame( id=1:10, age=runif(10, min=20, max=60) )
older <- df.5c[ which(df.5c$age >= 45) ,]
younger <- df.5c[ which(df.5c$age < 45) ,]


#5d, create data frame and store information based on given criteria (lots of ways to do this one)
assignments <- data.frame( id=1:100, dietary_intervention=NA, pharma_intervention=NA )

set.seed(54)
for(i in 1:100){
	assignments$dietary_intervention[i] <- if( runif(1) < 0.30 ){ 'D' }else{ 'ND' }
	assignments$pharma_intervention[i] <- if( rbinom(n=1,size=1,prob=0.30)==1 ){ 'P' }else{ 'NP' }
}

table(assignments$dietary_intervention,assignments$pharma_intervention)


#5e. creating data frame to simulate trial and response
#5e.i
set.seed(55)
df.trial <- data.frame( center=rep(1:5, times=40), improve=rbinom(n=5*40,size=1,prob=0.70) )

#5e.ii
numimprove.vec <- NULL #initialize vector to store number improved at each hospital
for(k in 1:5){
	numimprove.vec <- c(numimprove.vec, sum(df.trial[which(df.trial$center==k),'improve']) )
}

#5e.iii, mean number of patients improving across all hospitals
mean( numimprove.vec )


#############################################################################
#Exercise 6---Rlab1

#6a, read in data
NAWS <- read.csv("filepath/NAWS2014.csv", header=T)

#6b, create histogram of years of schooling
hist(NAWS$A09, main = "Histogram of Educational Attainment \n for Migrant Farmers", xlab = "Years")

#6d, create new column with A09 (years of schooling) represented as a categorical variable
NAWS$category_edu <- "00 - 05"
NAWS[NAWS$A09 >= 6 & NAWS$A09 <= 8, ]$category_edu <- "06 - 08"
NAWS[NAWS$A09 >= 9 & NAWS$A09 <= 11, ]$category_edu <- "09 - 11"
NAWS[NAWS$A09 >= 12, ]$category_edu <- "12+"

#6e, create table and check proportions are equal to 0.250, 0.291, 0.208 and 0.251
round( table(NAWS$category_edu) / sum(table(NAWS$category_edu)), 3)

#6f, mock data set creation
set.seed(6)
mockdata <- data.frame(subject = 1:800, random = runif(n = 800))

mockdata$educ_cat <- '00 - 05'
mockdata$educ_cat[which(mockdata$random >= 0.248 & mockdata$random < 0.248+0.289)] <- '06 - 08'
mockdata$educ_cat[which(mockdata$random >= 0.248+0.289 & mockdata$random < 0.248+0.289+0.212)] <- '09 - 11'
mockdata$educ_cat[which(mockdata$random >= 0.248+0.289+0.212)] <- '12+'

mockdata$educ_years <- NA #create column to store results in
mockdata[which(mockdata$educ_cat=='00 - 05'),'educ_years'] <- runif(n=sum(mockdata$educ_cat=='00 - 05'),0,6)
mockdata[which(mockdata$educ_cat=='06 - 08'),'educ_years'] <- runif(n=sum(mockdata$educ_cat=='06 - 08'),6,9)
mockdata[which(mockdata$educ_cat=='09 - 11'),'educ_years'] <- runif(n=sum(mockdata$educ_cat=='09 - 11'),9,12)
mockdata[which(mockdata$educ_cat=='12+'),'educ_years'] <- runif(n=sum(mockdata$educ_cat=='12+'),12,17)

mockdata$edu_stop_yn <- rbinom(n=800, size=1, prob=0.8)

mockdata$educ_years[which( mockdata$edu_stop_yn == 1 & mockdata$educ_cat=='06 - 08')] <- 6
mockdata$educ_years[which( mockdata$edu_stop_yn == 1 & mockdata$educ_cat=='09 - 11')] <- 9
mockdata$educ_years[which( mockdata$edu_stop_yn == 1 & mockdata$educ_cat=='12+')] <- 12

mockdata$educ_years2 <- as.integer(mockdata$educ_years)

#6f, create histogram of mockdata$educ_years
hist(mockdata$educ_years2, breaks=seq(0,16,1), main = "Histogram of Educational Attainment \n for Mock Data", xlab = "Years")



