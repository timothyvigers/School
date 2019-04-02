##########################
### R Lab 0 Answer Key ###
##########################

#######
###Datasaurus Problem

## Part A
# Read datasaurus dataset in (replace ~ with your path file)
dino <- read.csv(file = "~/datasaurus.csv", header = T)

# Show data
dino

## Part B
# First few rows of dataset
head(dino)

# Column/Variable names
names(dino)

## Part C
# Histogram of x
hist(dino$x)

# Boxplot of x
boxplot(dino$x)

# Dot plot of x
plot(dino$x)

## Part D
# Scatterplot of y vs x
plot(dino$x, dino$y)

## Part E 
# Request more info on basic plot function
?plot

# modified plot code
plot(dino$x, dino$y, main="T-Rex", sub="technology is truly amazing")

## Part F
typeof(dino$x)
dino$x[1:2] <- c("hello", "there")
typeof(dino$x)

# Why do the first and last line of code in Part F differ? 
## Hint: run "head(dino)" to see what the data frame dino now looks like
## The output from the first and third line differ because each column in our data frame must be of the same type of data, and by replacing the numbers with characters we have forced the entire vector of column "x" to be character.

## Part G 
identical(dino$y, dino[ , 2])

# Why is TRUE returned here?
## You can reference specific columns of a data frame in multiple ways. In this case our column named "y" is the second column of the data frame, hence they're comparing the same information

## Part H, what typeof() R vector is returned here?
typeof( dino$y < 20 )  # Logical vector returned here

## Part I, indexing in R
dino_subset_1 <- dino[dino$y < 20, ]
dino_subset_2 <- dino[c(1, 3, 6:10), ]

# What can you conclude about ways you can index things in R?
## For subset 1, we've only extracted rows where values of "y" are less than 20
## For subset 2, we've only extracted rows 1, 3, and 6-10
## You can index things by:
### - name (of the row or column)
### - the number (of the row or column)
### - by using logic and/or matching

## Part J, combine B and F -> change column names to my first and last name.
names(dino) <- c("alex", "kaizer")
head(dino) #check that column names were changed




#######
###Traffic Accidents Problem

## Part A, read dataset in and name the data frame traffic (replace ~ with your path file)
traffic <- read.table(file = "~/denvertraffic.txt", header = T)

## Part B, print the column names
colnames(traffic)

## Part C, run the following command given in R Lab 0
dia_only <- traffic[traffic$NEIGHBORHOOD_ID == "DIA",  ]
head(dia_only)

## Part D, run code given in R Lab 0
table(traffic$OFFENSE_TYPE_ID)

## Part E, what does the following code do
traffic[traffic$OFFENSE_TYPE_ID == "TRAF-ACCIDENT-", "OFFENSE_TYPE_ID"] <- "TRF-ACCIDENT"
# Answer: It replaces any record in the OFFENSE_TYPE_ID column that exactly matches "TRAF-ACCIDENT-" with "TRF-ACCIDENT" to match the other existing category

## Part F, subset traffic data frame to only include offenses marked as DUI
dui_only <- traffic[traffic$OFFENSE_TYPE_ID == 'TRF-ACDT-DUI',]

## Part G, plot latitude and longitude of all traffic accidents
plot( x=traffic$GEO_LON, y=traffic$GEO_LAT, col='gray65')

## Part H, color the dia_only points in blue and dui_only points in red using the points() function
points( x=dia_only$GEO_LON, y=dia_only$GEO_LAT, col='blue')
points( x=dui_only$GEO_LON, y=dui_only$GEO_LAT, col='red')

## Part I, the pound sign can be used to add comments to your code, annotate what each part is doing
# save a table of records involving a bicycle by neighborhood in object called bike_accidents
bike_accidents <- table(traffic$NEIGHBORHOOD_ID,traffic$BICYCLE_IND)

# identify how many total elements are in the table (i.e., number of rows x number of columns)
length(bike_accidents)

# determine proportion of accidents in each neighborhood that are bike accidents
prop <- bike_accidents[,2] / (bike_accidents[,1]+bike_accidents[,2])

# identify the largest proportion in prop
max(prop)

# identify which neighborhood has the highest proportion
names(prop)[ prop == max(prop) ]

