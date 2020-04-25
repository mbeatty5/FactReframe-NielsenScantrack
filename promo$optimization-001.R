#### Promotion Dollar Optimization
#### Seminar

## CODE BANK ##


# new.frame <- cbind(dollars.total, dollarsya.total, dollars.d0f1, d0f1.cost, dollarsya.d0f1, ya.d0f1.cost,
#                    dollars.d1f0, d1f0.cost, dollarsya.d1f0, ya.d1f0.cost, dollars.d1f1, d1f1.cost,
#                    dollarsya.d1f1, ya.d1f1.cost, dollars.tpr, tpr.cost, dollarsya.tpr, ya.tpr.cost,
#                    dollars.anypromo, anypromo.cost, dollarsya.anypromo, ya.anypromo.cost)

# new.frame1 <- cbind(item, container, container.mat, size, desc, type, pack, period, dollars.total, 
#                    dollarsya.total, dollars.d0f1, d0f1.cost, dollarsya.d0f1, ya.d0f1.cost,
#                    dollars.d1f0, d1f0.cost, dollarsya.d1f0, ya.d1f0.cost, dollars.d1f1, d1f1.cost,
#                    dollarsya.d1f1, ya.d1f1.cost, dollars.tpr, tpr.cost, dollarsya.tpr, ya.tpr.cost,
#                    dollars.anypromo, anypromo.cost, dollarsya.anypromo, ya.anypromo.cost)


## CODE BANK ##

## Load packages
require(bit64) #allows us to use large integers using 64bit memory so r doesn't choke on UPC numbers
require(data.table) #a structure for dealing with very large tables
require(reshape2) #functions for reshaping data - moving data between columns and rows
require(dplyr) #functions for efficiently operating on data in columns and rows
require(dtplyr) #tranlating the above dplyr package to work with the data.table structres
require(gtools) #an assembly of math functions
require(lattice) # for multivariate graphs
# require(robust) 
# require(robustbase)
# require(MASS)
# require(quantreg)
require(car) # many variations on scatterplots
# require(rms)


### Import data from csv file and store in "data.beer"
## R gets pissy if numbers have commas
data.beer <- read.csv(file.choose(),
            header = TRUE)
View(data.beer)

### Export New Data Frame
#write.table(new.frame1, "C:/Users/cmeuli/Desktop/R/beerframe004.csv", sep = ",")

### Import Trimmed Data
#data.trim1 <- read.csv("C:\\Users\\cmeuli\\Desktop\\R\\beerframe004.csv",
 #                     header = TRUE)
data.trim1 <- read.csv(file.choose(),
                       header = TRUE)
View(data.trim1)

### creating variables
item <- data.beer$UPC
container <- data.beer$PACKAGEGENERALSHAPE
container.mat <- data.beer$PACKAGEMATERIALSUBSTANCE
size <- data.beer$BASESIZE
desc <- data.beer$COMMONCONSUMERNAME
type <- data.beer$COMPETITIVECATEGORY
pack <- data.beer$PACKSIZE
period <- data.beer$Period
dollars.total <- data.beer$Dollars
dollarsya.total <- data.beer$DollarsYA
#dollars.nopromo <- data.beer$NoPromoDollars
#dollarsya.nopromo <- data.beer$NoPromoDollarsYA
dollars.d1f0 <- data.beer$DispwoFeatDollars
dollarsya.d1f0 <- data.beer$DispwoFeatDollarsYA
dollars.d0f1 <- data.beer$FeatwoDispDollars
dollarsya.d0f1 <- data.beer$FeatwoDispDollarsYA
dollars.d1f1 <- data.beer$Feat.DispDollars
dollarsya.d1f1 <- data.beer$Feat.DispDollarsYA
dollars.tpr <- data.beer$PriceDecrDollars
dollarsya.tpr <- data.beer$PriceDecrDollarsYA
dollars.anypromo <- dollars.d1f0 + dollars.d0f1 + dollars.d1f1 + dollars.tpr
dollarsya.anypromo <- dollarsya.d1f0 + dollarsya.d0f1 + dollarsya.d1f1 + dollarsya.tpr

### create cost variables
d1f0.cost <- round(dollars.d1f0*rnorm(24162, mean = .8, sd = .15), digits = 2)
ya.d1f0.cost <- round(dollarsya.d1f0*rnorm(24162, mean = .8, sd = .15), digits = 2)
d0f1.cost <- round((dollars.d0f1^(0.5))*rnorm(24162, mean = .8, sd = .15), digits = 2)
ya.d0f1.cost <- round((dollarsya.d0f1^(0.5))*rnorm(24162, mean = .8, sd = .15), digits = 2)
d1f1.cost <- round((dollars.d1f1^(2))*rnorm(24162, mean = .8, sd = .15), digits = 2)
ya.d1f1.cost <- round((dollarsya.d1f1^(2))*rnorm(24162, mean = .8, sd = .15), digits = 2)
tpr.cost <- round(rnorm(24162, mean = .8, sd = .15)*dollars.tpr^3, digits = 2)
ya.tpr.cost <- round(rnorm(24162, mean = .8, sd = .15)*dollarsya.tpr^3, digits = 2)
anypromo.cost <- d1f0.cost + d0f1.cost + d1f1.cost + tpr.cost
ya.anypromo.cost <- ya.d1f0.cost + ya.d0f1.cost + ya.d1f1.cost + ya.tpr.cost

### Graphs
xyplot(dollars.d1f0~d1f0.cost)
xyplot(dollars.d1f0~d1f0.cost)
xyplot(dollars.tpr~tpr.cost)
xyplot(dollars.tpr~tpr.cost^(1/3))
xyplot(dollars.d0f1~d0f1.cost)
xyplot(dollars.d0f1~d0f1.cost^2)
xyplot(dollars.d1f1~d1f1.cost)
xyplot(dollars.d1f1~sqrt(d1f1.cost))
xyplot(dollars.anypromo~(tpr.cost + d1f0.cost + d0f1.cost + d1f1.cost))
xyplot(dollars.anypromo~(tpr.cost^(1/3) + d1f0.cost + d0f1.cost^2 + sqrt(d1f1.cost)))

### Graphs of Segments

xyplot(dollars.anypromo~(tpr.cost + d1f0.cost + d0f1.cost + d1f1.cost)|type)
xyplot(dollars.anypromo~(tpr.cost^(1/3) + d1f0.cost + d0f1.cost^2 + sqrt(d1f1.cost))|type)

### Regression Preparation
## Variable definitions because lm() doesn't like powers
d0f1.cost.sq <-  d0f1.cost^2
d1f1.cost.sqrt <- sqrt(d1f1.cost)
tpr.cost.curt <- tpr.cost^(1/3)

### Regressions
reg.bfr <- lm(dollars.anypromo~(tpr.cost + d1f0.cost + d1f1.cost + d0f1.cost))
summary(reg.bfr)
reg.aftr <- lm(dollars.anypromo~(tpr.cost.curt + d1f0.cost + d1f1.cost.sqrt + d0f1.cost.sq))
summary(reg.aftr)

### Scatter Plot matrix of correlation
## any pattern is a yellow flag
## Run code for correlation coefficients if pattern
pairs(~tpr.cost + d0f1.cost + d1f0.cost + d1f1.cost)

### Correlation Coefficients
## Any value > 0.5 is a yellow flag, but not a deal breaker
cor(tpr.cost, d0f1.cost)
cor(tpr.cost, d1f0.cost)
cor(tpr.cost, d1f1.cost)
cor(d0f1.cost, d1f0.cost)
cor(d0f1.cost, d1f1.cost)
cor(d1f0.cost, d1f1.cost)

### Confirm Normality Assumption
## 95% of observations should be within +/- 2 sd
res <- residuals(reg.aftr)
std.res <- scale(res)
plot(scale(std.res))
boxplot(scale(std.res))


### Confirm Homoscedasticity
## Cones or half-cones are red flags
xyplot(res~dollars.anypromo, horizontal = FALSE)

### Confirm no serial correlation
## This only needs confirmed if data is time-series
## If p-value < 0.05 serial correlation exists
## In this case, look at Adjusted R^2 and coefficient of
## res[2:n] to see how much
# n <- length(res)
# xyplot(res[1:n-1]~res[2:n])
# ser.cor <- lm(res[1:n-1]~res[2:n])
# summary(ser.cor)









