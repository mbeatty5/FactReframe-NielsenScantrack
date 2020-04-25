# INSTRUCTIONS:
# 1. Open each excel file, highlight top row, find/replace the following
#     $ to Dollars
#     % to Percent
#     (space) to (blank)
#     - to (blank)
# 2. Save each excel file into the csv folder as file type: CSV (Comma delimited)
#     click YES when warning box pops up asking if you want to use that type
# 3. Run code below
#     MAKE SURE TO CHANGE YEAR VALUE IN NEW COLUMN AND UPDATE THE WRITTEN FILE NAME



# Clears your global environment
rm(list = ls())
# Garbage Collector, clears memory
gc()


## Set working drive
setwd("Z:/Kerry/Stater Bros/10yr bluebook files")


## Load packages
library(bit64) #allows us to use large integers using 64bit memory so r doesn't choke on UPC numbers
library(data.table) #a structure for dealing with very large tables
library(reshape2) #functions for reshaping data - moving data between columns and rows
library(dplyr) #functions for efficiently operating on data in columns and rows
library(dtplyr) #tranlating the above dplyr package to work with the data.table structres
library(gtools) #an assembly of math functions


## READ CSV file with StaterCode
statercodes <- fread(choose.files(),header=TRUE,sep=",",stringsAsFactors = FALSE)
#View(statercodes)


## Read CSV file - SELECT YOUR DESIRED CSV FILE
datafull <- fread(choose.files(),header=TRUE,sep=",",stringsAsFactors = FALSE)
#View(datafull) #Print the first 10 rows


#datafullcopy <- datafull
subset <- datafull[datafull$StaterCode %in% statercodes$StaterCode, ]
#View(subset)


## Adds new column for year - MANUALLY CHANGE THE YEAR IN THE CODE FOR EACH FILE
subset$Year <- "2015"
#View(subset)

#IGNORE
## Write the file, with name specified in code, to the working drive folder - MANUALLY CHANGE FILE NAME FOR EACH YEAR -INCLUDE .CSV
#write.csv(subset, "Ready Pac 2015 2.csv", row.names=FALSE, na="")
#getwd()




## Create subsets for each of the Vendor Codes
subset1 <- subset(datafull, VendorCode=="93297")
subset2 <- subset(datafull, VendorCode=="10720")
subset3 <- subset(datafull, VendorCode=="4890")
subset4 <- subset(datafull, VendorCode=="9574")
subset5 <- subset(datafull, VendorCode=="10753")
subset99999 <- subset(datafull, VendorCode=="99999")
#View(subset1)
#View(subset2)
#View(subset3)
#View(subset99999)


## Create additional subsets for each Ready Pac UPC with dummy Vendor Code 99999
subset9upc1 <- subset(subset99999, UPCCode=="7774500207")
subset9upc2 <- subset(subset99999, UPCCode=="7774520515")
subset9upc3 <- subset(subset99999, UPCCode=="7774521357")


## Combine subsets twice
subset9combined <- rbind(subset9upc1, subset9upc2, subset9upc3, deparse.level = 1)
#View(subset9combined)
combined <- rbind(subset1, subset2, subset3, subset4, subset5, subset9combined, deparse.level = 1)
#View(combined)


## Adds new column for year - MANUALLY CHANGE THE YEAR IN THE CODE FOR EACH FILE
combined$Year <- "2015"
#View(combined)


## Combine the combinations
duplication <- rbind(combined, subset, deparse.level = 1)
#View(duplication)


## Remove duplicate rows
unique <- duplication[!duplicated(duplication), ]


## Write the file, with name specified in code, to the working drive folder - MANUALLY CHANGE FILE NAME FOR EACH YEAR -INCLUDE .CSV
write.csv(unique, "Ready Pac 2015 3.csv", row.names=FALSE, na="")
#getwd()