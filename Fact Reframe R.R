rm(list = ls())

gc()

data.factswap <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

View(data.factswap)

data.cut <- data.factswap

drop <- c("Food.Retailers", "PB.Total.Store", "NB.Total.Store", "Current.Scan.Periods")

data.cut <- data.cut[,!names(data.cut) %in% drop]

View(data.cut)

## data.cut.copy <- data.cut

attach(data.cut)



idvars <- c("All.Markets", 
            "All.Products", 
            "All.Periods")
variablename <- c("Fact")

## TOTAL

all.col <- c("All.Markets", 
             "All.Products", 
             "All.Periods", 
             "Dollars", 
             "Units", 
             "EQ", 
             "PercentACV", 
             "TDP", 
             "Avg.Number.of.Items")
measurevars <- c("Dollars", 
                 "Units", 
                 "EQ", 
                 "PercentACV", 
                 "TDP", 
                 "Avg.Number.of.Items")
valuename <- c("Total")
dm.total <- melt(data.cut[,all.col], 
                 id.vars = idvars, 
                 measure.vars = measurevars, 
                 variable.name = variablename, 
                 value.name = valuename)
View(dm.total)

## ANY PROMO

all.col <- c("All.Markets", 
             "All.Products", 
             "All.Periods", 
             "Any.Promo.Dollars", 
             "Any.Promo.Units", 
             "Any.Promo.EQ", 
             "Any.Promo.PercentACV", 
             "Any.Promo.TDP", 
             "Any.Promo.Avg.Number.of.Items")
measurevars <- c("Any.Promo.Dollars", 
                 "Any.Promo.Units", 
                 "Any.Promo.EQ", 
                 "Any.Promo.PercentACV", 
                 "Any.Promo.TDP", 
                 "Any.Promo.Avg.Number.of.Items")
valuename <- c("Any.Promo")
dm.any.promo <- melt(data.cut[,all.col], 
                     id.vars = idvars, 
                     measure.vars = measurevars, 
                     variable.name = variablename, 
                     value.name = valuename)

levels(dm.any.promo$Fact)[match("Any.Promo.Dollars",levels(dm.any.promo$Fact))] <- "Dollars"
levels(dm.any.promo$Fact)[match("Any.Promo.Units",levels(dm.any.promo$Fact))] <- "Units"
levels(dm.any.promo$Fact)[match("Any.Promo.EQ",levels(dm.any.promo$Fact))] <- "EQ"
levels(dm.any.promo$Fact)[match("Any.Promo.PercentACV",levels(dm.any.promo$Fact))] <- "PercentACV"
levels(dm.any.promo$Fact)[match("Any.Promo.Avg.Number.of.Items",levels(dm.any.promo$Fact))] <- "Avg.Number.of.Items"
levels(dm.any.promo$Fact)[match("Any.Promo.TDP",levels(dm.any.promo$Fact))] <- "TDP"

View(dm.any.promo)

## DISPLAY ONLY

all.col <- c("All.Markets", 
             "All.Products", 
             "All.Periods", 
             "Disp.without.Feat.Dollars", 
             "Disp.without.Feat.Units", 
             "Disp.without.Feat.EQ", 
             "Disp.without.Feat.PercentACV", 
             "Disp.without.Feat.TDP", 
             "Disp.without.Feat.Avg.Number.of.Items")
measurevars <- c("Disp.without.Feat.Dollars", 
                 "Disp.without.Feat.Units", 
                 "Disp.without.Feat.EQ", 
                 "Disp.without.Feat.PercentACV", 
                 "Disp.without.Feat.TDP", 
                 "Disp.without.Feat.Avg.Number.of.Items")
valuename <- c("Display.Only")
dm.disp.only <- melt(data.cut[,all.col], 
                     id.vars = idvars, 
                     measure.vars = measurevars, 
                     variable.name = variablename, 
                     value.name = valuename)

levels(dm.disp.only$Fact)[match("Disp.without.Feat.Dollars",levels(dm.disp.only$Fact))] <- "Dollars"
levels(dm.disp.only$Fact)[match("Disp.without.Feat.Units",levels(dm.disp.only$Fact))] <- "Units"
levels(dm.disp.only$Fact)[match("Disp.without.Feat.EQ",levels(dm.disp.only$Fact))] <- "EQ"
levels(dm.disp.only$Fact)[match("Disp.without.Feat.PercentACV",levels(dm.disp.only$Fact))] <- "PercentACV"
levels(dm.disp.only$Fact)[match("Disp.without.Feat.Avg.Number.of.Items",levels(dm.disp.only$Fact))] <- "Avg.Number.of.Items"
levels(dm.disp.only$Fact)[match("Disp.without.Feat.TDP",levels(dm.disp.only$Fact))] <- "TDP"

View(dm.disp.only)

## FEATURE ONLY

all.col <- c("All.Markets", 
             "All.Products", 
             "All.Periods", 
             "Feat.without.Disp.Dollars", 
             "Feat.without.Disp.Units", 
             "Feat.without.Disp.EQ", 
             "Feat.without.Disp.PercentACV", 
             "Feat.without.Disp.TDP", 
             "Feat.without.Disp.Avg.Number.of.Items")
measurevars <- c("Feat.without.Disp.Dollars", 
                 "Feat.without.Disp.Units", 
                 "Feat.without.Disp.EQ", 
                 "Feat.without.Disp.PercentACV", 
                 "Feat.without.Disp.TDP", 
                 "Feat.without.Disp.Avg.Number.of.Items")
valuename <- c("Feature.Only")
dm.feat.only <- melt(data.cut[,all.col], 
                     id.vars = idvars, 
                     measure.vars = measurevars, 
                     variable.name = variablename, 
                     value.name = valuename)

levels(dm.feat.only$Fact)[match("Feat.without.Disp.Dollars",levels(dm.feat.only$Fact))] <- "Dollars"
levels(dm.feat.only$Fact)[match("Feat.without.Disp.Units",levels(dm.feat.only$Fact))] <- "Units"
levels(dm.feat.only$Fact)[match("Feat.without.Disp.EQ",levels(dm.feat.only$Fact))] <- "EQ"
levels(dm.feat.only$Fact)[match("Feat.without.Disp.PercentACV",levels(dm.feat.only$Fact))] <- "PercentACV"
levels(dm.feat.only$Fact)[match("Feat.without.Disp.Avg.Number.of.Items",levels(dm.feat.only$Fact))] <- "Avg.Number.of.Items"
levels(dm.feat.only$Fact)[match("Feat.without.Disp.TDP",levels(dm.feat.only$Fact))] <- "TDP"

View(dm.feat.only)

## FEATURE AND DISPLAY

all.col <- c("All.Markets", 
             "All.Products", 
             "All.Periods", 
             "Feat.and.Disp.Dollars", 
             "Feat.and.Disp.Units", 
             "Feat.and.Disp.EQ", 
             "Feat.and.Disp.PercentACV", 
             "Feat.and.Disp.TDP", 
             "Feat.and.Disp.Avg.Number.of.Items")
measurevars <- c("Feat.and.Disp.Dollars", 
                 "Feat.and.Disp.Units", 
                 "Feat.and.Disp.EQ", 
                 "Feat.and.Disp.PercentACV", 
                 "Feat.and.Disp.TDP", 
                 "Feat.and.Disp.Avg.Number.of.Items")
valuename <- c("Feature.and.Display")
dm.feat.and.disp <- melt(data.cut[,all.col], 
                         id.vars = idvars, 
                         measure.vars = measurevars, 
                         variable.name = variablename, 
                         value.name = valuename)

levels(dm.feat.and.disp$Fact)[match("Feat.and.Disp.Dollars",levels(dm.feat.and.disp$Fact))] <- "Dollars"
levels(dm.feat.and.disp$Fact)[match("Feat.and.Disp.Units",levels(dm.feat.and.disp$Fact))] <- "Units"
levels(dm.feat.and.disp$Fact)[match("Feat.and.Disp.EQ",levels(dm.feat.and.disp$Fact))] <- "EQ"
levels(dm.feat.and.disp$Fact)[match("Feat.and.Disp.PercentACV",levels(dm.feat.and.disp$Fact))] <- "PercentACV"
levels(dm.feat.and.disp$Fact)[match("Feat.and.Disp.Avg.Number.of.Items",levels(dm.feat.and.disp$Fact))] <- "Avg.Number.of.Items"
levels(dm.feat.and.disp$Fact)[match("Feat.and.Disp.TDP",levels(dm.feat.and.disp$Fact))] <- "TDP"

View(dm.feat.and.disp)

## PRICE DECREASE

all.col <- c("All.Markets", 
             "All.Products", 
             "All.Periods", 
             "Price.Decr.Dollars", 
             "Price.Decr.Units", 
             "Price.Decr.EQ", 
             "Price.Decr.PercentACV", 
             "Price.Decr.TDP", 
             "Price.Decr.Avg.Number.of.Items")
measurevars <- c("Price.Decr.Dollars", 
                 "Price.Decr.Units", 
                 "Price.Decr.EQ", 
                 "Price.Decr.PercentACV", 
                 "Price.Decr.TDP", 
                 "Price.Decr.Avg.Number.of.Items")
valuename <- c("Price.Decrease")
dm.price.decr <- melt(data.cut[,all.col], 
                      id.vars = idvars, 
                      measure.vars = measurevars, 
                      variable.name = variablename, 
                      value.name = valuename)

levels(dm.price.decr$Fact)[match("Price.Decr.Dollars",levels(dm.price.decr$Fact))] <- "Dollars"
levels(dm.price.decr$Fact)[match("Price.Decr.Units",levels(dm.price.decr$Fact))] <- "Units"
levels(dm.price.decr$Fact)[match("Price.Decr.EQ",levels(dm.price.decr$Fact))] <- "EQ"
levels(dm.price.decr$Fact)[match("Price.Decr.PercentACV",levels(dm.price.decr$Fact))] <- "PercentACV"
levels(dm.price.decr$Fact)[match("Price.Decr.Avg.Number.of.Items",levels(dm.price.decr$Fact))] <- "Avg.Number.of.Items"
levels(dm.price.decr$Fact)[match("Price.Decr.TDP",levels(dm.price.decr$Fact))] <- "TDP"

View(dm.price.decr)




combined1 <- merge(dm.total, 
                   dm.any.promo, 
                   by = c("All.Markets", 
                          "All.Products", 
                          "All.Periods", 
                          "Fact"), 
                   all.x = TRUE, 
                   all.y = TRUE)
combined2 <- merge(combined1, 
                   dm.disp.only, 
                   by = c("All.Markets", 
                          "All.Products", 
                          "All.Periods", 
                          "Fact"), 
                   all.x = TRUE, 
                   all.y = TRUE)
combined3 <- merge(combined2, 
                   dm.feat.only, 
                   by = c("All.Markets", 
                          "All.Products", 
                          "All.Periods", 
                          "Fact"), 
                   all.x = TRUE, 
                   all.y = TRUE)
combined4 <- merge(combined3, 
                   dm.feat.and.disp, 
                   by = c("All.Markets", 
                          "All.Products", 
                          "All.Periods", 
                          "Fact"), 
                   all.x = TRUE, 
                   all.y = TRUE)
combined5 <- merge(combined4, 
                   dm.price.decr, 
                   by = c("All.Markets", 
                          "All.Products", 
                          "All.Periods", 
                          "Fact"), 
                   all.x = TRUE, 
                   all.y = TRUE)
dm.all <- combined5


getwd()
dir()
setwd("C:/Users/mbeatty/Desktop/TEMP - HD/GIANT EAGLE")
getwd()

write.csv(dm.all, "Giant Eagle Total Promotions Fact Reframe.csv", row.names=FALSE, na="")




# finalnames <- c("Total.", 
#                 "Any.Promo.", 
#                 "Display.Only.", 
#                 "Feature.Only.", 
#                 "Feature.and.Display.", 
#                 "Price.Decrease.")

## dm.total <- melt(data.cut[,c()])



 # http://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
 # 
 # http://stackoverflow.com/questions/1544907/melt-to-two-variable-columns
 # 
 # http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
 # 
 # https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-reshape.html
 # 
 # http://stackoverflow.com/questions/11810605/replace-contents-of-factor-column-in-r-dataframe
 # 
 # http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
 # 












