setwd("C:/Users/perre/Desktop/RR cp2/repr-research-cp2")

# Download and read data

if(!file.exists("./data")) {
        dir.create("./data")
        url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        file_dest <- "./data/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(url, file_dest)
}

A <- read.csv("./data/repdata%2Fdata%2FStormData.csv.bz2")


# Required packages
require(plyr)


# Subset A to include only variables of interest

Asub <- subset(A, select = c("EVTYPE", "FATALITIES", "INJURIES",
                             "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"))


# Examine PROPDMGEXP and CROPDMGEXP

summary(Asub$PROPDMGEXP)
summary(Asub$CROPDMGEXP)

        # Rename all values to uppercase
Asub$PROPDMGEXP <- factor(toupper(Asub$PROPDMGEXP))
Asub$CROPDMGEXP <- factor(toupper(Asub$CROPDMGEXP))

        # Map CROP/PROP EXP factor levels to numbers so that we can then
        # multiply CROPDMG/PROPDMG * EXP and get the actual value for damage.
Asub$PROPDMGEXP_num <- 99
Asub$CROPDMGEXP_num <- 99
