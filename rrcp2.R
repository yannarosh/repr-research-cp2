setwd("C:/Users/perre/Desktop/RR cp2/repr-research-cp2")

## Download and read data

if(!file.exists("./data")) {
        dir.create("./data")
        url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        file_dest <- "./data/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(url, file_dest)
}

A <- read.csv("./data/repdata%2Fdata%2FStormData.csv.bz2")


## Required packages
require(plyr)

## Data transformation

        # Subset A to include only variables of interest

Asub <- subset(A, select = c("EVTYPE", "FATALITIES", "INJURIES",
                             "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"))


        # Examine PROPDMGEXP and CROPDMGEXP

summary(Asub$PROPDMGEXP)
summary(Asub$CROPDMGEXP)

        # Rename all values to uppercase. Function "toupper" 
        # also transforms the factor variables to character class

Asub$PROPDMGEXP <- toupper(Asub$PROPDMGEXP)
Asub$CROPDMGEXP <- toupper(Asub$CROPDMGEXP)

        # CROPDMGEXP and PROPDMGEXP values are now converted to numeric so that 
        # they can be multiplied with CROPDMG and PROPDMG respectively and 
        # yield the actual value of damage.

Asub$CROPDMGEXP <- revalue(Asub$CROPDMGEXP, c("?" = 1,
                                        "0" = 1,
                                        "2" = 10^2,
                                        "K" = 10^3,
                                        "M" = 10^6,
                                        "B" = 10^9
))

Asub$CROPDMGEXP <- as.numeric(as.character(Asub$CROPDMGEXP))
Asub$CROPDMGEXP[is.na(Asub$CROPDMGEXP)] <- 1

Asub$PROPDMGEXP <- revalue(Asub$PROPDMGEXP, c("?" = 1,
                                              "1" = 1,
                                              "+" = 1,
                                              "-" = 1,
                                              "0" = 1,
                                              "1" = 1,
                                              "2" = 10^2,
                                              "H" = 10^2,
                                              "3" = 10^3,
                                              "K" = 10^3,
                                              "4" = 10^4,
                                              "5" = 10^5,
                                              "6" = 10^6,
                                              "M" = 10^6,
                                              "7" = 10^7,
                                              "8" = 10^8,
                                              "B" = 10^9      
))

Asub$PROPDMGEXP <- as.numeric(as.character(Asub$PROPDMGEXP))
Asub$PROPDMGEXP[is.na(Asub$PROPDMGEXP)] <- 1

        # Now let's look at the different values of PROPDMGEXP and CROPDMGEXP
        # again

levels(as.factor(Asub$CROPDMGEXP))
levels(as.factor(Asub$PROPDMGEXP))

        # Now we calculate the value of actual damage and put it into a 
        # new variable

Asub$cropdamage <- Asub$CROPDMG * Asub$CROPDMGEXP
Asub$propdamage <- Asub$PROPDMG * Asub$PROPDMGEXP

        # Create aggregate matrices with the total crop/prop/total economic
        # damage by type of event. Then create another matrix that combines all
        # 3 and give better column names.

Crop_dmg_agr <- aggregate(cropdamage ~ EVTYPE, data = Asub, FUN = sum)
Prop_dmg_agr <- aggregate(propdamage ~ EVTYPE, data = Asub, FUN = sum)
Econ_dmg_agr <- aggregate(cropdamage + propdamage ~ EVTYPE, data = Asub, FUN = sum)

Econ_dmg_full <- cbind(Econ_dmg_agr, Crop_dmg_agr[2], Prop_dmg_agr[2])
colnames(Econ_dmg_full) <- c("Event", "Total economic damage", "Crop damage", 
                             "Property damage")
        
        # Order the matrices by dmg (desc.)

Crop_dmg_agr <- arrange(Crop_dmg_agr, desc(cropdamage))
Prop_dmg_agr <- arrange(Prop_dmg_agr, desc(propdamage))
Econ_dmg_agr <- arrange(Econ_dmg_agr, desc(Econ_dmg_agr[2]))
Econ_dmg_full <- arrange(Econ_dmg_full, desc(Econ_dmg_full[2]))
        
        # Do exactly the same for injuries, fatalities and total health damage

Injuries_agr <- aggregate(INJURIES ~ EVTYPE, data = Asub, FUN = sum)
Fatalities_agr <- aggregate(FATALITIES ~ EVTYPE, data = Asub, FUN = sum)
Health_dmg_agr <- aggregate(FATALITIES + INJURIES ~ EVTYPE, data = Asub, FUN = sum)

Health_dmg_full <- cbind(Health_dmg_agr, Injuries_agr[2], Fatalities_agr[2])
colnames(Health_dmg_full) <- c("Event", "Total health damage", "Injuries", 
                             "Fatalities")

Injuries_agr <- arrange(Injuries_agr, desc(INJURIES))
Fatalities_agr <- arrange(Fatalities_agr, desc(FATALITIES))
Health_dmg_agr <- arrange(Health_dmg_agr, desc(Health_dmg_agr[2]))
Health_dmg_full <- arrange(Health_dmg_full, desc(Health_dmg_full[2]))

#-----------------------------------------------------------------------------

## Results
