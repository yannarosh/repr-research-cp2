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
require(reshape2)
require(ggplot2)

## Data transformation

        # Subset A to include only variables of interest

Asub <- subset(A, select = c("EVTYPE", "FATALITIES", "INJURIES",
                             "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP"))


        # Examine PROPDMGEXP and CROPDMGEXP

summary(Asub$PROPDMGEXP)
summary(Asub$CROPDMGEXP)

        # Transform all values to uppercase. Function "toupper" 
        # also transforms the factor variables to character class

Asub$PROPDMGEXP <- toupper(Asub$PROPDMGEXP)
Asub$CROPDMGEXP <- toupper(Asub$CROPDMGEXP)

        # CROPDMGEXP and PROPDMGEXP values are now converted to numeric so that 
        # they can be multiplied with CROPDMG and PROPDMG respectively and 
        # yield the actual value of each type of damage.

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

        # Calculate the value of actual damage and put it into a 
        # new variable

Asub$cropdamage <- Asub$CROPDMG * Asub$CROPDMGEXP
Asub$propdamage <- Asub$PROPDMG * Asub$PROPDMGEXP

        # Create aggregate matrices with the total crop/property/total economic
        # damage by type of event. Then create another matrix that combines all
        # 3 and rename columns.

Crop_dmg_agr <- aggregate(cropdamage ~ EVTYPE, data = Asub, FUN = sum)
Prop_dmg_agr <- aggregate(propdamage ~ EVTYPE, data = Asub, FUN = sum)
Econ_dmg_agr <- aggregate(cropdamage + propdamage ~ EVTYPE, data = Asub, FUN = sum)

Econ_dmg_full <- cbind(Econ_dmg_agr, Crop_dmg_agr[2], Prop_dmg_agr[2])
colnames(Econ_dmg_full) <- c("event", "total", "crop", "property")
        
        # Order the matrices by dmg (desc.)

Crop_dmg_agr <- arrange(Crop_dmg_agr, desc(cropdamage))
Prop_dmg_agr <- arrange(Prop_dmg_agr, desc(propdamage))
Econ_dmg_agr <- arrange(Econ_dmg_agr, desc(Econ_dmg_agr[2]))
Econ_dmg_full <- arrange(Econ_dmg_full, desc(Econ_dmg_full[2]))

        # Subset the full matrix to include only the first 10 rows (events)
Econ_dmg_full <- Econ_dmg_full[1:10, ]

        # Create a "long" version of the final full combined matrix
E_melt <- melt(data = Econ_dmg_full, id.vars = "event", value.name = "damage")
        
        # Do exactly the same for injuries, fatalities and total health damage

Injuries_agr <- aggregate(INJURIES ~ EVTYPE, data = Asub, FUN = sum)
Fatalities_agr <- aggregate(FATALITIES ~ EVTYPE, data = Asub, FUN = sum)
Health_dmg_agr <- aggregate(FATALITIES + INJURIES ~ EVTYPE, data = Asub, FUN = sum)

Health_dmg_full <- cbind(Health_dmg_agr, Injuries_agr[2], Fatalities_agr[2])
colnames(Health_dmg_full) <- c("event", "total", "injuries", "fatalities")

Injuries_agr <- arrange(Injuries_agr, desc(INJURIES))
Fatalities_agr <- arrange(Fatalities_agr, desc(FATALITIES))
Health_dmg_agr <- arrange(Health_dmg_agr, desc(Health_dmg_agr[2]))
Health_dmg_full <- arrange(Health_dmg_full, desc(Health_dmg_full[2]))

Health_dmg_full <- Health_dmg_full[1:10, ]

H_melt <- melt(data = Health_dmg_full, id.vars = "event", value.name = "count")

#-----------------------------------------------------------------------------

## Results

        # 5 events that have caused the most crop damage

head(Crop_dmg_agr, 5)

        # 5 events that have caused the most property damage

head(Prop_dmg_agr, 5)

        # 5 events that have caused the most total economic damage

head(Econ_dmg_agr, 5)

        ### Floods are the event responsible for the most economic damage

        # 5 events that have caused the most fatalities

head(Fatalities_agr, 5)

        # 5 events that have caused the most injuries

head(Injuries_agr, 5)

        # 5 events that have caused the most total health damage

head(Health_dmg_agr, 5)

        ### Tornadoes have the biggest impact on health

        # Plots

g <- ggplot(E_melt, aes(event, damage, fill = variable))
g + geom_bar(position = "dodge", stat = "identity") + 
        theme(legend.title = element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle("Economic damage by type of event")
        
p <- ggplot(H_melt, aes(event, count, fill = variable))
p + geom_bar(position = "dodge", stat = "identity") + 
        theme(legend.title = element_blank(), 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle("Health damage by type of event")


