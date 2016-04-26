##Above all we set language preferences to English  and save current settings
##to avoid malfunction due to inapropriate settings
## In order to save my russian environment
curr_locale <- Sys.getlocale("LC_TIME")

## To set it in english
Sys.setlocale("LC_TIME","en_US.UTF-8")

##Loading neccesary libraries
library(ggplot2)
library(dplyr)
library(knitr)

##Setting working directory
setwd("./RR/Week2_CP")

##Downloading the file from the Internet source
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" 

if (!file.exists("stormData.csv.bz2")) {
        download.file(url, destfile = "stormData.csv.bz2", method ="curl")
}

##Reading data to the StormData variable. We don't need to unzip initial file, because
##read.csv function can handle compressed files and do this automatically.
StormData <- read.csv('StormData.csv.bz2', header = T)

##Make a quick view on the data
str(StormData)

##According to the page 12 of the Coockbook
##Estimates should be rounded to three significant digits, 
##followed by an alphabetical character signifying the magnitude of the number,
##i.e., 1.55B for $1,550,000,000. Alphabetical characters used to signify 
##magnitude include “K” for thousands, “M” for millions, and “B” for billions.
##So, we decided than others symbols represents the multiplier.


##Select only fileds that we need to perform analysis to speed-up calculations
StormData2 <-StormData[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
str(StormData2)
head(StormData2)

##Lets view the multipliers
unique(StormData2$PROPDMGEXP)
unique(StormData2$CROPDMGEXP)

unique(StormData2$EVTYPE)

##Conver multipliers to sumarize and calculate effect:
##
StormData2$PROPDMGEXP <- as.character(StormData2$PROPDMGEXP)
StormData2$PROPDMGEXP[(toupper(StormData2$PROPDMGEXP)=="K")] <- 3
StormData2$PROPDMGEXP[(toupper(StormData2$PROPDMGEXP)=="M")] <- 6
StormData2$PROPDMGEXP[(toupper(StormData2$PROPDMGEXP)=="B")] <- 9
StormData2$PROPDMGEXP[(toupper(StormData2$PROPDMGEXP)=="H")] <- 2
StormData2$PROPDMGEXP[(StormData2$PROPDMGEXP=="")|
                              (StormData2$PROPDMGEXP=="?")|
                              (StormData2$PROPDMGEXP=="-")|
                              (StormData2$PROPDMGEXP=="0")|
                              (StormData2$PROPDMGEXP=="+")] <- 0
StormData2$PROPDMGEXP <- as.numeric(StormData2$PROPDMGEXP)



StormData2$CROPDMGEXP <- as.character(StormData2$CROPDMGEXP)
StormData2$CROPDMGEXP[(toupper(StormData2$CROPDMGEXP)=="K")] <- 3
StormData2$CROPDMGEXP[(toupper(StormData2$CROPDMGEXP)=="M")] <- 6
StormData2$CROPDMGEXP[(toupper(StormData2$CROPDMGEXP)=="B")] <- 9
StormData2$CROPDMGEXP[(toupper(StormData2$CROPDMGEXP)=="H")] <- 2
StormData2$CROPDMGEXP[(StormData2$CROPDMGEXP=="")|
                              (StormData2$CROPDMGEXP=="?")] <- 0
StormData2$CROPDMGEXP <- as.numeric(StormData2$CROPDMGEXP)

## converting values to multipliers
StormData2$PROPDMG <- StormData2$PROPDMG*(10^StormData2$PROPDMGEXP)
StormData2$CROPDMG <- StormData2$CROPDMG*(10^StormData2$CROPDMGEXP)



##SOme event type are called in different way, so to be more accurate 
##we conver them to one basis
##to better work of grep convert all of event types to uppercase
##According to the page 6
StormData2$EVTYPE<- toupper(StormData2$EVTYPE)


StormData2[StormData2$EVTYPE == "TSTM WIND", ]$EVTYPE = "THUNDERSTORM WIND"
StormData2[StormData2$EVTYPE == "THUNDERSTORM WINDS", ]$EVTYPE = "THUNDERSTORM WIND"
StormData2[StormData2$EVTYPE == "RIVER FLOOD", ]$EVTYPE = "FLOOD"
StormData2[StormData2$EVTYPE == "HURRICANE/TYPHOON", ]$EVTYPE = "HURRICANE"


##COunting damages for people and economic
##to avoid messy data on axis lets limit damage factors to count 10
PeoplesDamages <- 
        StormData2%>%
        group_by(EVTYPE) %>% 
        summarize(DAMAGE = sum(INJURIES) + sum(FATALITIES))

EconomicDamages <-
        StormData2 %>%
        group_by(EVTYPE) %>% 
        summarize(DAMAGE = sum(PROPDMG) + sum(CROPDMG))

PDtop10 <- arrange(PeoplesDamages, desc(DAMAGE))[1:10,]

EDtop10 <- arrange(EconomicDamages, desc(DAMAGE))[1:10,]

CropDamages <-
        StormData2 %>%
        group_by(EVTYPE) %>% 
        summarize(DAMAGE = sum(CROPDMG))


PropDamages <-
        StormData2 %>%
        group_by(EVTYPE) %>% 
        summarize(DAMAGE = sum(PROPDMG))

PrDtop10 <- arrange(PropDamages, desc(DAMAGE))[1:10,]
CDtop10 <- arrange(CropDamages, desc(DAMAGE))[1:10,]

##Plotting the results

##Setting the Graph parameters
par(oma=c(5,1,1,2))
par(mfrow = c(1, 1), mar = c(7, 4, 3, 2), mgp = c(3, 1, 0), las=2, cex = 0.6)

##Peoples Damage (Fatalities and Injures)
barplot(PDtop10$DAMAGE, names.arg = PDtop10$EVTYPE, las = 3,
        main = "Fatalities and Injuries",
        ylab = "People damage",col = "magenta4")
title(main="Top 10 Dangerous Weather Event Types for People, 1950-2011",outer=T)

##Economic impact
par(mfrow = c(1, 2), mar = c(10, 4, 4, 5), mgp = c(3, 1, 0), las=2, cex = 0.6)

barplot(PrDtop10$DAMAGE/(1e9), names.arg = PrDtop10$EVTYPE, las = 3,
        main = "Property Damages",
        ylab = "Damage Cost ($ billions)",col = "blue")
barplot(CDtop10$DAMAGE/(1e9), names.arg = CDtop10$EVTYPE, las = 3,
        main = "Crop Damages",
        ylab = "Damage Cost ($ billions)",col = "wheat")
title(main="TOP 10 Dangerous Weather Event Types for Economic, 1950-2011",outer=T)



