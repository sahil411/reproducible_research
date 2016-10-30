library("ggplot2")
library("grid")
library("utils")

data_filename <- "repdata_data_StormData.csv.bz2"

if (!file.exists(data_filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL, data_filename)
}  

if (!exists('stormData')) {
  stormData <- read.csv("repdata_data_StormData.csv.bz2", stringsAsFactors = FALSE)
}

summary(stormData)

# Trim the data set to required columns only
stormEvent <- stormData[, c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", 
                            "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

# Create subset for Question 1 and Question 2

# Select data for Fatalities and injuries for Question 1
eventHealth <- subset(stormEvent, !stormEvent$FATALITIES == 0 & !stormEvent$INJURIES == 
                        0, select = c(EVTYPE, FATALITIES, INJURIES))

# Select data for Property Damage and Crop Damage for Question 2
eventEconomic <- subset(stormEvent, !stormEvent$PROPDMG == 0 & !stormEvent$CROPDMG == 
                          0, select = c(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))

# Create separate data set for Injury and Fatalities Fatalities
eventHealth_Death <- aggregate(eventHealth$FATALITIES, by = list(eventHealth$EVTYPE), 
                               FUN = sum)
# Give proper name for columns
colnames(eventHealth_Death) <- c("EVENTTYPE", "FATALITIES")

# Injury
eventHealth_Inj <- aggregate(eventHealth$INJURIES, by = list(eventHealth$EVTYPE), 
                             FUN = sum)
# Give column name
colnames(eventHealth_Inj) <- c("EVENTTYPE", "INJURIES")

# Let's reorder 2 dataset and filter top 5 events for both dataset
eventHealth_Death <- eventHealth_Death[order(eventHealth_Death$FATALITIES, decreasing = TRUE), 
                                       ][1:5, ]

eventHealth_Inj <- eventHealth_Inj[order(eventHealth_Inj$INJURIES, decreasing = TRUE), 
                                   ][1:5, ]


# plot top 5 events for fatalities and injuries

# Plot Fatalities and store at Death_plot
Death_plot <- ggplot() + geom_bar(data = eventHealth_Death, aes(x = EVENTTYPE, 
                                                                y = FATALITIES, fill = interaction(FATALITIES, EVENTTYPE)), stat = "identity", 
                                  show.legend = F) + theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  xlab("Harmful Events") + ylab("No. of fatailities") + ggtitle("Top 5 weather events causing fatalities") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Plot injuries and store at variable Inj_plot
Inj_plot <- ggplot() + geom_bar(data = eventHealth_Inj, aes(x = EVENTTYPE, y = INJURIES, 
                                                            fill = interaction(INJURIES, EVENTTYPE)), stat = "identity", show.legend = F) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + xlab("Harmful Events") + 
  ylab("No. of Injuries") + ggtitle("Top 5 weather events causing Injuries") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


Death_plot
Inj_plot
