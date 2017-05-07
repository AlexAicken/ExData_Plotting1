#This code produces explorartory graphs off the Individual household electric power consumption Data Set
#for dates 1 Feb 2007 - 2 Feb 2007

#Load libraries required
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)

#Read in Data
electricData <- read.table(
  "household_power_consumption.txt",
  sep=";", header=TRUE, na.strings = "?", stringsAsFactors = FALSE)

#Ensure all data has correct data types
electricData$DateTime <- dmy_hms(paste((electricData$Date), electricData$Time))
electricData$Date <- dmy(electricData$Date)
electricData$Global_active_power <- as.numeric(electricData$Global_active_power)
electricData$Global_reactive_power <- as.numeric(electricData$Global_reactive_power)
electricData$Voltage <- as.numeric(electricData$Voltage)
electricData$Global_intensity <- as.numeric(electricData$Global_intensity)
electricData$Sub_metering_1 <- as.numeric(electricData$Sub_metering_1)
electricData$Sub_metering_2 <- as.numeric(electricData$Sub_metering_2)
electricData$Sub_metering_3 <- as.numeric(electricData$Sub_metering_3)

#Filter for date period analysed
electricDataSubset <- subset(electricData, Date == dmy("02/02/2007") | Date == dmy("01/02/2007"))

#Produce plot1
ggplot(electricDataSubset,aes(x=electricDataSubset$Global_active_power)) +
geom_histogram(bins=18,color="black", fill="red") +
scale_y_continuous(name = "Frequency", breaks=seq(200,1200,200)) +
scale_x_continuous(name = "Global Active Power (kilowatts)", limits = c(0,6)) +
ggtitle("Global Active Power") + 
theme_classic() +
theme(axis.line.x = element_line(color="black", size = 0.1), axis.line.y = element_line(color="black", size = 0.1)) +
theme(plot.title = element_text(face="bold")) +
theme(axis.title.x=element_text(margin=margin(20,0,0,0))) +
theme(axis.title.y=element_text(margin=margin(0,20,0,0)))

#Save output PNG image
ggsave("plot1.png", width=4.8, height=4.8, dpi = 100)