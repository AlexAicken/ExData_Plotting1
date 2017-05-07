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

#Change data shape for looking at metering over time
electricDataSubsetMetering1 <- data.frame(DateTime=electricDataSubset$DateTime, MeteringName="Sub_metering_1", Metering=electricDataSubset$Sub_metering_1)
electricDataSubsetMetering2 <- data.frame(DateTime=electricDataSubset$DateTime, MeteringName="Sub_metering_2", Metering=electricDataSubset$Sub_metering_2)
electricDataSubsetMetering3 <- data.frame(DateTime=electricDataSubset$DateTime, MeteringName="Sub_metering_3", Metering=electricDataSubset$Sub_metering_3)
electricDataSubsetMetering <- rbind(electricDataSubsetMetering1,electricDataSubsetMetering2,electricDataSubsetMetering3)

#Produce plot3
ggplot(electricDataSubsetMetering,aes(x=DateTime, y=Metering ,color=MeteringName)) +
scale_color_manual(values=c("black", "red", "blue")) +
geom_line() +
scale_y_continuous(name = "Energy sub metering") +
scale_x_datetime(name = "", date_labels="%a",date_breaks  ="1 days") +
theme_classic() +
theme(legend.title=element_blank()) +
theme(legend.position = c(0.81, 0.87), legend.background = element_rect(color = "black", size = 0.1, linetype = "solid")) +
theme(axis.line.x = element_line(color="black", size = 0.1), axis.line.y = element_line(color="black", size = 0.1)) +
theme(plot.title = element_text(face="bold")) +
theme(axis.title.y=element_text(margin=margin(0,20,0,0))) +
theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))

#Save output PNG image
ggsave("plot3.png", width=4.8, height=4.8, dpi = 100)