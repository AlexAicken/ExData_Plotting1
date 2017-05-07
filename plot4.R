#This code produces explorartory graphs off the Individual household electric power consumption Data Set
#for dates 1 Feb 2007 - 2 Feb 2007

#Load libraries required
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(grid)
library(gridExtra)

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

#Produce plot4
plot4_1 <- ggplot(electricDataSubset,aes(y=electricDataSubset$Global_active_power, x= electricDataSubset$DateTime)) +
geom_line(color="black") +
scale_y_continuous(name = "Global Active Power (kilowatts)") +
scale_x_datetime(name = "", date_labels="%a",date_breaks  ="1 days") +
theme_classic() +
theme(axis.line.x = element_line(color="black", size = 0.1), axis.line.y = element_line(color="black", size = 0.1)) +
theme(plot.title = element_text(size = 9, face="bold")) +
theme(axis.title.y=element_text(size = 9, margin=margin(0,10,0,0))) +
theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))

plot4_2 <- ggplot(electricDataSubset,aes(y=electricDataSubset$Voltage, x= electricDataSubset$DateTime)) +
geom_line(color="black") +
scale_y_continuous(name = "Voltage", breaks=seq(234,246,4)) +
scale_x_datetime(name = "datetime", date_labels="%a",date_breaks  ="1 days") +
theme_classic() +
theme(axis.line.x = element_line(color="black", size = 0.1), axis.line.y = element_line(color="black", size = 0.1)) +
theme(axis.title.x=element_text(size = 9)) +
theme(plot.title = element_text(size = 9, face="bold")) +
theme(axis.title.y=element_text(size = 9, margin=margin(0,10,0,0))) +
theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))

plot4_3 <- ggplot(electricDataSubsetMetering,aes(x=DateTime, y=Metering ,color=MeteringName)) +
scale_color_manual(values=c("black", "red", "blue")) +
geom_line() +
scale_y_continuous(name = "Energy sub metering") +
scale_x_datetime(name = "", date_labels="%a",date_breaks  ="1 days") +
theme_classic() +
theme(legend.title=element_blank()) +
theme(legend.position = c(0.718, 0.74)) +
theme(legend.text=element_text(size = 6)) +
theme(axis.line.x = element_line(color="black", size = 0.1), axis.line.y = element_line(color="black", size = 0.1)) +
theme(plot.title = element_text(face="bold")) +
theme(axis.title.y=element_text(size = 9, margin=margin(0,10,0,0))) +
theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))

plot4_4 <- ggplot(electricDataSubset,aes(y=electricDataSubset$Global_reactive_power, x= electricDataSubset$DateTime)) +
geom_line(color="black") +
scale_y_continuous(name = "Global_reactive_power") +
scale_x_datetime(name = "datetime", date_labels="%a",date_breaks  ="1 days") +
theme_classic() +
theme(axis.title.x=element_text(size = 9)) +
theme(axis.line.x = element_line(color="black", size = 0.1), axis.line.y = element_line(color="black", size = 0.1)) +
theme(plot.title = element_text(size = 9, face="bold")) +
theme(axis.title.y=element_text(size = 9, margin=margin(0,10,0,0))) +
theme(panel.border = element_rect(colour = "black", fill=NA, size=0.1))

#Multiplot 4 graphs
grid.arrange(plot4_1, plot4_2, plot4_3, plot4_4, ncol = 2)
g <- arrangeGrob(plot4_1, plot4_2, plot4_3, plot4_4, ncol = 2)

#Save output PNG image
ggsave("plot4.png", g, width=4.8, height=4.8, dpi = 100)