expAnaPlot2 <- function() {
  eConsumption=read.table("household_power_consumption.txt",header = TRUE,sep=";",colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),na.strings = c("?")) 
  eConsumption$Date=as.Date(eConsumption$Date,"%d/%m/%Y")
  eTwoDay=eConsumption[eConsumption$Date==as.Date("2007-02-01") | eConsumption$Date==as.Date("2007-02-02"),]
  eTwoDay$DateTime=paste(format(eTwoDay$Date,"%Y-%m-%d "),eTwoDay$Time)
  eTwoDay$DateTime=as.POSIXlt(eTwoDay$DateTime,"%Y-%m-%d %H:%M:%S")
  eTwoDaySort=eTwoDay[order(eTwoDay$DateTime),]
  plot(eTwoDaySort$DateTime,eTwoDaySort$Sub_metering_1,type="n")
  points(eTwoDaySort$DateTime,eTwoDaySort$Sub_metering_1,type="l")
  points(eTwoDaySort$DateTime,eTwoDaySort$Sub_metering_2,type="l",col="red")
  points(eTwoDaySort$DateTime,eTwoDaySort$Sub_metering_3,type="l",col="blue")
  legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty = c(1,1,1), col=c("black","red","blue"),lwd=c(2.5,2.5,2.5),cex=0.5)
}