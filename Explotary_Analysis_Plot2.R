expAnaPlot2 <- function() {
  eConsumption=read.table("household_power_consumption.txt",header = TRUE,sep=";",colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),na.strings = c("?")) 
  eConsumption$Date=as.Date(eConsumption$Date,"%d/%m/%Y")
  eTwoDay=eConsumption[eConsumption$Date==as.Date("2007-02-01") | eConsumption$Date==as.Date("2007-02-02"),]
  eTwoDay$DateTime=paste(format(eTwoDay$Date,"%Y-%m-%d "),eTwoDay$Time)
  eTwoDay$DateTime=as.POSIXlt(eTwoDay$DateTime,"%Y-%m-%d %H:%M:%S")
  eTwoDaySort=eTwoDay[order(eTwoDay$DateTime),]
  plot(eTwoDaySort$DateTime,eTwoDaySort$Global_active_power,type = "l")
}