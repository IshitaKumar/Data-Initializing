NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


totalYearPollute=tapply(NEI$Emissions,NEI$year,sum)
years=unique(NEI$year)
plot(years,totalYearPollute,type = "b",xlab = "Year",col="blue",ylab="Yearly PM2.5",pch=20,main="Annual PM2.5 (1999-2008)")

baltimaoreData=NEI[NEI$fips=="24510",]
yrBaltiPo=tapply(baltimaoreData$Emissions,baltimaoreData$year,sum)
yrBalt=distinct(baltimaoreData$year)
plot(yrBalt,yrBaltiPo,type = "b",xlab = "Year",col="tomato2",ylab="Yearly PM2.5",pch=20,main="Baltimore Yearly PM2.5")

library(dplyr)
pollute= group_by(baltimaoreData,type,year)
pointData=summarize(pollute,emissionSum=sum(Emissions))
pointData$type=factor(pointData$type)
q<-qplot(year,emissionSum,data=pointData,facets = .~type,color=type,geom=c("point","line"),ylab = "Total Emissions",xlab = "Year",main="Baltimore PM2.5 Emissions")

coalSources=SCC[grep("(.*)[Cc]oal(.*)",SCC$EI.Sector),1]
coalEmissions=NEI[NEI$SCC %in% coalSources,]
yearlyCoalEmissions=tapply(coalEmissions$Emissions,coalEmissions$year,sum)
qplot(unique(coalEmissions$year),yearlyCoalEmissions,geom=c("point","line"),color="darkmagenta",margins = T,xlab = "Year",ylab="Emission From Coal Sources",
      main="Yearly PM2.5 Emissions From Coal Sources")
+theme(plot.title=element_text(size=15, vjust=1.5,face="bold",color="black"))
+theme(axis.title=element_text(size=12, vjust=1,face="bold",color="grey51"))
+theme(legend.position="none")

motorSources=SCC[grep("^(Mobile - On-Road)(.*)",SCC$EI.Sector),1]
motorBatimoreEmissions=NEI[NEI$SCC %in% motorSources & NEI$fips == "24510",]
yrBaltMotoEmissions=tapply(motorBatimoreEmissions$Emissions,motorBatimoreEmissions$year,sum)
q<-qplot(unique(motorBatimoreEmissions$year),yrBaltMotoEmissions,geom=c("point","line"),color="darkmagenta",margins = T,xlab = "Year",ylab="Emission From Motor Sources",main="Baltimore Yearly PM2.5 Emissions (Motor Vehicles)")
q=q+theme(plot.title=element_text(size=15, vjust=1.5,face="bold",color="black"))
q=q+theme(axis.title=element_text(size=12, vjust=1,face="bold",color="grey51"))
q+theme(legend.position="none")

motorLosAngelesEmissions=NEI[NEI$SCC %in% motorSources & NEI$fips == "06037",]
meanBalt=mean(motorBatimoreEmissions$Emissions)
sdBalt=sd(motorBatimoreEmissions$Emissions)
motorBatimoreEmissions$normalEm=(motorBatimoreEmissions$Emissions-meanBalt)/sdBalt
meanLA=mean(motorLosAngelesEmissions$Emissions)
sdLA=sd(motorLosAngelesEmissions$Emissions)
motorLosAngelesEmissions$normalEm=(motorLosAngelesEmissions$Emissions-meanLA)/sdLA
plot(years,yrMotoBaltEmission,type="n",ylim = c(min(yrMotoBaltEmission,yrLAMotoEmissions),max(yrMotoBaltEmission,yrLAMotoEmissions)),main = "PM2.5 Motor Emissions : Baltimore vs Los Angeles",ylab = "Normalized Emissions",xlab = "Years")
points(years,yrLAMotoEmissions,col="green",pch=20)
points(years,yrMotoBaltEmission,col="brown",pch=20)
baltModel=lm(yrMotoBaltEmission~years)
abline(baltModel,lwd=2,col="brown")
yaModel=lm(yrLAMotoEmissions~years)
abline(yaModel,lwd=2,col="green")
