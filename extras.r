stormData=read.csv("repdata-data-StormData.csv")
totalDead=tapply(stormData$FATALITIES,stormData$EVTYPE,sum)
totalInjured=tapply(stormData$INJURIES,stormData$EVTYPE,sum)
quantile(totalDead,probs = seq(0.0,1.0,by=0.1))
quantile(totalInjured,probs = seq(0.0,1.0,by=0.1))
quantile(totalInjured,probs = seq(0.95,1.0,by=0.01))
quantile(totalDead,probs = seq(0.95,1.0,by=0.01))
par(mfrow=c(2,1))
 barplot(totalDead[totalDead>208],names.arg = substr(names(totalDead[totalDead>208]),0,10),ylim = c(208,5633),col="darkblue",main="FATAL CASES BY ENVIRONMENTAL ISSUES",xlab = "Environmental Disasters",ylab = "Total Fatal Cases",cex.main=0.6,cex.lab=0.3,font.main=2,font.lab=2,col.main="darkblue",col.lab="grey",cex.names = 0.5)