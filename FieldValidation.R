library(ggplot2)
library(dplyr)
library(reshape2)
library(lme4)
source("geodist.R")

#Field Validation
PopData<-read.csv("CompleteData.csv", header=T, stringsAsFactors = FALSE)
FieldData<-read.csv("FieldPhenology.csv", header=T, stringsAsFactors = FALSE)
FieldData<-FieldData[-c(13,20),]

PopData<-subset(PopData, Year >=1960)

#Binning into populations around the field populations

binx<-1 ## long bin size in degrees
biny<-1## lat bin size in degrees

PopList<-{}
ClosePop<-{}
SMin<-3
SMax <-10
for (i in 1:nrow(FieldData)){
  ##find all samples within range of binx and bin y
  ClosePop<- PopData[PopData$Longitude >= FieldData$Longitude[i]-binx & PopData$Longitude <= FieldData$Longitude[i]+binx & PopData$Latitude >= FieldData$Latitude[i]-biny & PopData$Latitude <= FieldData$Latitude[i]+binx,]
  if (nrow(ClosePop >= SMin)) {
    for(j in 1:nrow(ClosePop)){ # Cycle through each station
      ClosePop$Dist[j]<-geodist(c(ClosePop$Latitude[j],ClosePop$Longitude[j],PopData$Latitude[i],PopData$Longitude[i]))   
    }
  }
    ##only take the Smax closest stations
    if (nrow(ClosePop)>= SMax) {
    ClosePop<-ClosePop[order(ClosePop$Dist),][1:SMax,]
    }
  ClosePop<-cbind(ClosePop[,c(1,10,11,14,34)], FieldData$Population[i], FieldData$weighted.mean.date.of.peak.flowering[i], FieldData$weighted.average...days.after.growing.season.to.flowering[i], FieldData$Latitude[i], FieldData$Longitude[i])
  PopList<-rbind(PopList, ClosePop)
}

##rename data set
names(PopList)<-c("Pop_Code", "Latitude", "Longitude", "Pop_GD", "Pop_phind", "FieldPop", "FieldTotalFlower", "FlowerGFlower", "FieldLatitude", "FieldLongitude")


##aggregate phind by field populations to "estimate" phind for the population
agField<-aggregate(PopList, by=list(PopList$FieldPop), FUN=mean)

ggplot(data=agField, aes(x = Pop_phind, y = FieldTotalFlower)) + geom_point()+geom_smooth()

ggplot(data=agField, aes(x = Pop_phind, y = FlowerGFlower)) + geom_point()+geom_smooth()


fit1<-lm(agField$Pop_phind~agField$FieldTotalFlower)
summary(fit1)
cor(agField$Pop_phind,agField$FieldTotalFlower)

fit2<-lm(agField$Pop_phind~agField$FlowerGFlower)
summary(fit2)

cor(agField$Pop_phind,agField$FlowerGFlower)
##plotting every sample to field flowering


ggplot(data=PopList, aes(x = Pop_phind, y = FieldTotalFlower)) + geom_point()+geom_smooth()



