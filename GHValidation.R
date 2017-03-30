source("geodist.r")
library(dplyr)
library(ggplot2)
library(lme4)
##Data import and setup
PhenolAllData<-read.csv("PhenolAllData.csv", header=T, stringsAsFactors=FALSE)
PhenolAllData<-subset(PhenolAllData, Year >=1960)

GHData<-read.csv("MontagueData_Cleaned.csv", header=T, stringsAsFactors = F)
GHData<-GHData[-1,] # Remove dummy variable from 1st row
GHData$Longitude<-GHData$Longitude*-1
##Create list of populations and the locations
##pull out unique populations and their coordinates
ghpop <- data.frame(unique(GHData$Population), as.numeric(unique(GHData$Latitude)), as.numeric(unique(GHData$Longitude)))
names(ghpop)<- c("Population", "Latitude", "Longitude")
###ghpop<-ghpop[-c(15,23),] # Remove populations without enough herbarium records

write.csv(ghpop, "MontaguePop.csv", row.names = FALSE)

##set bounds to get subset for validation in greenhouse population range
##north and west bounds
ValidHerb<- PhenolAllData[(PhenolAllData$Latitude < 49) & (PhenolAllData$Longitude > -85), ]
ValidHerb<-ValidHerb[(ValidHerb$Latitude > 38) & (ValidHerb$Longitude < -74), ]

ggplot(ValidHerb, aes(x=GD, y=fti))+
  geom_point(alpha=0.2)+geom_smooth(method="lm")
summary(lm(fti~Latitude*Region, data=ValidHerb))




###scrap
ghpoplist<-{}
ClosePop<-{}
SMin <-5 # Limit to at least 5 herbarium records
SMax <-100 # Optional limit on # herbarium records
for (i in 1:nrow(ghpop)){
  ##find all samples within range of binx and bin y
  ClosePop<- PopData[PopData$Longitude >= ghpop$Longitude[i]-binx & PopData$Longitude <= ghpop$Longitude[i]+binx & PopData$Latitude >= ghpop$Latitude[i]-biny & PopData$Latitude <= ghpop$Latitude[i]+binx,]
  ##only include populations with Smin number of herbarium records
  if (nrow(ClosePop) >=SMin) {
    for(j in 1:nrow(ClosePop)){ # Cycle through each station
      ClosePop$Dist[j]<-geodist(c(ClosePop$Latitude[j],ClosePop$Longitude[j],PopData$Latitude[i],PopData$Longitude[i]))   
    }
   
  ##only bind to data frame if there are more than five samples near the population
  if (nrow(ClosePop)>= SMax) {
    ClosePop<-ClosePop[order(ClosePop$Dist),][1:SMax,]
  }
    ClosePop<-cbind(ClosePop[,c(1,10,11,14,34)], ghpop$Population[i], ghpop$Latitude[i], ghpop$Longitude[i])
    ghpoplist<-rbind(ghpoplist, ClosePop)
  }  
}

names(ghpoplist)<-c("Pop_Code", "Latitude", "Longitude", "GD", "phind", "GHPop", "GHLat", "GHLong")


names(GHData)
##greenhouse data
agGH<- aggregate(GHData, by=list(GHData$Population), FUN=mean, na.rm=TRUE)
names(agGH)[1]<-"Population"
##only take population, lat, long, and Days
agGH<-agGH[,c(1,7,8,10)]

##aggregate herbarium samples for populations
agPop<-aggregate(ghpoplist, by=list(ghpoplist$GHPop), FUN=mean, na.rm=TRUE)
names(agPop)[1]<-"Population"
##take population, GD, and phind
agPop1<-agPop[,c(1,5,6)]
agPop1$Population<-as.character(agPop1$Population)
##merge data sets: completedata has averages of herbarium samples against averages of greenhouse samples
completedata<- merge(agPop1, agGH, by="Population")



ggplot(completedata, aes(x=Days, y=phind))+geom_point()
cor(completedata$Days, completedata$phind) ##weak correlation
fit1<-lm(completedata$phind~completedata$Days) ##phind of herbarium samples vs Days of flowering in greenhouse, 
summary(fit1) ##significant p=0.0358

ggplot(completedata, aes(x=Days, y=GD))+geom_point() 
fit2<-lm(completedata$GD~completedata$Days)##GD of herbarium record locations against Days of flowering
summary(fit2) ##strongly significant 


##create data set with averages of herbarium records against greenhouse individuals

lmdata<-merge(GHData, agPop1, by="Population")

##this does not work
lmmodel<-lmer(phind ~ Days +(1|Population:Family), data=lmdata)
