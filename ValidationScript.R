####Validation script
library(ggplot2)
##Data files: GreenhouseData_wGDD2003.csv from GHValidation.R
### FieldData_wGDDtest.csv from FieldValidation.R
### PhenolAllData.csv from MeasurementPCA.R



##Data import
GreenhouseData<-read.csv("GreenhouseData_wGDDYDay.csv", header=T)
FieldData<-read.csv("FieldData_wGDDtest.csv", header=T)
PhenolAllData<-read.csv("PhenolAllData.csv", header=T, stringsAsFactors=FALSE)


## create find and fti for Greenhouse and Field data

GreenhouseData$fti<-(GreenhouseData$yday-mean(GreenhouseData$yday, na.rm=TRUE))/sd(GreenhouseData$yday, na.rm=TRUE)
FieldData$find<-(FieldData$yday-mean(FieldData$yday, na.rm=TRUE))/sd(FieldData$yday, na.rm=TRUE)
FieldData$fti<- (FieldData$find) + (FieldData$GDDs-mean(FieldData$GDDs,na.rm=TRUE))/sd(FieldData$GDDs,na.rm=TRUE)

##Subset data frames
GreenhouseData<-GreenhouseData[,c(1,7:8,18,17,19:28)]
FieldData<-FieldData[,c(1:3, 10, 17:26, 28)]

##change years here, will see different slopes for herbarium. Herbarium slope flattens out in more recent years, ie 1990, 2000
PhenolAllData<-subset(PhenolAllData, Year >=1960)
ValidHerb<-PhenolAllData[(PhenolAllData$Latitude < 49) & (PhenolAllData$Longitude > -85), ]
ValidHerb<-ValidHerb[(ValidHerb$Latitude > 38) & (ValidHerb$Longitude < -74), ]
HerbData<-ValidHerb[,c(1, 10:22,35)]

##Attach data source as columns to all data frames

GreenhouseData$Source<-"Greenhouse"
FieldData$Source<-"Field"
HerbData$Source<-"Herbarium"
##bind all three data frames
AllData<-rbind(GreenhouseData, FieldData, HerbData)


ggplot(AllData, aes(x=GD, y=fti, color=Source))+
  geom_point(alpha=0.2)+geom_smooth(method="lm")
summary(lm(fti~GD*Source, data=AllData))
anova(lm(fti~GD*Source, data=AllData))

ggplot(AllData, aes(x=Latitude, y=fti))+
  geom_point(aes(colour=Source),data=AllData[AllData$Source=="Herbarium",],alpha=0.2) +
  geom_point(aes(colour=Source),data=AllData[AllData$Source!="Herbarium",],alpha=0.6,size=4) +
  geom_smooth(method="lm")+xlim(37.5,47)
summary(lm(fti~Latitude*Source, data=AllData))
anova(lm(fti~Latitude*Source, data=AllData))

