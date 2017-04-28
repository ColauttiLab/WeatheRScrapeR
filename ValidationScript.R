####Validation script
library(ggplot2)
##Data files: GreenhouseData_wGDD2003.csv from GHValidation.R
### FieldData_wGDDtest.csv from FieldValidation.R
### PhenolAllData.csv from MeasurementPCA.R



##Data import
GreenhouseData<-read.csv("MontagueGreenhousePopulations_wGDD_IDW.csv", header=T, stringsAsFactors = F)
FieldData<-read.csv("MontagueFieldPopulations_wGDD_IDW.csv", header=T, stringsAsFactors = F)

CommonGarden<-read.csv("CommonGarden_wGDD_IDW.csv", stringsAsFactors = F)
PhenolAllData<-read.csv("PhenolAllData.csv", header=T, stringsAsFactors=FALSE)


## create find and fti for Greenhouse and Field data

GreenhouseData$fti<-(GreenhouseData$yday-mean(GreenhouseData$yday, na.rm=TRUE))/sd(GreenhouseData$yday, na.rm=TRUE)
FieldData$find<-(FieldData$yday-mean(FieldData$yday, na.rm=TRUE))/sd(FieldData$yday, na.rm=TRUE)
FieldData$fti<- (FieldData$find) + (FieldData$GDDs-mean(FieldData$GDDs,na.rm=TRUE))/sd(FieldData$GDDs,na.rm=TRUE)

CommonGarden$find<-(CommonGarden$yday-mean(CommonGarden$yday, na.rm=TRUE))/sd(CommonGarden$yday, na.rm=TRUE)
CommonGarden$fti<- (CommonGarden$find) + (CommonGarden$GDDs-mean(CommonGarden$GDDs,na.rm=TRUE))/sd(CommonGarden$GDDs,na.rm=TRUE)



##Subset data frames
GreenhouseData<-GreenhouseData[,c("Pop_Code", "Latitude", "Longitude", "yday", "Year", "GD", "GDs", "GDD", "GDDs", "meanGDeg","varGDeg", "skewGDeg","kurtGDeg", "numStns", "fti")]
FieldData<-FieldData[,c("Pop_Code", "Latitude", "Longitude", "yday", "Year", "GD", "GDs", "GDD", "GDDs", "meanGDeg","varGDeg", "skewGDeg","kurtGDeg", "numStns", "fti")]
CommonGarden<- CommonGarden[,c("Pop_Code", "Latitude", "Longitude", "yday", "Year", "GD", "GDs", "GDD", "GDDs", "meanGDeg","varGDeg", "skewGDeg","kurtGDeg", "numStns", "fti")]

##change years here, will see different slopes for herbarium. Herbarium slope flattens out in more recent years, ie 1990, 2000
PhenolAllData<-subset(PhenolAllData, Year >=1960)
ValidHerb<-PhenolAllData[(PhenolAllData$Latitude < 49) & (PhenolAllData$Longitude > -85), ]
ValidHerb<-ValidHerb[(ValidHerb$Latitude > 38) & (ValidHerb$Longitude < -74), ]
HerbData<-ValidHerb[,c("Pop_Code", "Latitude", "Longitude", "yday", "Year", "GD", "GDs", "GDD", "GDDs", "meanGDeg","varGDeg", "skewGDeg","kurtGDeg", "numStns", "fti")]

##Attach data source as columns to all data frames

GreenhouseData$Source<-"Greenhouse"
FieldData$Source<-"Field"
CommonGarden$Source<-"CommonGardens"
HerbData$Source<-"Herbarium"

##bind all three data frames
AllData<-rbind(GreenhouseData, FieldData, CommonGarden, HerbData)


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

