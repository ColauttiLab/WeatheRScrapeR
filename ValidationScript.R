####Validation script
library(ggplot2)
library(tidyverse)
##Data files: GreenhouseData_wGDD2003.csv from GHValidation.R
### FieldData_wGDDtest.csv from FieldValidation.R
### PhenolAllData.csv from MeasurementPCA.R



##Data import
GreenhouseData<-read.csv("MontagueGreenhousePopulations_GDD_byDay.csv", header=T, stringsAsFactors = F)
#FieldData<-read.csv("MontagueFieldPopulations_GDD_byDay.csv", header=T, stringsAsFactors = F)

CommonGarden<-read.csv("CBCommonGarden_GDD_byDay.csv", stringsAsFactors = F)
names(CommonGarden)[1]<-"Individualnumber"

PhenolAllData<-read.csv("PhenolAllData.csv", header=T, stringsAsFactors=FALSE)


## create find and fti for Greenhouse and Field data

GreenhouseData$fti<-(GreenhouseData$yday-mean(GreenhouseData$yday, na.rm=TRUE))/sd(GreenhouseData$yday, na.rm=TRUE)

#FieldData$find<-(FieldData$GDs-mean(FieldData$GDs, na.rm=TRUE))/sd(FieldData$GDs, na.rm=TRUE)
#FieldData$fti<- (FieldData$find) + (FieldData$GDDs-mean(FieldData$GDDs,na.rm=TRUE))/sd(FieldData$GDDs,na.rm=TRUE)


CommonGarden %>% separate(Plant_ID, c("Sitenumber", "Site1", "Mat", "Row", "Position"), sep="_" ) -> CommonGarden

CommonGarden$Pop<-CommonGarden$Mat # Extract population code from maternal family code
CommonGarden$Pop<-gsub("[0-9]","",CommonGarden$Pop)
# CommonGarden$Origin<-CommonGarden$Pop 



AllData<-read.table("AllDataFixed.txt", header=T, sep=",")
AllData<-AllData[,c("Pop_Code", "Pop", "Lat")]
names(AllData)<-c("Pop", "Pop_Code", "Lat")
AllData<-unique(AllData)
AllData %>% mutate_if(is.factor, as.character) -> AllData
CommonGarden<-left_join(CommonGarden, AllData)

# CommonGarden$Origin[CommonGarden$Origin %in% c("A","C")]<-"Early" # Pair populations into early (north), intermediate, and late (south) 
# CommonGarden$Origin[CommonGarden$Origin %in% c("E","J")]<-"Int"
# CommonGarden$Origin[CommonGarden$Origin %in% c("S","T")]<-"Late"
# names(CommonGarden)[22]<-"Source"

CommonGarden$find<-(CommonGarden$GDs-mean(CommonGarden$GDs, na.rm=TRUE))/sd(CommonGarden$GDs, na.rm=TRUE)
CommonGarden$fti<- (CommonGarden$find) + (CommonGarden$GDDs-mean(CommonGarden$GDDs,na.rm=TRUE))/sd(CommonGarden$GDDs,na.rm=TRUE)



##Subset data frames
GreenhouseData<-GreenhouseData[,c("Pop_Code", "Latitude", "Longitude", "yday", "Year", "GD", "GDs", "GDD", "GDDs", "meanGDeg","varGDeg", "skewGDeg","kurtGDeg", "numStns", "fti")]
#FieldData<-FieldData[,c("Pop_Code", "Latitude", "Longitude", "yday", "Year", "GD", "GDs", "GDD", "GDDs", "meanGDeg","varGDeg", "skewGDeg","kurtGDeg", "numStns", "fti")]
CommonGarden<- CommonGarden[,c("Pop_Code","Longitude","yday", "Year", "GD", "GDs", "GDD", "GDDs", "meanGDeg","varGDeg", "skewGDeg","kurtGDeg", "numStns", "fti", "Lat", "Site1")]

##change years here, will see different slopes for herbarium. Herbarium slope flattens out in more recent years, ie 1990, 2000
PhenolAllData<-subset(PhenolAllData, Year >=1960)
ValidHerb<-PhenolAllData[(PhenolAllData$Latitude < 49) & (PhenolAllData$Longitude > -85), ]
ValidHerb<-ValidHerb[(ValidHerb$Latitude > 38) & (ValidHerb$Longitude < -74), ]
HerbData<-ValidHerb[,c("Pop_Code", "Latitude", "Longitude", "yday", "Year", "GD", "GDs", "GDD", "GDDs", "meanGDeg","varGDeg", "skewGDeg","kurtGDeg", "numStns", "fti")]

##Attach data source as columns to all data frames

GreenhouseData$Source<-"Greenhouse"
#FieldData$Source<-"Field"
HerbData$Source<-"Herbarium"
#CommonGarden$Source<-"CommonGarden"
names(CommonGarden)[15]<-"Latitude"
names(CommonGarden)[16]<-"Source"
##bind all three data frames
AllData<-rbind(GreenhouseData, FieldData, CommonGarden, HerbData)



ggplot(AllData, aes(x=Latitude, y=fti, color=Source))+
  geom_point(alpha=0.2)+geom_smooth(method="lm")

summary(lm(fti~GD, data=AllData))
anova(lm(fti~GD*Source, data=AllData))

Validation<-ggplot(AllData, aes(x=Latitude, y=fti))+
  geom_point(aes(colour=Source),data=AllData[AllData$Source=="Herbarium",],alpha=0.2, size=4) +
  geom_point(aes(colour=Source),data=AllData[AllData$Source!="Herbarium"  ,],alpha=0.6,size=6) +
  geom_smooth(method="lm", aes(color=Source), size=5) +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) +
  theme(text = element_text(size=20))


ggsave(Validation, filename = "Validation.png",
       width = 10,height=8,dpi = 300)

summary(lm(fti~Latitude*Source, data=AllData))
anova(lm(fti~Latitude*Source, data=AllData))

