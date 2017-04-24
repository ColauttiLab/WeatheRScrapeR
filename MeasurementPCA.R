### NOTE: MUST INSPECT PCA to determine which axis best captures phenology
### For Leila's (smaller) dataset it was PPC1
### For Yihan's (larger) dataset it is now PPC2
### If dataset changes, the PC loadings should be checked and code adjusted accordingly
### See ~line 60

## Libraries
library(ggplot2)
library(dplyr)
### Measurement data
data <-read.csv("PopData_wGDD.csv", header=T)
str(data)
names(data)
summary(data)##check if there are NAs for yday and Year or the measurements.

## convert data frame factors to character

data %>% mutate_if(is.factor, as.character) -> data
#data$Pop_Code<-as.character(data$Pop_Code)

## exclude 'populations' (i.e. sample locations) that are missing climate data
data<-subset(data,!(is.na(data["numStns"])))
# calculate actual lengths in cm
# length of flower portion of inflorescence (converting pixels to cm)
data$flower.cm<-data$actual/data$standard*data$flower.inf.length
# length of bud portion of inflorescence
data$bud.cm<-data$actual/data$standard*data$bud.inf.Length
# length of fruit portion of inflorescence
data$fruit.cm<-data$actual/data$standard*data$fruit.inf.Length
# length of aborted flower portion of inflorescence 
data$a.flower.cm<-data$actual/data$standard*data$aborted.inf.length
# length of inflorescence
data$infl.cm<-data$flower.cm+data$bud.cm+data$fruit.cm+data$a.flower.cm

# Calculate a phenology index based on proportion of buds, flowers and fruits:

## fruit has a value of -1
## flower has a value of 0
## bud has a value of +1
## (fruit*(-1) + flower*0 + bud*(+1))/(fruit+flower+bud)
## aborted flowers treated as fruits (earliest)
## THEREFORE: -- means early phenology ++ means late phenology 
data$phind<-(data$bud.cm-data$fruit.cm-data$a.flower.cm)/data$infl.cm

## Alternatively, use PCA
## principal components analysis of inflorescence structure
PhenolPC<-princomp(~log(bud.cm+1)+log(flower.cm+1)+log(fruit.cm+a.flower.cm+1), data=data, cor=TRUE)
summary(PhenolPC)
loadings(PhenolPC) # See which PC captures phenology
## double-check that a PC-score exists for each individual in data
nrow(data)
nrow(PhenolPC$scores)
## replace AllData.csv with working dataset (i.e. excludes sites with missing climate data)
write.csv(data, "PCA.csv", row.names=F)

## Take PC scores and name
PhenolPCScores<-data.frame(PhenolPC$scores)
names(PhenolPCScores)<-c("PPC1","PPC2","PPC3")

## Examine PCA plots
qplot(PPC1,PPC2,data=PhenolPCScores)
qplot(PPC1,PPC3,data=PhenolPCScores)
qplot(PPC2,PPC3,data=PhenolPCScores)

## merge PC scores back to data
PhenolAllData<-cbind(data,PhenolPCScores)

##### IMPORTANT!!! MUST CHECK EACH TIME DATA SET CHANGES
### Replace with PPC that best captures phenological stage
phPC<-1
PhenolAllData$phPC<-PhenolAllData[,grep(paste0("PPC",phPC),names(PhenolAllData))]

## if PPC is negative, then multiply by -1 so that larger values are later phenologies (i.e. more fruits than buds)
loads<-loadings(PhenolPC)[,phPC]
if(loadings(PhenolPC)[grep("fruit",names(loads)),phPC]<0){
  PhenolAllData$phPC<-(PhenolAllData$phPC*-1)
}

## Compare PCA to phenology index
qplot(phind,phPC,data=PhenolAllData)
cor(PhenolAllData$phind,PhenolAllData$phPC)
qplot(PhenolAllData$phind)
qplot(PhenolAllData$phPC)
## phPC is more normally distributed, but phind captures min/max phenol stage (-1 to +1)
## But if phPC (above) is calculated from non-(log)transformed measurements, then correlation is very tight
## NOTE few points with phind == -1 and phPC ~0. This seems to be due to lack of flowers creating an upward bias in phPC
## Therefore, phind seems to be the biologically more relevant parameter

## Separate data into east vs. west
PhenolAllData$Region <-NULL
PhenolAllData$Region[PhenolAllData$Longitude > -76]<-"EastCoast"
PhenolAllData$Region[PhenolAllData$Longitude <= -76 & PhenolAllData$Longitude > -95]<-"MidWest"
PhenolAllData$Region[PhenolAllData$Longitude <= -95 & PhenolAllData$Longitude > -122]<-"West"
PhenolAllData$Region[PhenolAllData$Longitude <= -122]<-"WestCoast"
PhenolAllData$Region<-as.factor(PhenolAllData$Region)

## Separate by date
qplot(PhenolAllData$Year)
PhenolAllData$Era <-NULL
PhenolAllData$Era[PhenolAllData$Year < 1960]<-"Early"
#PhenolAllData$Era[PhenolAllData$Year >= 1920 & PhenolAllData$Year < 1960]<-"Middle"
PhenolAllData$Era[PhenolAllData$Year >= 1960]<-"Recent"
PhenolAllData$Era<-as.factor(PhenolAllData$Era)


#### NOTE: Most of the code below be moved to an R markdown file for publication

## Compare total growing season (GD & GDD) vs. growing season to date of sampling (GDs & GDDs)
qplot(GD,GDs,data=PhenolAllData)
cor(PhenolAllData$GD,PhenolAllData$GDs)
qplot(GDD,GDDs,data=PhenolAllData)
cor(PhenolAllData$GDD,PhenolAllData$GDDs)

## Weaker correlation across GD and GDD; 
## Use GDDs in model and GDD
qplot(GDDs,GD,data=PhenolAllData)
cor(PhenolAllData$GD,PhenolAllData$GDDs)
qplot(GDD,GDs,data=PhenolAllData)
cor(PhenolAllData$GDD,PhenolAllData$GDs)

## Some significant colinearity; which is a better predictor of phenology?
summary(lm(phPC~GD,data=PhenolAllData)) # Total growing season (GD) is a good predictor
summary(lm(phPC~GDs,data=PhenolAllData)) # Growing days until sampling (GDs) is a good predictor, as expected
summary(lm(phPC~GDs*GD,data=PhenolAllData)) # GD becomes non-significant after controlling for GDs; interaction may suggest non-linear relationship

## Similar results for phind
summary(lm(phind~GD,data=PhenolAllData)) 
summary(lm(phind~GDs,data=PhenolAllData)) 
summary(lm(phind~GDs*GD,data=PhenolAllData)) 

##Yihan: 
##GD is significant
##GDs is significant
##GDs and GDs:GD is significant, GD becomes non-significant


## Similar result for growing degree days
summary(lm(phPC~GDD,data=PhenolAllData)) # Total growing degree days (GDD) not a good predictor
summary(lm(phPC~GDDs,data=PhenolAllData)) # GDD until sampling (GDDs) is a good predictor, as expected
summary(lm(phPC~GDDs*GDD,data=PhenolAllData)) # GDD becomes significant after controlling for GDDs


summary(lm(phind~GDD,data=PhenolAllData)) # Total growing degree days (GDD) not a good predictor
summary(lm(phind~GDDs,data=PhenolAllData)) # GDD until sampling (GDDs) is a good predictor, as expected
summary(lm(phind~GDDs*GDD,data=PhenolAllData)) # GDD becomes significant after controlling for GDDs



## Adjust phenology for local climate and sampling date
## NOTE: GDs is the number of days from start of growing season to collection date
## GDDs is the number of growing-degrees from start of season to collection date
qplot(GDDs,phind,data=PhenolAllData)
PhenolAllData$fti <- (PhenolAllData$phind) + (PhenolAllData$GDDs-mean(PhenolAllData$GDDs,na.rm=TRUE))/sd(PhenolAllData$GDDs,na.rm=TRUE)


write.csv(PhenolAllData, "PhenolAllData.csv", row.names = FALSE)

## Look at clines by region
ggplot(PhenolAllData, aes(x=Latitude, y=fti, color=Region)) + facet_grid(Region~.) +
  geom_point(alpha=0.2)+geom_smooth(method="lm")
summary(lm(fti~Latitude*Region, data=PhenolAllData))

## Cool clines with latitude, steeper in east than west, but what about GD:
ggplot(PhenolAllData, aes(x=Latitude, y=GD, color=Region)) + facet_grid(Region~.) +
  geom_point(alpha=0.2)+geom_smooth(method="lm") # Also steeper in east than west
summary(lm(Latitude~GD*Region, data=PhenolAllData))

## What about fti vs. GD?
ggplot(PhenolAllData, aes(x=GD, y=fti, color=Region)) + facet_grid(Region~.) +
  geom_point(alpha=0.2)+geom_smooth(method="lm")
summary(lm(fti~GD*Region, data=PhenolAllData))

## Clines by era/year
summary(lm(fti~GD*Region*Year, data=PhenolAllData))
ggplot(PhenolAllData, aes(x=GD, y=fti, color=Region)) + facet_grid(Region~Era) +
  geom_point(alpha=0.2)+geom_smooth(method="lm")

mod1<-lm(fti~GD+Era, data=PhenolAllData[PhenolAllData$Region=="EastCoast",])
mod2<-lm(fti~GD*Era, data=PhenolAllData[PhenolAllData$Region=="EastCoast",])
anova(mod1,mod2)
summary(mod2)

mod3<-lm(fti~GD+Era, data=PhenolAllData[PhenolAllData$Region=="MidWest",])
mod4<-lm(fti~GD*Era, data=PhenolAllData[PhenolAllData$Region=="MidWest",])
anova(mod3,mod4)
summary(mod3)

## Why is cline evolving in East Coast but not Midwest
## Overall advanced phenology in Midwest but not East Coast
## What about GD and GDD vs. time?
mod5<-lm(GD~Era*Latitude, data=PhenolAllData[PhenolAllData$Region=="EastCoast",])
summary(mod5)

mod6<-lm(GDD~Era*Latitude, data=PhenolAllData[PhenolAllData$Region=="EastCoast",])
summary(mod6)

mod5<-lm(GD~Era*Latitude, data=PhenolAllData[PhenolAllData$Region=="MidWest",])
summary(mod5)

mod6<-lm(GDD~Era*Latitude, data=PhenolAllData[PhenolAllData$Region=="MidWest",])
summary(mod6)


## Bin into lat x long squares to calculate mean and S.E. for better cline estimate 
## i.e., generate geographic 'populations' from point samples

binx<-0.5 ## long bin size in degrees
biny<-0.5 ## lat bin size in degrees

## Define long x lat
longs<-seq(min(PhenolAllData$Longitude,na.rm=T)+binx,max(PhenolAllData$Longitude,na.rm=T)-binx,by=binx*2)
lats<-seq(min(PhenolAllData$Latitude,na.rm=T)+biny,max(PhenolAllData$Latitude,na.rm=T)-biny,by=biny*2)

## Make into matrix
bindat<-data.frame(Long=rep(longs,length(lats)),Lat=sort(rep(lats,length(longs))),fti=NA,GD=NA)
## Make sure it looks right
qplot(bindat$Long,bindat$Lat)

for (i in 1:nrow(bindat)){
  bindat$fti[i]<-mean(PhenolAllData$fti[PhenolAllData$Longitude >= bindat$Long[i]-binx & PhenolAllData$Longitude < bindat$Long[i]+binx &
                                          PhenolAllData$Latitude >= bindat$Lat[i]-biny & PhenolAllData$Latitude < bindat$Lat[i]+biny],na.rm=T)
  bindat$GD[i]<-mean(PhenolAllData$GD[PhenolAllData$Longitude >= bindat$Long[i]-binx & PhenolAllData$Longitude < bindat$Long[i]+binx &
                                        PhenolAllData$Latitude >= bindat$Lat[i]-biny & PhenolAllData$Latitude < bindat$Lat[i]+biny],na.rm=T)
}
qplot(GD,fti,data=bindat,alpha=I(0.2))+geom_smooth(method="lm")
##Yihan: include warning message about missing values, some bins do not have any specimens in them

bindat$Region <-NULL
bindat$Region[bindat$Long > -80]<-"EastCost"
bindat$Region[bindat$Long <= -80 & bindat$Long > -95]<-"MidWest"
bindat$Region[bindat$Long <= -95 & bindat$Long > -122]<-"West"
bindat$Region[bindat$Long <= -122]<-"WestCoast"
bindat$Region<-as.factor(bindat$Region)

# Finally, calculate average for each latitude
bindat<-aggregate(cbind(bindat$fti,bindat$GD),by=list(bindat$Lat,bindat$Region),FUN="mean",na.rm=T)
names(bindat)<-c("Lat","Region","fti","GD")

## Look at clines by region
ggplot(bindat, aes(x=GD, y=fti,colour=Region)) + facet_grid(Region~.)+
  geom_point(size=3)+geom_smooth(method="lm") 
summary(lm(fti~GD*Region, data=bindat))

ggplot(bindat, aes(x=Lat, y=fti,colour=Region)) + facet_grid(Region~.)+
  geom_point(size=3)+geom_smooth(method="lm") 
summary(lm(fti~Lat*Region, data=bindat))

qplot(GD,fti,data=PhenolAllData,alpha=I(0.2))+geom_smooth(method="lm")

#### TO DO:
## Validation: Comparing cline in fti from herbarium records vs. cline in weighted ft from Jessica's study and ft in greenhouse and field from Rob's study
## Temporal analysis for plastic vs. evolutionary response
## Explore effects of growing season variables (e.g. variance, skew, etc.)






