library(ggplot2)

### Measurement data
data <-read.csv("CombinedData.csv", header=T)
str(data)
names(data)
summary(data)##check if there are NAs for yday and Year or the measurements.

## convert population code from factor to character
data$Pop_Code<-as.character(data$Pop_Code)

## exclude 'populations' (i.e. sample locations) that are missing climate data
data<-subset(data,!(is.na(data["numStns"])))
# calculate actual lengths in cm
# length of flowered portion of inflorescence (converting pixels to cm)
data$flowered.cm<-data$actual/data$standard*data$flower.inf.length
# length of bud portion of inflorescence
data$bud.cm<-data$actual/data$standard*data$bud.inf.Length
# length of fruit portion of inflorescence
data$fruit.cm<-data$actual/data$standard*data$fruit.inf.Length
# length of aborted flower portion of inflorescence 
data$a.flower.cm<-data$actual/data$standard*data$aborted.inf.length
# length of inflorescence
data$infl.cm<-data$flowered.cm+data$bud.cm+data$fruit.cm+data$a.flower.cm

## principal components analysis of inflorescence structure
PhenolPC<-princomp(~log(bud.cm+1)+log(flowered.cm+1)+log(infl.cm+1), data=data, cor=TRUE)
summary(PhenolPC)
## double-check that a PC-score exists for each individual in data
nrow(data)
nrow(PhenolPC$scores)
## replace AllData.csv with working dataset (i.e. excludes sites with missing climate data)
write.csv(data, "CompleteData.csv", row.names=F)

## Take PC scores and name
PhenolPCScores<-data.frame(PhenolPC$scores)
names(PhenolPCScores)<-c("PPC1","PPC2","PPC3")
## merge PC scores back to data
PhenolAllData<-cbind(data,PhenolPCScores)
## if PC1 is negative, then multiply by -1 so that larger values are later phenologies (i.e. more fruits than buds)
if(loadings(PhenolPC)[3,1]<0){
  PhenolAllData$PPC1<-(PhenolAllData$PPC1*-1)
}

## Adjust phenology for local climate and sampling date
## NOTE: GDs is the number of days from start of growing season to collection date
## GDDs is the number of growing-degrees from start of season to collection date
PhenolAllData$fti <- (PhenolAllData$PPC1-mean(PhenolAllData$PPC1,na.rm=TRUE))/sd(PhenolAllData$PPC1,na.rm=TRUE) + (PhenolAllData$GDs-mean(PhenolAllData$GDs,na.rm=TRUE))/sd(PhenolAllData$GDs,na.rm=TRUE)
PhenolAllData$Region <-NULL

for (i in 1:nrow(PhenolAllData)) {if (PhenolAllData$Longitude[i] > -100) {
  PhenolAllData$Region[i] <- "East"
}
  else {PhenolAllData$Region[i] <- "West"
  }
  } ##not complete
#preicting fti using GD

fti.model<-lm(fti~Latitude*Region,data=PhenolAllData)
summary(fti.model)


ggplot(PhenolAllData, aes(x=Latitude, y=fti, color=Region)) +
  geom_point(alpha=0.2)+geom_smooth()

ggplot(PhenolAllData, aes(x=GD, y=fti, color=Region)) +
  geom_point(alpha=0.2)+geom_smooth()

qplot(GDD,GDDs,data=PhenolAllData)


summary(lm(fti~Latitude*Region, data=PhenolAllData))
#plot(All.herb$GD,All.herb$fti)
#All.herb3<-All.herb3[All.herb3$GD>120,]
#summary(lm(fti~GD*Region, data=All.herb3))