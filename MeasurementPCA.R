
###Measurement data
data <-read.csv("AllData.csv", header=T)
str(data)
names(data)
summary(data)##check if there are NAs for yday and Year or the measurements.


##adapted from Leila's analysis
data<-subset(data,!(is.na(data["numStns"])))
#calculate actual lengths in cm
#length of flowered portion of inflorescence
data$flowered.cm<-data$actual/data$standard*data$flower.inf.length
#length of bud portion of inflorescence
data$bud.cm<-data$actual/data$standard*data$bud.inf.Length
#length of fruit portion of inflorescence
data$fruit.cm<-data$actual/data$standard*data$fruit.inf.Length
#length of aborted flower portion of inflorescence 
data$a.flower.cm<-data$actual/data$standard*data$aborted.inf.length
#length of inflorescence
data$infl.cm<-data$flowered.cm+data$bud.cm+data$fruit.cm+data$a.flower.cm


data$flowered.mm<-data$flowered.cm*10
data$bud.mm<-data$bud.cm*10
data$fruit.mm<-data$fruit.cm*10
data$a.flower.mm<-data$a.flower.cm*10
data$infl.mm<-data$infl.cm*10


PhenolPC<-princomp(~log(bud.mm+1)+log(flowered.mm+1)+log(infl.mm+1), data=data, cor=TRUE)
summary(PhenolPC)
ev<-PhenolPC$sdev^2
summary(ev)
nrow(data)
nrow(PhenolPC$scores)

write.csv(data, "AllData.csv", row.names=F)




PhenolPCScores<-data.frame(PhenolPC$scores)
names(PhenolPCScores)<-c("PPC1","PPC2","PPC3")
# merge PhemolPC1 back to All.herb2
PhenolAllData<-cbind(data,PhenolPCScores)
if(loadings(PhenolPC)[3,1]<0){
  PhenolAllData$PPC1<-(PhenolAllData$PPC1*-1)
}


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
  geom_point()



summary(lm(fti~Latitude*Region, data=PhenolAllData))
#plot(All.herb$GD,All.herb$fti)
#All.herb3<-All.herb3[All.herb3$GD>120,]
#summary(lm(fti~GD*Region, data=All.herb3))