
## Libraries
library(ggmap)
library(tidyverse)
library(ggthemes)

### Measurement data
data <-read.csv("HerbariumPopData_GDD_byDay.csv", header=T)
## histogram of collection by year
histogramyear<-ggplot(data, aes(data$Year)) + 
  geom_histogram() + 
  theme_bw() + 
  labs(y="Number of Specimens", x="Collection Year") +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) +
  theme(text = element_text(size=25))
       


ggsave(histogramyear, filename = paste0("YearHistogram.png"),
       width = 8,height=8,dpi = 150)
## histogram of collection by days of year
histogramyday<-ggplot(data, aes(data$yday)) + 
  geom_histogram() + 
  theme_bw() + 
  labs(y="Number of Specimens", x="Day of Year") +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) +
  theme(text = element_text(size=25)) + 
  coord_cartesian(xlim = c(1, 365)) 

ggsave(histogramyday, filename = paste0("YdayHistogram.png"),
       width = 12,height=8,dpi = 150)


PhenolAllData<-read.csv("PhenolAllData.csv", header=T)
## Plot of latitudinal clines by four regions

Latclinebyregion<-ggplot(PhenolAllData, aes(x=Latitude, y=fti, color=Region))+ 
  facet_grid(Region~.) +
  geom_point(alpha=0.3, size=5)+
  geom_smooth(method="lm", size=4) +
  theme_bw() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(color = 'black'), legend.position = "none") +
  labs(y="Phenology Index") +
  theme(text = element_text(size=25), strip.text.y = element_text(angle = 0))
ggsave(Latclinebyregion, filename = "Latclinebyregion.png", height=8, width=8, dpi=300)


## Plot of latitudinal clines by regions and by era 

levels(PhenolAllData$Era)<-c("Pre-1960", "Post-1960")
levels(PhenolAllData$Region)<- c("East Coast", "Midwest", "West", "West Coast")

Latclinebyera<-ggplot(PhenolAllData, aes(x=Latitude, y=fti, color=Region)) + 
  facet_grid(Region~Era) +
  geom_point(alpha=0.2, size=5)+
  geom_smooth(method="lm", size=3)+
  theme_bw() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(color = 'black'), legend.position = "none") +
  labs(y="Phenology Index") +
  theme(text = element_text(size=25), strip.text.y = element_text(angle = 0, color = "white"))
ggsave(Latclinebyera, filename = "Latclinebyera.png", height=8, width=12, dpi=300)


## Plot of growing season clines by regions and by era


GDclinebyera<-ggplot(PhenolAllData, aes(x=GD, y=fti, color=Region)) + 
  facet_grid(Region~Era) +
  geom_point(alpha=0.2, size=5)+
  geom_smooth(method="lm", size=3)+
  theme_bw() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.line = element_line(color = 'black'), legend.position = "none") +
  labs(y="Phenology Index", x = "Growing Season Length") +
  theme(text = element_text(size=25), strip.text.y = element_text(angle = 0))
ggsave(GDclinebyera, filename = "GDclinebyera.png", height=8, width=12, dpi=300)


## Map of all stations in the study and the herbarium specimens
# get station data
StnData<- read.csv("GeoStns.csv", header=T)
StnData<-StnData[,c("StationID", "Latitude", "Longitude")]
# make unique list
UniqueStns<-aggregate(StnData, by=list(StnData$StationID), FUN=mean)

mapWorld <- borders("world", colour="gray50", fill="gray100") # create a layer of borders
Stnmap<-ggplot() + 
  ## uses the world map from above
  mapWorld +
  geom_point(data= UniqueStns, aes(x=Longitude, y=Latitude), color="gray50", alpha=0.3) +
  geom_point(data=data, aes(x=Longitude, y=Latitude), color="#CC33FF", alpha=0.5) +
  ## set limits on map to only show North American range
  coord_cartesian(ylim=c(32,53), xlim=c(-128,-60)) + 
  theme_map()+
  ## removes all titles and markings on x and y axis
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
ggsave(Stnmap, filename ="Stnmap.png" ,width = 10,height=5,dpi = 300)

load("testmap.RData")
#test<-c(left=-128, bottom= 30, right = -60, top= 65)
#testmap<- get_map(location=test, source="stamen", maptype="watercolor", zoom = 6, crop=FALSE)
#save(testmap, file= "testmap6.RData")
#ggmap(testmap)

#googlemap<-get_map(location=test,source="google", maptype="terrain", zoom=14)

Stnmap<- ggmap(testmap) +
  geom_point(data= UniqueStns, aes(x=Longitude, y=Latitude), color="gray50", alpha=0.3, size=2) +
  geom_point(data=data, aes(x=Longitude, y=Latitude), color="#CC33FF", alpha=0.5, size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
  
ggsave(Stnmap, filename = "StamenStationmap.svg", width=61, height=41.4, units="cm")




Regionmap<- ggplot() + 
  ## uses the world map from above
  mapWorld +
  ## set limits on map to only show North American range
  coord_cartesian(ylim=c(32,53), xlim=c(-128,-60)) + 
  geom_rect(aes(xmin=-76, xmax=Inf, ymin=-Inf, ymax=Inf), fill="#F8766D", alpha=0.4)+
  geom_rect(aes(xmin=-76, xmax=-95, ymin=-Inf, ymax=Inf), fill="#7CAE00", alpha=0.4)+
  geom_rect(aes(xmin=-95, xmax=-122, ymin=-Inf, ymax=Inf), fill="#00BFC4", alpha=0.4)+
  geom_rect(aes(xmin=-122, xmax=-Inf, ymin=-Inf, ymax=Inf), fill="#C77CFF", alpha=0.4)+
  geom_point(data= data, aes(x=Longitude, y=Latitude), color="#CC33FF", alpha=0.7) +
  
  ## removes all titles and markings on x and y axis
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

ggsave(Regionmap, filename ="Regionmap.png" ,width = 10,height=5,dpi = 300)

# plot stations around SRP060375 as example

Examplestns<-StnData[33376:33394,]
Examplestns %>% mutate_if(is.factor, as.character) -> Examplestns
Examplestns<-rbind(Examplestns, c("SRP060375", "43.52961", "-116.63041"))
#SRP060375 is at "43.52961", "-116.63041"
Examplestns$Latitude<-as.numeric(Examplestns$Latitude)
Examplestns$Longitude<-as.numeric(Examplestns$Longitude)
Examplemap<-ggplot() + 
  ## uses the world map from above
  mapWorld +
  geom_point(data=Examplestns[Examplestns$StationID!="SRP060375",], aes(x=Longitude, y=Latitude), size=1, alpha=0.1) +
  geom_point(data=Examplestns[Examplestns$StationID=="SRP060375",], aes(x=Longitude, y=Latitude), size=1, color="#CC33FF") +
  ## set limits on map to only show North American range
  coord_cartesian(ylim=c(32,53), xlim=c(-128,-60)) + 
  ## removes all titles and markings on x and y axis
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
ggsave(Examplemap, filename = "Examplemap.png", width=4, height=4, dpi=300)


GDData<-read.csv("WeatherRawData/NOAAStnsClose1866.csv", header=T)
#use station USC00288878
GDData<-GDData[GDData$StationID=="USC00288878",]
GDData<-GDData[!is.na(GDData$GDeg),]
GDData$TMAX<-GDData$TMAX/10
GDData$TMIN<- GDData$TMIN/10
GDData$TMEAN<-(GDData$TMAX+ GDData$TMIN)/2
GDData$CGDeg<- cumsum(GDData$GDeg)

Ggraph<-ggplot(data=GDData) + 
  labs(y="Degree Days", x="Day of Year") +
  theme_bw() +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 25))+
  #geom_point(aes(x=Day, y=TMAX), color="#b73e3e", size=2, alpha=0.5) +
  #geom_point(aes(x=Day, y=TMIN), color="#3c7aad", size=2, alpha=0.5) + 
  #geom_smooth(aes(x=Day, y=TMEAN),size=1, method="loess", alpha=0.5, color="#1ea806") +
  #geom_hline(yintercept = 8, size=2)# +
  geom_line(aes(x=Day, y=CGDeg), size= 1, color="#1ea806") 
ggsave(Ggraph, filename="Degreeday.png", width=12, height=8)
