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
names(GHData)[1]<-"Pop_Code"


##set bounds to get subset for validation in greenhouse population range
##north and west bounds
ValidHerb<- PhenolAllData[(PhenolAllData$Latitude < 49) & (PhenolAllData$Longitude > -85), ]
ValidHerb<-ValidHerb[(ValidHerb$Latitude > 38) & (ValidHerb$Longitude < -74), ]

###Create list of populations and the locations
###pull out unique populations and their coordinates
ghpop <- data.frame(unique(GHData$Pop_Code), as.numeric(unique(GHData$Latitude)), as.numeric(unique(GHData$Longitude)))
names(ghpop)<- c("Pop_Code", "Latitude", "Longitude")
write.csv(ghpop, "MontaguePop.csv", row.names = FALSE)



##Done on the server: make list of stations associated with each population, and get station data.
##Below is what is needed to create GreenhouseData_wGDD2003.csv
GreenhouseData<- aggregate(GHData, by=list(GHData$Pop_Code), FUN=mean)
StnData<-read.csv("WeatherRawData/NOAAMontagueStation.csv", header=T)
##Data Setup
GreenhouseData$Year<-2003
GreenhouseData$yday<-round(GreenhouseData$Days) ##rounded in order to make GD and GDD calcuations
names(GreenhouseData)[1]<-"Pop_Code"

Cntr<-0
for(year in (2003:2003)){ 
  
  # Open file with GD data
  GDFilePath<-paste0("WeatherRawData/NOAAStnsMontague",year,".csv") 
  GDData<-read.csv(GDFilePath)
  for(Pop in GreenhouseData$Pop_Code[GreenhouseData$Year==year]){ # Cycle through pop_codes sampled in same year as GD data
    Cntr<-Cntr+1
    # Find names of nearby stations
    LocStns<-paste(unique(StnData$StationID[StnData$Pop_Code==Pop & StnData$Measure=="TMAX"]))
    # Subset GDData for stations of interest from Jan 1 to day of sampling
    PopGDData<-GDData[GDData$StationID %in% LocStns,]
    
    
    ## Remove stations with >10% missing data
    LocStns<-names(summary(PopGDData$StationID[!is.na(PopGDData$GDeg)])[summary(PopGDData$StationID[!is.na(PopGDData$GDeg)])>365*0.90])
    if(length(LocStns)<20){# If less than 20 stations, only remove stations with >30% missing data
      LocStns<-names(summary(PopGDData$StationID[!is.na(PopGDData$GDeg)])[summary(PopGDData$StationID[!is.na(PopGDData$GDeg)])>365*0.70])
    }
    if(length(LocStns)>20){
      # Sort remaining stations by distance
      LocStns<-unique(StnData[StnData$Pop_Code==Pop & StnData$StationID %in% LocStns,c("StationID","Dist")])
      # Keep closest 20 stations 
      LocStns<-paste(LocStns$StationID[order(LocStns$Dist)][1:20])
    }
    # Re-Subset GDData now that we have screened for missing data and distance
    PopGDData<-GDData[GDData$StationID %in% LocStns,]
    # Check for problems with climate data using mean and var to identify outliers
    OutTest<-aggregate(PopGDData$TMAX,by=list(PopGDData$StationID),FUN=c("mean"),na.rm=T)
    names(OutTest)<-c("Stn","mean")
    OutTest$var<-aggregate(PopGDData$TMAX,by=list(PopGDData$StationID),FUN=c("var"),na.rm=T)$x
    OutTest$mean<-abs((OutTest$mean-median(OutTest$mean))/median(OutTest$mean))<0.3
    OutTest$var<-abs((OutTest$var-median(OutTest$var))/median(OutTest$var))<0.3
    ##Update LocStns with Stations that are kept
    LocStns <- OutTest$Stn[OutTest$var & OutTest$mean]
    # Re-subset to remove outlier stations
    PopGDData<-GDData[GDData$StationID %in% LocStns,]
    
    # Make data frame to collect GDD, lat and long for each station
    GeoDat<-unique(StnData[StnData$StationID %in% LocStns,1:3])
    GeoDat$GD<-NA ##Tota Growing Season Length (in Days)
    GeoDat$GDs<-NA ##Growing Seaon Length to collection (in Days)
    GeoDat$GDD<-NA ##Total Growing Degree Days over season (in Growing Degrees)
    GeoDat$GDDs<-NA ##Growing Degree Days cumulative to time of collection (in Growing Degrees)
    GeoDat$meanGDeg<-NA ##mean of growing degrees per day / total season length (GDD/GD)
    GeoDat$varGDeg<-NA ##variance of growing degrees per day over the season 
    GeoDat$skewGDeg<-NA ##skew of growing degrees per day over the season 
    GeoDat$kurtGDeg<-NA ##variance of growing degrees per day over the season 
    
    ##Calculate GD, GDs, GDD, GDDs values for each station
    for (Stn in LocStns) { 
      ###
      yday<-as.numeric(GreenhouseData$yday[GreenhouseData$Pop_Code==Pop])
      test<-GDData[grep(Stn, GDData$StationID), ]   
      summary(test$GDeg) ## if NA exist for GDeg, run zoo
      if(anyNA(test$GDeg)){
        test$GDeg <-na.approx(test$GDeg, rule=2) ##rule 2 to deal with NA values at start and finish, not a good way because it repeats values
      }
      test$Indicator <-FALSE
      ####Set days with positive GDD to true
      test$Indicator <- ifelse(test$GDeg > 0, TRUE, test$Indicator)
      ## Give sequences of growing degree days and non-growing days
      rletest<-rle(test$Indicator)
      ##put rle results into table
      length<-rletest$lengths
      value<-rletest$values
      df<-data.frame(length, value)
      df<- df %>% rownames_to_column()
      ##find values
      intervals<-df[which(df$length >= 10 & df$value ==TRUE),]
      ## row numbers for the beginning(min) and end(max) of the growing season
      rowdates<-c(min(as.numeric(intervals$rowname)), max(as.numeric(intervals$rowname)))  ## first value is beginning of season, second value is end of season
      df$cumDayminus<-pcumsum(df$length) ### these values are for beginning of season
      df$cumDay<-cumsum(df$length) ### these values are used for end of season
      begin <- as.numeric(df$cumDayminus[as.integer(rowdates[1])]) +1 ##first day of season, need one day added because cumDayminus means it starts at day before the actual start of season
      end <- as.numeric(df$cumDay[as.integer(rowdates[2])]) ##last day of season
      
      
      #Calculations for season length, and season to collection, moments of distribution
      GD <- (end - begin) + 1  ##season length, need +1 so start of season is included
      GDs <- yday - begin + 1 ##length of season to collection, need +1 so start of season is included
      GDD <- sum(test$GDeg[begin:end]) ## GDD for the entire season
      GDDs <- sum(test$GDeg[begin:yday]) ##GDD from start of season to collection
      
      test <- test[c(begin:end),] ##subset data to only growing season
      meanGDeg <- mean(test$GDeg) ##mean of growing degrees per day
      varGDeg <- sum((test$GDeg - meanGDeg)^2)/GD ## var of growing degrees per day for growing season, no adjustion for sample size
      skewGDeg <- ((sum((test$GDeg - meanGDeg)^3))/GD) /(varGDeg)^(3/2) ##skewness, Fisher-Pearson (not adjusted for sample size)
      kurtGDeg <- ((sum((test$GDeg - meanGDeg)^4))/GD) /(varGDeg)^(4/2) -3 ##excess kurtosis for univariate data, 
      
      
      
      ##Data Values to Put into GeoDat (all stations for each pop)
      GeoDat$GD[GeoDat$StationID==Stn] <- GD
      GeoDat$GDs[GeoDat$StationID==Stn] <- GDs
      GeoDat$GDD[GeoDat$StationID==Stn] <- GDD
      GeoDat$GDDs[GeoDat$StationID==Stn] <- GDDs
      GeoDat$meanGDeg[GeoDat$StationID==Stn] <- meanGDeg
      GeoDat$varGDeg[GeoDat$StationID==Stn] <- varGDeg
      GeoDat$skewGDeg[GeoDat$StationID==Stn] <- skewGDeg
      GeoDat$kurtGDeg[GeoDat$StationID==Stn] <- kurtGDeg
    }
    
    
    # Spatial interpolation of GDD
    if(nrow(GeoDat)>=5){ # Only interpolate if at least 10 stations available
      # Interpolate GDD for Pop
      ### ALSO CHANGE THIS TO INTERPOLATE FOR GDD, GD, GDDs, GDs
      tpsGD<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$GD,scale.type="unscaled", lon.lat=T)
      tpsGDs<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$GDs,scale.type="unscaled", lon.lat=T)
      tpsGDD<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$GDD,scale.type="unscaled", lon.lat=T) # see details in ?Tps  
      tpsGDDs<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$GDDs,scale.type="unscaled", lon.lat=T)
      tpsmeanGDeg<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$meanGDeg,scale.type="unscaled", lon.lat=T)
      tpsvarGDeg<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$varGDeg,scale.type="unscaled", lon.lat=T)
      tpsskewGDeg<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$skewGDeg,scale.type="unscaled", lon.lat=T)
      tpskurtGDeg<-Tps(x=GeoDat[,c("Longitude","Latitude")],Y=GeoDat$kurtGDeg,scale.type="unscaled", lon.lat=T)
      
      GreenhouseData$GD[GreenhouseData$Pop_Code==Pop]<-predict(tpsGD,x=GreenhouseData[GreenhouseData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      GreenhouseData$GDs[GreenhouseData$Pop_Code==Pop]<-predict(tpsGDs,x=GreenhouseData[GreenhouseData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      GreenhouseData$GDD[GreenhouseData$Pop_Code==Pop]<-predict(tpsGDD,x=GreenhouseData[GreenhouseData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")      
      GreenhouseData$GDDs[GreenhouseData$Pop_Code==Pop]<-predict(tpsGDDs,x=GreenhouseData[GreenhouseData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")   
      GreenhouseData$meanGDeg[GreenhouseData$Pop_Code==Pop]<-predict(tpsmeanGDeg,x=GreenhouseData[GreenhouseData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      GreenhouseData$varGDeg[GreenhouseData$Pop_Code==Pop]<-predict(tpsvarGDeg,x=GreenhouseData[GreenhouseData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      GreenhouseData$skewGDeg[GreenhouseData$Pop_Code==Pop]<-predict(tpsskewGDeg,x=GreenhouseData[GreenhouseData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      GreenhouseData$kurtGDeg[GreenhouseData$Pop_Code==Pop]<-predict(tpskurtGDeg,x=GreenhouseData[GreenhouseData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      GreenhouseData$numStns[GreenhouseData$Pop_Code==Pop]<-nrow(GeoDat)
      # SAVE output
      write.csv(GreenhouseData,"GreenhouseData_wGDD2003.csv",row.names=F)
    }
    cat("***************\nIteration ",Cntr," of",length(unique(GreenhouseData$Pop_Code)),"\nYear: ",year,"\nPop: ",Pop,"\n",Sys.time(),"seconds","\nGDD: ",GreenhouseData$GDD[GreenhouseData$Pop_Code==Pop],"\nGDays: ",GreenhouseData$GDays[GreenhouseData$Pop_Code==Pop],"\n***************")
    yday<-LocStns<-PopGDData<-GeoDat<-tps<-NA # clean up for next iteration of pop
  }
  GDData<-GDFilePath<-NA # Clean-up for next iteration of year
}


##need to fill in two populations, ONEC, and ONTI, use CA006072225 for ONEC , use CA006078285 for ONTI



#Draft version of validation script. 
# ##Shortcut: 
# 
# ##merge GD calculated in GreenhouseData to GHData list
# 
# GreenhouseData<-read.csv("GreenhouseData_wGDD2003.csv", header=T)
# 
# names(GreenhouseData)
# 
# GreenhouseData<-GreenhouseData[,c(1,17:27)]
# 
# AllData<-inner_join(GHData, GreenhouseData)
# 
# ###normalizing flowering time 
# AllData$find<-(AllData$Days-mean(AllData$Days, na.rm=TRUE))/sd(AllData$Days, na.rm=TRUE)
# AllData$fti <- (AllData$find) + (AllData$GDDs-mean(AllData$GDDs,na.rm=TRUE))/sd(AllData$GDDs,na.rm=TRUE)
# 
# 
# ggplot(AllData, aes(x=GD, y=fti))+
#   geom_point(alpha=0.2)+geom_smooth(method="lm")
# 
# summary(lm(fti~GD, data=AllData))
# 
# 
# ggplot(ValidHerb, aes(x=GD, y=fti))+
#   geom_point(alpha=0.2)+geom_smooth(method="lm")
# summary(lm(fti~GD, data=ValidHerb))
# 
# 
# 
# 
# ##Assume fti for herbarium and greenhouse are similar measures
# dat1<-ValidHerb[,c(1,10:22, 35)]
# dat1$data<-"Herbarium"
# ###rearrange test so column order is the same as for ValidHerb
# dat2<-test[,c(1,7:8,18, 17, 19:27, 29)]
# names(dat2)[1]<- "Pop_Code"
# dat2$data<-"Greenhouse"
# 
# dat<-rbind(dat1,dat2)
# 
# ggplot(dat, aes(x=GD, y=fti, color=data))+
#   geom_point(alpha=0.2)+geom_smooth(method="lm")
# summary(lm(fti~GDD*data, data=dat))


