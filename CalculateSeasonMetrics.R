############################################
## This script calculates growing degree days (GDD)
## using weather data from DownloadStationData.R
## then interpolates GDD for each population
## and growing season metrics.
############################################


##############################
## Load functions
##############################
library(fields) # Used for spatial interpolation of GDD for each population
library(zoo) # Used to impute missing data points for some weather stations in some days
library(dplyr)
library(tibble)
library(FSA)
######################################################
### Loop to calculate Growing Degree Days (do this first)
######################################################
setwd("C:/Users/YIHANWU/Documents/2016 Queens/WeatherScraper/WeatherScraper/WeatherRawData")
files <- list.files("C:/Users/YIHANWU/Documents/2016 Queens/WeatherScraper/WeatherScraper/WeatherRawData", pattern = "NOAAStnsClose*")
files
for ( i in 1:length(files)) {
  data <- read.csv(files[i], header=T)
  ##if this is not the first run and GDeg has already been calculated in the previous run. Ncol: depends on the ncol of DownloadStationData.R output csv files + 1 (new column added when GDeg was calculated previously).
  if (ncol(data)==9) {
  } else {
  data$GDeg <- (data$TMAX/10 + data$TMIN/10)/2 - 8
  data$GDeg <- ifelse(data$GDeg < 0, 0, data$GDeg)
  write.csv(data, files[i], row.names=F)
  }
}
############

##############################
## Load data
##############################
setwd("C:/Users/YIHANWU/Documents/2016 Queens/WeatherScraper/WeatherScraper/")
StnData<-read.csv("WeatherRawData/NOAAStationData.csv")
PopData<-read.csv("PopData.csv", header=T)


##############################
## Calculate GDD for 20 closest stations for each Pop
##############################

# For each population in the dataset
# Find nearby stations in NOAAData 
# load station growing degrees for each day (GD)
# NOTE: GD = (TMAX+TMIN)/20-8 and set GD=0 if GD<0


##GDeg : growing degree day per day
PopData$GD<-NA # Number of growing days above 5oC (Season Length)
PopData$GDs<-NA # Number of growing days from start of season to collection date
PopData$GDD<-NA # Standard Growing-Degree Days, as above
PopData$GDDs<-NA # GDD from start of season to date of collection
PopData$meanGDeg<-NA ##mean growing-degress per day over season
PopData$varGDeg<-NA ##variance of growing degrees per day over season
PopData$skewGDeg<-NA ## skew of """"
PopData$kurtGDeg<-NA ## kurtosis of """"
PopData$numStns <- NA ##number of stations used for the analysis




Cntr<-0

# For each year: 


for(year in (1866:2015)){ 
  
  # Open file with GD data
  GDFilePath<-paste0("WeatherRawData/NOAAStnsClose",year,".csv") 
  GDData<-read.csv(GDFilePath)
  for(Pop in PopData$Pop_Code[PopData$Year==year]){ # Cycle through pop_codes sampled in same year as GD data
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
      yday<-as.numeric(PopData$yday[PopData$Pop_Code==Pop])
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
    if(nrow(GeoDat)>=5){ # Only interpolate if at least 5 stations available
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
      
      PopData$GD[PopData$Pop_Code==Pop]<-predict(tpsGD,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      PopData$GDs[PopData$Pop_Code==Pop]<-predict(tpsGDs,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      PopData$GDD[PopData$Pop_Code==Pop]<-predict(tpsGDD,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")      
      PopData$GDDs[PopData$Pop_Code==Pop]<-predict(tpsGDDs,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")   
      PopData$meanGDeg[PopData$Pop_Code==Pop]<-predict(tpsmeanGDeg,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      PopData$varGDeg[PopData$Pop_Code==Pop]<-predict(tpsvarGDeg,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      PopData$skewGDeg[PopData$Pop_Code==Pop]<-predict(tpsskewGDeg,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      PopData$kurtGDeg[PopData$Pop_Code==Pop]<-predict(tpskurtGDeg,x=PopData[PopData$Pop_Code==Pop,c("Longitude","Latitude")],scale.type="unscaled")
      PopData$numStns[PopData$Pop_Code==Pop]<-nrow(GeoDat)
      
    } else {
      if (nrow(GeoDat)>1) {
        GeoDat<-GeoDat[order(GeoDat$Dist),]
      }
      PopData$GD[PopData$Pop_Code==Pop]<-GeoDat[1, "GD"]
      PopData$GDs[PopData$Pop_Code==Pop]<-GeoDat[1, "GDs"]
      PopData$GDD[PopData$Pop_Code==Pop]<-GeoDat[1, "GDD"]
      PopData$GDDs[PopData$Pop_Code==Pop]<-GeoDat[1, "GDDs"]
      PopData$meanGDeg[PopData$Pop_Code==Pop]<-GeoDat[1, "meanGDeg"]
      PopData$varGDeg[PopData$Pop_Code==Pop]<-GeoDat[1, "varGDeg"]
      PopData$skewGDeg[PopData$Pop_Code==Pop]<-GeoDat[1, "skewGDeg"]
      PopData$kurtGDeg[PopData$Pop_Code==Pop]<-GeoDat[1, "kurtGDeg"]
      PopData$numStns[PopData$Pop_Code==Pop]<-1
    }
    cat("***************\nIteration ",Cntr," of",length(unique(PopData$Pop_Code)),"\nYear: ",year,"\nPop: ",Pop,"\n",Sys.time(),"seconds","\nGDD: ",PopData$GDD[PopData$Pop_Code==Pop],"\nGDays: ",PopData$GDays[PopData$Pop_Code==Pop],"\n***************")
    yday<-LocStns<-PopGDData<-GeoDat<-tps<-NA # clean up for next iteration of pop
    # SAVE output
    write.csv(PopData,"PopData_wGDDtest.csv",row.names=F)
  }
  GDData<-GDFilePath<-NA # Clean-up for next iteration of year
}


