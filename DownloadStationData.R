############################################
## This script imports weather data 
############################################

##############################
## Load functions
##############################
library(ggplot2)
# Key paramaters:
# datasetid - see: http://www.ncdc.noaa.gov/cdo-web/datasets
dset<-"GHCND" # Daily climate observations

##############################
## Load functions
##############################
source("geodist.R")
source("clstation.R")

##############################
## Load population data
##############################
PopData<-read.csv("Test.csv")

##############################
## 1. Find Weather Stations
##############################
# Make Map Directory if needed
dir.create("WeatherRawData", showWarnings = FALSE)
# Download station locations unless they already exist:
if(!file.exists("WeatherRawData/ghcnd-inventory.txt")){  
  download.file(url=paste0("http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"),destfile="WeatherRawData/ghcnd-inventory.txt")
}
# Data format note:
# No headers V1. Station ID; V2. Latitude; V3. Longitude; V4. Observation type; V5. Observation value; V6. Earliest date; V7. Latest Date
# See http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
# A few key measurements in V4:
# PRCP - precipitation (1/10mm); SNWD - snow depth (mm); TMAX - max temp (1/10 deg C); TMIN - min temp (1/10 deg C); WESD - water equivalent of snow on ground (1/10mm)
# Measurements of interest
MsrInt<- c("PRCP","SNWD","TMAX","TMIN","WESD")
# Create object with weather station info
InData<-read.table("WeatherRawData/ghcnd-inventory.txt") 

# Skip import data step if file already exists
if(file.exists("WeatherRawData/NOAAStationData.csv")){  
  NOAAData<-read.csv("WeatherRawData/NOAAStationData.csv")
} else {
  NOAAData<-{}
  Ndg<-0.5 # Range (+/- degrees lat/log) to search for weather stations
  SMax<-100 # Max number of stations to retain (NOTE: Final number will be smaller due to missing weather data, etc.)
  for(i in 1:nrow(PopData)){
    # Find all stations +/- Ndg degrees
    CloseStations<-InData[InData$V2 > PopData$Latitude[i]-Ndg  & InData$V2 < PopData$Latitude[i]+Ndg &
                            InData$V3 > PopData$Longitude[i]-Ndg  & InData$V3 < PopData$Longitude[i]+Ndg,1:4 ]
    names(CloseStations)<-c("StationID","Latitude","Longitude","Measure")
    # Use wider area if < 20 stations
    if(length(unique(CloseStations$StationID[CloseStations$Measure=="TMAX"]))<SMax){ 
      Ndg<-1
      CloseStations<-InData[InData$V2 > PopData$Latitude[i]-Ndg  & InData$V2 < PopData$Latitude[i]+Ndg &
                              InData$V3 > PopData$Longitude[i]-Ndg  & InData$V3 < PopData$Longitude[i]+Ndg, 1:4]
      names(CloseStations)<-c("StationID","Latitude","Longitude","Measure")
    }
    
    # Add pop info and calculate distances
    if(nrow(CloseStations)>0){ # if at least one result is returned..
      # Calculate distance from population to each station
      CloseStations<-cbind(CloseStations,Pop_Code=PopData$Pop_Code[i])
      CloseStations$Dist<-NA
      for(j in 1:nrow(CloseStations)){ # Cycle through each station
        CloseStations$Dist[j]<-geodist(c(CloseStations$Latitude[j],CloseStations$Longitude[j],PopData$Latitude[i],PopData$Longitude[i]))   
      }
    }
    # Find up to SMax closest stations for EACH measure of interest
    for (Msr in MsrInt) { # Note - need to use same stations for tmax & tmin so don't do separately
      KeepSt<-subset(CloseStations,Measure==Msr)
      # If >SMax results, keep closest SMax 
      if(nrow(KeepSt)>SMax){
        KeepSt<-KeepSt[order(KeepSt$Dist),][1:SMax,]
      }
      # Add to main Stations list
      NOAAData<-rbind(NOAAData,KeepSt)
    }      
  }
  # Eliminate unused factor levels
  ## Rename some columns for later merging
  NOAAData$Measure<-factor(NOAAData$Measure)
  NOAAData$Type<-"NOAA_GHCN"
  write.csv(NOAAData,"WeatherRawData/NOAAStationData.csv", row.names=FALSE) 
}

##############################
## Download data and calculate Growing Degrees per day
##############################
# Download weather data from NOAA (ftp server has all locations for each year)
# Use FTP site to download data since we have >1,000 stations * years
for (year in 1973:1974){
  WthrData<-{} # Reset 'stations' data in each iteration
  FilePath<-paste0("WeatherRawData/NOAA_GHCN",year,".csv.gz")
  if(!file.exists(FilePath)){  # Skip if file already downloaded
    download.file(url=paste0("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/",year,".csv.gz"),destfile=FilePath)
    # Data format note:
    # No headers V1. Station ID; V2. Date, V3. Observation type; V4. Observation Value; V5. Observation time
    # See http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
    # A few key measurements:
    # PRCP - precipitation (1/10mm); SNWD - snow depth (mm); TMAX - max temp (1/10 deg C); TMIN - min temp (1/10 deg C); WESD - water equivalent of snow on ground (1/10mm)    
  }
  GDFilePath<-paste0("WeatherRawData/NOAAStnsClose",year,".csv") 
  if(!file.exists(GDFilePath)){  # Skip if data already calculated
    # Open Weather data
    WthrData<-read.csv(FilePath,header=F)[,1:4]
    ## NOTE: Opening file takes ~ 5sec/MB; or about 15min per file ()
    # Keep only stations in NOAAData and only weather data of interest
    WthrData<-WthrData[WthrData$V1 %in% NOAAData$StationID & WthrData$V3 %in% c("TMAX","TMIN"),]
    # Reorganize data into 'wide' format
    ## For each station and each day:
    # Calculate growing degrees above 5 deg C for each day: [(TMAX/10-TMIN/10)-5]/2 
    # NOTE: /10 because measured in 1/10 degrees
    # NOTE: Keep negative values to avoid bias in interpolation
    WthrData<-reshape(WthrData,v.names="V4",idvar=c("V1","V2"),timevar="V3",direction="wide")
    WthrData$GD<-((WthrData$V4.TMAX/10-WthrData$V4.TMIN/10)-0)/2 # Calculate growing degrees > 5 deg C
    WthrData$GD[!is.na(WthrData$GD) & WthrData$GD<0]<-0
    WthrData<-WthrData[,c("V1","V2","GD")]
    names(WthrData)<-c("StationID","Date","GD")
    WthrData$Date<-as.Date(gsub("([0-9]{4})([0-9]{2})([0-9]{2})","\\1-\\2-\\3",WthrData$Date))
    WthrData$Day<-strptime(WthrData$Date,format="%Y-%m-%d")$yday
    # Save data frame
    write.csv(WthrData,GDFilePath,row.names=FALSE)
  }
}

##############################
## Other stuff to explore weather station locations
##############################
# Count number of weather stations for each population
NStations<-aggregate(NOAAData$Pop_Code,by=list(NOAAData$Pop_Code),length)
qplot(x,data=NStations) # All have at least 400
sum(is.na(NStations[,2])|NStations[,2]==0) # Count number of stations with no data
# Separate for each measure
NStations<-aggregate(NOAAData$Pop_Code,by=list(NOAAData$Pop_Code,NOAAData$Measure),length)
qplot(x,data=NStations) + facet_wrap(~Group.2) # Min 100

# Calculate avg distance for 10 closest stations for each populations 
CloseStation<-clstation(data=NOAAData,N=10)
qplot(Dist,data=CloseStation[CloseStation$Measure=="TMAX",])+scale_x_log10()
length(unique(CloseStation$StationID)) # Total number of stations for which data is needed
# Plot each measure separately
qplot(Dist,data=CloseStation)+scale_x_log10()+facet_wrap(~Measure)

# Find the closest stations for each population
CloseStation<-clstation(data=NOAAData,N=1)
qplot(Dist,data=CloseStation[CloseStation$Measure=="TMAX",])+scale_x_log10()
length(unique(CloseStation$StationID))
# Closest stations more than > 10km; 20km; 100km:
aggregate(CloseStation$Pop_Code[CloseStation$Dist>10],by=list(CloseStation$Measure[CloseStation$Dist>10]),length)
aggregate(CloseStation$Pop_Code[CloseStation$Dist>20],by=list(CloseStation$Measure[CloseStation$Dist>20]),length)
aggregate(CloseStation$Pop_Code[CloseStation$Dist>100],by=list(CloseStation$Measure[CloseStation$Dist>100]),length)


