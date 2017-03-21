# Key paramaters:
# datasetid - see: http://www.ncdc.noaa.gov/cdo-web/datasets
dset<-"GHCND" # Daily climate observations

##############################
## Load functions
##############################
source("geodist.R")
source("clstation.R")


##Validation



##Montague's data were collected during 2004.  See paper at https://doi.org/10.1111/j.1420-9101.2007.01456.x

##import Montague et al. field data
Mont.field<-read.csv("FieldPhenology3.csv")
Mont.field$Longitude<- Mont.field$Longitude*-1
##set year of collection
CYear<-2004

MsrInt<- c("PRCP","SNWD","TMAX","TMIN","WESD")
# Create object with weather station info
InData<-read.table("WeatherRawData/ghcnd-inventory.txt") 

# Skip import data step if file already exists
if(file.exists("WeatherRawData/NOAAStnsValidField.csv")){  
  NOAAData<-read.csv("WeatherRawData/NOAAStnsValidField.csv")
} else {
  NOAAData<-{}
  Ndg<-0.5 # Range (+/- degrees lat/log) to search for weather stations
  SMax<-100 # Max number of stations to retain (NOTE: Final number will be smaller due to missing weather data, etc.)
  for(i in 1:nrow(Mont.field)){
    # Find all stations +/- Ndg degrees and have records in year of collection
    CloseStations<-InData[InData$V2 > Mont.field$Latitude[i]-Ndg & InData$V2 < Mont.field$Latitude[i]+Ndg &
                            InData$V3 > Mont.field$Longitude[i]-Ndg & InData$V3 < Mont.field$Longitude[i]+Ndg & InData$V5 <= CYear & InData$V6 >= CYear,1:4]
    names(CloseStations)<-c("StationID","Latitude","Longitude","Measure")
    # Use wider area if < SMax stations
    if(length(unique(CloseStations$StationID[CloseStations$Measure=="TMAX"]))<SMax){ 
      Ndg<-1
      CloseStations<-InData[InData$V2 > Mont.field$Latitude[i]-Ndg & InData$V2 < Mont.field$Latitude[i]+Ndg &
                              InData$V3 > Mont.field$Longitude[i]-Ndg & InData$V3 < Mont.field$Longitude[i]+Ndg & InData$V5 <= CYear & InData$V6 >= CYear,1:4]
      names(CloseStations)<-c("StationID","Latitude","Longitude","Measure")
    }
    
    # Add pop info and calculate distances
    if(nrow(CloseStations)>0){ # if at least one result is returned..
      # Calculate distance from population to each station
      CloseStations<-cbind(CloseStations,Population=Mont.field$Population[i])
      CloseStations$Dist<-NA
      for(j in 1:nrow(CloseStations)){ # Cycle through each station
        CloseStations$Dist[j]<-geodist(c(CloseStations$Latitude[j],CloseStations$Longitude[j],Mont.field$Latitude[i],Mont.field$Longitude[i]))   
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
  write.csv(NOAAData,"WeatherRawData/NOAAStnsValidField.csv", row.names=FALSE) 
}


print("Station Search Complete")




