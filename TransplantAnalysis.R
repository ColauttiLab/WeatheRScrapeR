##package setup 
library(tidyverse)
library(chron)
library(lubridate)
library(fields) # Used for spatial interpolation of GDD for each population
library(zoo) # Used to impute missing data points for some weather stations in some days
library(FSA)

##read in data

tdata<-read.csv("S1_RecipTransInput.csv", header=T)
tdata<-tdata[,-c(6:9, 22:28)]

tdata %>% unite(Pop_Code, Site, Mat, Row, Pos, remove=FALSE) -> tdata


##make new data frames with names and dates for each year
Years<-c("08", "09", "10")
FloweringData<-NULL
##make individual rows for each individual
for (i in 1:length(Years)) {
  theyear<- paste0("Flwr", Years[i])
  year<-tdata[,c("Pop_Code", theyear, "Site")]
  dates<-month.day.year(year[,2], origin = c(month = 1, day = 1, year = 1900))
  year$Year<-paste0("20",Years[i])
  year$month<-dates$month
  year$day<-dates$day
  year %>% drop_na() -> year
  year %>%unite(date, Year, month, day, sep="-", remove=FALSE) ->year
  year$yday<-yday(year$date)
  year<-year[c("Pop_Code", "Year", "yday", "Site")]
  FloweringData<-rbind(FloweringData, year)
}


##Common garden gps coordinates
Site<-c("1_BEF", "2_KSR", "3_Timmins")
Latitude<-c(39.059479, 44.027957, 48.468117)
Longitude<-c(-78.059835, -79.541863, -81.351663)
Location<-as.data.frame(cbind(Site, Latitude, Longitude), stringsAsFactors = FALSE)
Location$Latitude<-as.numeric(Location$Latitude)
Location$Longitude<-as.numeric(Location$Longitude)


##Join locations to flowering data
FloweringData<-inner_join(FloweringData, Location)
FloweringData %>% rownames_to_column() -> FloweringData
names(FloweringData)[1:2]<-c("Pop_Code", "Plant_ID")

write.csv(FloweringData, "CommonGardenFloweringData.csv", row.names=FALSE)

##create station list for each location, done on server, 


CommonGardenData<-read.csv("CommonGarden_wGDD.csv", header=T)


