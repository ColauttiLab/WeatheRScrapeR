# Find closest stations for each populations 
clstation<-function(data=CloseStation,N=10){
  ClosestStation<-{}
  for (Pop in unique(data$Pop_Code)){ # Cycle through each population
    for (Msr in unique(data$Measure)){
      SData<-data[data$Pop_Code==Pop & data$Measure==Msr,]
      if (nrow(SData)>N){ # IF more than N stations, then pick N closest
        SData<-SData[order(SData$Dist),][1:N,]
      }
      ClosestStation<-rbind(ClosestStation,SData)        
      SData<-NA # Resetn for next Pop
    }
  }
  return(ClosestStation)
}