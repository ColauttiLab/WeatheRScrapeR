library(tidyverse)
library(magick)
library(purrr)
## read in herbarium data
data <-read.csv("HerbariumPopData_wGDD_IDW.csv", header=T)
## map map of the world
mapWorld <- borders("world", colour="gray50", fill="gray100") # create a layer of borders


## loop creates one image for every year in range, 
for(i in 1866:2016) {
  gif<-ggplot(data, aes(x=Longitude, y=Latitude)) + 
    ## uses the world map from above
    mapWorld +
    geom_point(data=data[data$Year<i,], size=1.5, alpha=0.5, color="#CC33FF") +
    geom_point(data=data[data$Year==i,], size=2, color="#000033") +
    ## set limits on map to only show North American range
    coord_cartesian(ylim=c(32,53), xlim=c(-128,-60)) + 
    ## removes all titles and markings on x and y axis
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(), 
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank())+
    ## adds label for year, positioning at bottom right corner
    geom_text(aes(family="serif", label=paste0("Year: ", i), y=33, x=-70), color="black")
  print(paste0("saving plot ", i))
  ggsave(gif, filename = paste0("Lythrummap",i,".png"),
         width = 4,height=2,dpi = 150)
}

## gif making loop
list.files(path="C:/Users/YIHANWU/Documents/2016 Queens/WeatherScraper/WeatherScraper", pattern = "*.png") %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=20) %>% # animates, can opt for number of loops
  image_write("Lythrummap.gif") # write to current dir
