if (!require("sf")) install.packages("sf")
if (!require("sp")) install.packages("sp")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("magick")) install.packages("magick")

library(sf)
library(sp)
library(tidyverse)
library(RColorBrewer)
library(magick)


### Import London shape. Source: https://data.london.gov.uk/dataset?tags=map&tag=boundaries - please download yourselves after reading licence
wards <- read_sf('London_Ward_CityMerged.shp')

### Import crimes data. Source: https://data.london.gov.uk/dataset/ward-profiles-and-atlas
crime_rates <- read.csv(file="ward_atlas_data_crimes.csv", header=TRUE,skip=2, sep=",", na.strings=c("n/a"),stringsAsFactors=FALSE)
crime_rates <- crime_rates[,c("Codes","New.Code","Borough","Names","X2001.02","X2002.03","X2003.04","X2004.05","X2005.06","X2006.07","X2007.08","X2008.09","X2009.10" ,"X2010.11","X2011.12","X2012.13","X2013.14","X2014.15" )]


### Merging the London shape with crimes data
## Pull out data from shape and explore merging logic
y<- as_Spatial(wards, cast = TRUE, IDs = paste0("ID", 1:length(from)))

colnames(crime_rates)[2] <- "GSS_CODE"
data <- merge(as.data.frame(y),crime_rates, by="GSS_CODE", all.x = TRUE)

data<-data[,c("GSS_CODE","X2001.02","X2002.03","X2003.04","X2004.05","X2005.06","X2006.07","X2007.08","X2008.09","X2009.10","X2010.11","X2011.12","X2012.13","X2013.14","X2014.15")]
names <- c("y2001t02","y2002t03","y2003t04","y2004t05","y2005t06","y2006t07","y2007t08","y2008t09","y2009t10","y2010t11","y2011t12","y2012t13","y2013t14","y2014t15")
colnames(data) <- c("GSS_CODE",names)
crimes_max <- max(sapply(data, max, na.rm = TRUE)[2:ncol(data)])
crimes_min <- min(sapply(data, min, na.rm = TRUE)[2:ncol(data)])

years <- c()
for(year in names){
  data[,paste0(year,"_cropped")] <- sapply(data[,year], function(x){return(min(c(x,500)))} )
  years <- c(years,paste0(year,"_cropped"))
}


## Push crimes data back into shape
y <- merge(y, data, by="GSS_CODE")
crimes <- st_as_sf(y)


### Plotting crimes separately 
for(col in years){
  ggplot() + theme_dark() + geom_sf(data = crimes,aes_string(fill = col),color="NA")+scale_fill_distiller(name = "Crime rate",type = "seq", palette = "Reds", direction = 1,values = NULL, space = "Lab", na.value = "grey50",guide = "colourbar", aesthetics = "fill",breaks=c(100,200,300,400,500),labels=c("100","200","300","400","500+")) +labs(title=paste0(substr(col,2,5),"/", substr(col,7,8)))+
    labs(title=paste0(substr(col,2,5),"/", substr(col,7,8)),caption="Contains National Statistics data © Crown copyright and database right [2015] \nContains Ordnance Survey data © Crown copyright and database right [2015] \nContains public sector information licensed under the Open Government Licence v2.0.") +
    theme(plot.caption = element_text(size=6,hjust=0))
  ggsave(paste(col,".png"))
}


### Creating GIF of crime rates
img <- image_graph(600, 340, res = 96)
out <- list()
for(col in years){
  y <- ggplot() + theme_dark() + geom_sf(data = crimes,aes_string(fill = col),color="NA")+scale_fill_distiller(name = "Crime rate",limits = c(as.numeric(crimes_min),500),type = "seq", palette = "Reds", direction = 1,values = NULL, space = "Lab", na.value = "grey50",guide = "colourbar", aesthetics = "fill",breaks=c(100,200,300,400,500),labels=c("100","200","300","400","500+")) +labs(title=paste0(substr(col,2,5),"/", substr(col,7,8))) +
    labs(title=paste0(substr(col,2,5),"/", substr(col,7,8)),caption="Contains National Statistics data © Crown copyright and database right [2015] \nContains Ordnance Survey data © Crown copyright and database right [2015] \nContains public sector information licensed under the Open Government Licence v2.0.") +
    theme(plot.caption = element_text(size=6,hjust=0))
  print(y)
  out[[paste0("",col)]] <- y
}
dev.off()
animation <- image_animate(img, fps = 2)
image_write(animation, "london_crime_rates.gif")



