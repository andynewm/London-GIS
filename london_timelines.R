if (!require("sf")) install.packages("sf")
if (!require("sp")) install.packages("sp")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("magick")) install.packages("magick")
if (!require("ggpubr")) install.packages("ggpubr")

library(sf)
library(sp)
library(tidyverse)
library(RColorBrewer)
library(magick)
library(ggpubr)


### Import London shape. Source: https://data.london.gov.uk/dataset?tags=map&tag=boundaries - please download yourselves after reading licence
wards <- read_sf('London_Ward_CityMerged.shp')

### Import crimes data. Source: https://data.london.gov.uk/dataset/ward-profiles-and-atlas
crime_rates <- read.csv(file="ward_atlas_data_crimes.csv", header=TRUE,skip=2, sep=",", na.strings=c("n/a"),stringsAsFactors=FALSE)
crime_rates <- crime_rates[,c("Codes","New.Code","Borough","Names","X2001.02","X2002.03","X2003.04","X2004.05","X2005.06","X2006.07","X2007.08","X2008.09","X2009.10" ,"X2010.11","X2011.12","X2012.13","X2013.14","X2014.15" )]


### Merging London shape file with crime rates
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


## Push data back into shape
y <- merge(y, data, by="GSS_CODE")
crimes <- st_as_sf(y)


### GIF with timeline title
## Prep timeline data
year <- sapply(names, function(x) paste0(substr(x,2,5),"/", substr(x,7,8)))
df_year = data.frame(year)

## Create gif
img <- image_graph(600, 340, res = 96)
out <- list()
for(col in years){
  ## Create timeline
  timeline_plot<-ggplot(df_year,aes(x=paste0(substr(col,2,5),"/", substr(col,7,8)),y=0)) + theme_classic()+ geom_hline(yintercept=0, color = "black", size=0.3)
  # Don't show axes, appropriately position legend
  timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                     axis.text.y=element_blank(),
                                     axis.title.x=element_blank(),
                                     axis.title.y=element_blank(),
                                     axis.ticks.y=element_blank(),
                                     axis.text.x =element_blank(),
                                     axis.ticks.x =element_blank(),
                                     axis.line.x =element_blank(),
                                     legend.position = "bottom"
  )
  # Scale
  timeline_plot <- timeline_plot+ylim(-0.03, 0.03)
  # Add years
  timeline_plot<-timeline_plot+ geom_text(data=df_year, aes(x=year,y=-0.02,label=year, fontface="bold"),size=2.5, color='black')
  # Add point
  timeline_plot <- timeline_plot+scale_color_manual(values="2001/02", labels="2001/02", drop = FALSE)
  timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2)
  
  ## Create map
  geom <- ggplot() + theme_dark() + geom_sf(data = crimes,aes_string(fill = col),color="NA")+scale_fill_distiller(name = "",limits = c(as.numeric(crimes_min),500),type = "seq", palette = "Reds", direction = 1,values = NULL, space = "Lab", na.value = "grey50",guide = "colourbar", aesthetics = "fill",breaks=c(100,200,300,400,500),labels=c("100","200","300","400","500+")) +
    labs(caption="Contains National Statistics data © Crown copyright and database right [2015] \nContains Ordnance Survey data © Crown copyright and database right [2015] \nContains public sector information licensed under the Open Government Licence v2.0.") +
    theme(plot.caption = element_text(size=4,hjust=0))
  
  ## Merge graphs
  y <-ggarrange(timeline_plot, geom, heights = c(1, 7),ncol = 1, nrow = 2)  
  y <-annotate_figure(y,top = text_grob("Crime Rates London Wards", color = "black", face = "bold", size = 14))
  print(y)
  out[[paste0("",col)]] <- y
}
dev.off()
animation <- image_animate(img, fps = 2)
image_write(animation, "london_crime_rates_timeline.gif")



