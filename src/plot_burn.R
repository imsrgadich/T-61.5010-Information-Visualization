setwd("/home/imsrgadich/Documents/gitrepos/Courses/T-61.5010-Information-Visualization/data")

#install.packages("mapproj")
#install.packages("ggmap")
#install.packages("DeducerSpatial")

library(xlsx)
library("mapproj")
library("ggmap")
library("DeducerSpatial")
library(ggplot2)
require(maps)
library(ggmap)
library(maps)
library(ggplot2)
library(scales)
library(ggsubplot)
library(devtools)
library(sqldf)
library(stats)
require(reshape2)
require(ggplot2); theme_set(theme_bw())

mortality <- read.xlsx("Burns age-adjusted mortality from GBD 20100812.xlsx",sheetName="Data",header = T)

mortality$diff <- mortality[,3] - mortality[,2]

# Plot unemployment by country
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")

world <- map_data("world",zoom = 10)
mortality$colorBuckets <- as.numeric(cut(mortality$Y2004, c(-1, 4, 8, 12, 16)))

colorsmatched <- mortality$colorBuckets[match(world$region, mortality$Country)]


################################

mortality_df = melt(mortality)

colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")

world <- map_data("world",zoom = 10)

newdata <- subset(mortality_df, variable=="diff" ,select=Country:value) 
colnames(world)[3] <- "groups"


mod_data1 <- sqldf("select w.groups as groups, w.Region as Country, n.value as value, w.lat as Lat, w.long as Long from world as w LEFT JOIN newdata as n on n.Country = w.region")
mod_data1[is.na(mod_data1)] <- 0

mod_data2 <- merge(world,newdata,by.x = "region",by.y = "Country")

##########World labels
library(maptools)

worldmap <- map('world', fill=TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(worldmap$names, ":"), function(x) x[1])

world_sp <- map2SpatialPolygons(worldmap, IDs=IDs,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
# coordinates pulls out the centroid of 
#the largest (area) polygon for a country
world.label <- data.frame(
  country = names(world_sp), 
  coordinates(world_sp))

## Take the countries which are provded in the dataset
world.label.new <- merge(world.label,newdata,by.x = "country",by.y = "Country", all.y=T)
######
#install_github("garrettgman/ggsubplot")



dev.off()
ggplot(data=mod_data1, aes(x=Long, y=Lat, group=groups, fill=value))+ 
  geom_polygon() + 
  geom_text(aes(X1, X2, label = country,group = NULL), data = world.label.new,size = 2.50,check_overlap=T)+
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank()) +
  scale_fill_distiller(name="Deaths/100k", palette = "RdGy", breaks = pretty_breaks(n = 3))+
  theme_nothing(legend = TRUE)+
  theme(plot.title = element_text(size=25),legend.margin = unit(0.5, "line"),panel.border = element_blank()) +
  labs(title="Trend of burns deaths/accidents age adjusted mortality in 2004 and 2002",x=NULL, y=NULL)

+
  geom_subplot2d(aes(X1, X2, subplot = geom_bar(aes(value, ..count.., fill = value))), bins = c(15,12), ref = NULL, width = rel(0.8), data = world.label.new)
theme(panel.border = element_blank())
############################################################################################  
## Try putting manual pallete

dev.off()
ggplot(data=mortality, aes(y=Y2004,factor(Country)))+ 
  geom_polygon() 

means.barplot <- qplot(x=Country, y=Y2004,
                       data=mortality, geom="bar")

means.barplot


##########################################################################################


dev.off()
ggplot(data=mod_data1, aes(x=Long, y=Lat, group=groups, fill=value))+ 
  geom_polygon() + 
  geom_text(aes(X1, X2, label = country,group = NULL), data = world.label.new,size = 2.50,check_overlap=T)+
  theme(legend.position = "none") +
  theme(axis.text = element_blank(), axis.title=element_blank()) +
  scale_fill_distiller(name="Deaths/100k", palette = "YlOrRd", breaks = pretty_breaks(n = 3),trans = "reverse")+
  theme_nothing(legend = TRUE)+
  theme(plot.title = element_text(size=25),legend.margin = unit(0.5, "line"),panel.border = element_blank()) +
  labs(title="Burns deaths/accidents age adjusted mortality per 100 000 standard population in 2004",x=NULL, y=NULL)

