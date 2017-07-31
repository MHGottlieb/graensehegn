library(rworldmap)
library(raster)
library(rgeos)
library(magrittr)
library(rgdal)
library(geosphere)
library(sp)

Africa <- c("DZA", "AGO", "SHN", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD", "COM", "COG", "COD", "DJI", "EGY", 
            "GNQ", "ERI", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "CIV", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", 
            "MRT", "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "STP", "REU", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", 
            "ZAF", "SSD", "SHN", "SDN", "SWZ", "TZA", "TGO", "TUN", "UGA", "COD", "ZMB", "TZA", "ZWE", "ESH", "TUR")

Europe <- c("ALB", "AND", "AUT", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GIB", 
            "GRC", "HUN", "ISL", "IRL", "IMN", "ITA", "XKX", "LVA", "LIE", "LTU", "LUX", "MKD", "MLT", "MCO", "MNE", "NLD", 
            "NOR", "POL", "PRT", "ROU", "SMR", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "GBR", "VAT", "RSB", "KOS")

worldMap <- getMap(resolution = "low")


Europe_map <- worldMap[worldMap$ISO_A3%in%Europe,] %>% aggregate(FUN=mean) %>% spTransform(CRS("+proj=longlat +datum=WGS84")) # WGS84 Commonly used by organizations that provide GIS data for the entire globe or many countries. CRS used by Google Earth
Africa_map <- worldMap[worldMap$ISO_A3%in%Africa,] %>% aggregate(FUN=mean) %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
world_map <- worldMap %>% aggregate(FUN=mean) %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

costal_africa <- gBuffer(Africa_map, byid=FALSE, width=9)
costal_europe <- intersect(as(Europe_map, "SpatialLines"), costal_africa)

xlim <- c(-10, 30)
ylim <- c(25, 55)

plot(world_map, border="darkgrey", col="lightgrey", bg="grey", xlim=xlim, ylim=ylim, asp=1)
#plot(Europe_map, border="darkblue", xlim=xlim, ylim=ylim, asp=1, add=TRUE)
#plot(Africa_map, border="darkgreen", xlim=xlim, ylim=ylim, asp=1, add=TRUE)
plot(costal_africa, lty=2, xlim=xlim, ylim=ylim, add=TRUE)
plot(costal_europe, col="red", lwd=2, xlim=xlim, ylim=ylim, add=TRUE)

SpatialLinesLengths(costal_europe, longlat=TRUE)


#Her gør vi en forsimplet antagelse om, at i længde/breddegrad = 111 km. 
radii <- c(250, 500, 750, 1000, 1500, 2000, 2500, 3000)

par(mfrow=c(2,4), oma = c(0, 0, 4, 0))

for(i in 1:length(radii)){
  costal_africa <- gBuffer(Africa_map, byid=FALSE, width=radii[i]/111)
  costal_europe <- intersect(as(Europe_map, "SpatialLines"), costal_africa)
  plot(world_map, border="darkgrey", col="lightgrey", bg="white", xlim=xlim, ylim=ylim, asp=1, main=paste0(radii[i], " km"))
  plot(costal_africa, lty=2, xlim=xlim, ylim=ylim, bg="grey", add=TRUE)
  plot(costal_europe, col="red", lwd=2, xlim=xlim, ylim=ylim, bg="grey", add=TRUE)
  box(which="plot")
  length <- round(SpatialLinesLengths(costal_europe, longlat=TRUE), digits = 0)
  text(x=-8, y=28, paste0("Grænse = ", length, " km"), pos=4)
  text(x=-8, y=24, paste0("Hegn = ", round(length*4/1000, digits=0), " mia EUR"), pos=4)
  text(x=-8, y=20, paste0("Mur = ", round(length*8.5/1000, digits=0), " mia EUR"), pos=4)
  }

mtext("Hegn langs Europas grænser - længde og pris efter afstand fra Afrika/Tyrkiet", outer = TRUE, cex = 1.4)
