library(mapdata)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(maps)
library(sf)
library(tools)

world <- ne_countries(scale = "medium", returnclass = "sf")
sites <- data.frame(longitude = airBNB$longitude, 
                    latitude = airBNB$latitude
                    )
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)

head(states)


ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 2) +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 3, 
             shape = 16, fill = "red") +
  coord_sf(xlim = c(-120, -70), ylim = c(25, 50), expand = FALSE)



ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 2) +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 1, 
  ) +
  coord_sf(xlim = c(-117, -119), ylim = c(33.5, 35), expand = FALSE)+
  labs(title = "Sebaran Properti Airbnb pada Negara Bagian California (San Fransisco) ")

usa <- ggplot2::map_data('world2', 'usa')
class(usa)
head(usa)


fff<-ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon()+ 
  coord_cartesian(xlim = c(230, 295), ylim = c(25, 50))
fff


ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
  
geom_point(x=c(241),y=c(41),color='red')+
  geom_point(x=c(242),y=c(42),color='red')

fff

yyy<-  geom_point(x=c(241),y=c(41),color='red')+
  geom_point(x=c(242),y=c(42),color='red')
class(yyy)

ggg<-ggplot(df,aes(x=long,y=lat))+geom_point(color='red')
  geom_point(x=df$long,y=df$lat,color='red')
ggg
+
  geom_point(x=c(241),y=c(41),color='red')
hhh<-fff+ggg


df <- data.frame(long = 360+airBNB$longitude,
                 lat = airBNB$latitude
)
coordinates(df) <- ~ long + lat
class(df)

library(ggfortify)
usa <- ggplot2::map_data('world2', 'usa')
class(usa)
autoplot(usa)
p <- fff+autoplot(usa, geom = 'polygon', fill = 'subregion') + 
  theme(legend.position="none")
p

library(sp)
df <- data.frame(long = 360+airBNB$longitude,
                 lat = airBNB$latitude
                 )
coordinates(df) <- ~ long + lat
class(df)
autoplot(df,usa, colour = 'red', size = 2)


library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)
points(5.7040488, 45.8767041, col = "red", cex = .6)
