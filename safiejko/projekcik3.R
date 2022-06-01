getwd()
setwd("Documents")
setwd("projekt3")
setwd("PDU_Projekt3")
setwd("safiejo")

library("dplyr")
library("shiny")
library(ggplot2)
library("leaflet")
library("leaflet.extras")
library(data.table)

Bikes1 <- read.csv("202001-citibike-tripdata.csv")
Bikes2 <- read.csv("202002-citibike-tripdata.csv")
Bikes3 <- read.csv("202003-citibike-tripdata.csv")
Bikes4 <- read.csv("202004-citibike-tripdata.csv")
Bikes5 <- read.csv("202005-citibike-tripdata.csv")
Bikes6 <- read.csv("202006-citibike-tripdata.csv")
Bikes7 <- read.csv("202007-citibike-tripdata.csv")
Bikes8 <- read.csv("202008-citibike-tripdata.csv")




#WYKRESY
Accidents_bikes%>%
  mutate(CRASH.DATE= substr(CRASH.DATE, 1,7))%>%
  group_by(CRASH.DATE)%>% 
  summarise(Count=n()) -> Accidents_bikes1
colnames(Accidents_bikes1) <- c("Data", "Licznik_wypadków")
head(Accidents_bikes1)



#### wykresik wypadkow
p <- ggplot(Accidents_bikes1, aes(x=Data, y=Licznik_wypadków, group = 1)) +
  geom_line()
p
r <- ggplot(rides_bikes, aes(x=date, y=Count, group = 1)) +
  geom_line()
r

all_bikes %>%
  mutate(date = substr(starttime, 1, 7)) %>%
  group_by(date) %>%
  summarize(Count = n()) -> rides_bikes
head(rides_bikes)


res <- rides_bikes %>%
  inner_join(Accidents_bikes1, by = c("date" = "CRASH.DATE"))

colnames(res)[colnames(res) == "Count.x"] <- "rides"
colnames(res)[colnames(res) == "Count.y"] <- "crashes"
head(res)

p_rides <- ggplot(res, aes(x=date, group = 1)) +
  geom_line(aes(y = rides), color = "purple")
p_crashes <- ggplot(res, aes(x=date, group=1)) + 
  geom_line(aes(y=crashes), color = "black")

#używam logarytmu aby dwa wykresy mogły być na jednej skali
p <- ggplot(res, aes(x=date, group = 1)) +
  geom_line(aes(y = rides), color = "purple")+
  geom_line(aes(y = crashes), color = "black") + 
  scale_y_log10()
p


#MAPA z punktami
#koordynacje odpowiednich ramek
all_bikes <- rbind(Bikes1,Bikes2,Bikes3,Bikes4,Bikes5,Bikes6,Bikes7,Bikes8)
all_bikes_coordinates <- all_bikes[c("start.station.longitude", "start.station.latitude")]
all_bikes_coordinates <- as.data.table(all_bikes_coordinates)
all_bikes_groups <- all_bikes_coordinates[, .(count=.N), by=.(start.station.longitude, start.station.latitude)]

Accidents <- read.csv("NYC_Accidents_2020.csv")
Accidents_bikes <- Accidents[Accidents$NUMBER.OF.CYCLIST.INJURED!=0|Accidents$NUMBER.OF.CYCLIST.KILLED!=0,]
Accidents_coordinates <- Accidents_bikes[c("LONGITUDE","LATITUDE")]
all_bikes_coordinates <- unique(all_bikes_coordinates)
Accidents_map <- leaflet(data= Accidents_coordinates) %>%
  addTiles() %>%
  
  
  addCircleMarkers(lng=all_bikes_coordinates$start.station.longitude, lat=all_bikes_coordinates$start.station.latitude , fillColor = "yellow",
                   fillOpacity = 1,
                   radius=1.5,
                   stroke = F)

setView(Accidents_map,-73.935242, 40.730610, zoom=10)  



#heatmapa
Mapa_wypadki <- leaflet(data=Accidents_coordinates) %>%
  addCircleMarkers(lng=Accidents_coordinates$LONGITUDE, lat=Accidents_coordinates$LATITUDE , fillColor = "black",
                   fillOpacity = 0.5,
                   radius=2,
                   stroke = F) %>%
  
  addTiles() 
setView(Mapa_wypadki,-73.935242, 40.730610, zoom=10) 

Mapa_pusta <- leaflet(data=Accidents_coordinates) %>%
  addTiles() 


Heatmapa_stacje <- addHeatmap(map=Mapa_wypadki, lng = all_bikes_groups$start.station.longitude, lat = all_bikes_groups$start.station.latitude, intensity = all_bikes_groups$count,
           layerId = NULL, group = NULL, minOpacity = 0.05, max = 1,
           radius = 8 ,blur = 15, gradient = NULL, cellSize = NULL,
           data = all_bikes_groups) 
setView(Heatmapa_stacje,-73.935242, 40.730610, zoom=10)  
Accidents_coordinates <- na.omit(Accidents_coordinates) 
Heatmapa_wypadki <- addHeatmap(map=Mapa_pusta, lng = Accidents_coordinates$LONGITUDE, lat = Accidents_coordinates$LATITUDE,
                       intensity = NULL,
                       layerId = NULL, group = NULL, minOpacity = 1, max = 1,
                       radius = 8, blur = 16, gradient = NULL, cellSize = NULL,
                       data = Accidents_coordinates) 
setView(Heatmapa_wypadki,-73.935242, 40.730610, zoom=10)  

