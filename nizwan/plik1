r201503 <- read.csv("201503-citibike-tripdata.csv")
r201504 <- read.csv("201504-citibike-tripdata.csv")
r201505 <- read.csv("201505-citibike-tripdata.csv")
r201506 <- read.csv("201506-citibike-tripdata.csv")
r201507 <- read.csv("201507-citibike-tripdata.csv")
r201508 <- read.csv("201508-citibike-tripdata.csv")
r201509 <- read.csv("201509-citibike-tripdata.csv")
r201510 <- read.csv("201510-citibike-tripdata.csv")
r201511 <- read.csv("201511-citibike-tripdata.csv")
r201512 <- read.csv("201512-citibike-tripdata.csv")
r201601 <- read.csv("201601-citibike-tripdata.csv")
r201602 <- read.csv("201602-citibike-tripdata.csv")
r201603 <- read.csv("201603-citibike-tripdata.csv")
r201604 <- read.csv("201604-citibike-tripdata.csv")
r201605 <- read.csv("201605-citibike-tripdata.csv")
r201606 <- read.csv("201606-citibike-tripdata.csv")
r201607 <- read.csv("201607-citibike-tripdata.csv")
r201608 <- read.csv("201608-citibike-tripdata.csv")
r201609 <- read.csv("201609-citibike-tripdata.csv")
londyn <- read.csv("london_merged.csv")

# przygotowanie danych nowy york

daty <- r201606[c("starttime","bikeid")]

temp <- strptime(daty[,"starttime"],format = "%m/%d/%Y %H:%M:%OS")

df <- data.frame(
  dzien = strftime(temp, format = "%A"),
  godzina = strftime(temp, format = "%H:%M")
)

godziny_w_weekend <- df[df$dzien == "Saturday" | df$dzien == "Sunday",]
godziny_w_weekend <- aggregate(godziny_w_weekend$dzien, by = godziny_w_weekend["godzina"], FUN = length)
colnames(godziny_w_weekend)[2] <- "wypożyczenia_w_weekend"
godziny_w_weekend["wypożyczenia_w_weekend"] <- godziny_w_weekend["wypożyczenia_w_weekend"] / 2
godziny_w_weekend

godziny_w_tygodniu <- df[df$dzien != "Saturday" & df$dzien != "Sunday",]
godziny_w_tygodniu <- aggregate(godziny_w_tygodniu$dzien, by = godziny_w_tygodniu["godzina"], FUN = length)
colnames(godziny_w_tygodniu)[2] <- "wypożyczenia_w_tygodniu"
godziny_w_tygodniu["wypożyczenia_w_tygodniu"] <- godziny_w_tygodniu["wypożyczenia_w_tygodniu"] / 5
godziny_w_tygodniu

koncowa <- merge(x = godziny_w_weekend, y = godziny_w_tygodniu, by = "godzina", all.x = TRUE, all.y = TRUE)

date <- "2014-01-01"
koncowa$godzina <- paste(date, koncowa$godzina, sep = " ")
koncowa$godzina <- as.POSIXct(koncowa[,"godzina"],format = "%Y-%m-%d %H:%M")


pom <- koncowa$wypożyczenia_w_tygodniu[seq(1, length(koncowa$wypożyczenia_w_tygodniu), 30)]
koncowa$wypożyczenia_w_tygodniu <- stats::spline(x = 1:length(pom), pom,n = length(koncowa$wypożyczenia_w_tygodniu))[2]$y



# przygotowanie danych londyn 

library(stringi)
londyn <- londyn[substr(londyn$timestamp,1,7) == "2016-06",]
londyn$timestamp <- substr(londyn$timestamp,12,16)
londyn_wolne <- londyn[londyn$is_weekend == 1 | londyn$is_holiday == 1,]
londyn_zwykle <- londyn[londyn$is_weekend != 1 & londyn$is_holiday != 1,]

londyn_wolne <- aggregate(londyn_wolne$cnt, by = londyn_wolne["timestamp"], FUN = sum)
londyn_zwykle <- aggregate(londyn_zwykle$cnt, by = londyn_zwykle["timestamp"], FUN = sum)

londyn_free <- stats::spline(x = 1:24 ,londyn_wolne$x, n = length(koncowa$wypożyczenia_w_weekend))
londyn_dzien <- stats::spline(x = 1:24 ,londyn_zwykle$x, n = length(koncowa$wypożyczenia_w_weekend))

londyn_free[2]$y <- londyn_free[2]$y / (2 * 60)
londyn_dzien[2]$y <- londyn_dzien[2]$y / (5 * 60)



install.packages("ggplot2")                             
library("ggplot2")      
library(scales)

# wykres o ktorej godzinie jezdzi sie w weekendy i dni

ggplot(koncowa, aes(x = godzina)) +           
  geom_smooth(aes(y = wypożyczenia_w_weekend, color = "#74cc66", ),size=1.5) + 
  geom_line(aes(y = wypożyczenia_w_tygodniu, color = "#296914"),size=1.5) + 
  geom_line(aes(y = londyn_dzien[2]$y, color = "cornflowerblue"),size=1.5)+
  geom_line(aes(y = londyn_free[2]$y, color = "Yellow"),size=1.5)+
  scale_x_datetime(breaks = breaks_width("2 hour"),labels=date_format("%H:%M"))+ 
  ggtitle("Porównanie wypożyczania rowerów w Nowym Yorku i Londynie") +
  xlab("Godzina wypożyczenia") + 
  ylab("Ilość rowerów wypożyczonych w danej minucie")+
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  scale_color_manual(name="Kiedy i gdzie \nodbył się przejazd", 
                     values=c("#74cc66", "#246b2c", "#4d6cf7","#1622c9"),
                     labels = c("zwykły dzień New York","weekend New York",
                                "zwykły dzień Londyn","weekend Londyn"))



# przygotowanie danych do 2 wykresu

polaczone <- rbind(
  r201503,r201504,r201505,r201506,r201507,r201508,
  r201509,r201510,r201511,r201512,r201601,r201602,
  r201603,r201604,r201605,r201606,r201607,r201608,
  r201609)


polaczone$starttime <- strptime(polaczone[,"starttime"],format = "%m/%d/%Y %H:%M:%OS")

polaczone$rok <- strftime(polaczone$starttime, format = "%Y")
polaczone$mies <- strftime(polaczone$starttime, format = "%m")
polaczone$dzien <- strftime(polaczone$starttime, format = "%d")

pomocnicza <- function(x){
  c(length(unique(x)),length(x))
}

tescik2 <- aggregate(x = polaczone$bikeid, by = polaczone[c("rok","mies","dzien")], FUN = pomocnicza)
tescik2 <- tescik2[order(tescik2$rok,tescik2$mies,tescik2$dzien),]



tescik2$data <- paste(tescik2$rok, tescik2$mies,tescik2$dzien, sep = "-")
tescik2$data <- as.POSIXct(tescik2[,"data"],format = "%Y-%m-%d")

tescik2$x[,1] <- tescik2$x[,1] / max(tescik2$x[,1])
tescik2$x[,2] <- tescik2$x[,2] / max(tescik2$x[,2])

tescik2$x[,1] <- frollmean(tescik2$x[,1],7)
tescik2$x[,2] <- frollmean(tescik2$x[,2],7)


#ilosc rowerow a jazd

ggplot(tescik2, aes(x = data)) +          
  geom_line(aes(y = x[,2], color = "darkred")) + 
  geom_line(aes(y = x[,1], color="steelblue")) +
  geom_smooth(aes(y = x[,2], color = "darkred")) + 
  geom_smooth(aes(y = x[,1], color="steelblue")) +
  scale_x_datetime(breaks = breaks_width("1 month"),labels=date_format("%Y-%m"))+ 
  ggtitle("Relacja pomiędzy ilością rowerów a ilością przejazdów") +
  xlab("Data") + 
  ylab("Ilośc przejazdów i rowerów w danym dniu unormowane do przedziału [0,1]")+
  scale_colour_discrete(name="",
                        labels = c("Unormowana ilośc \nrowerów w obiegu","Unormowana ilośc \nprzejzdów"))+
  theme(
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )



