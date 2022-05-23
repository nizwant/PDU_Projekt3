getwd()
library("dplyr")
library(ggplot2)
i(reshape2)
libra
Bikes1 <- read.csv("JC-202001-citibike-tripdata.csv")
Bikes2 <- read.csv("JC-202002-citibike-tripdata.csv")
Bikes3 <- read.csv("JC-202003-citibike-tripdata.csv")
Bikes4 <- read.csv("JC-202004-citibike-tripdata.csv")
Bikes5 <- read.csv("JC-202005-citibike-tripdata.csv")
Bikes6 <- read.csv("JC-202006-citibike-tripdata.csv")
Bikes7 <- read.csv("JC-202007-citibike-tripdata.csv")
Bikes8 <- read.csv("JC-202008-citibike-tripdata.csv")

all_bikes <- rbind(Bikes1,Bikes2,Bikes3,Bikes4,Bikes5,Bikes6,Bikes7,Bikes8)
Accidents <- read.csv("NYC_Accidents_2020.csv")
Accidents_bikes <- Accidents[Accidents$NUMBER.OF.CYCLIST.INJURED!=0|Accidents$NUMBER.OF.CYCLIST.KILLED!=0,]

Accidents_bikes%>%
  mutate(CRASH.DATE= substr(CRASH.DATE, 1,7))%>%
  group_by(CRASH.DATE)%>% 
  summarise(Count=n()) -> Accidents_bikes1
head(Accidents_bikes1)
?plot
plot(Accidents_bikes1$CRASH.DATE, Accidents_bikes1$Count)


#### wykresik wypadkow
p <- ggplot(Accidents_bikes1, aes(x=CRASH.DATE, y=Count, group = 1)) +
  geom_line()
p

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
p_crashes <- gglot(res, aes(x=date, group=1)) + 
  geom_line(aes(y=crashes), color = "black")


p <- ggplot(res, aes(x=date, group = 1)) +
  geom_line(aes(y = rides), color = "purple")+
  geom_line(aes(y = crashes), color = "black") + 
  scale_y_log10()
p
