library(dplyr)
library(data.table)
library(stringi)

#Wczytanie danych:
options(stringsAsFactors=FALSE)
PrzejazdyCzerwiec <- read.csv("Dane/DaneRowery/201906-citibike-tripdata.csv")
PrzejazdyLipiec <- read.csv("Dane/DaneRowery/201907-citibike-tripdata.csv")
PrzejazdySierpien <- read.csv("Dane/DaneRowery/201908-citibike-tripdata.csv")
Przejazdy <- rbind(PrzejazdyCzerwiec, PrzejazdyLipiec, PrzejazdySierpien)





#Tworzenie kolumny z wiekiem i pozbywanie się fakeowych kont
age_modification <- function(Przejazdy){
  Przejazdy %>% 
    select(tripduration, usertype,birth.year) %>% 
    mutate(age = 2022 - birth.year) %>% filter(age < 100)
}

PrzejazdyAge <- age_modification(Przejazdy)


agegroups <- function(PrzejazdyAge){
  PrzejazdyAge%>%
    mutate(agegroup = case_when(age <= 21 ~ "teen", age > 21 & age < 66 ~ "adult", age >= 66 ~ "retired"))
}

PrzejazdyAgeGroups <- agegroups(PrzejazdyAge)


#Sumowanie długosci przejazdów dla róznych typów użytkownika
tripsum <- function(PrzejazdyAge){
  SubCount <- dim(PrzejazdyAge %>% filter(usertype == "Subscriber"))[1]      #liczba przejazdow użytkowników z subskrypcją
  CustomerCount <- dim(PrzejazdyAge %>% filter(usertype == "Customer"))[1]   #liczba przejadow zwykłych użytkowników
  PrzejazdyAge %>% 
    group_by(usertype) %>% summarise(tripsum = sum(tripduration/3600)) %>% 
    mutate(meantrip = tripsum) ->SumaPrzejazdow
  SumaPrzejazdow[1,3] <- SumaPrzejazdow[1,2]/(CustomerCount)
  SumaPrzejazdow[2,3] <- SumaPrzejazdow[2,2]/(SubCount)
  SumaPrzejazdow
}
  

SumaPrzejazdow <- tripsum(PrzejazdyAge)

tripsumAge <- function(PrzejazdyAgeGroups){
  SubCountAdult <- dim(PrzejazdyAgeGroups %>% filter(usertype == "Subscriber" & agegroup == "adult"))[1]      
  CustomerCountAdult <- dim(PrzejazdyAgeGroups %>% filter(usertype == "Customer" & agegroup == "adult"))[1]
  SubCountTeen <- dim(PrzejazdyAgeGroups %>% filter(usertype == "Subscriber" & agegroup == "teen"))[1]      
  CustomerCountTeen <- dim(PrzejazdyAgeGroups %>% filter(usertype == "Customer" & agegroup == "teen"))[1] 
  SubCountRetired <- dim(PrzejazdyAgeGroups %>% filter(usertype == "Subscriber" & agegroup == "retired"))[1]      
  CustomerCountRetired <- dim(PrzejazdyAgeGroups %>% filter(usertype == "Customer" & agegroup == "retired"))[1]
  PrzejazdyAgeGroups %>% 
    group_by(usertype,agegroup) %>% summarise(tripsum = sum(tripduration/3600)) %>% 
    mutate(meantrip = tripsum) ->SumaPrzejazdow
  SumaPrzejazdow[SumaPrzejazdow$usertype == "Customer" & SumaPrzejazdow$agegroup == "adult",4] <- SumaPrzejazdow[SumaPrzejazdow$usertype == "Customer" & SumaPrzejazdow$agegroup == "adult",3]/(CustomerCountAdult)
  SumaPrzejazdow[SumaPrzejazdow$usertype == "Customer" & SumaPrzejazdow$agegroup == "teen",4] <- SumaPrzejazdow[SumaPrzejazdow$usertype == "Customer" & SumaPrzejazdow$agegroup == "teen",3]/(CustomerCountTeen)
  SumaPrzejazdow[SumaPrzejazdow$usertype == "Customer" & SumaPrzejazdow$agegroup == "retired",4] <- SumaPrzejazdow[SumaPrzejazdow$usertype == "Customer" & SumaPrzejazdow$agegroup == "retired",3]/(CustomerCountRetired)
  SumaPrzejazdow[SumaPrzejazdow$usertype == "Subscriber" & SumaPrzejazdow$agegroup == "adult",4] <- SumaPrzejazdow[SumaPrzejazdow$usertype == "Subscriber" & SumaPrzejazdow$agegroup == "adult",3]/(SubCountAdult)
  SumaPrzejazdow[SumaPrzejazdow$usertype == "Subscriber" & SumaPrzejazdow$agegroup == "teen",4] <- SumaPrzejazdow[SumaPrzejazdow$usertype == "Subscriber" & SumaPrzejazdow$agegroup == "teen",3]/( SubCountTeen)
  SumaPrzejazdow[SumaPrzejazdow$usertype == "Subscriber" & SumaPrzejazdow$agegroup == "retired",4] <- SumaPrzejazdow[SumaPrzejazdow$usertype == "Subscriber" & SumaPrzejazdow$agegroup == "retired",3]/(SubCountRetired)
  SumaPrzejazdow
}

SumaPrzejazdowWiek <- tripsumAge(PrzejazdyAgeGroups)

head(SumaPrzejazdowWiek)
head(SumaPrzejazdow)


