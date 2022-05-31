#³adujemy potrzebne biblioteki

library(dplyr)
library(data.table)
library(stringi)
library("RColorBrewer")
library(wesanderson)
library(stringi)


#Wczytanie danych:
options(stringsAsFactors=FALSE)
PrzejazdyCzerwiec <- read.csv("Dane/DaneRowery/201906-citibike-tripdata.csv")
PrzejazdyLipiec <- read.csv("Dane/DaneRowery/201907-citibike-tripdata.csv")
PrzejazdySierpien <- read.csv("Dane/DaneRowery/201908-citibike-tripdata.csv")
Przejazdy <- rbind(PrzejazdyCzerwiec, PrzejazdyLipiec, PrzejazdySierpien)



#Tworzenie kolumny z wiekiem i pozbywanie sie fakeowych kont (takich w których wiek jest za duzy)

age_modification <- function(Przejazdy){
  Przejazdy %>% 
    select(tripduration, usertype,birth.year) %>% 
    mutate(age = 2022 - birth.year) %>% filter(age < 100)
}

PrzejazdyAge <- age_modification(Przejazdy)



#tworzymy kolumne agegrup przyjmujaca wartosci adult teen retired

agegroups <- function(PrzejazdyAge){
  PrzejazdyAge%>%
    mutate(agegroup = case_when(age <= 21 ~ "teen", age > 21 & age < 66 ~ "adult", age >= 66 ~ "retired"))
}

PrzejazdyAgeGroups <- agegroups(PrzejazdyAge)


#Sumowanie czasu dlugosci przejazdow oraz wyliczanie sredniej dlugosci czasu przejazdu dla roznych typow uzytkownikow (wyrazona w godzinach)

tripsum <- function(PrzejazdyAge){
  SubCount <- nrow(PrzejazdyAge %>% filter(usertype == "Subscriber"))      #liczba przejazdow uzytkownikow z subskrypcja
  CustomerCount <- nrow(PrzejazdyAge %>% filter(usertype == "Customer"))   #liczba przejazdow zwyklych uzytkownikow 
  PrzejazdyAge %>% 
    group_by(usertype) %>% summarise(tripsum = sum(tripduration/3600)) %>% 
    mutate(meantrip = tripsum) ->SumaPrzejazdow
  SumaPrzejazdow[1,3] <- SumaPrzejazdow[1,2]/(CustomerCount)  #liczenie sredniej
  SumaPrzejazdow[2,3] <- SumaPrzejazdow[2,2]/(SubCount)       #liczenie sredniej
  SumaPrzejazdow
}
  
SumaPrzejazdow <- tripsum(PrzejazdyAge)

#sumowanie dlugosci przejazdow dla roznych grup wiekowych oraz wyliczenie sredniej dla tych grup (w minutach)

tripsumAge <- function(PrzejazdyAgeGroups){
  SubCountAdult <- nrow(PrzejazdyAgeGroups %>% filter(usertype == "Subscriber" & agegroup == "adult"))   
  CustomerCountAdult <- nrow(PrzejazdyAgeGroups %>% filter(usertype == "Customer" & agegroup == "adult"))
  SubCountTeen <- nrow(PrzejazdyAgeGroups %>% filter(usertype == "Subscriber" & agegroup == "teen"))   
  CustomerCountTeen <- nrow(PrzejazdyAgeGroups %>% filter(usertype == "Customer" & agegroup == "teen"))
  SubCountRetired <- nrow(PrzejazdyAgeGroups %>% filter(usertype == "Subscriber" & agegroup == "retired"))     
  CustomerCountRetired <- nrow(PrzejazdyAgeGroups %>% filter(usertype == "Customer" & agegroup == "retired"))
  PrzejazdyAgeGroups %>% 
    group_by(usertype,agegroup) %>% summarise(tripsum = sum(tripduration/60)) %>% 
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

#wykres kolowy procentowy udzial liczby przejazdow wykonanych przez subscriberow i customerow 

kolowy_ilosc_sub_cus <- function(PrzejazdyAge){
  
  elo <- aggregate(PrzejazdyAge, by = PrzejazdyAge["usertype"], length)
  slices <- elo[,3]
  lbls <- c("Customer", "Subscriber")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) 
  lbls <- paste(lbls,"%",sep="") 
  pie(slices,labels = lbls, col= c("skyblue", "royalblue"), cex.main = 2.5,
      main="Rozk³ad u¿ytkowników")
}

kolowy_ilosc_sub_cus(PrzejazdyAge)


#wykres kolowy sumy (wyrazonej w procentach)  czasow przejazdow  subscriberow i customerow 

kolowy_tripsum <- function(SumaPrzejazdow){
  slices <- unlist(SumaPrzejazdow[,2])
  lbls <- c("Customer", "Subscriber")
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) 
  lbls <- paste(lbls,"%",sep="") 
  pie(slices,labels = lbls, col= c("skyblue", "royalblue"), cex.main = 2.5,
      main="Suma dlugosci przejazdow")
  
}

kolowy_tripsum(SumaPrzejazdow)

#wykres sredniej dlugosci przejazdow w minutach  subow i customerow 

wykresSubvsCustomer <-function(SumaPrzejazdow){
  SumaPrzejazdow <- SumaPrzejazdow %>% mutate(meantrip = meantrip*60)
  wykres <- transpose(SumaPrzejazdow)
  wykres <- wykres[3,]
  colnames(wykres) <- c("Customer", "Subscriber")
  barplot(as.matrix(wykres), col=c("skyblue", "royalblue"), cex.axis=2.5)
  title('Œrednia d³ugosc przejazdu w minutach')
}

wykresSubvsCustomer(SumaPrzejazdow)


# Zestawieni najwiekszej  liczby wypozyczen danego dnia z dniem rowerowym w lato 2019 

PrzejazdyDanegDnia <- function(Przejazdy){
  
  daty <- Przejazdy %>% 
    mutate(Data = stri_sub(starttime, from = 1L, to = 10L)) %>% count(Data)       #zliczanie ilosci przejazdow danego dnia 
  colnames(daty) <- c("Data","Ilosc_przejazdow")
  daty <- daty[order(daty$Ilosc_przejazdow, decreasing = TRUE),]
  datadzien <- daty[daty$Data == "2019-06-03",]                                   #wiersz dnia rowerowego
  datadzien <- rbind(daty[c(1:5),],datadzien)
  datadzien <- transpose(datadzien)
  colnames(datadzien) <- stri_sub(datadzien[1,], from = 6L, to = 10L)
  barplot(as.matrix(datadzien[2,]),col=c("skyblue", "royalblue"), xlab = "06-03 to dzieñ rowerowy", cex.lab=1.5, cex.axis=2)
  title('Najwiêcej wypo¿yczeñ danego dnia w lato 2019')
}

PrzejazdyDanegDnia(Przejazdy)

#srednia dlugosc przejazdow grup wiekowych z podzialem na tym uzytkownika

wykressredniprzejazd <- function(SumaPrzejazdowWiek){
  select(ungroup(SumaPrzejazdowWiek),usertype,agegroup, meantrip) %>% arrange(agegroup) %>% select(usertype,meantrip) %>% 
    mutate(adult = meantrip) %>% mutate(teen = meantrip) %>% mutate(retired = meantrip) -> wykres
  wykres[1,4] <- wykres[3,2]
  wykres[2,4] <- wykres[4,2]
  wykres[1,5] <- wykres[5,2]
  wykres[2,5] <- wykres[6,2]
  wykres <- wykres[c(1:2),-c(1,2)]
  row.names(wykres) <- c("Customer","Subscriber")
  barplot(as.matrix(wykres), beside = T, col=c("skyblue", "royalblue"), xlab = "Grupy wiekowe", ylab = "Czas w minutach", cex.lab=1.5, cex.axis=2)
  title('Sredni czas dlugosci przejazdu')
  legend("topright", row.names(wykres), fill=c("skyblue", "royalblue"), title='Klient', cex = 1)
}

wykressredniprzejazd(SumaPrzejazdowWiek)


#Œredia ilosc przejazdow jednego dnia

wyliczanie_sredniej <- function(Przejazdy){
  
  Przejazdy %>% 
    mutate(Data = stri_sub(starttime, from = 1L, to = 10L)) %>% 
    count(Data) %>% summarise_if(is.numeric, mean)
  
}

srednia_ilosc_przejazdow <- wyliczanie_sredniej(Przejazdy)


