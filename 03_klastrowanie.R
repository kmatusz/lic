library(readr)
library(tidyverse)
library(factoextra)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(leaflet)
library(dbscan)

data <- read.csv("data_cuisines_meters.csv", stringsAsFactors = FALSE)

#coords_grid zawiera koordynaty w metrach, gdzie (0,0) to początek bbox Warszawy
#dane wyglądają w porządku ale warto by sprawdzić

#robimy dbscan gdzie epsilon to teoretyczny promień dostępu dla chodzenia dla człowieka (5km/h= 0.8km/10 min)
#poprawka na przeszkody po drodze więc 700 m

eps<-700
db_res_walk<-dbscan(data[c("lat_m", "lng_m")], eps, minPts = 4)
db_res_walk

#sprawdzamy zróżnicowanie w klastrze
data_clustered<-cbind(data,db_res_walk$cluster)
names(data_clustered)[13]<-'cluster'


#zróżnicowanie w klastrze
# 1. Klastrowanie z 3 punktami w radiusie 400 m
# Najpiew miary bez skalowania ze względu na ilość restauracji w klastrze
# -ilość restauracji w klastrze (ok)
# -ilość serwowanych kuchni w danym klastrze (ok)
# -czy kuchnie są rozłożone równie różnorodnie jak w całym datasecie? (czy np są obszary z tylko kuchnią włoską) (TODO)
# -średnia cena w całym datasecie i w poszczególnych klastrach, odchylenie (ok)
# -IQR, min i max z ceny w klastrze w porównaniu do całego datasetu (ok)
# Miary ze skalowaniem:
# -% restauracji które mają unikatową kuchnię (TODO)

#kuchnie które mają sumarycznie mniej niż 25% udziału w liczbie wszystkich kuchni
cuisine_rare <- data$kuchnia1 %>% 
  table() %>%
  as.tibble() %>%
  arrange(-n) %>% 
  mutate(freq = n / sum(n),
         cum_freq = cumsum(freq)) %>%
  filter(cum_freq < 0.75) %>%
  .$`.`




stats_clust<-data_clustered%>%group_by(cluster)%>%summarise(rest_cnt=n(), #ilość restauracji w klastrze
                                                  cuisine_cnt=table(kuchnia1)%>%nrow(), #ilość kuchni w klastrze
                                                  if_rare=table(kuchnia1)%>%as.tibble()%>%select(1), #czy w klastrze jest 1 z rzadkich kuchni?
                                                  price_avg= mean(price, na.rm=TRUE), #średnia cena w klastrze
                                                  price_sd= sd(price, na.rm=TRUE), #sdev ceny w klastrze
                                                  price_iqr= IQR(price, na.rm=TRUE), #iqr ceny w klastrze
                                                  price_min=min(price, na.rm=TRUE), #min ceny w klastrze
                                                  price_max=max(price, na.rm=TRUE) #max ceny w klastrze
)

#TODO: dodać zmienne z częstotliwością danej kuchni w klastrze 




