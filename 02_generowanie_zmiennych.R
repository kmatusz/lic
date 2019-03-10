#wczytanie danych----
library(readr)
library(tidyverse)
library(factoextra)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(leaflet)
library(dbscan)

data_zomato<-read.csv('data_zomato_full.csv', stringsAsFactors = F)
data_zomato<-data_zomato%>%filter(if_correct)
coords<-data_zomato%>%select(lat, lng)


#wstępna EDA rodzajów kuchni ----
kuchnie<-data_zomato%>%select(res_id, cuisines)%>%
  tidyr::separate(cuisines, into=paste0('X', 1:10), sep=',')%>%
  gather(starts_with('X'), key='key', value='value')
kuchnie<-kuchnie%>%mutate(value=str_replace(value, ' ', ''))
kuchnie<-kuchnie%>%mutate(value=str_replace(value, '"', ''))
kuchnie<-kuchnie%>%filter(!value%>%is.na())%>%select(-key)


#rodzaje kuchni
#kuchnie%>%group_by(value)%>%summarise(cnt=n())%>%arrange(desc(cnt))%>%View()

#dodanie 1 kuchni do data_zomato

data_zomato%>%select(res_id, cuisines)%>%
  tidyr::separate(cuisines, into=paste0('X', 1:10), sep=',')%>%
  gather(starts_with('X'), key='key', value='kuchnia1')%>%
  filter(key=='X1')%>%select(-key)->kuchnia1


data_zomato%>%select(res_id, cuisines)%>%
  tidyr::separate(cuisines, into=paste0('X', 1:10), sep=',')%>%
  gather(starts_with('X'), key='key', value='kuchnia2')%>%
  filter(key=='X2')%>%select(-key)->kuchnia2


data_zomato%>%select(res_id, cuisines)%>%
  tidyr::separate(cuisines, into=paste0('X', 1:10), sep=',')%>%
  gather(starts_with('X'), key='key', value='value')%>%
  na.omit()%>%
  group_by(res_id)%>%
  tally()->no_cuis

data_zomato%>%left_join(kuchnia1)%>%
  left_join(kuchnia2)%>%
  left_join(no_cuis)->data_zomato
data_zomato<-as.tibble(data_zomato)

#dodanie kuchni z opcją inne- dla <75% kuchni----
kuchnie%>%group_by(value)%>%summarise(cnt=n())%>%arrange(desc(cnt))%>%
  mutate(cum=cumsum(cnt), 
         czy_top=ifelse(cum<sum(cnt)*0.75, 1,0))->a
a%>%filter(czy_top==1)%>%.$value->top_kuchnie
data_zomato%>%mutate(kuchnia_bin=ifelse(kuchnia1 %in% top_kuchnie, kuchnia1, 'inne'))->data_zomato



#zostawienie istotnych zmiennych----
data_zomato%>%select(-X.1, -X, -links, -cuisines, -res_id)->data_zomato
data_zomato%>%select(-adress)->data_zomato





#zamiana koordynatów na projekcję w metrach----
coords_grid<-coords%>% select(2, 1) %>% SpatialPoints()
proj4string(coords_grid)<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")
coords_grid <-spTransform(coords_grid,  CRS("+proj=utm +zone=34 +datum=WGS84"))

 
coords_grid_m<-coords_grid@coords

# coords_grid<-coords_grid_m%>%
#   as.tibble()%>%
#   mutate(
#     lat_utm=lat,
#     lng_utm=lng
#          )
# rm(coords_grid_m)
#coords_grid zawiera koordynaty w metrach, gdzie (0,0) to początek bbox Warszawy
#dane wyglądają w porządku ale warto by sprawdzić

coords_grid_m <- as.tibble(coords_grid_m)

names(coords_grid_m)<-c("lng_utm", "lat_utm")
data_zomato<-data_zomato %>% select(-if_correct, -if_correct_lng, -if_correct_lat, -kuchnia2, -kuchnia_bin)

as.tibble(cbind(data_zomato, coords_grid_m)) %>% write.csv("data_cuisines_meters.csv")

