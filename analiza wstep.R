##### Analizy zescrapowanych danych - narazie mokotów, ochota i śródmieście

setwd("C:/rwiz/lic/zomato-http")
library(readr)
library(tidyverse)
library(factoextra)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
#dane administracyjne Warsawy (do obczajenia)
bound<-readShapePoly(file.choose())
bound$jpt_nazwa_->a
a<-as.character(levels(a))[a]
data.frame(a)%>%arrange(a)%>%View()
plot(bound[,5])


simp<-gSimplify(bound, 1, topologyPreserve = T)
bound_simp<-SpatialPolygonsDataFrame(simp, bound@data)
bound_simp@data%>%View()
bound_simp@data<-bound_simp@data%>%select(1:6)

gminy<-bound_simp@data$jpt_nazwa_
gminy<-levels(gminy)[gminy]
gminy%>%as.tibble()%>%filter(value=='')

a<-bound_simp[bound_simp@data$jpt_nazwa_
              %in%c('OCHOTA','MOKOTÓW', 'ŚRÓDMIEŚCIE',
                    'URSYNÓW', 'WILANÓW',
                    'WŁOCHY', 'Bielany', 'Żoliborz',
                    'Bemowo', 'WOLA', 
                    'URSUS', 'Białołęka',
                    'TARGÓWEK', 'Praga-Północ',
                    'PRAGA POŁUDNIE', 'REMBERTÓW',
                    'WAWER', 'WESOŁA'
                    ),]
plot(a)
a@data<-a@data%>%
  select(iip_identy, jpt_nazwa_)%>%
mutate_if(is.factor, function(x) levels(x)[x])%>%
  rename(subzone=jpt_nazwa_, id=iip_identy)
  
warszawa_polygons<-a
save(warszawa_polygons, file='warszawa_polygons.Rdata')


bound[,bound@data$jpt_nazwa_=='Lubień']%>%plot()
spTransform(bound, '+proj=WGS84')
#wczytanie danych

data_zomato <- rbind(read_csv("ver1_z_coords.csv"),
                     read_csv("ver2_z_coords.csv"),
                     read_csv("ver3_z_coords.csv"),
                     read_csv("ver4_z_coords.csv"))
data_zomato<-data_zomato%>%filter(if_correct)%>%
  select(-if_correct_lng, -if_correct_lat, -if_correct)

#wstępna EDA rodzajów kuchni ----
kuchnie<-data_zomato%>%select(res_id, cuisines)%>%
  tidyr::separate(cuisines, into=paste0('X', 1:10), sep=',')%>%
  gather(starts_with('X'), key='key', value='value')
kuchnie<-kuchnie%>%mutate(value=str_replace(value, ' ', ''))
kuchnie<-kuchnie%>%mutate(value=str_replace(value, '"', ''))

kuchnie<-kuchnie%>%filter(!value%>%is.na())%>%select(-key)
#po ile typów kuchni w restauracjach
kuchnie%>%group_by(res_id)%>%summarise(cnt=n())%>%group_by(cnt)%>%
  summarise(n())
#po ile typów kuchni w 1 restauracji
kuchnie%>%group_by(res_id)%>%summarise(cnt=n())

#rodzaje kuchni
kuchnie%>%group_by(value)%>%summarise(cnt=n())%>%arrange(desc(cnt))%>%
  View()

#mapa
leaflet(data_zomato)%>%
  addTiles()%>%
  addCircleMarkers()

#filtrowanie outlierów (narazie robocze z bounding box), wypada ok. 40
data_zomato%>%filter(between(lat, 52.1, 52.25))%>%
  filter(between(lng, 20.9, 21.05))->data_zomato

#usunięcie Nanów
data_zomato_model<-data_zomato%>%select(price, rating, votes_cnt, lat, lng )
data_zomato_model<-na.omit(data_zomato_model)

c2<-eclust(data_zomato_model[4:5], k=3, FUNcluster="kmeans") 

#plot na mapie 
data_kmeans<-data_zomato_model%>%mutate(cl=c2$cluster)
ggplot(data_kmeans, aes(lng,lat, color=as.factor(cl)))+geom_point()

mapa_test<-spTransform(warszawa_polygons, CRS('+proj=longlat +datum=WGS84'))
fviz_cluster(c2)
proj4string(warszawa_polygons) <- CRS("+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +units=m +no_defs")
degAxis(1)
save(mapa_test, file='warszawa_polygons2.Rdata')

library(tmap)
tm_shape(mapa_test)+
  tm_polygons()+
  tm_shape(pts)+
  tm_dots('rating')
#zamienione osie

pts<-coords%>%SpatialPointsDataFrame(data=data_zomato, coords = )
proj4string(pts)<-CRS('+proj=longlat +datum=WGS84')

plot(warszawa_polygons)
#mapa
factpal <- colorFactor(topo.colors(10), data_kmeans$cl)
leaflet(data_zomato)%>%
  addTiles()%>%
  addCircleMarkers()

#dodanie kolumny odległość od centrum
centrum_coords<-c(52.229969, 21.011446)

data_zomato<-data_zomato%>%mutate(odl_centrum=sqrt((centrum_coords[1]-lat)^2+
                                        (centrum_coords[2]-lng)^2))
data_zomato%>%ggplot(aes(y=price, x=rating))+geom_jitter()+
  geom_smooth()

cor(na.omit(data_zomato)%>%
      select(price, rating, votes_cnt, lat, lng,odl_centrum)) %>%corrplot::corrplot()                    
                   

