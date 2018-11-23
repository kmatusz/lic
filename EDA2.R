#wczytanie danych----
library(readr)
library(tidyverse)
library(factoextra)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(leaflet)

data_zomato<-read.csv('full_data.csv', stringsAsFactors = F)

coords<-data_zomato%>%select(lat, lng)


#wstępna EDA rodzajów kuchni ----
kuchnie<-data_zomato%>%select(res_id, cuisines)%>%
  tidyr::separate(cuisines, into=paste0('X', 1:10), sep=',')%>%
  gather(starts_with('X'), key='key', value='value')
kuchnie<-kuchnie%>%mutate(value=str_replace(value, ' ', ''))
kuchnie<-kuchnie%>%mutate(value=str_replace(value, '"', ''))
kuchnie<-kuchnie%>%filter(!value%>%is.na())%>%select(-key)


#rodzaje kuchni
kuchnie%>%group_by(value)%>%summarise(cnt=n())%>%arrange(desc(cnt))%>%View()

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

data_zomato%>%mutate(
                     kuchnia2=str_replace(kuchnia2,'//"', ''))#->data_zomato



#EDA-wykresy rozkłady----
data_zomato%>%summary()


qplot(data_zomato%>%filter(price<quantile(price, 0.98, na.rm = T))%>%select(price), geom = 'density')

qplot(data_zomato%>%select(rating), geom = 'density')
#rating ma rozkład normalny, ma 544 missingów

qplot(data_zomato$votes_cnt%>%log(), geom = 'density')
# rozkład potęgowy? zdecydowanie większość 0, kilka pojawia się 



data_zomato%>%summarise_all(function(x) sum(is.na(x)))
#NA są w ratingu, pojedynczych cenach i kuchnia2- bo tak miało być

#TODO:rozszerzyć
data_zomato%>%group_by(subzone)%>%summarise(n=n(), 
                                            med_price=median(price, na.rm = T),
                                            med_votes_cnt=median(votes_cnt, na.rm = T),
                                            #quantile(votes_cnt, c(0.5,0.75)),
                                            med_rating=median(rating, na.rm = T),
                                            kuchnia_najcz=table(kuchnia1)%>%
                                              as.tibble()%>%arrange(desc(n))%>%
                                              head(1)%>%.[1])


#klastrowanie----

fviz_nbclust(coords, kmeans, method="silhouette", k.max=50) #najlepiej 6 klastrów
fviz_nbclust(coords, cluster::clara, method="silhouette", k.max = 50) #najlepiej 4

#test z 6 klastrami
clust1<-eclust(coords, 'kmeans', k=6, stand = T)
clust1$cluster%>%table() #50% w 1 klastrze 
fviz_silhouette(clust1) #silhouette 0..53

data_zomato$clust<-clust1$cluster



#dla 6 klastrów podobne wyniki dla kmeans i pam
data_zomato%>%group_by(clust)%>%summarise(n=n(), 
                                            med_price=median(price, na.rm = T),
                                            med_votes_cnt=median(votes_cnt, na.rm = T),
                                            #quantile(votes_cnt, c(0.5,0.75)),
                                            med_rating=median(rating, na.rm = T),
                                            kuchnia_najcz=table(kuchnia1)%>%
                                              as.tibble()%>%arrange(desc(n))%>%
                                              head(1)%>%.[1])

#mapa----
pal<-RColorBrewer::brewer.pal(6, 'Set1')
factpal <- colorFactor(pal, data_zomato$clust)
leaflet(data_zomato)%>%
  addTiles()%>%
  addPolygons(data = mapa_test,
              color='white',
              weight = 1,
              fillColor = 'grey',
              fillOpacity = 0.7)%>%
  addCircleMarkers(radius = 1, fillOpacity = 1, fillColor = ~factpal(clust), 
                   color = ~factpal(clust)
  )%>%
  addLegend("bottomright", pal = factpal, values = ~clust, opacity = 1
  )

#test klastrowania na całym zbiorze----
clust1<-eclust(data_zomato%>%select(-name), 'pam', k=6)
clust1$cluster%>%table() #50% w 1 klastrze 
fviz_silhouette(clust1) #silhouette 0..38

data_zomato$clust2<-clust1$cluster

#dla wszystkich danych i Pam wyniki prawie takie same

#zróżnicowanie klastrem- ceny nie ma
#z głosów coś się da wyciągnąć
ggplot(data=data_zomato%>%filter(votes_cnt<quantile(votes_cnt,0.9, na.rm = T)), 
       aes(votes_cnt))+geom_density()+facet_grid(clust~.)


#wykres kuchni- analiza różnicy pomiędzy klastrami----
data_zomato%>%group_by(clust, kuchnia_bin)%>%
  summarise(Freq=n())%>%ungroup()%>%group_by(clust)%>%
  mutate(fr=Freq/sum(Freq))%>%ungroup()->data_bar
ggplot(data_zomato, aes(x=clust, fill=kuchnia_bin))+geom_bar()
ggplot(data_bar, aes(x=clust, y=fr, fill=kuchnia_bin))+geom_col()

#DBSCAN ----
library(dbscan)
#kNNdist(data_zomato%>%select(lat, lng), k=5)
kNNdistplot(coords, k=10 )
#0.015

eps<-0.015 #losowa wartość, dopiero tutaj wychodzi więcej niż 1 klaster
db_res<-dbscan(coords, eps, minPts = 10)
db_res%>%hullplot(data_zomato%>%select(lat, lng), .)
db_res

data_zomato$clust3<-db_res$cluster








