library(readr)
library(tidyverse)
library(factoextra)
library(sf)
library(dbscan)


load("C:/rwiz/lic_wne/analiza/05_mapa.Rdata")

#do points dodać do której kratki należy
points <- st_intersection(x = grid %>% select(grid_id), y = points)

#klastrowanie
#robimy dbscan gdzie epsilon to teoretyczny promień dostępu dla chodzenia dla człowieka (5km/h= 0.8km/10 min)
#poprawka na przeszkody po drodze więc 700 m

points_coords <- points %>% st_coordinates()

eps<-500
db_res_walk<-dbscan(points_coords, eps, minPts = 5)

#do points dodać do którego klastra należy (jak noise to NA)
points$cluster <- db_res_walk %>% .$cluster %>% ifelse(.==0 , NA, .) 

#df grid_id, czy ma klaster
points %>% select(grid_id, cluster) %>% st_set_geometry(NULL) %>%
  mutate(if_cluster=ifelse(!is.na(cluster), 1, 0)) %>%
  group_by(grid_id) %>%
  summarise(cluster = ifelse(sum(if_cluster)>0, 1, NA)) -> grids_clusters

grid %>% 
  left_join(grids_clusters) %>%
  mutate(cluster = ifelse(is.na(cluster), 0, cluster)) -> grid

#do grid dodać info o ilości przystanków


#ilu mieszkańców ma w obrębie mieszkania klaster?


