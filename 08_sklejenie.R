library(readr)
library(tidyverse)
library(factoextra)
library(sf)
library(dbscan)

load("C:/rwiz/lic_wne/analiza/05_mapa.Rdata")
load("C:/rwiz/lic_wne/analiza/07_bus_stops_roads.Rdata")

biznesy <- read_csv("biznesy_geocode.csv")

# biznesy----
#filtrowanie
biznesy <- biznesy %>% 
  select(lat, lng, `National ID`) %>%
  rename(business_id = `National ID`) %>%
  filter(!is.na(lat)) %>%
  filter(lat>51, lng>20, lat<53, lng<21.4) 

#zmiana na sf
biznesy<- biznesy %>% sf::st_as_sf(coords = c("lng", "lat")) 

#projekcja
sf::st_crs(biznesy) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "

#zmiana projekcji
proj_string_m <- "+proj=utm +zone=34 +datum=WGS84"
biznesy <- biznesy %>%
  st_transform(crs=proj_string_m)  

#biznesy jako kropki w dobrym crs






# binowanie do grida ----
grid <- grid %>% select(1, grid_id, rest_count)

#biznesy
biznesy_intersection <- st_intersection(y = grid %>% select(grid_id) , x = biznesy)


biznesy_intersection %>%
  group_by(grid_id) %>%
  count() %>%
  rename(biznes_count = n) %>%
  st_set_geometry(NULL) -> grid_biznes_counts

grid <- grid %>%
  left_join(grid_biznes_counts) %>%
  mutate(biznes_count = ifelse(is.na(biznes_count), 0, biznes_count))


plot(map_contour %>% st_geometry())
plot(biznesy %>% st_geometry(), add=TRUE)

#ok

#przystanki
stops_intersection <- st_intersection(y = grid %>% select(grid_id) , x = data.frame(bus_stops)%>% st_as_sf())


stops_intersection %>%
  group_by(grid_id) %>%
  count() %>%
  rename(bus_count = n) %>%
  st_set_geometry(NULL) -> grid_stops_counts

grid <- grid %>%
  left_join(grid_stops_counts) %>%
  mutate(bus_count = ifelse(is.na(bus_count), 0, bus_count))


plot(map_contour %>% st_geometry())
plot(bus_stops %>% st_geometry(), add=TRUE)

grid %>% select(bus_count) %>% plot()

#ok

#drogi
roads<- roads %>% data.frame() %>% st_as_sf()

# intersection

int = st_intersection(roads, grid)
# find out about the length of each line segment
int$len = st_length(int)

# spatial overlay
joined = st_join(grid, int)
# use the ID of the polygon for the aggregation
out = group_by(joined, grid_id.x) %>%
  summarize(roads = sum(len))

out <- mutate(out, roads = ifelse(is.na(roads), 0, roads))

grid <- grid %>% left_join(out%>% st_set_geometry(NULL), by= c("grid_id"= "grid_id.x"))

grid %>% select(roads) %>% plot()

grid <- grid %>% mutate(if_rest=ifelse(rest_count>0, 1, 0))
save(grid, file= "08_full_grid.Rdata")






