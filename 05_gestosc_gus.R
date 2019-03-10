library(raster)
library(tidyverse)
require(sf)
setwd("C:/rwiz/lic_wne/analiza")

load("warszawa_polygons2.Rdata")
warsaw_map <- mapa_test
rm(mapa_test)

#GUS
main_wd <- getwd()

#wczytanie siatki
setwd("C:/rwiz/lic_wne/gestosc/gus_demo")
pop_grid <- read_sf(dsn = ".")
setwd(main_wd)
#testy
pop_grid %>% sf::st_crs()
#CRS złe

#pop_grid %>% head(10000) %>% select(1) %>% plot()
#jest cała Polska 

#wczytanie danych o warszawie i kropkach

#konwersja danych o Warszawie na metry
proj_string_m <- "+proj=utm +zone=34 +datum=WGS84"

warsaw_map_m <- warsaw_map %>% 
  st_as_sf() %>%
  st_transform(crs=proj_string_m) %>%
  select(-id)

warsaw_map_m %>% st_bbox()
  
# xmin      ymin      xmax      ymax 
# 489864.0 5771924.4  518543.2 5801987.2 
#lng - x- EW
#lat - y- NS

#wczytanie kropek
data_cuisines <- read_csv("data_cuisines_meters.csv")
data_cuisines %>% summary()
data_cuisines <- data_cuisines %>% rename(
  x = lng_utm,
  y = lat_utm
)

data_cuisines_sf <- data_cuisines %>% 
  sf::st_as_sf(coords = c("x", "y")) 

st_crs(data_cuisines_sf) <- proj_string_m

# test 
warsaw_map_m %>% st_crs()
data_cuisines_sf %>% st_crs()
#crs ok

data_cuisines_sf %>% st_bbox()
warsaw_map_m %>% st_bbox()
#bbox ok

plot(st_geometry(warsaw_map_m))
plot(st_geometry(data_cuisines_sf))
# JEST MATCH

# obcięcie populacji do obszaru mapy warszawy

#transformacja 
pop_grid_m <- pop_grid %>% st_transform(crs = proj_string_m)
st_crs(pop_grid_m)
#legit

#obcięcie
pop_grid_m_warsaw <- st_intersection(pop_grid_m, warsaw_map_m)

#test 
plot(st_geometry(warsaw_map_m))
plot(pop_grid_m_warsaw %>% select(TOT), add = TRUE)
plot(data_cuisines_sf %>% st_geometry(), add=TRUE)
#BENGA


#dodać id do każdego polygona i join na wynikach
points <- data_cuisines_sf
grid <- pop_grid_m_warsaw
map_contour <- warsaw_map_m

plot(st_geometry(map_contour))
plot(grid %>% select(TOT), add = TRUE)
plot(points %>% st_geometry(), add=TRUE)

rm(data_cuisines_sf, pop_grid_m_warsaw, warsaw_map_m)

#analiza ----
grid$grid_id <- 1:nrow(grid)

#agregacja
intersection <- st_intersection(y = grid %>% select(grid_id) , x = points)


intersection %>%
  group_by(grid_id) %>%
  count() %>%
  rename(rest_count = nn) %>%
  st_set_geometry(NULL) -> grid_points_counts

grid <- grid %>%
  left_join(grid_points_counts) %>%
  mutate(rest_count = ifelse(is.na(rest_count), 0, rest_count))


grid_data_only <- grid %>% st_set_geometry(NULL)


#####
plot(st_geometry(map_contour)) #kontur
plot(grid %>% select(TOT), add = TRUE) #populacja
plot(grid %>% transmute(if_rest = ifelse(rest_count>0, 1, 0)), add=TRUE) #czy jest restauracja
plot(grid %>% select(rest_count), add=TRUE) #ilość restauracji



pop_rest <- grid_data_only %>% select(TOT, rest_count)


#potrzebne df/ wartości
#kontury Warszawy
#siatka z gęstością zaludnienia
#siatka z liczbą restaruacji w siatce
#dane o restauracjach z id danego polygona do którego należy
save(map_contour,
grid,
points,
file = "C:/rwiz/lic_wne/analiza/05_mapa.Rdata")

