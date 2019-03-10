install.packages("osmdata")
library(osmdata)

#wyciągnięcie pozycji przytanków
bus_stops <- opq(bbox = c(20.85169, 52.09785, 21.27115, 52.36815)) %>% 
  add_osm_feature(key = 'public_transport', value = "stop_position", value_exact = FALSE) %>%
  osmdata_sf() %>%
  .$osm_points %>%
  st_geometry()

#wyciągnięcie dróg
#trunk
#primary
roads_1<- opq(bbox = c(20.85169, 52.09785, 21.27115, 52.36815)) %>% 
  add_osm_feature(key = 'highway', value = "primary", value_exact = FALSE) %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  st_geometry() 

roads_2<- opq(bbox = c(20.85169, 52.09785, 21.27115, 52.36815)) %>% 
  add_osm_feature(key = 'highway', value = "secondary", value_exact = FALSE) %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  st_geometry() 

roads_3<- opq(bbox = c(20.85169, 52.09785, 21.27115, 52.36815)) %>% 
  add_osm_feature(key = 'highway', value = "tertiary", value_exact = FALSE) %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  st_geometry() 

roads_4<- opq(bbox = c(20.85169, 52.09785, 21.27115, 52.36815)) %>% 
  add_osm_feature(key = 'highway', value = "residential", value_exact = FALSE) %>%
  osmdata_sf() %>%
  .$osm_lines %>%
  st_geometry() 

roads <-c(roads_1, roads_2, roads_3, roads_4)

plot(roads)
degAxis(1)
# zamiana projekcji
proj_string_m <- "+proj=utm +zone=34 +datum=WGS84"

bus_stops_m <- bus_stops %>%
  st_transform(crs=proj_string_m)  

roads_m <- roads %>% 
  st_transform(crs=proj_string_m)









