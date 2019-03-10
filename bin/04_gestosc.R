library(raster)
library(tidyverse)
require(sf)

setwd("C:/rwiz/lic_wne/gestosc/Urban_Clusters")

load("C:/rwiz/lic_wne/analiza/warszawa_polygons2.Rdata")




#nowa wersja z kartografii ekstremalnej

setwd("C:/rwiz/lic_wne/gestosc/kartografia_nowe/Version 2_0_1/GEOSTATReferenceGrid")


density_values <- read_csv("GEOSTAT_grid_POP_1K_2011_V2_0_1.csv")


reference_grid <- read_sf(dsn = ".")
reference_grid_small <- head(reference_grid, 10000)
reference_grid_small$geometry[2]

reference_grid_small %>%
        st_intersection(warsaw_polygons_proj)

warsaw_polygons_proj<-spTransform(mapa_test, CRSobj = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))

cropped <- st_crop(reference_grid, c(xmin=5059482, xmax=5088592, ymin=3278201,  ymax=3308157))

cropped_density <- cropped %>% left_join(y = density_values)

st_as_sf(warsaw_polygons_proj) ->warsaw_polygons_proj_sf

warsaw_bounds_density<-st_intersection(cropped_density, warsaw_polygons_proj_sf)
#są wszystkie dane

warsaw_bounds_density <- warsaw_bounds_density %>% 
        #select(geometry, TOT_P) %>% 
        st_transform("+proj=utm +zone=34 +datum=WGS84") %>%
        select(-GRD_ID, -CNTR_CODE, -METHD_CL, -YEAR, -DATA_SRC, -TOT_P_CON_DT, -id)


#dodanie punktów z restauracjami do danych

data_cuisines_meters <- read_csv("C:/rwiz/lic_wne/analiza/data_cuisines_meters.csv")

#pozostawienie tylko punktów
cuisines_points <- data_cuisines_meters %>%
        select(lat_utm, lng_utm) %>%
        SpatialPoints(proj4string = CRS("+proj=utm +zone=34 +datum=WGS84"))

cuisines_points %>%
        sf::st_as_sf() -> cuisines_points_sf

cuisines_points_sf %>%mutate(id=row_number()) %>%
        left_join(data_cuisines_meters %>% mutate(id=row_number())) ->cuisines_points_sf


intersection <- st_intersection(x = warsaw_bounds_density, y = cuisines_points_sf)

# Plot intersection
plot(polygon, graticule = st_crs(4326), key.pos = 1)
plot(intersection[1], col = "black", pch = 19, add = TRUE)

plot(warsaw_bounds_density %>% select(geometry, TOT_P))
plot(cuisines_points_sf %>%select(votes_cnt, geometry), col="black")
degAxis(2)

plot(warsaw_bounds_density%>% select(geometry, TOT_P), graticule = st_crs(4326), key.pos = 1)
plot(cuisines_points_sf%>%select(votes_cnt, geometry), pch = 19, col = "black", add = TRUE)







