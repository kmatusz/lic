library(rpart)
library(readr)
library(tidyverse)
library(factoextra)
library(sf)
library(dbscan)
library(caret)
library(spdep)

load("C:/rwiz/lic_wne/analiza/05_mapa.Rdata")
load("C:/rwiz/lic_wne/analiza/08_full_grid.Rdata")
load("C:/rwiz/lic_wne/analiza/08_biznesy_points.Rdata")
ggplot2::theme_set(theme_bw())
options(scipen = 999)

grid %>% 
  st_set_geometry(NULL) -> non_spatial

columns_mapping <- 
  tribble(
    ~old, ~new,
    "TOT", "Population count",
    "rest_count", "Restaurants count",
    "biznes_count", "Business count",
    "bus_count", "Bus stops count",
    "roads", "Total roads length",
    "if_rest", "Restaurant indicator"
  )



#' ###wykresy pudełkowe restaruacje i TOT/biznes


box_populacja <- non_spatial %>% 
  select(-grid_id) %>%
  mutate(if_rest = ifelse(if_rest, "Yes", "No")) %>%
  ggplot(aes(x = as.factor(if_rest), y = TOT, fill=as.factor(if_rest)))+ 
  geom_boxplot() +
  labs(x="Restaurant present in the area",
       y = "Population density in the area",
       title = "Restaurants and population density",
       caption = "Source: Own work") +
  scale_y_continuous(breaks = seq(0, 22500, 2500))+
  scale_fill_brewer(palette = "Set3")+
  theme(legend.position = "none")


 non_spatial %>% 
  select(-grid_id) %>%
  mutate(if_rest = ifelse(if_rest, "Yes", "No")) %>%
  ggplot(aes(x = as.factor(if_rest), y = biznes_count, fill=as.factor(if_rest)))+ 
  geom_boxplot() +
  labs(x="Restaurant present in the area",
       y = "Business density in the area",
       title = "Restaurants and business density",
       caption = "Source: Own work") +
  scale_y_continuous(breaks = seq(0, 2100, 250))+
  scale_fill_brewer(palette = "Set3")+
  theme(legend.position = "none",
        title = element_text(size = 8))

box_plots <- gridExtra::arrangeGrob(box_populacja, box_biznesy, ncol=2)


#' Density- populacja

density_populacja <- non_spatial %>% 
  select(-grid_id) %>%
  ggplot(aes(x = TOT))+ 
  geom_density() +
  labs(x="Population count in the area",
       y = "Density",
       title = "Population density",
       caption = "Source: Own work") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

#' Density- biznesy

density_biznesy <- non_spatial %>% 
  select(-grid_id) %>%
  ggplot(aes(x = biznes_count))+ 
  geom_density() +
  labs(x="Businesses count in the area",
       y = "Density",
       title = "Businesses density",
       caption = "Source: Own work") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))


density_plots <- gridExtra::arrangeGrob(density_populacja, density_biznesy, ncol=2)



#' Tabelka ze statsami
#(min, Q1, med, avg, Q3, max, liczności dla kategorycznych
summarise_numeric_custom <- function(x){
  list(
    "min" = min(x, na.rm = TRUE),
    "Q1" = quantile(x, 0.25, na.rm = TRUE),
    "median" = quantile(x, 0.5, na.rm = TRUE),
    "mean" = mean(x, na.rm = TRUE),
    "Q3" = quantile(x, 0.75, na.rm = TRUE),
    "max" = max(x, na.rm = TRUE)
  ) %>% 
    as_tibble()
}


non_spatial %>% 
  select(1, 3, 4, 5, 6) %>%
  map_df(~summarise_numeric_custom(.)) -> stats_table


stats_table$Variable <- columns_mapping[-6,2]$new
stats_table <- stats_table %>% select(7, everything())

write.csv(stats_table, file="11_variables_stats.csv")


#' ###korelacja
                      
non_spatial %>% select(-if_rest, -grid_id) ->non_spatial_numeric
  
names(non_spatial_numeric) <- columns_mapping[-6,2]$new

non_spatial_numeric %>% cor() %>%
  as_tibble() %>%
  mutate(` `=names(.)) %>%
  select(6, everything()) -> cor_table

write.csv(cor_table, file = "11_cor_table.csv")


#' ### Mapki
map <- ggplot()+
  geom_sf(data=grid %>% 
            mutate(`log(Business count)`=
                     ifelse(is.infinite(log(biznes_count)),
                                                0, log(biznes_count))), 
          aes(fill=`log(Business count)`))+
  geom_sf(data=points, color="red", alpha= 0.3, size=1)+
  labs(fill="log(Business count)",
       title= "Business count and location of restaurants")


map +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


map_biznes <- ggplot()+
  geom_sf(data=grid %>% 
            mutate(`log(Business count)`=
                     ifelse(is.infinite(log(biznes_count)),
                            0, log(biznes_count))), 
          aes(fill=`log(Business count)`))+
  labs(fill="log(Business count)",
       title= "Business count",
       caption = "Source: Own work") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title = element_text(size = 20)) +
  scale_fill_viridis_c()


map_biznes


map_population <- ggplot()+
  geom_sf(data=grid, 
          aes(fill=TOT))+
  labs(fill="Population count",
       title= "Population count",
       caption = "Source: Own work") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title = element_text(size = 20)) +
  scale_fill_viridis_c()


map_population


map_restaurants <- ggplot()+
  geom_sf(data=grid, 
          aes(fill=ifelse(if_rest, "Yes", "No")))+
  labs(fill="Is any restaurant \nin the area?",
       title= "Restaurants presence",
       caption = "Source: Own work") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Set3")


map_restaurants


#' Statsy

w <- grid %>% as_Spatial() %>% poly2nb()
ww <- nb2listw(w, style='B')

joincount.test(as.factor(grid$if_rest), ww)
#' Dla 1-1: jest autokorelacja
#' Dla 0-0 nie ma 
#' Można założyć że jest
#' 


#' Moran' I

#'biznes_count- 0.15
moran(grid$biznes_count, ww, n=length(ww$neighbours), S0=Szero(ww))$I

#'bus_count- 0.4
moran(grid$bus_count, ww, n=length(ww$neighbours), S0=Szero(ww))$I

#'populacja- 0.58
moran(grid$TOT, ww, n=length(ww$neighbours), S0=Szero(ww))$I

#'rest_count- 0.46
moran(grid$rest_count, ww, n=length(ww$neighbours), S0=Szero(ww))$I

#'if_rest- 0.31
moran(grid$if_rest, ww, n=length(ww$neighbours), S0=Szero(ww))$I


#' Równość populacji
#' t-student

non_spatial %>%
  filter(if_rest==1) %>%
  .$biznes_count
t.test(non_spatial %>%
         filter(if_rest==1) %>%
         .$biznes_count,
       non_spatial %>%
         filter(if_rest==0) %>%
         .$biznes_count
       )
#'p-value = 1.092e-11, różnica w średnich różna od 0



t.test(non_spatial %>%
         filter(if_rest==1) %>%
         .$TOT,
       non_spatial %>%
         filter(if_rest==0) %>%
         .$TOT
)
#'p-value < 2.2e-16, różnica w średnich różna od 0









#' ### DUMP

non_spatial %>% 
  select(-grid_id) %>%
  ggplot(aes(x = as.factor(if_rest), y = bus_count))+ 
  geom_boxplot() 
#' ilość przystanków większa gdy jest restauracja

non_spatial %>% 
  select(-grid_id) %>%
  ggplot(aes(x = as.factor(if_rest), y = roads))+ 
  geom_boxplot() 

#' ilość dróg większa gdy jest restauracja

#' 
#' wszystkie scatterploty
non_spatial %>% 
  select(-grid_id) %>% 
  GGally::ggpairs()

# mapy- długo się ładuje ----


#' wszystkie mapy
#'
#' Restauracje:
#grid %>% ggplot(aes(fill = as.factor(if_rest))) + geom_sf()

#' Populacja
#grid %>% ggplot(aes(fill = TOT)) + geom_sf()

#' Biznesy
#grid %>% ggplot(aes(fill = biznes_count)) + geom_sf()

#' przystanki
#grid %>% ggplot(aes(fill = bus_count)) + geom_sf()


#' summary
#' 
non_spatial %>% summary()



#Korelacja przestrzenna ----

w <- grid %>% as_Spatial() %>% poly2nb()
ww <- nb2listw(w, style='B')

#'bus_count- 0.4
moran(grid$bus_count, ww, n=length(ww$neighbours), S0=Szero(ww))$I

#'biznes_count- 0.15
moran(grid$biznes_count, ww, n=length(ww$neighbours), S0=Szero(ww))$I

#'populacja- 0.58
moran(grid$TOT, ww, n=length(ww$neighbours), S0=Szero(ww))$I

#'rest_count- 0.46
moran(grid$rest_count, ww, n=length(ww$neighbours), S0=Szero(ww))$I

#'if_rest- 0.31
moran(grid$if_rest, ww, n=length(ww$neighbours), S0=Szero(ww))$I


#' wykres morana- o co cho?
zmienna<-as.data.frame(scale(grid$TOT))# standaryzacja
moran.plot(zmienna$V1, ww, pch=19)	









