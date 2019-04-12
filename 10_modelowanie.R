library(rpart)
library(readr)
library(tidyverse)
library(factoextra)
library(sf)
library(dbscan)
library(caret)
library(ROCR)
load("C:/rwiz/lic_wne/analiza/05_mapa.Rdata")

rm(grid, points)

load("C:/rwiz/lic_wne/analiza/08_full_grid.Rdata")

#doklejanie sąsiadów----
#utworzenie macierzy sąsiedztwa
grid %>% st_intersects(grid) %>%
  map2(1:808, function(x, y) {
  tibble(root = y, neighbours = x) %>% filter(root != neighbours)
  }) %>% 
  bind_rows() -> neighbours
#sąsiad jeżeli ma wspólną krawędź lub róg

aggr_neighbours <- function(grid_filtered) {
  grid_filtered %>% summarise(
    n_count = n(), 
    n_if_rest_sum = sum(if_rest, na.rm = TRUE),
    #n_roads_mean = sum(roads, na.rm=TRUE)/n(),
    n_roads_sum = sum(roads, na.rm=TRUE), 
    n_bus_count_sum = sum(bus_count, na.rm=TRUE),
    n_tot_sum = sum(TOT, na.rm = TRUE), 
    n_biznes_count_sum = sum(biznes_count, na.rm= TRUE)
  ) %>%
    st_set_geometry(NULL)
}

aggr_neighbours_data <- tibble()
  
for (i in 1:nrow(grid)){
  
  grid %>% filter(grid_id %in% (neighbours %>% 
                                  filter(root==i) %>% 
                                  .$neighbours)) %>%
    aggr_neighbours() ->temp
  temp$grid_id <- i 
  aggr_neighbours_data <- rbind(aggr_neighbours_data, temp)
}

grid %>%
  left_join(aggr_neighbours_data) ->grid_with_neigbours


st_intersects(grid_with_neigbours %>% st_centroid(), map_contour) %>%
  map_dbl(function(x) ifelse(length(x)==0, NA, x)) %>%
  as.tibble() %>%
  left_join(map_contour %>% 
              st_set_geometry(NULL) %>% 
              mutate(value = as.numeric(rownames(.)))) %>% 
  select(subzone) ->subzones

grid_with_neigbours$subzone <- subzones$subzone



grid_data<-grid_with_neigbours %>% 
  st_set_geometry(NULL) %>% 
  select(-rest_count, -grid_id, -n_count) %>%
  mutate(if_rest=ifelse(if_rest==1, "y", "n")) %>%
  mutate(if_rest=as.factor(if_rest))

# z blockCV i dalex ----



# 5 CVek
subzone_cv1 <- c("Białołęka", "Praga-Północ", "TARGÓWEK")
subzone_cv2 <- c("Bemowo" , "Bielany", "Żoliborz")
subzone_cv3 <- c("URSUS", "WŁOCHY", "MOKOTÓW", "URSYNÓW")
subzone_cv4 <- c("WOLA", "OCHOTA", "ŚRÓDMIEŚCIE", "PRAGA POŁUDNIE")
subzone_cv5 <- c("WAWER", "WESOŁA", "REMBERTÓW", "WILANÓW" )

mapa_env <-new.env()
load("C:/rwiz/lic_wne/analiza/05_mapa.Rdata", envir=mapa_env)

mapa_env$map_contour %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv1, "I", 0)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv2, "II", cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv3, "III (test)", cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv4, "IV", cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv5, "V", cv_no)) -> map_contour


folds_map <- map_contour %>% select(cv_no) %>% ggplot(aes(fill=cv_no))+geom_sf()+labs(fill="Fold")

rm(mapa_env)

# włożyć do careta
grid_data %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv1, 1, 0)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv2, 2, cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv3, 3, cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv4, 4, cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv5, 5, cv_no)) -> grid_data


training<-grid_data %>% 
  filter(cv_no != 3) 

test<-grid_data %>% 
  filter(cv_no == 3) 

# utworzenie numerów indeksów do cv (według dzielnic)

index_cv_list <- list()

index_cv_list[[1]] <- training %>% mutate(row=1:627) %>%
  filter(cv_no != 1) %>% .$row

index_cv_list[[2]] <- training %>% mutate(row=1:627) %>%
  filter(cv_no != 2) %>% .$row

index_cv_list[[3]] <- training %>% mutate(row=1:627) %>%
  filter(cv_no != 4) %>% .$row

index_cv_list[[4]] <- training %>% mutate(row=1:627) %>%
  filter(cv_no != 5) %>% .$row


training <- training %>% select(-subzone, -cv_no)
test <- test %>% select( -cv_no)


#dopasowywanie modeli ----

tr_cont<- trainControl(method="cv", 
                       index = index_cv_list,
                       summaryFunction = twoClassSummary, 
                       classProbs = T,
                       verboseIter = T)

rf_grid<- expand.grid(mtry=c(1:15))

tr_cont_rf<- trainControl(method="cv", 
                       index = index_cv_list,
                       summaryFunction = twoClassSummary, 
                       classProbs = T,
                       verboseIter = T)


# Metoda 1.- Random Forest ze wszystkimi zmiennymi i porównanie varImp----

model_rf_all<-train(if_rest~. , data= training,
                    metric="ROC",
                    #importance = "impurity",
                    trControl=tr_cont,
                    tuneGrid=rf_grid,
                    method="rf")


#' Tabelka z varimp jako mean decrease gini
model_rf_all$finalModel %>%
  randomForest::importance() %>%
  as_tibble(rownames = "Variable") %>%
  mutate(rel_imp=MeanDecreaseGini/77.1*100) %>%
  select(1,3) %>%
  arrange(-rel_imp) -> metoda1_varimp_table

metoda1_varimp_table

model_rf_all$results
# Metoda 2- po 4 modele (wszystkie zmienne, bez tot, bez biznes, bez obu)

#	Logistic regression ze wszystkimi zmiennymi ----

model_lr_all<-train(if_rest~. , data= training,
              metric="ROC",
              #importance = "impurity",
              trControl=tr_cont, 
              method="glm", 
              family="binomial")


#	Logistic regression bez biznes_count ----

model_lr_bez_biznes<-train(if_rest~.-biznes_count -n_biznes_count_sum , data= training,
                    metric="ROC",
                    #importance = "impurity",
                    trControl=tr_cont, 
                    method="glm", 
                    family="binomial")


#	Logistic regression bez TOT ----

model_lr_bez_tot<-train(if_rest~.-TOT-n_tot_sum , data= training,
                           metric="ROC",
                           #importance = "impurity",
                           trControl=tr_cont, 
                           method="glm", 
                           family="binomial")

#	Logistic regression bez biznes_count I bez TOT ----

model_lr_bez_biznes_tot<-train(if_rest~.-TOT-n_tot_sum-biznes_count -n_biznes_count_sum , data= training,
                               metric="ROC",
                               #importance = "impurity",
                               trControl=tr_cont, 
                               method="glm", 
                               family="binomial")


# RF ze wszystkimi-powyżej
#	RF bez biznes_count ----

model_rf_bez_biznes<-train(if_rest~.-biznes_count -n_biznes_count_sum , data= training,
                    metric="ROC",
                    #importance = "impurity",
                    trControl=tr_cont, 
                    tuneGrid=rf_grid,
                    method="rf")

#	RF bez TOT ----

model_rf_bez_tot<-train(if_rest~.-TOT-n_tot_sum , data= training,
                           metric="ROC",
                           #importance = "impurity",
                           trControl=tr_cont,
                        tuneGrid=rf_grid,
                           method="rf")




#	RF bez biznes_count I bez TOT ----

model_rf_bez_biznes_tot<-train(if_rest~. -TOT-n_tot_sum-biznes_count -n_biznes_count_sum, data= training,
                        metric="ROC",
                        #importance = "impurity",
                        trControl=tr_cont, 
                        tuneGrid=rf_grid,
                        method="rf")

# Porównanie wszystkich RF ----


models_names_list_rf <- list(
                          "model_rf_all",
                          "model_rf_bez_biznes",
                          "model_rf_bez_tot",
                          "model_rf_bez_biznes_tot"
                          
)

models_names_list_rf %>%
  map(function(x){
    eval(parse(text=x))
  }) -> models_expr_rf

names(models_expr_rf) <- models_names_list_rf

# Metoda 2
models_expr_rf %>% map(function(x) x$results) -> rf_results_table

models_expr_rf %>% map(function(x) {
  x %>%
    predict(test, type = "prob") %>%
    .[,2] %>%
    prediction(test$if_rest) %>%
    performance("auc") %>%
    .@y.values
}) %>%
  flatten() %>% 
  map_df(function(x) x) %>% 
  gather("model", "value",1:4)%>% 
  mutate(
         model = str_sub(model, start = 10)) -> rf_models_auc_test



# Porównanie wszystkich LR ----


models_names_list_lr <- list(
  "model_lr_all",
  "model_lr_bez_biznes",
  "model_lr_bez_tot",
  "model_lr_bez_biznes_tot"
  
)

models_names_list_lr %>%
  map(function(x){
    eval(parse(text=x))
  }) -> models_expr_lr

names(models_expr_lr) <- models_names_list_lr

# Metoda 2
models_expr_lr %>% map(function(x) x$results) -> lr_results_table

models_expr_lr %>% map(function(x) {
  x %>%
    predict(test, type = "prob") %>%
    .[,2] %>%
    prediction(test$if_rest) %>%
    performance("auc") %>%
    .@y.values
}) %>%
  flatten() %>% 
  map_df(function(x) x) %>% 
  gather("model", "value",1:4)%>% 
  mutate(
    model = str_sub(model, start = 10)) -> lr_models_auc_test

models_expr_lr %>% map(function(x) x$finalModel$coefficients) -> lr_models_coefs




# Porównywanie modeli ----
deparse(substitute(model_lr_bez_biznes))


eval(parse(text=deparse(substitute(model_lr_bez_biznes))))

models_names_list <- list("model_lr_all",
"model_rf_all",
"model_lr_bez_biznes",
"model_lr_bez_tot",
"model_rf_bez_biznes",
"model_rf_bez_tot",
"model_lr_bez_biznes_tot",
"model_rf_bez_biznes_tot",
"model_lr_z_biznes",
"model_lr_z_tot",
"model_lr_z_tot_biznes"

)

models_names_list %>%
  map(function(x){
    eval(parse(text=x))
  }) -> models_expr

names(models_expr) <- models_names_list

# Porównywanie modeli- caret resample ----
resamps_all <- resamples(models_expr[1:8])
ggplot(resamps_all)  
  
resamps_rf <- resamples(models_expr[c(2, 5, 6, 8)])
ggplot(resamps_rf)  

varImp(model_rf_all) %>% plot()

model_rf_all
model_rf_bez_biznes

# Porównanie modeli- AUC na zbiorze testowym ----

models_expr %>% map(function(x) {
  x %>%
    predict(test, type = "prob") %>%
    .[,2] %>%
    prediction(test$if_rest) %>%
    performance("auc") %>%
    .@y.values
}) %>%
  flatten() %>% 
  map_df(function(x) x) %>% 
  gather("model", "value",1:11)%>% 
  mutate(model_type = str_sub(model, 7, 8),
         model = str_sub(model, start = 10)) -> models_auc_test


models_auc_test %>% spread(key=model_type, value= value) %>%
  arrange(rf) 

# Porównanie modeli - varImp dla Random Forest ----
varimp_plot_rf <- model_rf_all %>% varImp() %>% plot()
varimp_plot_rf


# zapis ----
save.image(file="C:/rwiz/lic_wne/analiza/10_modele.Rdata")
rm(ls=ls())

load("C:/rwiz/lic_wne/analiza/10_modele.Rdata")

#' Co ma być na wyjściu?
#' Mapa z zaznaczonymi CV
folds_map
#' Metoda 1:
#' Rezultaty z fitowania (mtry)
model_rf_all$results
#' Varimp tabelka 
metoda1_varimp_table
#' Varimp plot
#'
#' Rezultaty ostatniego modelu
model_rf_all$results %>% filter(mtry==3)
#' Metoda 2:
#' Rezultaty z fitowania dla RF
rf_results_table
#' Parametry uzyskane dla LR dla każdego fitu oddzielnie
lr_models_coefs
#' AUC dla każdego modelu na testowym- tabelka 
rf_models_auc_test
lr_models_auc_test
#' Metoda 3:
#' Wykres+tabelka + parametry dla resamples
#' 





