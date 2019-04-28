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
  tibble::enframe(name = NULL) %>%
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


# Random Forest ze wszystkimi zmiennymi ----

model_rf_all<-train(if_rest~. , data= training,
                    metric="ROC",
                    #importance = "impurity",
                    trControl=tr_cont,
                    tuneGrid=rf_grid,
                    method="rf")

#	Logistic regression ze wszystkimi zmiennymi ----

model_lr_all<-train(if_rest~. , data= training,
                    metric="ROC",
                    #importance = "impurity",
                    trControl=tr_cont, 
                    method="glm", 
                    family="binomial")



#' Tabelka z varimp dla RF jako mean decrease gini
model_rf_all$finalModel %>%
  randomForest::importance() %>%
  as_tibble(rownames = "Variable") %>%
  mutate(rel_imp=MeanDecreaseGini) %>%
  select(1,3) %>%
  arrange(-rel_imp) -> metoda1_varimp_table

metoda1_varimp_table



rf_model <- model_rf_all
lr_model <- model_lr_all

# DALEX ----
library(DALEX)
explainer_rf <- DALEX::explain(rf_model, label = "Random Forest", 
                                     data = training, y = training$if_rest)

explainer_lr <- DALEX::explain(lr_model, label = "Logistic Regression", 
                               data = training, y = training$if_rest)



var_imp(explainer_rf, list(10, 20), loss_function = pROC::auc)
rf_imp <-variable_importance(explainer_rf, loss_function = pROC::auc)
rf_imp %>% arrange(-dropout_loss)

lr_imp <-variable_importance(explainer_lr, loss_function = pROC::auc, n_sample = -1)
lr_imp %>% arrange(-dropout_loss)


columns_mapping <- 
  tribble(
    ~old, ~new,
    "^TOT", "Population",
    "^rest_count", "Restaurants",
    "^biznes_count", "Businesses",
    "^bus_count", "Bus stops",
    "^roads", "Roads",
    "^if_rest", "If restaurant",
    "^n_bus_count_sum", "Bus stops in heighbouring areas",
    "^n_roads_sum", "Roads in heighbouring areas",
    "^n_tot_sum", "Population in heighbouring areas",
    "^n_biznes_count_sum", "Businesses in heighbouring areas",
    "^_full_model_", "Full model performance",
    "^n_if_rest_sum", "If restaurant in neighbouring areas"
    
  )

mapping <- columns_mapping$new
names(mapping) <- columns_mapping$old

 reorder(var_imp_to_plot$variable,
                                var_imp_to_plot$dropout_loss,
                                mean)




lr_imp %>% 
  rbind(rf_imp) %>%
  mutate(variable = str_replace_all(variable, mapping)) %>%
  filter(!(variable %in%  c("_baseline_", "If restaurant"))) -> var_imp_to_plot
 
var_imp_to_plot %>%
  mutate(variable=reorder(variable,
                          dropout_loss,
                          mean)) %>%
#  mutate(label = str_replace_all(label, c("lr"= "Logistic Regression", "rf"= "Random Forest")))
  ggplot(aes(x=variable, y=dropout_loss)) +
  geom_col()+
  facet_grid(rows =vars(label))+
  coord_flip()+ 
  theme_minimal()

# Porównywanie modeli ----

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

# Porównanie modeli - varImp dla Random Forest ----
varimp_plot_rf <- model_rf_all %>% varImp() %>% plot()
varimp_plot_rf


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





