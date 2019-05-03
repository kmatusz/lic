library(rpart)
library(readr)
library(tidyverse)
library(factoextra)
library(sf)
library(dbscan)
library(caret)
library(ROCR)
library(DALEX)
load("C:/rwiz/lic_wne/analiza/05_mapa.Rdata")

rm(grid, points)

load("C:/rwiz/lic_wne/analiza/08_full_grid.Rdata")
# funkcje do variable importance----
var_imp <- function(explainer,
                    loss_function = loss_sum_of_squares,
                    ...,
                    type = "raw",
                    n_sample = 1000) {
  if (!("explainer" %in% class(explainer))) stop("The variable_importance() function requires an object created with explain() function.")
  if (is.null(explainer$data)) stop("The variable_importance() function requires explainers created with specified 'data' parameter.")
  if (is.null(explainer$y)) stop("The variable_importance() function requires explainers created with specified 'y' parameter.")
  if (!(type %in% c("difference", "ratio", "raw"))) stop("Type shall be one of 'difference', 'ratio', 'raw'")
  variables_complex <- list(...)


  variables <- colnames(explainer$data)

  if (n_sample > 0) {
    sampled_rows <- sample.int(nrow(explainer$data), n_sample, replace = TRUE)
  } else {
    sampled_rows <- 1:nrow(explainer$data)
  }
  sampled_data <- explainer$data[sampled_rows, ]
  observed <- explainer$y[sampled_rows]

  loss_0 <- loss_function(
    observed,
    explainer$predict_function(explainer$model, sampled_data)
  )
  loss_full <- loss_function(
    sample(observed),
    explainer$predict_function(explainer$model, sampled_data)
  )
  res <- sapply(variables, function(variable) {
    ndf <- sampled_data
    ndf[, variable] <- sample(ndf[, variable])
    predicted <- explainer$predict_function(explainer$model, ndf)

    loss_function(observed, predicted)
  })


  if (length(variables_complex) != 0) {
    variables_complex <- variables_complex[[1]]
    res_complex <- c()
    res_complex_names <- c()
    for (var_set in variables_complex) {
      ndf <- sampled_data
      for (var_idx in var_set) {
        ndf[, var_idx] <- sample(ndf[, var_idx])
      }
      res_complex_names  <- c(res_complex_names, paste0(names(sampled_data)[var_set], collapse = " "))
      predicted <- explainer$predict_function(explainer$model, ndf)
      res_complex <- c(res_complex, loss_function(observed, predicted))
    }
    res_complex <- data.frame(variable = res_complex_names, dropout_loss = res_complex)
  }


  res <- sort(res)
  res <- data.frame(
    variable = c("_full_model_", names(res), "_baseline_"),
    dropout_loss = c(loss_0, res, loss_full)
  )
  if (type == "ratio") {
    res$dropout_loss <- res$dropout_loss / loss_0
  }
  if (type == "difference") {
    res$dropout_loss <- res$dropout_loss - loss_0
  }



  class(res) <- c("variable_importance_explainer", "data.frame")
  res$label <- explainer$label
  if (length(variables_complex) != 0) {
    if (type == "ratio") {
      res_complex$dropout_loss <- res_complex$dropout_loss / loss_0
    }
    if (type == "difference") {
      res_complex$dropout_loss <- res_complex$dropout_loss - loss_0
    }
    res_complex
  } else {
    res
  }
}

boot <- function(fun, ..., ntimes = 10) {
  res <- tibble()
  for (i in 1:ntimes) {
    temp <- fun(...)
    res <- rbind(res, temp)
  }
  res %>%
    group_by(variable) %>%
    summarise(dropout_loss = mean(dropout_loss)) %>%
    arrange(-dropout_loss)
}



# doklejanie sąsiadów----
# utworzenie macierzy sąsiedztwa
grid %>%
  st_intersects(grid) %>%
  map2(1:808, function(x, y) {
    tibble(root = y, neighbours = x) %>% filter(root != neighbours)
  }) %>%
  bind_rows() -> neighbours
# sąsiad jeżeli ma wspólną krawędź lub róg

aggr_neighbours <- function(grid_filtered) {
  grid_filtered %>%
    summarise(
      n_count = n(),
      n_if_rest_sum = sum(if_rest, na.rm = TRUE),
      # n_roads_mean = sum(roads, na.rm=TRUE)/n(),
      n_roads_sum = sum(roads, na.rm = TRUE),
      n_bus_count_sum = sum(bus_count, na.rm = TRUE),
      n_tot_sum = sum(TOT, na.rm = TRUE),
      n_biznes_count_sum = sum(biznes_count, na.rm = TRUE)
    ) %>%
    st_set_geometry(NULL)
}

aggr_neighbours_data <- tibble()

for (i in 1:nrow(grid)) {
  grid %>%
    filter(grid_id %in% (neighbours %>%
      filter(root == i) %>%
      .$neighbours)) %>%
    aggr_neighbours() -> temp
  temp$grid_id <- i
  aggr_neighbours_data <- rbind(aggr_neighbours_data, temp)
}

grid %>%
  left_join(aggr_neighbours_data) -> grid_with_neigbours


st_intersects(grid_with_neigbours %>% st_centroid(), map_contour) %>%
  map_dbl(function(x) ifelse(length(x) == 0, NA, x)) %>%
  tibble::enframe(name = NULL) %>%
  left_join(map_contour %>%
    st_set_geometry(NULL) %>%
    mutate(value = as.numeric(rownames(.)))) %>%
  select(subzone) -> subzones

grid_with_neigbours$subzone <- subzones$subzone



grid_data <- grid_with_neigbours %>%
  st_set_geometry(NULL) %>%
  select(-rest_count, -grid_id, -n_count) %>%
  mutate(if_rest = ifelse(if_rest == 1, "y", "n")) %>%
  mutate(if_rest = as.factor(if_rest))

# z blockCV i dalex ----



# 5 CVek
subzone_cv1 <- c("Białołęka", "Praga-Północ", "TARGÓWEK")
subzone_cv2 <- c("Bemowo", "Bielany", "Żoliborz")
subzone_cv3 <- c("URSUS", "WŁOCHY", "MOKOTÓW", "URSYNÓW")
subzone_cv4 <- c("WOLA", "OCHOTA", "ŚRÓDMIEŚCIE", "PRAGA POŁUDNIE")
subzone_cv5 <- c("WAWER", "WESOŁA", "REMBERTÓW", "WILANÓW")

mapa_env <- new.env()
load("C:/rwiz/lic_wne/analiza/05_mapa.Rdata", envir = mapa_env)

mapa_env$map_contour %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv1, "I", 0)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv2, "II", cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv3, "III (test)", cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv4, "IV", cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv5, "V", cv_no)) -> map_contour


folds_map <- map_contour %>%
  select(cv_no) %>%
  ggplot(aes(fill = cv_no)) + geom_sf() +
  labs(
    fill = "Fold",
    title = "Cross-Validation Folds",
    caption = "Source: Own work"
  ) +

  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    title = element_text(size = 20)
  ) +
  scale_fill_viridis_d()

rm(mapa_env)

# włożyć do careta
grid_data %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv1, 1, 0)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv2, 2, cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv3, 3, cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv4, 4, cv_no)) %>%
  mutate(cv_no = ifelse(subzone %in% subzone_cv5, 5, cv_no)) -> grid_data


training <- grid_data %>%
  filter(cv_no != 3)

test <- grid_data %>%
  filter(cv_no == 3)

# utworzenie numerów indeksów do cv (według dzielnic)

index_cv_list <- list()

index_cv_list[[1]] <- training %>%
  mutate(row = 1:627) %>%
  filter(cv_no != 1) %>%
  .$row

index_cv_list[[2]] <- training %>%
  mutate(row = 1:627) %>%
  filter(cv_no != 2) %>%
  .$row

index_cv_list[[3]] <- training %>%
  mutate(row = 1:627) %>%
  filter(cv_no != 4) %>%
  .$row

index_cv_list[[4]] <- training %>%
  mutate(row = 1:627) %>%
  filter(cv_no != 5) %>%
  .$row


training <- training %>% select(-subzone, -cv_no)
test <- test %>% select(-cv_no, -subzone)


# dopasowywanie modeli ----

tr_cont <- trainControl(
  method = "cv",
  index = index_cv_list,
  summaryFunction = twoClassSummary,
  classProbs = T,
  verboseIter = T
)

rf_grid <- expand.grid(mtry = c(1:10))

tr_cont_rf <- trainControl(
  method = "cv",
  index = index_cv_list,
  summaryFunction = twoClassSummary,
  classProbs = T,
  verboseIter = T
)


# Random Forest ze wszystkimi zmiennymi ----

model_rf_all <- train(if_rest ~ .,
  data = training,
  metric = "ROC",
  # importance = "impurity",
  trControl = tr_cont,
  tuneGrid = rf_grid,
  method = "rf"
)

# 	Logistic regression ze wszystkimi zmiennymi ----

model_lr_all <- train(if_rest ~ .,
  data = training,
  metric = "ROC",
  # importance = "impurity",
  trControl = tr_cont,
  method = "glm",
  family = "binomial"
)

model_lr_all_log <- train(if_rest ~ .,
  data = training %>% mutate(biznes_count = ifelse(biznes_count == 0, 0, log(biznes_count))),
  metric = "ROC",
  # importance = "impurity",
  trControl = tr_cont,
  method = "glm",
  family = "binomial"
)


rf_model <- model_rf_all
lr_model <- model_lr_all
lr_log_model <- model_lr_all_log
# DALEX ----
library(DALEX)
explainer_rf <- DALEX::explain(rf_model,
  label = "Random Forest",
  data = test, y = test$if_rest
)

explainer_lr <- DALEX::explain(lr_model,
  label = "Logistic Regression",
  data = test, y = test$if_rest
)

explainer_lr_log <- DALEX::explain(lr_log_model,
  label = "Logistic Regression",
  data = test %>% 
    mutate(biznes_count = ifelse(biznes_count == 0, 0, log(biznes_count))), y = test$if_rest
)


# boot(variable_importance, explainer_rf, loss_function = pROC::auc, n_sample = -1)
# var_imp(explainer_rf, list(10, 20), loss_function = pROC::auc)
# rf_imp <-variable_importance(explainer_rf, loss_function = pROC::auc, n_sample = -1)
# rf_imp %>% arrange(-dropout_loss)
#
# lr_imp <-variable_importance(explainer_lr, loss_function = pROC::auc, n_sample = -1)
# lr_imp %>% arrange(-dropout_loss)
complex <- list(
  spatial = c(6:10),
  pop = c(1, 9),
  biznes = c(2, 10),
  bus = c(3, 8),
  roads = c(4, 7)
)


rf_imp <- boot(var_imp, explainer_rf, loss_function = pROC::auc, n_sample = -1)
rf_imp <- rf_imp %>%
  mutate(type = "rf")

lr_imp <- boot(var_imp, explainer_lr, loss_function = pROC::auc, n_sample = -1)
lr_imp <- lr_imp %>%
  mutate(type = "lr")
lr_imp

lr_log_imp <- boot(var_imp, explainer_lr_log,  loss_function = pROC::auc, n_sample = -1)
lr_log_imp <- lr_log_imp %>%
  mutate(type = "lr_log")
lr_log_imp

# complex
rf_imp_c <- boot(var_imp, explainer_rf, complex,  loss_function = pROC::auc, n_sample = -1) %>%
  mutate(type = "rf")
rf_imp_c

lr_imp_c <- boot(var_imp, explainer_lr, complex,  loss_function = pROC::auc, n_sample = -1) %>%
  mutate(type = "lr")
lr_imp_c


lr_log_imp_c <- boot(var_imp, explainer_lr_log, complex,  loss_function = pROC::auc, n_sample = -1) %>%
  mutate(type = "lr_log")
lr_log_imp_c

# RF specific

var_imp_rf <- randomForest::importance(rf_model$finalModel)
var_imp_rf %>%
  as_tibble(rownames = "variable") %>%
  rename(dropout_loss = MeanDecreaseGini) %>%
  mutate(type = "rf_spec") -> var_imp_rf


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


lr_imp %>%
  rbind(lr_log_imp) %>%
  rbind(rf_imp) %>%
  rbind(var_imp_rf) %>%
  mutate(variable = str_replace_all(variable, mapping)) %>%
  filter(!(variable %in% c("_baseline_", "If restaurant"))) %>%
  spread(key = type, value = dropout_loss) -> importances

write.csv(importances, file = "var_imp.csv")

lr_imp_c %>%
  rbind(rf_imp_c) %>%
  rbind(lr_log_imp_c) %>%
  spread(key = type, value = dropout_loss) -> importances_complex


pROC::auc(test$if_rest, predict(rf_model, test, type = "prob")[, 1])
pROC::auc(test$if_rest, predict(lr_model, test, type = "prob")[, 1])
pROC::auc(test$if_rest, predict(lr_log_model, test, type = "prob")[, 1])


write.csv(importances_complex, file = "var_imp_c.csv")



# zapis ----
# save.image(file="C:/rwiz/lic_wne/analiza/10_modele.Rdata")
# rm(ls=ls())

# load("C:/rwiz/lic_wne/analiza/10_modele.Rdata")

#' #' Co ma być na wyjściu?
#' #' Mapa z zaznaczonymi CV
#' folds_map
#' #' Metoda 1:
#' #' Rezultaty z fitowania (mtry)
#' model_rf_all$results
#' #' Varimp tabelka
#' metoda1_varimp_table
#' #' Varimp plot
#' #'
#' #' Rezultaty ostatniego modelu
#' model_rf_all$results %>% filter(mtry==3)
#' #' Metoda 2:
#' #' Rezultaty z fitowania dla RF
#' rf_results_table
#' #' Parametry uzyskane dla LR dla każdego fitu oddzielnie
#' lr_models_coefs
#' #' AUC dla każdego modelu na testowym- tabelka
#' rf_models_auc_test
#' lr_models_auc_test
#' #' Metoda 3:
#' #' Wykres+tabelka + parametry dla resamples
#' #'
#'
