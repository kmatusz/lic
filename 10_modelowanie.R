library(rpart)
library(readr)
library(tidyverse)
library(factoextra)
library(sf)
library(dbscan)
library(caret)
load("C:/rwiz/lic_wne/analiza/08_full_grid.Rdata")

grid_data<-grid %>% st_set_geometry(NULL) %>% select(-rest_count) %>%
  mutate(if_rest=ifelse(if_rest==1, "y", "n")) %>%
  mutate(if_rest=as.factor(if_rest))

index_train<-createDataPartition(grid_data$if_rest, p=0.7, list=F)

training<-grid_data[index_train, ]
test<-grid_data[-index_train, ]

library(gbm)
tr_cont<- trainControl(method="repeatedcv", 
                       #add these two lines if you are using AUC as  a metric:
                       summaryFunction = twoClassSummary, 
                       classProbs = T, 
                       repeats=3, number=3)

model1<-train(if_rest~.-roads , data= training,
              method="rpart",
              #verbose=F,
              metric="ROC",
              trControl=tr_cont
)
model1%>% summary()

varImp(model1)%>%plot()

