suppressMessages({
  library(doParallel)
  library(logger)
  library(tidymodels)
  library(tidyverse)
})

log_info("STARTED: 03TrainModels.R")

#registerDoParallel()

source("./R/90TrainModel.R")

full_data_train <- suppressMessages(read_csv("./data/full_data_train.csv")) |> 
  mutate(TimePoint = TimePoint |> factor())
area_data_train <- suppressMessages(read_csv("./data/area_data_train.csv")) |> 
  mutate(TimePoint = TimePoint |> factor())
spatial_data_train <- suppressMessages(read_csv("./data/spatial_data_train.csv")) |> 
  mutate(TimePoint = TimePoint |> factor())

full_data_recipe <- readRDS("./models/full_data_recipe.rds")
area_data_recipe <- readRDS("./models/area_data_recipe.rds")
spatial_data_recipe <- readRDS("./models/spatial_data_recipe.rds")

full_data_model <- train_model(full_data_train, full_data_recipe)
log_info("Full Data Model: TRAINED")
area_data_model <- train_model(area_data_train, area_data_recipe)
log_info("Morphological Data Model: TRAINED")
spatial_data_model <- train_model(spatial_data_train, spatial_data_recipe)
log_info("Spatial Data Model: TRAINED")

full_data_model |> 
  saveRDS("./models/full_data_model.rds")

area_data_model |> 
  saveRDS("./models/area_data_model.rds")

spatial_data_model |> 
  saveRDS("./models/spatial_data_model.rds")

log_info("COMPLETED: 03TrainModels.R")
