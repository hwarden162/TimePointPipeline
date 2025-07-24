suppressMessages({
  library(logger)
  library(tidymodels)
  library(tidyverse)
})

log_info("STARTED: 02TrainPreprocessors.R")

full_data_train <- suppressMessages(read_csv("./data/full_data_train.csv"))
area_data_train <- suppressMessages(read_csv("./data/area_data_train.csv"))
spatial_data_train <- suppressMessages(read_csv("./data/spatial_data_train.csv"))

full_data_recipe <- recipe(TimePoint ~ ., data = full_data_train) |> 
  step_zv(all_numeric_predictors()) |> 
  step_nzv(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  prep()

area_data_recipe <- recipe(TimePoint ~ ., data = area_data_train) |> 
  step_zv(all_numeric_predictors()) |> 
  step_nzv(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  prep()

spatial_data_recipe <- recipe(TimePoint ~ ., data = spatial_data_train) |> 
  step_zv(all_numeric_predictors()) |> 
  step_nzv(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  prep()

full_data_recipe |> 
  saveRDS("./models/full_data_recipe.rds")

area_data_recipe |> 
  saveRDS("./models/area_data_recipe.rds")

spatial_data_recipe |> 
  saveRDS("./models/spatial_data_recipe.rds")

log_info("COMPLETED: 02TrainPreprocessors.R")
