suppressMessages({
  library(doParallel)
  library(fastshap)
  library(logger)
  library(shapviz)
  library(tidymodels)
  library(tidyverse)
  library(stacks)
})

NUMCLASSSAMPLES <- 400

full_data_recipe <- readRDS("./models/full_data_recipe.rds")
area_data_recipe <- readRDS("./models/area_data_recipe.rds")
spatial_data_recipe <- readRDS("./models/spatial_data_recipe.rds")

full_data_train <- suppressMessages(read_csv("./data/full_data_train.csv"))
area_data_train <- suppressMessages(read_csv("./data/area_data_train.csv"))
spatial_data_train <- suppressMessages(read_csv("./data/spatial_data_train.csv"))

full_data_test <- suppressMessages(read_csv("./data/full_data_test.csv")) |> 
  group_by(TimePoint) |> 
  slice_sample(n = NUMCLASSSAMPLES) |> 
  ungroup()
area_data_test <- suppressMessages(read_csv("./data/area_data_test.csv")) |> 
  group_by(TimePoint) |> 
  slice_sample(n = NUMCLASSSAMPLES) |> 
  ungroup()
spatial_data_test <- suppressMessages(read_csv("./data/spatial_data_test.csv")) |> 
  group_by(TimePoint) |> 
  slice_sample(n = NUMCLASSSAMPLES) |> 
  ungroup()

full_data_model <- readRDS("./models/full_data_model.rds")
area_data_model <- readRDS("./models/area_data_model.rds")
spatial_data_model <- readRDS("./models/spatial_data_model.rds")

get_shap_vals <- function(data_train, data_test, data_model, col) {
  
  X_test <- data_test |> 
    select(-TimePoint) |> 
    as.matrix()
  
  X_train <- data_train |> 
    select(-TimePoint) |> 
    as.matrix()
  
  predict_fn <- function(object, newdata) {
    preds <- predict(object, new_data = newdata |> as_tibble(), type = "prob")
    dplyr::pull(preds, col)
  }
  
  baseline <- mean(predict_fn(data_model, newdata = data_train))
  
  log_info("Starting shap explainer run")
  vals <- fastshap::explain(
    object = data_model,
    feature_names = X_test |> colnames(),
    X = X_train,
    nsim = 100,
    pred_wrapper = predict_fn,
    newdata = X_test,
    baseline = baseline
  )
  
  shapviz(vals, X=X_test, baseline=baseline)
}

full_data_vals_1w <- get_shap_vals(full_data_train, full_data_test, full_data_model, ".pred_7d")
full_data_vals_3w <- get_shap_vals(full_data_train, full_data_test, full_data_model, ".pred_21d")
full_data_vals_9w <- get_shap_vals(full_data_train, full_data_test, full_data_model, ".pred_9w")
log_info("Full Data SHAP Values: CALCULATED")
area_data_vals_1w <- get_shap_vals(area_data_train, area_data_test, area_data_model, ".pred_7d")
area_data_vals_3w <- get_shap_vals(area_data_train, area_data_test, area_data_model, ".pred_21d")
area_data_vals_9w <- get_shap_vals(area_data_train, area_data_test, area_data_model, ".pred_9w")
log_info("Morphological Data SHAP Values: CALCULATED")
spatial_data_vals_1w <- get_shap_vals(spatial_data_train, spatial_data_test, spatial_data_model, ".pred_7d")
spatial_data_vals_3w <- get_shap_vals(spatial_data_train, spatial_data_test, spatial_data_model, ".pred_21d")
spatial_data_vals_9w <- get_shap_vals(spatial_data_train, spatial_data_test, spatial_data_model, ".pred_9w")
log_info("Spatial Data SHAP Values: CALCULATED")

full_data_vals_1w |> 
  saveRDS("./data/full_shap_vals_1wk.rds")

full_data_vals_3w |> 
  saveRDS("./data/full_shap_vals_3wk.rds")

full_data_vals_9w |> 
  saveRDS("./data/full_shap_vals_9wk.rds")

area_data_vals_1w |> 
  saveRDS("./data/area_shap_vals_1wk.rds")

area_data_vals_3w |> 
  saveRDS("./data/area_shap_vals_3wk.rds")

area_data_vals_9w |> 
  saveRDS("./data/area_shap_vals_9wk.rds")

spatial_data_vals_1w |> 
  saveRDS("./data/spatial_shap_vals_1wk.rds")

spatial_data_vals_3w |> 
  saveRDS("./data/spatial_shap_vals_3wk.rds")

spatial_data_vals_9w |> 
  saveRDS("./data/spatial_shap_vals_9wk.rds")