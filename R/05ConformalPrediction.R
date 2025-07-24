suppressMessages({
  library(stacks)
  library(tidymodels)
  library(tidyverse)
})

full_calibration_data <- suppressMessages(read_csv("./data/full_data_test.csv"))
area_calibration_data <- suppressMessages(read_csv("./data/area_data_test.csv"))
spat_calibration_data <- suppressMessages(read_csv("./data/spatial_data_test.csv"))

full_test_data <- suppressMessages(read_csv("./data/full_data_calibration.csv"))
area_test_data <- suppressMessages(read_csv("./data/area_data_calibration.csv"))
spat_test_data <- suppressMessages(read_csv("./data/spatial_data_calibration.csv"))

full_data_model <- readRDS("./models/full_data_model.rds")
area_data_model <- readRDS("./models/area_data_model.rds")
spat_data_model <- readRDS("./models/spatial_data_model.rds")


get_conformal_scores <- function(data_model, calibration_data) {
  data_model |> 
    predict(calibration_data, type="prob") |> 
    mutate(
      conformal_score = case_match(
        calibration_data |> pull(TimePoint),
        "7d" ~ 1 - .pred_7d,
        "21d" ~ 1 - .pred_21d,
        "9w" ~ 1 - .pred_9w
      )
    ) |> 
    pull(conformal_score)
}

full_conformal_scores <- get_conformal_scores(full_data_model, full_calibration_data)
area_conformal_scores <- get_conformal_scores(area_data_model, area_calibration_data)
spat_conformal_scores <- get_conformal_scores(spat_data_model, spat_calibration_data)

full_test_preds <- full_data_model |> 
  predict(full_test_data, type="prob")
area_test_preds <- area_data_model |> 
  predict(area_test_data, type="prob")
spat_test_preds <- spat_data_model |> 
  predict(spat_test_data, type="prob")

full_agreement <- full_data_model |> 
  predict(full_test_data) |> 
  pull(.pred_class) == full_test_data |> 
  pull(TimePoint)
area_agreement <- area_data_model |> 
  predict(area_test_data) |> 
  pull(.pred_class) == area_test_data |> 
  pull(TimePoint)
spat_agreement <- spat_data_model |> 
  predict(spat_test_data) |> 
  pull(.pred_class) == spat_test_data |> 
  pull(TimePoint)

get_conformal_classes <- function(conformal_scores, test_preds, agreement, alpha) {
  n <- nrow(test_preds)
  q_hat <- ceiling((n + 1) * (1 - alpha)) / n
  target_quantile <- quantile(conformal_scores, q_hat) |> unname()
  test_preds |> 
    mutate(
      .pred_7d = .pred_7d > 1-target_quantile,
      .pred_21d = .pred_21d > 1-target_quantile,
      .pred_9w = .pred_9w > 1-target_quantile,
      agreement = agreement
    )
}

full_conf_preds <- bind_rows(
  map(
    seq(0.025, 1, 0.025),
    \(x){get_conformal_classes(full_conformal_scores, full_test_preds, full_agreement, x) |> mutate(alpha=x)}
  )
)

area_conf_preds <- bind_rows(
  map(
    seq(0.025, 1, 0.025),
    \(x){get_conformal_classes(area_conformal_scores, area_test_preds, area_agreement, x) |> mutate(alpha=x)}
  )
)

spat_conf_preds <- bind_rows(
  map(
    seq(0.025, 1, 0.025),
    \(x){get_conformal_classes(spat_conformal_scores, spat_test_preds, spat_agreement, x) |> mutate(alpha=x)}
  )
)

conformal_bands <- bind_rows(
  full_conf_preds |> mutate(from = "Full Data"),
  area_conf_preds |> mutate(from = "Morphology Data"),
  spat_conf_preds |> mutate(from = "Spatial Data")
)
write_csv("./data/conformal_bands.csv")
