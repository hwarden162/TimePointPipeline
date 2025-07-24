suppressMessages({
  library(logger)
  library(themis)
  library(tidymodels)
  library(tidyverse)
})

log_info("STARTED: 01PrepData.R")
file_list <- list.dirs("./imagedata")
file_list <- file_list[str_ends(file_list, "ndpi")]

TESTING_PATHS <- c(
  "./imagedata/1024455_Kras_P53_21d_IHC_20210520_161411.ndpi",
  "./imagedata/1046160_Kras_P53_22hr_IHC_20201007_102643.ndpi",
  "./imagedata/1046161_Kras_P53_7d_IHC_20201001_094000.ndpi",
  "./imagedata/572_Kras_P53_9w_IHC_20210520_160216.ndpi"
)

read_folder <- function(path, threshold = 2.5) {
  morph_data <- suppressMessages(read_csv(paste0(path, "/morphology.csv")))
  spat_data <- suppressMessages(read_csv(paste0(path, "/spatial.csv")))
  morph_data |> 
    left_join(spat_data, by = "Meta_Global_Mask_Label") |> 
    rename_with(~ str_replace_all(., "-", "")) |> 
    drop_na() |> 
    filter(
      QC_Global_Mask_SegVal != 0,
      Spatial_Nuclei_Spatial_LocalCounts200 > 5,
      AreaShape_Nuclei_Mask_Area > 20,
      AreaShape_Nuclei_Mask_Area < 400
    ) |> 
    mutate(
      ImagePath = path,
      TimePoint = str_split(path, "_")[[1]][4],
      Training = !(path %in% TESTING_PATHS),
      Cancerous1 = Intensity_Cytoplasm_Secondary_MedianIntensity / Spatial_Object_Spatial_LocalMeansIntensityCytoplasmSecondaryMedianIntensity200 > threshold,
      Cancerous2 = AreaShape_Nuclei_Mask_AxisMinorLength > 6.25,
      Cancerous = Cancerous1 & Cancerous2
    ) |> 
    filter(Cancerous) |> 
    select(
      TimePoint, 
      Training,
      starts_with("AreaShape_Nuclei"),
      starts_with("Spatial_Nuclei_Spatial_LocalCounts"),
      starts_with("Spatial_Object_Spatial_LocalMeansAreaShapeNucleiMaskArea"),
      starts_with("Spatial_Object_Spatial_LocalMeansAreaShapeNucleiMaskEccentricity"),
      starts_with("Spatial_Object_Spatial_LocalMeansAreaShapeNucleiMaskAxisMinorLength"),
      starts_with("Spatial_Object_Spatial_LocalMeansAreaShapeNucleiMaskEccentricity")
    ) |> 
    select(
      -AreaShape_Nuclei_Mask_AreaFilled,
      -AreaShape_Nuclei_Mask_EulerNumber,
      -starts_with("AreaShape_Nuclei_Mask_InertiaTensor"),
      -starts_with("AreaShape_Nuclei_Mask_InertiaTensorEigvals"),
      -AreaShape_Nuclei_Mask_AxisMinorLength
    )
}

full_data <- map(file_list, read_folder) |>
  bind_rows()

full_data_train <- full_data |> 
  filter(Training) |> 
  select(-Training)

full_data_test <- full_data |> 
  filter(!Training) |> 
  select(-Training)

full_data_eval <- full_data_test |> 
  initial_split(prop = 0.5, strata = TimePoint)

full_data |> 
  write_csv("./data/full_data.csv")

full_data_train |> 
  write_csv("./data/full_data_train.csv")

full_data_eval |>
  training() |> 
  write_csv("./data/full_data_calibration.csv")

full_data_eval |>
  testing() |> 
  write_csv("./data/full_data_test.csv")

full_data_train |> 
  select(-starts_with("Spatial")) |> 
  write_csv("./data/area_data_train.csv")

full_data_eval |>
  training() |> 
  select(-starts_with("Spatial")) |> 
  write_csv("./data/area_data_calibration.csv")

full_data_eval |>
  testing() |> 
  select(-starts_with("Spatial")) |> 
  write_csv("./data/area_data_test.csv")

full_data_train |> 
  select(-starts_with("AreaShape")) |> 
  write_csv("./data/spatial_data_train.csv")

full_data_eval |>
  training() |> 
  select(-starts_with("AreaShape")) |> 
  write_csv("./data/spatial_data_calibration.csv")

full_data_eval |>
  testing() |> 
  select(-starts_with("AreaShape")) |> 
  write_csv("./data/spatial_data_test.csv")

log_info("COMPLETED: 01PrepData.R")
