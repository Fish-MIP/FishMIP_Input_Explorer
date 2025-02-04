# Loading libraries -------------------------------------------------------
library(dplyr)
library(arrow)
library(janitor)


# Data folder -------------------------------------------------------------
base_folder <- "/rd/gem/public/fishmip/ISIMIP3a/InputData/effort_catch_data"


# Process catch and effort data  ------------------------------------------
effort_regional_ts <- 
  read_parquet(file.path(base_folder,
                         "effort_histsoc_1841_2017_regional_models.parquet")) |>
  filter(region != "") |>
  mutate(NomActive = as.numeric(NomActive)) |>
  filter(NomActive > 0) |> 
  clean_names()

catch_regional_ts <- read_parquet(
  file.path(base_folder, 
            "calibration_catch_histsoc_1850_2017_regional_models.parquet")) |>
  mutate(total_catch = Reported + IUU + Discards) |>
  clean_names()


# Save clean effort data --------------------------------------------------
effort_regional_ts |> 
  write_parquet(file.path(base_folder, 
                          "effort_1841_2017_regional_download.parquet"))

catch_regional_ts |> 
  write_parquet(file.path(base_folder, 
                          "catch_1850_2017_regional_download.parquet"))

