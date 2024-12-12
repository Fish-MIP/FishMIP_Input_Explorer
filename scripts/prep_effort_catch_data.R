library(data.table)
library(dplyr)
library(arrow)
library(here)


## load catch and effort data 
effort_regional_ts <- 
  fread(file.path("http://portal.sf.utas.edu.au/thredds/fileServer/gem/fishmip", 
                  "ISIMIP3a/InputData/effort_catch_data",
                  "effort_histsoc_1841_2017_regional_models.csv")) |>
  filter(region != "",
         Year >=1950) |>
  mutate(NomActive = as.numeric(NomActive)) |>
  rename("Functional Group" = "FGroup") |>
  filter(NomActive > 0)

catch_regional_ts <- fread(
  file.path("http://portal.sf.utas.edu.au/thredds/fileServer/gem/fishmip/", 
  "ISIMIP3a/InputData/effort_catch_data/",
  "calibration_catch_histsoc_1850_2017_regional_models.csv")) |>
  mutate(catch = Reported + IUU + Discards) |>
  rename("Functional Group" = "FGroup") |>
  filter(Year >= 1950, 
         catch > 0)

write_parquet(effort_regional_ts, here("www/effort_1950_2017_regional_prepped.parquet"))

write_parquet(catch_regional_ts, here("www/catch_1950_2017_regional_prepped.parquet"))

