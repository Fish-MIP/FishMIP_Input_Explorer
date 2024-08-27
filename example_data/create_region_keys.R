library(sf)
library(tidyverse)

base_dir <- "/scratch/nf33/la6889/fishmip" # directory for OHW datasets
fishmip <- read_sf(paste0(base_dir, "/FishMIP_regional_models/FishMIP_regional_models.shp"))

# Create region keys for reading into shiny
fishmip %>% 
  st_drop_geometry() %>% 
  rowid_to_column("id") %>%
  select(id, region) %>% 
  write_csv(paste0(base_dir, "/FishMIP_regions_keys.csv"))

# Define bounding boxes for data processing
bind_rows(lapply(fishmip$geometry, st_bbox)) |> 
  mutate(region = fishmip$region) |> 
  write_csv(paste0("example_data", "/FishMIP_regions_bbox.csv"))
