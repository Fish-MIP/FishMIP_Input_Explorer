# Merging times series from GFDL and WOA -----
#
# Author: Denisse Fierro Arcos
# Date: 2024-10-04
#  

## Loading libraries ----
library(arrow)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)

# Define basic variables --------------------------------------------------
# Regions
regions <- read_csv("www/FishMIP_regions_keys.csv")

# Folders where WOA and GFDL time series data are stores
woa_base <- "/rd/gem/public/fishmip/WOA_data/regional/monthly/comp_clim/"

gfdl_base <- file.path("/rd/gem/public/fishmip/ISIMIP3a/InputData/climate",
                       "ocean/obsclim/regional/monthly/historical", 
                       "GFDL-MOM6-COBALT2/ts_data/comp_clim")

# Folder where outputs will be stored
out_folder <- file.path("/rd/gem/public/fishmip/ISIMIP3a/InputData/climate",
                        "ocean/obsclim/regional/monthly/historical", 
                        "GFDL-MOM6-COBALT2/comp_ts")
if(!dir.exists(out_folder)){
  dir.create(out_folder)
}


# Define function to merge files ------------------------------------------
merge_ts <- function(woa_base, gfdl_base, region, var_woa, var_gfdl,
                     out_folder){
  #Inputs:
  # woa_base (character): Full file path to folder containing time series from
  # WOA
  # gfdl_base (character): Full file path to folder containing time series from
  # GFDL
  # region (character): Name of FishMIP regional model to be matched
  # var_woa (character): Name of variable from WOA to be used in merging
  # var_gfdl (character): Name of variable from GFDL to be used in merging
  # out_folder (character): Full file path to folder where merged files will be 
  # stored
  
  #Loading data
  woa <- list.files(woa_base, paste0("_", region, ".*", var_woa),
                    full.names = T) |>
    read_parquet(col_select = month:weighted_sd) |>
    #Ensuring months appear correctly in data
    mutate(month = factor(month, levels = month.name, ordered = T))
  
  gfdl <- list.files(gfdl_base, paste0("_", var_gfdl, "_.*_", region),
                     full.names = T) |>
    read_parquet(col_select = month:vals) |>
    #Ensuring months appear correctly in data
    mutate(month = factor(month.name[month], levels = month.name, ordered = T))
  
  #Merging files together
  merge <-  woa |>
    select(!weighted_sd) |>
    full_join(gfdl, by = join_by(month, depth)) |>
    rename(woa = weighted_mean, gfdl = vals) |>
    pivot_longer(cols = woa:gfdl, names_to = "type", values_to = "vals")

  #Defining file name to save data
  file_out <- paste0("weighted_mean_ts_", var_woa, "_", region, 
                     "_woa_gfdl_1981-2010.parquet")

  #Saving merged file
  merge |>
    write_parquet(file.path(out_folder, file_out))
}

regions$lookup |>
  map(\(x) merge_ts(woa_base, gfdl_base, x, "temp", "thetao", out_folder))

regions$lookup |>
  map(\(x) merge_ts(woa_base, gfdl_base, x, "sal", "so", out_folder))

