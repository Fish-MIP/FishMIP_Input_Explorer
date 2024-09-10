# Merging GFDL and WOA metadata -----
#
# Author: Denisse Fierro Arcos
# Date: 2024-09-09
#
# Metadata files for GFDL and WOA data were produced using scripts available at:
# https://github.com/Fish-MIP/FishMIP_regions/blob/main/scripts/05_compiling_GFDL_metadata.ipynb
# https://github.com/Fish-MIP/processing_WOA_data/blob/main/scripts/04P_compiling_WOA_metadata.ipynb

## Loading libraries ----
library(readr)
library(dplyr)
library(stringr)

## Loading WOA metadata ----
woa_meta <- read_csv("/g/data/vf71/WOA_data/woa_var_keys.csv", 
                     show_col_types = F) |> 
  rename(woa_name = short_name) |> 
  mutate(woa_name_code = case_when(woa_name == "t_an" ~ "temp",
                                    woa_name == "s_an" ~ "sal"),
         standard_name = str_to_title(str_replace_all(standard_name, "_", " ")),
         units = case_when(str_detect(units, "degrees") ~ "°C",
                           T ~ units))

all_meta <- file.path("/g/data/vf71/fishmip_inputs/ISIMIP3a/global_inputs", 
                       "obsclim/gfdl_var_keys.csv") |> 
  read_csv(show_col_types = FALSE) |> 
  rename(gfdl_name = short_name) |> 
  select(gfdl_name:units) |> 
  mutate(woa_name = case_when(str_detect(standard_name, "temperature") ~ "t_an",
                              str_detect(standard_name, "salinity") ~ "s_an",
                              T ~ NA),
         units = case_when(str_detect(units, "degC") ~ "°C",
                           T~ units)) |> 
  left_join(woa_meta, by = join_by(woa_name), suffix = c(".gfdl", ".woa"))

# Saving result ----
all_meta |> 
  write_csv("www/woa_gfdl_var_keys.csv")

