  

## Loading libraries ----
library(arrow)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(Hmisc)


# Defining base variables -------------------------------------------------
#Location of parquet files
base_dir <- "/g/data/vf71/fishmip_inputs/ISIMIP3a/regional_inputs/obsclim/025deg"
#Folder where mean climatologies with all data will be saved
base_out_maps <- file.path(base_dir, "maps_data")
#Folder where mean climatologies for comparison will be saved
base_out_comp <- file.path(base_out_maps, "comp_clim")

#Folder where mean climatologies with all data will be saved
base_out_ts <- file.path(base_dir, "ts_data")
#Folder where mean climatologies for comparison will be saved
base_out_ts_comp <- file.path(base_out_ts, "comp_clim")

#Getting list of parquet files available
par_list <- list.files(base_dir, pattern = "parquet$", full.names = T)

#Loading area file
area_df <- file.path("/g/data/vf71/shared_resources/grid_cell_area_ESMs/isimip3a",
                     "gfdl-mom6-cobalt2_areacello_15arcmin_global_fixed.csv") |> 
  read_csv() |> 
  rename(lon = x, lat = y)


# Calculating mean climatology --------------------------------------------
clim_calc <- function(file_path, min_year = NULL, max_year = NULL,
                      folder_out = NULL){
  #Inputs:
  # file_path (character): Full file path to parquet file to calculate 
  # climatology
  # min_year (integer): Optional. First year to be included in climatology
  # max_year (integer): Optional. Last year to be included in climatology
  # folder_out (character): Optional. Full path to folder where climatology 
  # will be saved
  
  #Load file
  df <- read_parquet(file_path)
  
  #Getting metadata
  meta <- df |> 
    select(!lat:vals) |> 
    drop_na()
  
  #Create file name to save climatology - if path provided
  if(!is.null(folder_out)){
    base_name <- basename(file_path) |> 
      str_replace("monthly", "climatological_mean")
  }
  
  #Extract years in name
  years <- df |> 
    mutate(year = year(time)) |> 
    distinct(year) |> 
    filter(year == min(year) | year == max(year))
  
  if(!is.null(min_year)){
    if(min_year < min(years$year)){
      print(paste0("'min_year' must be later or equal to the first year ",
                   "included in the data. Calculating mean climatology from ",
                   min(years$year)))
    }else{
      print(paste0("Calculating mean climatology from ", min_year))
      df <- df |> 
        filter(year(time) >= min_year)
      if(!is.null(folder_out)){
        base_name <- str_replace(base_name, "\\d{4}", as.character(min_year))
      }
    }
  }
  
  if(!is.null(max_year)){
    if(max_year > max(years$year)){
      print(paste0("'max_year' must be earlier or equal to the last year ",
                   "included in the data. Calculating mean climatology until ",
                   max(years$year)))
    }else{
      print(paste0("Calculating mean climatology until ", max_year))
      df <- df |> 
        filter(year(time) <= max_year)
      if(!is.null(folder_out)){
        base_name <- str_replace(base_name, "\\d{4}\\.", paste0(max_year, "."))
      }
    }
  }
  
  #Calculating climatology
  clim_maps <- df |> 
    select(lat:vals) |> 
    group_by(lat, lon) |> 
    summarise(clim_mean = mean(vals)) |> 
    bind_cols(meta)
    
  if(!is.null(folder_out)){
    clim_maps |>
      write_parquet(file.path(folder_out, base_name))
  }
  
  return(clim_maps)
}



# Calculating mean over time ----------------------------------------------
mean_ts <- function(file_path, min_year = NULL, max_year = NULL, monthly = F,
                      weights_df = NULL, folder_out = NULL){
  #Inputs:
  # file_path (character): Full file path to parquet file to calculate 
  # climatology
  # min_year (integer): Optional. First year to be included in climatology
  # max_year (integer): Optional. Last year to be included in climatology
  # weights (data frame): Optional. If provided, mean climatology will be 
  # weighted using these values
  # folder_out (character): Optional. Full path to folder where climatology 
  # will be saved
  
  #Load file
  df <- read_parquet(file_path)
  
  #Getting metadata
  meta <- df |> 
    select(!lat:vals) |> 
    drop_na()
  
  #Create file name to save climatology - if path provided
  if(!is.null(folder_out) & !is.null(weights_df)){
    base_name <- basename(file_path) |> 
      str_replace("monthly", "weighted_mean_ts")
  }
  
  #Extract years in name
  years <- df |> 
    mutate(year = year(time)) |> 
    distinct(year) |> 
    filter(year == min(year) | year == max(year))
  
  if(!is.null(min_year)){
    if(min_year < min(years$year)){
      print(paste0("'min_year' must be later or equal to the first year ",
                   "included in the data. Calculating mean values from ",
                   min(years$year)))
    }else{
      print(paste0("Calculating mean values from ", min_year))
      df <- df |> 
        filter(year(time) >= min_year)
      if(!is.null(folder_out)){
        base_name <- str_replace(base_name, "\\d{4}", as.character(min_year))
      }
    }
  }
  
  if(!is.null(max_year)){
    if(max_year > max(years$year)){
      print(paste0("'max_year' must be earlier or equal to the last year ",
                   "included in the data. Calculating mean values until ",
                   max(years$year)))
    }else{
      print(paste0("Calculating mean values until ", max_year))
      df <- df |> 
        filter(year(time) <= max_year)
      if(!is.null(folder_out)){
        base_name <- str_replace(base_name, "\\d{4}\\.", paste0(max_year, "."))
      }
    }
  }
  
  #Check if weights exist
  if(!is.null(weights_df) & !monthly){
    ts_df <- df |> 
      left_join(weights_df, join_by(lon, lat)) |> 
      group_by(time) |> 
      summarise(weighted_mean = weighted.mean(vals, cellareao, na.rm = T),
                weighted_sd = sqrt(wtd.var(vals, cellareao, na.rm = T)))
  }else if(is.null(weights_df) & !monthly){
    ts_df <- df |> 
      group_by(time) |> 
      summarise(mean = mean(vals, na.rm = T))
  }else if(is.null(weights_df) & monthly){
    ts_df <- df |>
      mutate(month = month(time)) |> 
      group_by(month) |> 
      summarise(mean = mean(vals, na.rm = T))
  }else if(!is.null(weights_df) & monthly){
    ts_df <- df |>
      left_join(weights_df, join_by(lon, lat)) |> 
      mutate(month = month(time)) |> 
      group_by(month) |> 
      summarise(weighted_mean = weighted.mean(vals, cellareao, na.rm = T),
                weighted_sd = sqrt(wtd.var(vals, cellareao, na.rm = T)))
  }
  
  #Adding metadata
  ts_df <- ts_df |> 
    bind_cols(meta)
  
  if(!is.null(folder_out)){
    ts_df |>
      write_parquet(file.path(folder_out, base_name))
  }
  
  return(ts_df)
}


# Calculating climatologies -----------------------------------------------
#Entire period available in GFDL-MOM6-COBALT2
par_list |> 
  map(\(x) clim_calc(x, folder_out = base_out_maps))

#Same period as WOA data for comparison
par_list |> 
  map(\(x) clim_calc(x, min_year = 1981, max_year = 2010,
                     folder_out = base_out_comp))



# Calculating time series -------------------------------------------------
#Entire period available in GFDL-MOM6-COBALT2
par_list |> 
  map(\(x) mean_ts(x, weights_df = area_df, folder_out = base_out_ts))

#Same period as WOA data for comparison
par_list |> 
  map(\(x) mean_ts(x, min_year = 1981, max_year = 2010, monthly = T, 
                   weights_df = area_df, folder_out = base_out_ts))


