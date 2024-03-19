###########################################################################
#
# Merging datasets for variables with multiple depth bins
# Author: Denisse Fierro Arcos
# Date: 2024-03-19
# The aim of this notebook is to merge data for one variable into a single
# dataset, which will be available to download via the Shiny app
#
###########################################################################

# Loading libraries -------------------------------------------------------
library(data.table)
library(tidyverse)


# Setting up --------------------------------------------------------------
# Base folder containing Earth System Model (ESM) data
base_dir <- "/rd/gem/public/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/regional/monthly/historical/GFDL-MOM6-COBALT2"

# Getting list of all files within folder
data_files <- list.files(base_dir, pattern = "15arcmin", full.names = T)

# Extracting names of models available
models <- str_extract(data_files, ".*15arcmin_(.*)_monthly.*", group = 1) |> 
  unique()

# Extracting name of variables 
vars <- str_extract(data_files, ".*obsclim_(.*)_[0-9]{2}arc.*", group = 1) |> 
  #Keeping only variables with multiple depths
  str_subset("_depth-.*") |> 
  #Remove depth from name
  str_remove("_depth-.*") |> 
  #Keep unique variable names
  unique()

# Looping through each model
for(m in models){
  mod_list <- str_subset(data_files, m)
  #Looping through each variable
  for(v in vars){
    var_path <- str_subset(mod_list, v)
    
    #Loading all files for the same model and variable
    df <- var_path |> 
      map_df(~fread(.)) 
    
    #Keep metadata columns aside
    meta <- df |> 
      select(!c(lat, lon, depth_m, matches("[0-9]{4}"))) |> 
      distinct()
    
    #Reorganise data
    df <- df |> 
      #Keep all columns except metadata
      select(c(lat, lon, depth_m, matches("[0-9]{4}"))) |> 
      #Reorganise data
      pivot_longer(matches("[0-9]{4}"), names_to = "date",
                   values_to = "vals") |> 
      #Remove rows with NA values
      drop_na(vals) |> 
      #Format column names as date
      mutate(date = my(date),
             #Include a column for month
             month = month(date),
             #Include a column for year
             year = year(date)) |> 
      #Arrange data by date
      arrange(date) |> 
      #Add metadata
      cbind(meta)
    
    #Create name to save merged dataset
    f_out <- var_path[1] |> 
      str_remove("_depth-[0-9]{1,4}\\.[0-9]{1,2}")
    
    #Saving dataframe
    write_csv(df, f_out)
    
    #Remove old files
    
  }
}

