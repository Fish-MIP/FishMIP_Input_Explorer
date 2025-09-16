# Cleaning fishing effort and catch data 
# Author: Denisse Fierro Arcos
# Date: 2024-11-26


# Loading libraries -------------------------------------------------------
library(arrow)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)

# Loading effort data -----------------------------------------------------
base_folder <- "/rd/gem/public/fishmip/ISIMIP3a/InputData/effort_catch_data"

effort_data <- file.path(base_folder,
                         "effort_histsoc_1841_2017_regional_models.parquet") |> 
  read_parquet() |> 
  #Keep data up from 1950 onwards
  filter(Year >= 1950)


# Cleaning effort data ----------------------------------------------------
# Not all columns available in the dataset are relevant, so we will summarise
# it using the columns that will be used in making plots in the shiny app
effort_data <- effort_data |> 
  clean_names() |>
  #Removing rows with no sector information (nom_active values not available
  #either)
  drop_na(sector) |> 
  group_by(year, sector, region, gear, f_group) |> 
  summarise(nom_active_t = sum(nom_active, na.rm = T)) |> 
  ungroup() 

#Create a data frame with all combinations of select columns
all_combs <- effort_data |> 
  expand(year, sector, region, gear, f_group) |> 
  drop_na(sector)

#Creating a single data frame with all combinations
effort_data <- effort_data |> 
  right_join(all_combs) |> 
  #Transforming to millions of hours
  mutate(nom_active_million = nom_active_t*1e-6)

#Removing all possible combinations
rm(all_combs)

#Fixing functional group and gear labels
effort_data <- effort_data |> 
  mutate(f_group = str_replace(f_group, "([a-z])(\\d|>|<)", "\\1 \\2"),
         gear = str_replace_all(gear, "_", " ")) |> 
  mutate(gear = str_remove(gear, "Others ")) |> 
  separate_wider_delim(f_group, delim = " ", names = c("group", "size"),
                       too_few = "align_start", cols_remove = F) |> 
  mutate(size = factor(size, levels = c("<90cm", "<30cm", "30-90cm", ">=90cm"),
                       ordered = T)) |>
  mutate(f_group = case_when(f_group == "demersalmollusc" ~ 
                               "demersal mollusc",
                             f_group == "lobsterscrab" ~ "lobsters crab",
                             T ~ f_group))

#Creating indexes to arrange data groups
func_groups <- effort_data |> 
  distinct(f_group, group, size) |> 
  arrange(group, size) |> 
  rowid_to_column("f_group_index") |> 
  select(f_group, f_group_index)

#Adding group index to dataset
effort_data <- effort_data |> 
  left_join(func_groups) |> 
  select(!c(group, size, nom_active_t))


# Saving clean effort data ------------------------------------------------
effort_data |> 
  rename("Functional Group" = "f_group", "Sector" = "sector",
         "Gear" = "gear") |> 
  write_parquet("www/effort_1950-2017_FishMIP_regions.parquet")


# Loading catch data ------------------------------------------------------
catch_data <- file.path(base_folder,
  "calibration_catch_histsoc_1850_2017_regional_models.parquet") |> 
  read_parquet() |> 
  #Keep data up from 1950 onwards
  filter(Year >= 1950)

# Cleaning catch data -----------------------------------------------------
catch_data <- catch_data |> 
  clean_names() |> 
  rowwise() |> 
  mutate(catch = sum(c(reported, iuu, discards))) |> 
  group_by(year, sector, region, f_group) |> 
  summarise(tot_catch = sum(catch, na.rm = T)) |> 
  ungroup()

#Create a data frame with all combinations of select columns
all_combs <- catch_data |> 
  expand(year, sector, region, f_group)

#Creating a single data frame with all combinations
catch_data <- catch_data |> 
  right_join(all_combs) |> 
  #Transforming to thousands
  mutate(catch_thousands = tot_catch*1e-3)

#Removing all possible combinations
rm(all_combs)

#Fixing functional group labels
# catch_data <- catch_data |> 
#   mutate(f_group = str_replace(f_group, "([a-z])(\\d|>|<)", "\\1 \\2")) |> 
#   mutate(f_group = case_when(f_group == "demersalmollusc" ~ 
#                                "demersal mollusc",
#                              f_group == "lobsterscrab" ~ "lobsters crab",
#                              T ~ f_group))

#Adding group index to dataset
catch_data <- catch_data |> 
  select(!tot_catch) |> 
  rename(f_group_index = f_group) |> 
  left_join(func_groups) |> 
  mutate(f_group = ifelse(is.na(f_group), "other", f_group))

# Saving clean catch data -------------------------------------------------
catch_data |> 
  rename("Functional Group" = "f_group", "Sector" = "sector") |> 
  write_parquet("www/catch_1950-2017_FishMIP_regions.parquet")

