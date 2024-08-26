###########################################################################
#
# Programmatically downloading data from WOA
# Author: Tormey Reimer
# Date: 2024-08-27
# This script downloads temperature and salinity data from the World Ocean 
# Atlas thredds server. All the files downloaded are:
# * at the 0.25 degree grid resolution
# * the average of seven decadal means from 1955 to 2022 (decav)
# * one file per month (01-12)
# Documentation here: https://www.ncei.noaa.gov/data/oceans/woa/WOA23/DOCUMENTATION/WOA23_Product_Documentation.pdf
#
###########################################################################

library(thredds)
library(curl)
library(tidyverse)

# Function to download the correct file from the thredds server
get_thredds <- function(domain, catalogue, destdir, month){
  noaa_reg <- CatalogNode$new(catalogue, prefix = "thredds")
  base_http <- noaa_reg$list_services()$http[["base"]]

  file <- noaa_reg$list_datasets()
  file <- file[str_extract(names(file), "t(\\d{2})", group = 1) == month]
  name <- names(file)
  
  curl_download(paste0(domain, base_http, noaa_reg$get_datasets()[[names(file)]]$url), 
                destfile = paste0(destdir, name))
}

# If you source this script it might take a while
start <- Sys.time()
print(paste0("Started downloads at ", start))

# Apply the function to all the desired files
sapply(X = c("02", "03"), 
       FUN = get_thredds, 
       domain = "https://www.ncei.noaa.gov/",
       catalogue = paste0(domain, "thredds-ocean/catalog/woa23/DATA/temperature/netcdf/decav/0.25/catalog.xml"),
       destdir = "example_data/WOA_data/")

sapply(X = c("02", "03"), 
       FUN = get_thredds, 
       domain = "https://www.ncei.noaa.gov/",
       catalogue = paste0(domain, "thredds-ocean/catalog/woa23/DATA/salinity/netcdf/decav/0.25/catalog.xml"),
       destdir = "example_data/WOA_data/")

end <- Sys.time()
print(paste0("Finished downloads at ", end, ". Total time taken: ", round(difftime(end, start, units='mins'), 2), " minutes."))
