# Programmatically downloading data from WOA ----
#
# Author: Tormey Reimer
# Date: 2024-08-27
# Edited by: Denisse Fierro Arcos
# Edited on: 2024-09-03
# This script downloads temperature and salinity data from the World Ocean 
# Atlas thredds server. All the files downloaded are:
# * at the 0.25 degree grid resolution
# * the average of seven decadal means from 1955 to 2022 (decav)
# * one file per month (01-12)
# Documentation here: 
# https://www.ncei.noaa.gov/data/oceans/woa/WOA23/DOCUMENTATION/WOA23_Product_Documentation.pdf



# Loading libraries -------------------------------------------------------
library(thredds)
library(curl)
library(stringr)
library(purrr)


# Downloading function ----------------------------------------------------
# Supporting download function to make it easier to download files for multiple
# months
download_thredds <- function(name, destdir, base_http, noaa_datasets, 
                             overwrite = F){
  #Inputs:
  # name (character vector) - Name of dataset being downloaded 
  # destdir (character) - Full path to folder where data will be downloaded
  # base_http (character) - Base URL where WOA data will be downloaded from
  # noaa_datasets (list) - List containing all available downloadable datasets
  # overwrite (boolean) - Default is FALSE. If set to TRUE, it will overwrite 
  # previously downloaded data
  #
  #Output:
  # No outputs return. Instead, the function downloads and saves WOA data
  
  file_out <- file.path(destdir, name)
  url_file <- paste0(base_http, noaa_datasets[[name]]$url)
  if(!file.exists(file_out) | overwrite == T){
    curl_download(url_file, destfile = file_out)
  }
}


# Connecting to THREDDS server --------------------------------------------
# Function to connect and download the correct files from WOA's THREDDS server
get_thredds <- function(domain, catalogue, destdir, code, overwrite = F){
  #Inputs:
  # domain (character) - Base domain of WOA's THREDDS server
  # catalogue (character) - Full path to WOA's THREDDS server where data of
  # interest is available
  # destdir (character) - Full path to folder where data will be downloaded
  # code (character vector) - Two digit code identifying averaging period
  # overwrite (boolean) - Default is FALSE. If set to TRUE, it will overwrite 
  # previously downloaded data
  #
  #Output:
  # No outputs return. Instead, the function downloads and saves WOA data
  
  #Check if output directory exists, otherwise create it
  if(!dir.exists(destdir)){
    dir.create(destdir, recursive = T)
  }
  noaa_reg <- CatalogNode$new(catalogue, prefix = "thredds")
  base_http <- paste0(domain, noaa_reg$list_services()$http[["base"]])
  
  file <- noaa_reg$list_datasets()
  file <- file[str_extract(names(file), "t(\\d{2})", group = 1) %in% code]
  names(file) |> 
    map(\(x) download_thredds(x, destdir, base_http, noaa_reg$get_datasets(), 
                              overwrite))
}


# Applying download functions ---------------------------------------------
# If you source this script it might take a while
start <- Sys.time()
print(paste0("Started downloads at ", start))

months <- str_pad(1:12, width = 2, pad = 0)
domain <- "https://www.ncei.noaa.gov"
catalogue <- paste0(domain, "/thredds-ocean/catalog/woa23/DATA/temperature/",
                     "netcdf/decav81B0/0.25/catalog.xml")
destdir <- "/g/data/vf71/WOA_data"

get_thredds(domain, catalogue, destdir, months, overwrite = F)


############# TO DO ######################
#incorporate code into months - temp data should look like "t01", but salinity
#should look like "s01"


print(paste0("Halfway there at ", Sys.time()))

sapply(X = months, 
       code = "s",
       FUN = get_thredds, 
       domain = domain,
       catalogue = paste0(domain, "thredds-ocean/catalog/woa23/DATA/salinity/netcdf/decav81B0/0.25/catalog.xml"),
       destdir = "example_data/WOA_data/")

end <- Sys.time()
print(paste0("Finished downloads at ", end, ". Total time taken: ", round(difftime(end, start, units='mins'), 2), " minutes."))
