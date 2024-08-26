library(thredds)
library(curl)
library(stringr)

#Defining domain name for THREDDS server
domain <- "https://www.ncei.noaa.gov/thredds-ocean/catalog/woa23/DATA/temperature/netcdf/decav/0.25/catalog.xml"
noaa_reg <- CatalogNode$new(domain, prefix = "thredds")
seasonal_temp <- noaa_reg$list_datasets()
seasonal_temp <- seasonal_temp[str_extract(names(seasonal_temp), "t(\\d{2})", group = 1) %in% 13:16]
base_http <- noaa_reg$list_services()$http[["base"]]
noaa_reg$get_dataset_names()
curl_download(paste0(base_http, noaa_reg$get_datasets()[[names(seasonal_temp)[1]]]$url))