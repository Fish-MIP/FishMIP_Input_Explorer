library(thredds)
library(curl)
library(stringr)

#Defining domain name for THREDDS server
domain <- "https://www.ncei.noaa.gov/thredds-ocean/catalog/woa23/DATA/temperature/netcdf/decav/0.25/catalog.xml"

get_thredds <- function(domain, destdir, code){
  noaa_reg <- CatalogNode$new(domain, prefix = "thredds")
  base_http <- noaa_reg$list_services()$http[["base"]]

  file <- noaa_reg$list_datasets()
  file <- file[str_extract(names(file), "t(\\d{2})", group = 1) %in% code]
  name <- names(file)
  
  curl_download(paste0("https://www.ncei.noaa.gov/", 
                       base_http, 
                       noaa_reg$get_datasets()[[names(file)]]$url), 
                destfile = paste0(destdir, name))
}

get_thredds(destdir = "example_data/", domain = domain, code = "02")

