# Helper function to get correct parquet filename from the selected region
get_filenames <- function(region_nicename, var_nicename, model){
  reg_filename <- region_nicename |>
    str_to_lower() |> 
    str_replace_all(" ", "-") |> 
    str_replace_all("'", "")
  
  if(model == "woa"){
    var_filename <- var_metadata |> 
      select(starts_with("woa"), ends_with("woa")) |> 
      drop_na() |>
      distinct() |> 
      filter(standard_name.woa == var_nicename) 
    
    woa_name <- var_filename$woa_name_code
    units <- str_replace_all(var_filename$units.woa, "_", " ")
    
    map <- woa_maps |> 
      str_subset(reg_filename) |> 
      str_subset(woa_name)
    
    files <- list(map = map,
                  units = units)
    
  }else if(model == "gfdl"){
    var_filename <- var_metadata |> 
      filter(long_name.gfdl == var_nicename) 
    
    gfdl_name <- var_filename$gfdl_name
    units <- var_filename$units.gfdl
    
    #Construct pattern to search files
    pat_glob <- paste0(".*", gfdl_name, "_15arcmin_", reg_filename)
    
    #Find map and time series files
    map <- maps_files |> 
      str_subset(pattern = pat_glob)
    ts <- ts_files |> 
      str_subset(pattern = pat_glob)
    down <- download_files |> 
      str_subset(pattern = pat_glob)
    
    files <- list(map = map,
                  ts = ts, 
                  download = down,
                  units = units)
  }
  return(files)
}