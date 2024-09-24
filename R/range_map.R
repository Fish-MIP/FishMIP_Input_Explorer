# Function to improve map ratios for plotting
scaler <- function(x, type, ratio = F){
  if((x > 0 & type == "min") | (x < 0 & type == "min")){
    x <- ifelse(ratio == T, x-3, x-6)
  }else if((x < 0 & type == "max") | (x > 0 & type == "max")){
    x <- ifelse(ratio == T, x+2, x+5)
  }else if(x == 0 & type == "min"){
    x <- ifelse(ratio == T, x-1, x-2)
  }else{
    x <- ifelse(ratio == T, x+1, x+2)
  }
  return(x)
}


range_map <- function(df, region){
  minx <- min(df$lon)
  maxx <- max(df$lon)
  miny <- min(df$lat)
  maxy <- max(df$lat)
  
  # Calculate range
  rangex <- abs(abs(maxx)-abs(minx))
  rangey <- abs(abs(maxy)-abs(miny))
  
  # Check if map crosses international date line
  if(rangex == 0 & str_detect(region, "Southern Ocean", negate = T)){
    df <- df |>
      mutate(lon = lon%%360)
    minx <- min(df$lon)
    maxx <- max(df$lon)
  }
  
  if(rangex >= 1.15*rangey){
    ylims <- c(scaler(miny, "min"),
               scaler(maxy, "max"))
    xlims <- c(scaler(minx, "min", ratio = T),
               scaler(maxx, "max", ratio = T))
  }else if(rangey >= 1.15*rangex){
    xlims <- c(scaler(minx, "min"),
               scaler(maxx, "max"))
    ylims <- c(scaler(miny, "min", ratio = T),
               scaler(maxy, "max", ratio = T))
  }else{
    xlims <- c(scaler(minx, "min"),
               scaler(maxx, "max"))
    ylims <- c(scaler(miny, "min"),
               scaler(maxy, "max"))
  }
  
  return(list(xlims = xlims,
              ylims = ylims))
}
