# Loading libraries -------------------------------------------------------
library(arrow)
library(shiny)
library(shinyWidgets)
library(readr)
library(tidyverse)
library(bslib)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(plotly)
library(lubridate)
options(scipen = 99)


# Setting up  -------------------------------------------------------------
# Load mask for regional ecosystem models
# masks_dir <- "/rd/gem/private/shared_resources/FishMIPMasks"
masks_dir <- "example_data"

# Keys to interpret raster mask
region_keys <- read_csv(file.path(masks_dir, "FishMIP_regions_keys.csv"))
var_keys <- read_csv(file.path(masks_dir, "WOA_variables_keys.csv"))

# First tab data ----------------------------------------------------------
# Folders containing Earth System Model (ESM) data
# fishmip_dir <- file.path("/rd/gem/public/fishmip/ISIMIP3a/InputData/climate/ocean",
#                       "obsclim/regional/monthly/historical/GFDL-MOM6-COBALT2")
fishmip_dir <- "/scratch/nf33/la6889/fishmip" # directory for OHW datasets
download_dir <- file.path(fishmip_dir, "download_data")
maps_dir <- file.path(fishmip_dir, "maps_data")
ts_dir <- file.path(fishmip_dir, "ts_data")

# Getting list of all files within folders
MOM_map_files <- list.files(maps_dir, full.names = T) 
MOM_ts_files <- list.files(ts_dir, full.names = T) 
MOM_download_files <- list.files(download_dir, full.names = T) 

# Getting names of environmental variables available
varNames <- str_extract(MOM_map_files, 
                        ".*obsclim_(.*)_[0-9]{2}arc.*", 
                        group = 1) |> 
  unique()
varNames <- data.frame(varNames = varNames) %>% 
  mutate(nicenames = NA)
varNames$nicenames <- case_when(varNames$varNames %in% var_keys$MOM_code ~ var_keys$key_name[var_keys$MOM_code == varNames$varNames],
                                TRUE ~ NA)

world <- ne_countries(returnclass = "sf", scale = "medium")

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

prettyplot_theme <- theme_classic() +
  theme(text = element_text(colour = "black", size = 15),
        legend.position = "bottom", legend.key.width = unit(4, "cm"),
        plot.title = element_text(size = 18, hjust = 0.5), 
        axis.text.y = element_text(size = 13, hjust = 0.5, vjust = 0.5), #angle = 45
        axis.text.x = element_text(size = 13, hjust = 0.5, vjust = 0.5),
        # axis.title = element_blank(), 
        legend.text = element_text(size = 13)#, 
        # legend.title = element_text(size = 15)
        )

prettymap_guide <- guide_colorbar(
  ticks.linewidth = 0.75,
  frame.colour = "blue", 
  title.vjust = 0.75)

# Data files  ------------------------------------------------------------------
WOA_files <- list.files(file.path("example_data", "WOA_data"), full.names = T) %>% 
  str_subset(pattern = ".parquet")

# Helper function to get correct parquet filename from the selected region
get_WOA_filename <- function(reg_nicename, var_nicename) {
  reg_filename <- reg_nicename %>% 
    str_replace_all(" ", "_") %>% # Replace spaces with underscores
    str_replace_all("i'i", "ii") # Replace Hawai'i with Hawaii
  
  var_filename <- var_keys$variable[var_keys$key_name == var_nicename]
  
  WOA_files %>% 
    str_subset(reg_filename) %>% 
    str_subset(var_filename)
} # Gives a single filename

get_MOM_filename <- function(reg_nicename, var_nicename = NA, var_MOM_code = NA) {
  reg_filename <- reg_nicename %>% 
    str_replace_all(" ", "-") %>% # Replace spaces with underscores
    str_replace_all("i'i", "ii") %>% # Replace Hawai'i with Hawaii
    str_to_lower()
  
  if (is.na(var_nicename)) {
    var_filename <- var_MOM_code
  } else {
    var_filename <- var_keys$MOM_code[var_keys$key_name == var_nicename]
  }

  return(list(
    map = MOM_map_files %>% 
      str_subset(reg_filename) %>% str_subset(var_filename),
    ts = MOM_ts_files %>% 
      str_subset(reg_filename) %>% str_subset(var_filename)
  )) # gives two filenames
}

# Defining user interface ------------------------------------------------------
## Global UI -------------------------------------------------------------------
ui <- fluidPage(
    theme = bs_theme(bootswatch = "yeti"),
    titlePanel(title = span(img(src = "FishMIP_logo.jpg", 
                                height = 100, width = 300, 
                                style = "display: block; margin-left: auto; margin-right:auto"),
                            h1("Regional Climate Forcing Data Explorer",
                            style = "background-color:#f3f3f3; border:1.5px solid #c9d5ea; 
                            padding-left: 15px; padding-bottom: 10px; padding-top: 10px;
                            text-align: center; font-weight: bold")),
               windowTitle = "FishMIP Regional Climate Forcing Data Explorer"),
## Model tab -------------------------------------------------------------------
    tabsetPanel(
      tabPanel("Model outputs",
               sidebarLayout(
                 sidebarPanel(
                   h4(strong("Instructions:")),
                   
                   # Choose region of interest
                   p("1. Select the FishMIP regional model you would like to visualise."),
                   selectInput(inputId = "region_MOM", label = NULL,
                               choices = region_keys$region, selected = "Central North Pacific"),
                   
                   # Choose variable of interest
                   p("2. Choose an environmental variable to visualise."),
                   selectInput(inputId = "variable_MOM",label = NULL,
                               choices = varNames$nicenames, selected = "Temperature"),
                   
                   p("3a. Click on the ", strong('Climatological maps'), " tab on the right to see a map of the 
                     climatological mean (1961-2010) for the variable of your choice within your region of interest"),
                   p("3b. Click on the ", strong('Time series plot'), " tab to see a time series of area 
                     weighted yearly mean for the variable and region of your choice."),
                   
                   p(em("Optional: "), "Get a copy of the data used to create these plots by clicking the 'Download' button below."),
                   # Download option
                   downloadButton(outputId = "download_data_MOM", label = "Download")
                   ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Climatological maps",
                              mainPanel(plotOutput(outputId = "map_MOM", width = "100%"))
                              ),
                     tabPanel("Time series plot",
                              mainPanel(plotOutput(outputId = "ts_MOM", width = "100%"))
                              )
                     )
                   )
                 )
               ),
## Observations tab ------------------------------------------------------------
      tabPanel("World Ocean Atlas data",
               sidebarLayout(
                 sidebarPanel(
                   h4(strong("Instructions:")),
                   
                   # Choose region of interest
                   p("1. Select the region you would like to visualise."),
                   selectInput(inputId = "region_WOA", label = NULL,
                               choices = region_keys$region, selected = "Central North Pacific"),
                   
                   # Choose variable of interest
                   p("2. Choose an environmental variable to visualise."),
                   selectInput(inputId = "variable_WOA", label = NULL,
                               choices = var_keys$key_name, selected = "Salinity"),
                   p("3a. Click on the ", strong('Climatological maps'), " tab on the right to see a map of the climatological 
                     mean (1981-2010) for observations of the variable of your choice within your region of interest"),
                   p("3b. Click on the ", strong('Time series plot'), " tab to see a time series of area-weighted 
                     monthly mean for the variable and region of your choice."),
                   
                   p(em("Optional: "), "Get a copy of the data used to create these plots by clicking the 'Download' button below."),
                   # Download option
                   downloadButton(outputId = "download_WOA", label = "Download")
                   ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Climatological maps", 
                              mainPanel(plotOutput(outputId = "map_WOA", width = "100%"))),
                     tabPanel("Time series plot",
                              mainPanel(plotOutput(outputId = "ts_WOA", width = "100%")))
                     )
                   )
                 )
               ),
## Comparison tab --------------------------------------------------------------
      tabPanel("Compare model with observations",
               sidebarLayout(
                 sidebarPanel(
                   h4(strong("Instructions:")),

                   # Choose region of interest
                   p("1. Select region"),
                   selectInput(inputId = "region_compare", label = NULL,
                               choices = region_keys$region, selected = "Central North Pacific"),

                   # Choose variable of interest
                   p("2. Choose variable"),
                   selectInput(inputId = "variable_compare", label = NULL,
                               choices = var_keys$key_name,  selected = "Salinity"),
                   p("3a. Click on the ", strong('Climatological maps'), " tab "),
                   p("3b. Click on the ", strong('Time series plot'), " tab "),
                   p(em("Optional: "), "Get a copy"),

                   # Download option
                   downloadButton(outputId = "download_data_compare", label = "Download")
                   ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Climatological maps",
                              mainPanel(plotlyOutput(outputId = "map_compare", width = "100%"))),
                     tabPanel("Time series plot",
                              mainPanel(plotOutput(outputId = "ts_compare", width = "100%")))
                     )
                   )
                 )
               ),
## About tab -------------------------------------------------------------------
    tabPanel(title = "About",
             mainPanel(
               br(), h3(strong("About this website")),
               p("This tool allows regional modellers to visualise environmental data from GFDL-MOM6-COBALT2 and from observations to 
                 determine if bias correction needs to be applied to the data prior to its use as forcings of a regionalmarine ecosystem model."),
               
               br(), h3(strong("Who is FishMIP?")),
               p("The Fisheries and Marine Ecosystem Model Intercomparison Project (FishMIP) is an network of more than 100 marine ecosystem
               modellers and researchers from around the world. Our goal is to bring together our collective understanding to help better 
               project the long-term impacts of climate change on fisheries and marine ecosystems, and to use our findings to help inform policy.
               You can find more information about FishMIP on our ", tags$a(href="https://fishmip.org/", "website.")),
               
               br(), h3(strong("How should I use this tool?")),
               p("This site has xxx tabs."),
               
               br(), h3(strong("How should I cite data from this site?")),
               p("You can download the data used to create the plots shown in this interactive tool using the 'Download' button included 
                 under each tab. As a condition of this tool to access data, you must cite its use. Please use the following citations:"),
               
               p("- Citation 1"),
               p("When using the data product in a publication, please include the following citation in addition to the data product citations 
               provided above:"),
               p("- Citation 2"), 
               
               br(), h3(strong("How can I contact you?")),
               p("If you would   "),
               
               br(), h4(strong("Acknowledgments")),
               p("The development of this tool was funded by the Australian Government through the Australian Research Council (ARC) 
                 XXXXX Project XXXX. We gratefully acknowledge contributions from coordinators and contributing modellers of the FishMIP 
                 and ISIMIP communities. We would also like to acknowledge the use of computing facilities provided by Digital Research 
                 Services, IT Services at the University of Tasmania."),
               
               br(),# card(img(src = "IMAS_logo.png", height = 150, width = 300, style = "display: block; margin-left: auto; margin-right:auto")),
               br(),br()
               )
             )
    )
    )

# Define actions ---------------------------------------------------------------
server <- function(input, output, session) {
  #bs_themer()
## Model tab -------------------------------------------------------------------
  # Selecting correct file based on inputs from region and env variable selected
  select_model_file <- reactive({
    get_MOM_filename(reg_nicename = input$region_MOM, 
                     # var_MOM_code = input$variable_MOM,
                     var_nicename = input$variable_MOM)
  })
  
  # Loading dataset
  MOM_data <- reactive({
    map_df <- read_csv(select_model_file()$map, col_select = c('lon', 'lat', 'vals', "units")) #|> 
    ts_df <- read_csv(select_model_file()$ts, col_select = c('date', 'vals'))
    
    # Get nice units
    unit <- unique(map_df$units)
    # vname <- var_keys$key_name[var_keys$MOM_code == input$variable_MOM]

    title <- paste0("Mean ", input$variable_MOM) %>% 
        str_to_sentence()
    map_title <- paste0(title, " from GFDL-MOM6-COBALT2 model (1961-2010)")
    ts_title <- paste0(title, " from GFDL-MOM6-COBALT2 model, ", input$region_MOM, " region")
    
    if (unit == 'degC' | input$variable_MOM == "Temperature") {
      cb_lab <- expression("Temperature ("*degree*"C)")
    } else {
      cb_lab <- paste0(input$variable_MOM, " (", unit, ")")
    }
    
    return(list(
      map_data = map_df,
      map_title = map_title, 
      ts_data = ts_df,
      ts_title = ts_title, 
      cb_lab = cb_lab
    ))
  })
  
  # Creating first plot
  output$map_MOM <- renderPlot({
    df <- MOM_data()$map_data
    
    # Adjusting map proportions
    minx <- min(df$lon)
    maxx <- max(df$lon)
    miny <- min(df$lat)
    maxy <- max(df$lat)
    
    # Calculate range
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))
    
    # Check if map crosses international date line
    if(rangex == 0 & str_detect(input$region_MOM, "Southern Ocean", negate = T)){
      df <- df |>
        mutate(lon = lon%%360)
      minx <- min(df$lon)
      maxx <- max(df$lon)
      rangex <- abs(abs(maxx)-abs(minx))
    }
    
    # Apply scaler function
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
    
    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      geom_tile() +
      prettyplot_theme +
      coord_cartesian() +
      scale_fill_viridis_c(guide = prettymap_guide, na.value = NA) +
      geom_sf(inherit.aes = F, data = world, lwd = 0.25, color = "black", show.legend = F)+
      coord_sf(xlims, ylims) +
      guides(fill = guide_colorbar(title = MOM_data()$cb_lab, title.position = "top", title.hjust = 0.5))+
      labs(title = MOM_data()$map_title, x = "Longitude", y = "Latitude")
  }, height = 500, width = 750)

  output$ts_MOM <- renderPlot({
    df <- MOM_data()$ts_data
    
    # Calculate spatially weighted average of variables selected

    ggplot(df, aes(x = date, y = vals)) +
      geom_line() +
      # geom_line(aes(color = "area weighted mothly mean")) +
      # geom_smooth(aes(color = "linear temporal trend")) +
      # scale_color_manual(breaks = c("area weighted mothly mean", "linear temporal trend"), 
      #                    values = c("#004488", "#bb5566"))+
      scale_x_date(date_labels = "%b-%Y", date_breaks = "24 months", expand = expansion(0.02)) +
      guides(color = guide_legend(title = element_blank())) +
      labs(title = MOM_data()$ts_title) +
      prettyplot_theme
   }, height = 500, width = 750)

  # Loading download dataset
  MOM_down_data <- reactive({
    filestring <- str_split(select_model_file()$map, "/")
    filestring <- filestring[[1]][length(filestring[[1]])]
    
    # Get full file path to relevant file
    down_file <- file.path(download_dir, filestring)
    
    # Load file
    down_df <- read_csv(down_file)
    return(list(
      filestring = filestring,
      down_file = down_file
    ))
   })
   
  output$download_data <- downloadHandler(
    filename = function(){MOM_down_data()$filestring},
    # Creating name of download file based on original file name
    content = function(file){write_csv(MOM_down_data()$down_file, file)}
    )
  
## Observations tab ------------------------------------------------------------
  
  # Select correct file based on inputs from region and variable selected
  select_WOA_file <- reactive({
    fname <- get_WOA_filename(reg_nicename = input$region_WOA, 
                              var_nicename = input$variable_WOA)
    return(fname)
    })
  
  # Read WOA data from file, surface only
  map_WOA_data <- reactive({
    df <- read_parquet(select_WOA_file()) %>%
      filter(!is.na(value)) %>% 
      select(-depth, -variable) %>% 
      group_by(lat, lon) %>% 
      reframe(value = mean(value))
    
    return(list(
      df = df, 
      title = "Map WOA title", 
      figlabel = "Map WOA legend label",
      xlab = "Map WOA xlabel", 
      ylab = "Map WOA ylabel"
    ))
  })

  ts_WOA_data <- reactive({
    df <- read_parquet(select_WOA_file()) %>%
      filter(!is.na(value)) %>% 
      mutate(date = as.Date(time)) %>% 
      select(-depth, -variable, -time) %>% 
      group_by(date) %>% 
      reframe(value = mean(value))

    return(list(
      df = df, 
      title = "TS WOA title", 
      figlabel = "Map WOA legend label",
      xlab = "TS WOA Xlabel", 
      ylab = "TS WOA ylabel"
    ))
  })
  
  output$map_WOA <- renderPlot({
    df <- map_WOA_data()$df
    
    # Adjusting map proportions
    minx <- min(df$lon)
    maxx <- max(df$lon)
    miny <- min(df$lat)
    maxy <- max(df$lat)

    # Calculate range
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))

    # Check if map crosses international date line
    if(rangex == 0 & str_detect(input$region_WOA, "Southern Ocean", negate = T)){
      df <- df |>
        mutate(lon = lon%%360)
      minx <- min(df$lon)
      maxx <- max(df$lon)
      rangex <- abs(abs(maxx)-abs(minx))
    }
    # Apply scaler function
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
    
    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = value)) +
      geom_tile() +
      coord_cartesian() +
      scale_fill_viridis_c() +
      geom_sf(inherit.aes = F, data = world, lwd = 0.25, color = "black", show.legend = F) +
      guides(fill = guide_colorbar(title = map_WOA_data()$figlabel, 
                                   title.position = "top", title.hjust = 0.5)) +
      coord_sf(xlims, ylims) +
      prettyplot_theme +
      labs(title = map_WOA_data()$title,
           x = map_WOA_data()$xlab,
           y = map_WOA_data()$ylab)
  }, height = 500, width = 1000)

  output$ts_WOA <- renderPlot({
    df <- ts_WOA_data()$df
    
    ggplot(df, aes(x = date, y = value)) +
      geom_line() +
      prettyplot_theme +
      labs(title = map_WOA_data()$title,
           x = map_WOA_data()$xlab,
           y = map_WOA_data()$ylab)
  }, height = 500, width = 1000)
  
  output$download_WOA <- downloadHandler(
    filename = function(){
      str_c("downloaded_", select_WOA_file()$fname, ".csv") # Needs some trimming, currently gives full path
      },
    # Creating name of download file based on original file name
    content = function(file){write_csv(x = map_WOA_data()$df, file = file)}
  )
  
## Comparison tab --------------------------------------------------------------

  select_compare_file <- reactive({
    fname_WOA <- get_WOA_filename(reg_nicename = input$region_compare, 
                                  var_nicename = input$variable_compare)
    # returns list of map and ts
    fname_MOM <- get_MOM_filename(reg_nicename = input$region_compare, 
                                  var_nicename = input$variable_compare) 
    
    df_WOA <- read_parquet(fname_WOA) %>% drop_na()
    map_MOM <- read_csv(fname_MOM$map, col_select = c('lat', 'lon', 'vals'))
    ts_MOM <- read_csv(fname_MOM$ts, col_select = c('date', 'vals'))
    
    # fname <- fname # change to combine them somehow
    return(list(
      fname = fname_WOA, 
      df_WOA = df_WOA,
      map_MOM = map_MOM,
      ts_MOM = ts_MOM
    ))
  })
  
  select_compare_data <- reactive({
    map_WOA <- select_compare_file()$df_WOA %>% 
      filter(!is.na(value)) %>% 
      select(-depth, -variable) %>% 
      group_by(lat, lon) %>% 
      reframe(value = mean(value)) %>% 
      mutate(source = "WOA observations")
    
    map_MOM <- select_compare_file()$map_MOM %>% 
      mutate(value = vals,
             source = "MOM5 model output") %>% 
      select(-vals)

    map_compare = rbind(map_MOM, map_WOA)
    
    map_compare <- map_compare %>% 
      pivot_wider(names_from = source, 
                  values_from = value) %>% 
      mutate(percent_diff = round((( `MOM5 model output` - `WOA observations`) / `WOA observations`) * 100, 2))
    
      
    ts_MOM <- select_compare_file()$ts_MOM %>% 
      mutate(value = vals,
             month = month(date)) %>% 
      group_by(month) %>% 
      reframe(value = mean(value)) %>% 
      mutate(source = "MOM5 model output")
      
    ts_WOA <- select_compare_file()$df_WOA %>% 
      filter(!is.na(value)) %>% 
      mutate(date = as.Date(time),
             month = month(date)) %>% 
      group_by(month) %>% 
      reframe(value = mean(value)) %>% 
      mutate(source = "WOA observations")
    
    ts_compare = rbind(ts_MOM, ts_WOA)
    
    return(list(
      map_compare = map_compare, 
      map_title = "Compare map title",
      map_figlabel = "Compare map figlabel",
      map_xlab = "Compare map xlab",
      map_ylab = "Compare map ylab",
      ts_compare = ts_compare,
      ts_title = "Compare ts title",
      ts_figlabel = "Compare ts figlabel",
      ts_xlab = "Compare ts xlab",
      ts_ylab = "Compare ts ylab"
    ))
  })
  
  output$map_compare <- renderPlotly({
    df <- select_compare_data()$map_compare 
    
    # Compare processing goes here
    
    # Adjusting map proportions
    minx <- min(df$lon)
    maxx <- max(df$lon)
    miny <- min(df$lat)
    maxy <- max(df$lat)
    
    # Calculate range
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))
    
    # Check if map crosses international date line
    if(rangex == 0 & str_detect(input$region_MOM, "Southern Ocean", negate = T)){
      df <- df |>
        mutate(lon = lon%%360)
      minx <- min(df$lon)
      maxx <- max(df$lon)
      rangex <- abs(abs(maxx)-abs(minx))
    }
    # Apply scaler function
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

    p <- ggplot(df, aes(x = lon, y = lat, fill = percent_diff)) +
      geom_tile() +
      coord_cartesian() +
      scale_fill_viridis_c() +
      #facet_grid(cols = vars(source)) +
      geom_sf(inherit.aes = F, data = world, lwd = 0.25, color = "black", show.legend = F) +
      guides(fill = guide_colorbar(title = select_compare_data()$map_figlabel, 
                                   title.position = "top", title.hjust = 0.5)) +
      coord_sf(xlims, ylims) +
      #prettyplot_theme +
      theme_minimal() +
      labs(title = select_compare_data()$map_title,
           x = select_compare_data()$map_xlab,
           y = select_compare_data()$map_ylab) +
      theme(legend.key.size = unit(0.2, "cm"),    # Decrease size of legend keys
            legend.text = element_text(size = 5),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    ggplotly(p)
    
  })
  
  output$ts_compare <- renderPlot({
    df <- select_compare_data()$ts_compare %>% 
      mutate(source = as.factor(source))
      
    ggplot(df, aes(x = month, y = value, colour = source)) +
      geom_line() +
      prettyplot_theme +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      ggtitle(label = select_compare_data()$ts_title) +
      labs(x = select_compare_data()$ts_xlab, y = select_compare_data()$ts_ylab)
  }, height = 500, width = 750)
  
  # output$download_compare <- downloadHandler(
  #   filename = function(){
  #     str_c("downloaded_", select_WOA_file()$fname, ".csv") # Needs some trimming, currently gives full path
  #   },
  #   # Creating name of download file based on original file name
  #   content = function(file){write_csv(x = map_WOA_data()$data, file = file)}
  # )
  
}

shinyApp(ui = ui, server = server)