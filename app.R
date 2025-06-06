# Loading libraries -------------------------------------------------------
library(arrow)
library(shiny)
library(shinyWidgets)
library(shinycustomloader)
library(bslib)
library(tibble)
library(stringr)
library(dplyr)
library(rnaturalearth)
library(tidyr)
library(sf)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(glue)
library(forcats)
options(scipen = 0)

# Loading supporting files ------------------------------------------------
# Get list of all regions available
region_keys <- read_csv_arrow("www/FishMIP_regions_keys.csv",
                              col_select = !id) |> 
  deframe()

# Getting names of environmental variables available with equivalent from WOA
var_metadata <- read_csv_arrow("www/woa_gfdl_var_keys.csv") 

# GFDL variables - named vector
gfdl_variables <- var_metadata |> 
  select(long_name.gfdl, gfdl_name) |> 
  arrange(long_name.gfdl) |> 
  deframe()

woa_variables <- var_metadata |> 
  distinct(standard_name.woa, woa_name_code) |> 
  drop_na() |> 
  deframe()

# Shapefile with FishMIP regional model boundaries
fish_reg <- file.path("/rd/gem/private/shared_resources/FishMIP_regional_models",
                      "FishMIP_regional_models.shp") |> 
  read_sf() |> 
  mutate(region = str_to_lower(str_remove_all(region, "'")))


# Load catch and effort data
effort_regional_ts <- read_parquet(
  "www/effort_1950-2017_FishMIP_regions.parquet")
  
catch_regional_ts <- read_parquet("www/catch_1950-2017_FishMIP_regions.parquet")

# Define variables for effort and catch datasets we use
effort_variables <- c("Functional Group", "Sector", "Gear")
catch_variables <- c("Functional Group", "Sector")


# Defining location of relevant data sources ------------------------------
# Folders containing Earth System Model (ESM) data
fishmip_dir <- file.path("/rd/gem/public/fishmip/ISIMIP3a/InputData/climate",
                         "ocean/obsclim/regional/monthly/historical",
                         "GFDL-MOM6-COBALT2")

#Get a list of GFDL files available 
#For download
download_files <- list.files(file.path(fishmip_dir, "download_data"), 
                             full.names = T)
#For mapping
maps_files <- file.path(fishmip_dir, "maps_data") 
#For time series
ts_files <- file.path(fishmip_dir, "ts_data")

#For differences
map_comp_files <- file.path(fishmip_dir, "comp_maps")
ts_comp_files <- file.path(fishmip_dir, "comp_ts")

# Getting list of all files within folders
woa_maps <- "/rd/gem/public/fishmip/WOA_data/regional/climatology"
woa_ts <- "/rd/gem/public/fishmip/WOA_data/regional/monthly/ts"

# Loading map of the world
world <- ne_countries(returnclass = "sf", scale = "medium")

#Defining palette to be used with effort and catch data
mypal <- c(brewer.pal(n = 9, name = "Set1"), brewer.pal(n = 12, name = "Set3"),
           brewer.pal(n = 8, name = "Accent"))

#Defining plot themes
prettymap_theme <- list(geom_tile(),
                        theme_classic(),
                        scale_fill_viridis_c(na.value = NA),
                        geom_sf(inherit.aes = F, data = world, lwd = 0.25, 
                                color = "black", show.legend = F),
                        theme(text = element_text(colour = "black", size = 15),
                              legend.position = "bottom", 
                              axis.title = element_blank(),
                              legend.key.width = unit(3.5, "cm"),
                              legend.key.height = unit(1, "cm"),
                              plot.title = element_text(size = 18, hjust = 0.5),
                              axis.text.y = element_text(hjust = 0.5, 
                                                         vjust = 0.5, 
                                                         size = 15), 
                              axis.text.x = element_text(angle = 45, 
                                                         hjust = 0.5, 
                                                         vjust = 0.5, 
                                                         size = 15), 
                              legend.ticks = element_line(linewidth = 0.75, 
                                                          colour = "gray"),
                              legend.frame = element_rect(linewidth = 0.75, 
                                                          colour = "dark gray"),
                              legend.text = element_text(size = 15), 
                              legend.title = element_text(size = 15)))

prettyts_theme <- list(theme_bw(),
                       theme(axis.text.y = element_text(size = 14),
                             axis.text.x = element_text(angle = 45, vjust = 1, 
                                                        hjust = 1, size = 14),
                             axis.title.y = element_text(size = 15), 
                             axis.title.x = element_blank(),
                             plot.title = element_text(hjust = 0.5, size = 18),
                             legend.position = "bottom", 
                             legend.text = element_text(size = 18)))


fishing_theme <- list(geom_area(stat = "identity", alpha = 0.85,
                                position = "stack"),
                      scale_x_continuous(breaks = seq(1950, 2017, 5)),
                      scale_fill_manual(values = mypal),
                      theme_bw(),
                      guides(fill = guide_legend(title.position = "top",
                                                 title.hjust = 0.5)),
                      theme(axis.text.y = element_text(size = 12),
                            axis.text.x = element_text(angle = 45, vjust = 1,
                                                       hjust = 1, size = 12),
                            axis.title.y = element_text(size = 12),
                            axis.title.x = element_blank(),
                            legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_text(size = 12,
                                                        face = "bold"),
                            legend.text = element_text(size = 12)))



# Defining functions ------------------------------------------------------
# Function to improve map ratios for plotting
scaler <- function(x, type, ratio = F){
  if((x > 0 & type == "min") | (x < 0 & type == "min")){
    x <- ifelse(ratio == T, x-3, x-5)
  }else if((x < 0 & type == "max") | (x > 0 & type == "max")){
    x <- ifelse(ratio == T, x+2, x+4)
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
  if(rangex == 0 & str_detect(region, "southern-ocean", negate = T)){
    df <- df |>
      mutate(lon = lon%%360)
    minx <- min(df$lon)
    maxx <- max(df$lon)
  }
  
  if(rangex >= 1.1*rangey){
    ylims <- c(scaler(miny, "min"),
               scaler(maxy, "max"))
    xlims <- c(scaler(minx, "min", ratio = T),
               scaler(maxx, "max", ratio = T))
  }else if(rangey >= 1.1*rangex){
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
  
  return(list(df = df,
              xlims = xlims,
              ylims = ylims))
}


# Defining user interface ------------------------------------------------------

## Global UI -------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(bootswatch = "materia"),
  titlePanel(title = span(img(src = "FishMIP_logo.jpg", 
                              height = 100, width = 300, 
                              style = "display: block; margin-left: auto; 
                              margin-right:auto"),
                          h1("Regional Climate Forcing Data Explorer",
                             style = "color: #095c9e; background-color:#f3f3f3; 
                             border:1.5px solid #c9d5ea; 
                             padding-left: 15px; padding-bottom: 10px; 
                             padding-top: 10px;
                             text-align: center; font-weight: bold")),
             windowTitle = "FishMIP Regional Climate Forcing Data Explorer"),
  ## Model tab -----------------------------------------------------------------
  tabsetPanel(
    tabPanel("GFDL model outputs",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose region of interest
                 p("1. Select a FishMIP regional model:"),
                 selectInput(inputId = "region_gfdl", label = NULL,
                             choices = region_keys, 
                             selected = "east-bass-strait"),
                 
                 # Choose variable of interest
                 p("2. Select an environmental variable:"),
                 selectInput(inputId = "variable_gfdl", 
                             label = NULL,
                             choices = gfdl_variables,
                             selected = "tos"),
                 
                 # Depth drop down only appears for variables that contain this
                 # information
                 conditionalPanel(
                   condition = "['o2', 'chl', 'zmeso', 'zmicro', 'phydiat', 
                   'phydiaz', 'phypico', 'phyc', 'thetao', 'so', 'uo', 'vo', 
                   'zooc', 'ph'].includes(input.variable_gfdl)",
                   selectizeInput(inputId = "depth_gfdl",
                                  label = "Choose depth you want to visualise:",
                                  choices = NULL)),
                 
                 p("3a. Click on the ", strong('Climatological map'), 
                 " tab on the right to see a map of the 
                     climatological mean (1961-2010)."),
                 p("3b. Click on the ", strong('Time series plot'), 
                 " tab to see a time series of the area-weighted monthly
                     mean and the linear temporal trend."),
                 p(em("Optional: "), "Get a copy of the data used to create 
                   these plots by clicking the 'Download' button below. Note 
                   that variables that are three dimensional (i.e., have 
                   multiple depth levels) will be downloaded as ", em(".zarr"), 
                   " files. Refer to the ", strong('About'), " tab for details 
                   on how to open this file format in R."),
                 # Download option
                 downloadButton(outputId = "download_gfdl", 
                                label = "Download")
               ),
               mainPanel(
                 br(),
                 "All figures shown in this tab are based on the ", 
                 em("Observation-based climate related forcing"), " (obsclim) 
                 outputs from the GFDL-MOM6-COBALT2 model. These data were 
                 originally obtained from the ", 
                 tags$a(href = paste0("https://data.isimip.org/search/tree/",
                                      "ISIMIP3a/InputData/climate/ocean/",
                                      "gfdl-mom6-cobalt2/obsclim/"), 
                        "ISIMIP Data Repository."),
                 br(), br(),
                 "The ", em("Climatological map"), " tab below shows the mean 
                 climatology (1961-2010) for the environmental variable and 
                 within the boundaries of the regional model of interest 
                 selected on the left.",
                 br(), br(),
                 "The ", em("Time series plot"), "tab below shows the 
                 area-weighted monthly mean between 1961 and 2010.",  
                 br(), br(),
                 strong("Note: "), "The variable names and units shown in the
                 dropdown list and plots come from the ",
                 tags$a(href = paste0("https://protocol.isimip.org/#/ISIMIP3a/",
                                      "marine-fishery_global/32-climate-", 
                                      "related-forcing"),
                        "GFDL-MOM6-COBALT2 model."), "We have chosen not apply 
                 any transformation to the original model outputs. Instead, we 
                 summarised data so we could create the map and time series 
                 plots within the limits of all FishMIP regional models. If 
                 your model requires environmental data to be in a unit or grid 
                 that is different to the one available in the GFDL-MOM6-COBALT2
                 model, you can download the data from this website and 
                 post-process it to meet your needs.",
                 br(), br(),
                 tabsetPanel(
                   tabPanel("Climatological map",
                            mainPanel(
                              br(), 
                              withLoader(
                                plotOutput(outputId = "map_gfdl", 
                                           width = "100%"),
                                type = "html",
                                loader = "myloader"))
                   ),
                   tabPanel("Time series plot",
                            mainPanel(
                              br(), 
                              withLoader(
                                plotOutput(outputId = "ts_gfdl", 
                                               width = "100%"),
                                type = "html",
                                loader = "myloader"))
                   )
                 )
               )
             )
    ),
    
    ## Observations tab --------------------------------------------------------
    tabPanel("World Ocean Atlas 2023 data",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose region of interest
                 p("1. Select a FishMIP regional model:"),
                 selectInput(inputId = "region_WOA", label = NULL,
                             choices = region_keys, 
                             selected = "east-bass-strait"),
                 
                 # Choose variable of interest
                 p("2. Select an environmental variable:"),
                 selectInput(inputId = "variable_WOA", label = NULL,
                             choices = woa_variables, 
                             selected = "temp"),
                 
                 # Select depth (if available)
                 selectizeInput(inputId = "depth_woa",
                                label = "Choose depth you want to visualise:",
                                choices = NULL),
                 
                 p("3a. Click on the ", strong('Climatological map'),
                 " tab on the right to see a map of the climatological 
                     mean (1981-2010) of observations."),
                 p("3b. Click on the ", strong('Time series plot'), 
                 " tab to see a time series of area-weighted 
                     monthly mean of observations."),
                 
                 #Optional download
                 p(em("Optional: "), "Get a copy of the data used to create 
                   climatological maps as a csv file by clicking the 'Download' 
                   button below."),
                 # Download option
                 downloadButton(outputId = "download_WOA", label = "Download")
               ),
               mainPanel(
                 br(),
                 "Figures shown in this tab use data from the ", 
                 tags$a(href = 
                          "https://www.ncei.noaa.gov/products/world-ocean-atlas", 
                        "World Ocean Atlas 2023 (WOA23)"), 
                 ". We used the ", em("objectively analysed climatologies"), 
                 "field to create the climatological maps and area-weighted 
                 monthly climatology time series plot. While, the ",
                 em("number of observations "), "variable was used to create the
                 maps shown in the sub-tab under the same name.",  
                 br(), br(),
                 strong("Note: "), "The variable names and units shown in the
                 dropdown list and plots come from the WOA23. We have chosen not
                 apply any transformation to the original data. Instead, we 
                 summarised data so we could create maps and time series plots
                 within the limits of all FishMIP regional models. If your model
                 requires environmental data to be in a unit or grid that is 
                 different to the one available in the WOA23 you can download 
                 the data from ", 
                 tags$a(href=paste0("https://www.ncei.noaa.gov/products/",
                                    "world-ocean-atlas"), "this website"), 
                 "and post-process it to meet your needs.",
                 br(), br(),
                 "For some regions, the WOA23 dataset may have a very limited 
                 number of observations and so it may not offer the most 
                 realistic representation of your area of interest. In this 
                 case, you may choose to use a different observational product
                 to assess the performance of GFDL-MOM6-COBALT2 outputs. We have
                 an ", tags$a(href = 
                                paste0("https://github.com/Fish-MIP/",
                                       "processing_WOA_data/blob/main/scripts/",
                                       "P_regridding_woa_data.ipynb"),
                        "example notebook"), " showing how you can regrid this
                 data to match the grid used by the GFDL-MOM6-COBALT2 model.",
                 br(), br(),
                 tabsetPanel(
                   tabPanel("Climatological map", 
                            mainPanel(
                              br(), 
                              withLoader(
                                plotOutput(outputId = "map_WOA", 
                                           width = "100%"),
                                type = "html",
                                loader = "myloader"))
                   ),
                   tabPanel("Time series plot",
                            mainPanel(
                              br(), 
                              withLoader(
                                plotOutput(outputId = "ts_WOA", 
                                           width = "100%"),
                                type = "html",
                                loader = "myloader")),
                            br(),
                            br(),
                            br(),
                            br(),
                            strong("Note: "), "The grey ribbon in the plot 
                            above shows the spatial variance in the variable
                            of interest.",
                            br(),
                            br(),
                            br()
                   ),
                   tabPanel("Number of in-situ observations",
                            mainPanel(
                              br(),
                              withLoader(
                                plotOutput(outputId = "count_WOA",
                                           width = "100%"),
                                type = "html",
                                loader = "myloader")
                            )),
                 )
               )
             )
    ),
    ## Comparison tab ----------------------------------------------------------
    tabPanel("Model outputs against observations",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose region of interest
                 p("1. Select a FishMIP regional model:"),
                 selectInput(inputId = "region_compare", label = NULL,
                             choices = region_keys, 
                             selected = "east-bass-strait"),
                 
                 # Choose variable of interest
                 p("2. Select an environmental variable:"),
                 selectInput(inputId = "variable_compare", label = NULL,
                             choices = woa_variables,
                             selected = "temp"),
                 
                 # Select depth (if available)
                 selectizeInput(inputId = "depth_comp",
                                label = "Choose depth you want to visualise:",
                                choices = NULL),
                 
                 p("3a. Click on the ", strong('Climatological map'), 
                 " tab on the right to see a map of the differences 
                 in the climatological mean (1981-2010) from the model output
                 and observations."),
                 p("3b. Click on the ", strong('Time series plot'), 
                 " tab to see the difference in climatological monthly 
                 area-weighted mean (1981-2010) between the model output and 
                 observations."),
                 
                 #Optional download
                 p(em("Optional: "), "Get a copy of the data for bias correction
                   (if needed) as a compressed folder (", em(".zip"), ") by 
                   clicking the 'Download' button below. Note that the ", 
                   em(".zip"), "folder has files in ", em(".parquet"), " format,
                   which is designed to store and retrieve tabular data in an
                   efficient way. Refer to the ", strong('About'), " tab for 
                   details on how to open this file format in R."),
                 
                 # Download option
                 downloadButton(outputId = "download_comp", 
                                label = "Download")
               ),
               mainPanel(
                 br(),
                 "The following processing steps were taken before comparing
                 GFDL model outputs and WOA data:", br(),
                 "1. Climatological mean was calculated using GFDL outputs
                 between 1981 and 2010.", br(),
                 "2. WOA data was regridded to match the GFDL outputs.", br(),
                 "3. Difference was calculated by substracting WOA data from 
                 GFDL model outputs.", br(),
                 br(),
                 "This means that positive values in the maps identify areas 
                 where GFDL overestimated mean conditions.",
                 br(), br(),
                 tabsetPanel(
                   tabPanel("Climatological maps",
                            mainPanel(
                              br(), 
                              withLoader(
                                plotOutput(outputId = "map_compare", 
                                            width = "140%"),
                                type = "html",
                                loader = "myloader"))
                   ),
                   tabPanel("Time series plot",
                            mainPanel(
                              br(), 
                              withLoader(
                                plotOutput(outputId = "ts_compare", 
                                           width = "100%"),
                                type = "html",
                                loader = "myloader"))
                   )
                 )
               )
             )
    ),
    
    ## Catch and effort tab ----------------------------------------------------
    tabPanel("Fishing effort and catch data",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose region of interest
                 p("1. Select a FishMIP regional model:"),
                 selectInput(inputId = "region_effort", label = NULL,
                             choices = names(region_keys),
                             selected = "East Bass Strait"),
                 
                 # Choose catch or effort data
                 p("2. Select dataset to visualise:"),
                 radioButtons(inputId = "catch_effort_select", label = NULL, 
                              choiceNames = c("Fishing Effort", 
                                              "Fisheries Catch"), 
                              choiceValues = c("effort", "catch"),
                              selected = "effort"),
                 
                 # Choose variable of interest
                 p("3. Select how data should be classified in the plot:"),
                 selectInput(inputId = "variable_effort", 
                             label = NULL,
                             choices = effort_variables,
                             selected = "Functional Group"),
                 
                 # Inline layout for download button
                 p(em("Optional: "), "Get a copy of the data used to create 
                   these plots as a compressed folder (", em(".zip"), ") by 
                   clicking the 'Download' button below. The downloaded folder
                   also includes two dictionaries: one will help you interpret 
                   the column names in the fishing data and the other will allow
                   upi to interpret country codes. Note that the ", em(".zip"), 
                   "folder has files in ", em(".parquet"), " format, which is 
                   designed to store and retrieve tabular data in an efficient 
                   way. Refer to the ", strong('About'), " tab for details on 
                   how to open this file format in R."),
                 
                 downloadButton(outputId = "download_data", label = "Download")
                 ),
               mainPanel(
                 br(),
                 "The fishing effort and catch data used to create plots in this
                 tab were obtained from 'ISIMIP3a reconstructed fishing activity
                 data (v1.0)'",
                 tags$a(href = "https://data.isimip.org/10.48364/ISIMIP.240282", 
                        "(Novaglio et al. 2024)."),
                 br(),
                 br(),
                 "The fishing effort and catch data start in 1950, but the 
                 fishing effort forcing was reconstructed starting in 1841, 
                 which is available for download on the left panel.",
                br(),
                tabPanel("",
                         mainPanel(
                           br(), 
                           withLoader(plotlyOutput(outputId = "ts_effort", 
                                                   width = "100%", 
                                                   height = "500px"),
                                      type = "html", loader = "myloader")
                           ))
                )
               )
             ),
    
    ## About tab ---------------------------------------------------------------
    tabPanel(title = "About",
             mainPanel(
               br(),
               h3(strong("About this website")),
               p("This tool allows regional modellers to visualise 
                 environmental data from GFDL-MOM6-COBALT2 and from 
                 observations to determine if bias correction (Step 3 below) 
                 needs to be applied to the data prior to its use as forcings of
                 a regional marine ecosystem model."),
               br(),
               img(src = "FishMIP_regional_model_workflow.png", height = 600,
                   width = 715, style = "display: block; 
                                          margin-left: auto;
                                          margin-right: auto"),
               br(),
               h3(strong("Who is FishMIP?")),
               p("The Fisheries and Marine Ecosystem Model Intercomparison 
               Project (FishMIP) is an network of more than 100 marine ecosystem
               modellers and researchers from around the world. Our goal is to 
               bring together our collective understanding to help better 
               project the long-term impacts of climate change on fisheries and
               marine ecosystems, and to use our findings to help inform policy.
               You can find more information about FishMIP on our ",
               tags$a(href="https://fishmip.org/", "website.")),
               br(),
               img(src = "FishMIP_regional_models.png", height = 750,
                   width = 800, style = "display: block; 
                                          margin-left: auto;
                                          margin-right: auto"),
               br(),
               h3(strong("How should I use this tool?")),
               p("This site has three main tabs:"),
               p("1.", em(strong("GFDL model outputs:")), " Here you can 
                 download the GFDL-MOM6-COBALT2 ocean outputs as originally 
                 available in the ", tags$a(href="https://data.isimip.org/", 
                                            "ISIMIP data repository."), 
                 "Data available for download in this tab has not been 
                 processed in any way, we have simply extracted all available
                 data within the boundaries of your region of interest."),
               p("2.", em(strong("World Ocean Atlas 2023 data:")), " In this 
                 tab you can download World Ocean Atlas 2023 (WOA23) data that 
                 has been extracted for your region of interest. Note that data
                 available for downloaded here has not been processed in any 
                 way and it is excatly as available in the original form. For
                 more information refer to their ", 
                 tags$a(href=paste0("https://www.ncei.noaa.gov/products/",
                                    "world-ocean-atlas"), "documentation.")),
               p("3.", em(strong("Model outputs against observations: ")), "In 
                 this tab you can download GFDL-MOM6-COBALT2 and WOA23 data for
                 your region of interest. The WOA23 data available for download 
                 in this tab has been regridded to match GFDL-MOM6-COBALT2 to
                 allow users to compare these products with ease."),
               p("4:", em(strong("Fishing effort and catch data: ")), " Here 
                 you can download the fishing effort and catch data that should 
                 be used to force your regional marine ecosystem models 
                 following ",
                 tags$a(href="https://github.com/Fish-MIP/FishMIP2.0_ISIMIP3a",
                        "FishMIP protocol 3a.")),
               h4(strong("What are .zarr and .parquet files?")),
               p("Although these two file formats store different types of data:
                 zarr is designed for gridded data, while parquet files store 
                 tabular data. Both of them are cloud optimised file formats 
                 that are designed to make data storage and retrieval more 
                 efficient. This means that filesizes are smaller and you can 
                 load them faster than other files formats storing the same 
                 type of data (e.g., .csv, .txt, .nc)."),
               p("Using these file formats also have the benefit of decreasing 
                 the time you need to wait for data to be downloaded from this
                 website. This is especially true for regional models that cover
                 a large geographical area."),
               p("To load parquet files into R we recommend you install the ",
                 tags$a(href="https://arrow.apache.org/docs/r/", "arrow"),
                 " package. Then use the ", em("read_parquet()")," function to
                 load the parquet file as a tibble. From here, you can use ",
                 em("tidyverse"), " or base R to process the data as you would
                 with any tabular dataset. If you are a Linux user, you may 
                 also want to consider the ", 
                 tags$a(href="https://nanoparquet.r-lib.org/", "nanoparquet"),
                 "package."),
               p("To load zarr files in R, we recommend you use the ", 
                 tags$a(href=paste0("https://www.bioconductor.org/packages/",
                                    "release/bioc/html/Rarr.html"), "Rarr"), 
                 " package. For instructions on how to load these files in R, 
                 we created an ",
                 tags$a(href = paste0("https://github.com/Fish-MIP/",
                                      "FishMIP_extracting-data/blob/main/", 
                                      "scripts/loading_zarr_files.md"), 
                        "example notebook.")),
               br(),
               h3(strong("How should I cite data from this site?")),
               p("You can download the data used to create the plots shown in 
                 this interactive tool using the 'Download' button included 
                 under each tab. As a condition of this tool to access data, 
                 you must cite its use. Please use the following citations:"),
               p("- Fierro-Arcos, D., Blanchard, J. L., Clawson, G., Flynn, C., 
                 Ortega Cisneros, K., Reimer, T. (2024). FishMIP input explorer 
                 for regional ecosystem modellers. ", 
                 tags$a(href = paste0("https://rstudio.global-ecosystem-model.", 
                                      "cloud.edu.au/shiny/",
                                      "FishMIP_Input_Explorer/"))),
               p("When using the data products in a publication, please include 
               the following citation in addition to the data product citation 
               provided above:"),
               p("- Ortega-Cisneros, K., Fierro-Arcos, D. Lindmark, M., et al.
                 (2025). An Integrated Global-to-Regional Scale Workflow for
                 Simulating Climate Change Impacts on Marine Ecosystems.", 
                 em("Earth's Future,"), " 13, e2024EF004826. DOI:", 
                 tags$a(href = "https://doi.org/10.1029/2024EF004826",
                 "https://doi.org/10.1029/2024EF004826")),
               p("When using GFDL-MOM6-COBALT2 model outputs, you 
                 also need to include the following citation:"),
               p("- Xiao Liu, Charles Stock, John Dunne, Minjin Lee, Elena 
                 Shevliakova, Sergey Malyshev, Paul C.D. Milly, Matthias Büchner
                 (2022): ISIMIP3a ocean physical and biogeochemical input data 
                 [GFDL-MOM6-COBALT2 dataset] (v1.0). ISIMIP Repository. DOI:", 
                 tags$a(href = "https://doi.org/10.48364/ISIMIP.920945", 
                        "10.48364/ISIMIP.920945")),
               p("If using WOA23 data, please refer to their ", 
                 tags$a(href = 
                          "https://www.ncei.noaa.gov/products/world-ocean-atlas", 
               "product documentation"), " for the most appropriate citation."),
               p("The fishing and catch data should be cited as follows:"),
               p("- Camilla Novaglio, Yannick Rousseau, Reg A. Watson, Julia L.
                 Blanchard (2024): ISIMIP3a reconstructed fishing activity data 
                 (v1.0). ISIMIP Repository. DOI: ", 
                 tags$a(href = "https://doi.org/10.48364/ISIMIP.240282",
                        "10.48364/ISIMIP.240282")),
               br(),
               h3(strong("How can I contact you?")),
               p("If you are interested in our regional modelling work and would like to be 
                 part of the FishMIP community, you can head to the ",
                 tags$a(href =  "https://fishmip.org/joinus.html", "'Join us'"),
                 " section of our website for more information."),
               p("If you would like to suggest changes or have spotted an 
                 issue with this app, you can create an issue in our ",
                 tags$a(href = "https://github.com/Fish-MIP/FishMIP_Input_Explorer/issues",
                        "GitHub repository.")),
               br(),
               h4(strong("Acknowledgments")),
               p("The development of this tool was funded by the Australian 
                 Government through the Australian Research Council (ARC) 
                 Future Fellowship Project FT210100798. This tool is supported
                 by the use of the ARDC Nectar Research Cloud, a collaborative 
                 Australian research platform supported by the NCRIS-funded 
                 Australian Research Data Commons (ARDC). We gratefully 
                 acknowledge contributions from coordinators and contributing
                 modellers of the FishMIP and ISIMIP communities. We would also
                 like to acknowledge OceanHackWeek participants for contributing
                 to the development of this tool. Finally, we would also like 
                 to acknowledge the use of computing facilities provided by 
                 Digital Research Services, IT Services at the University of 
                 Tasmania."),
               br(),
               br(),
               fluidRow(
                 column(4, img(src = "IMAS_logo.png", height = 150, width = 300,
                               style = "display: block; margin-left: auto; 
                               margin-right: auto")),
                 column(4, br(),
                        img(src = "FishMIP_logo.jpg", height = 125,
                               width = 350,
                               style = "display: block; margin-left: auto; 
                               margin-right: auto")),
                 column(4, img(src = "UN_OceanDecadeLogo_cropped.png", 
                               height = 145, width = 280,
                               style = "display: block; margin-left: auto; 
                               margin-right: auto"))),
               br()
             )
    )
  )
)

# Define actions ---------------------------------------------------------------
server <- function(input, output, session) {

  ## Model tab -----------------------------------------------------------------
  # Merging region and variable information to filter files
  lookup_file <- reactive({
    #Get variable metadata
    var_meta <- var_metadata |> 
      filter(gfdl_name == input$variable_gfdl) 
    #Variable long name
    var <- var_meta$long_name.gfdl
    
    ## Create title for colour bar
    unit <- var_meta$units.gfdl
    if(unit == "1"){
      unit <- "unitless"
    }else if(unit == "0.001"){
      unit <-  "parts per thousand"
    }
    cb_lab <- paste0(var, " (", unit, ")")
    
    
    #Create keywords to search files
    search_file <- paste0("_", input$variable_gfdl, "_.*_", 
                          input$region_gfdl, "_")
    
    #Return items
    return(list(search_file = search_file,
                long_name = var,
                cb_lab = cb_lab))
  })
  
  # Loading relevant data
  gfdl_data <- reactive({
    #Loading maps dataset
    df_map <- list.files(maps_files, pattern = lookup_file()$search_file, 
                            full.names = T) |> 
      read_parquet(col_select = lat:vals)
    #Loading time series dataset
    df_ts <- tryCatch({
      list.files(ts_files, pattern = lookup_file()$search_file, 
                        full.names = T) |> 
        read_parquet(col_select = time:vals)
      },
      error = function(cond){
        NA
      })
    #Loading region boundaries
    shp_map <- fish_reg |> 
      filter(region == str_replace_all(input$region_gfdl, "-", " "))
    
    return(list(df_map = df_map,
                df_ts = df_ts,
                shp_map = shp_map))
  })
  
  observeEvent(gfdl_data(), {
    #Getting depth information
    if("depth_bin_m" %in% colnames(gfdl_data()$df_map)){
      depths <- unique(gfdl_data()$df_map$depth_bin_m)
    }else{
      depths <- NULL
    }
    updateSelectizeInput(session, "depth_gfdl",
                         choices = depths, server = T)
    })

  gfdl_maps_df <- reactive({
    df <- gfdl_data()$df_map
    if("depth_bin_m" %in% colnames(df)){
      df <- df |>
        filter(depth_bin_m == input$depth_gfdl)
    }
    
    validate(
      need(df$lon != "",
           "Rendering map"))
    
    range_map <- range_map(df, input$region_gfdl)
    
    if(max(range_map$df$lon) > 180){
      shp_map <- gfdl_data()$shp_map |> 
        st_shift_longitude()
    }else{
      shp_map <- gfdl_data()$shp_map
    }

    title <- paste0("Climatological mean (1961-2010) ",
                        lookup_file()$long_name) |>
      str_to_sentence()

    return(list(df = range_map$df,
                shp_map = shp_map,
                ylim = range_map$ylims,
                xlim = range_map$xlims,
                title = title))
  })

  # Creating first plot
  output$map_gfdl <- renderPlot({
    df <- gfdl_maps_df()$df
    shp_map <- gfdl_maps_df()$shp_map
    
    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
      geom_sf(inherit.aes = F, data = shp_map, colour = "red", fill = NA, 
              linewidth = 0.75)+
      coord_sf(ylim = gfdl_maps_df()$ylim, xlim = gfdl_maps_df()$xlim,
               expand = F) +
      guides(fill = guide_colorbar(title = lookup_file()$cb_lab,
                                   title.position = "top", title.hjust = 0.5))+
      labs(title = str_wrap(gfdl_maps_df()$title, 65))
  },
  height = 600, width = 750)


  gfdl_ts_df <- reactive({
    df <- gfdl_data()$df_ts
    if("depth" %in% colnames(df)){
      df <- df |>
        filter(depth == input$depth_gfdl)
    }

    title <- paste0("Area weighted mean (1961-2010) for ",
                    lookup_file()$long_name) |>
      str_to_sentence()

    return(list(df = df,
                title = title))
  })

  output$ts_gfdl <- renderPlot({
    df <- gfdl_ts_df()$df
    
    validate(
      need(!is.na(df),
           paste0("Fixed variable selected from dropdown list.\n",
                  "Time series is not available for this variable.")))

    # Calculate spatially weighted average of variables selected
    ggplot(df, aes(x = time, y = vals)) +
      geom_line(aes(color = "area weighted monthly mean")) +
      geom_smooth(aes(color = "linear temporal trend")) +
      scale_color_manual(breaks = c("area weighted monthly mean",
                                    "linear temporal trend"),
                         values = c("#004488", "#bb5566")) +
      scale_x_date(date_labels = "%b-%Y", date_breaks = "24 months",
                   expand = expansion(0.02)) +
      guides(color = guide_legend(title = element_blank())) +
      prettyts_theme +
      labs(title = str_wrap(gfdl_ts_df()$title, 60),
           y = str_wrap(lookup_file()$cb_lab, 50))
  }, height = 500, width = 800)

  #Loading download dataset
  gfdl_down_path <- reactive({
    file_path <- str_subset(download_files, lookup_file()$search_file)
    if(str_detect(file_path, "parquet$")){
      file_out <- basename(file_path) |> 
        str_replace(".parquet", ".csv")
    }else{
      file_out <- basename(file_path)
    }
    return(list(file_path = file_path,
                file_out = file_out))
  })
  
  
  gfdl_down_data <- reactive({
    file_path <- gfdl_down_path()$file_path
    if(str_detect(file_path, "parquet$")){
      df <- read_parquet(file_path)
    }else{
      df <- file_path
    }
    return(df)
  })

  output$download_gfdl <- downloadHandler(
    filename = function(){
      gfdl_down_path()$file_out
      },
    # Creating name of download file based on original file name
    content = function(file){
      id <- showNotification("Preparing Download...", type = "message",
                             duration = NULL, closeButton = F)
      df <- gfdl_down_data()
      if(str_detect(file, ".csv$")){
        write_csv_arrow(df, file)
      }else{
        file.copy(df, file)
      }
      on.exit(removeNotification(id), add = TRUE)
      }
  )
  
  # output$test <- renderPrint({
  #   c(head(gfdl_down_data()),
  #   gfdl_down_path()$file_out)
  #   })

  ## Observations tab ----------------------------------------------------------

  # Select correct file based on inputs from region and variable selected
  lookup_woa <- reactive({
    #Get variable metadata
    var_meta <- var_metadata |>
      select(contains("woa")) |>
      distinct() |>
      filter(woa_name_code == input$variable_WOA)

    ## Create title for colour bar
    var <- var_meta$standard_name.woa
    unit <- var_meta$units.woa
    if(unit == "1"){
      unit <- "unitless"
    }
    cb_lab <- paste0(var, " (", unit, ")")
    
    #Create keywords to search files
    search_file <- paste0("_", input$region_WOA, "_.*mean.*_", 
                          input$variable_WOA)
    
    count_file <- paste0("_", input$region_WOA, "_number_obs_", 
                         input$variable_WOA)

    #Return items
    return(list(search_file = search_file,
                count_file = count_file,
                long_name = var,
                cb_lab = cb_lab))
  })

  woa_data <- reactive({
    #Loading maps dataset
    df_map <- list.files(woa_maps, pattern = lookup_woa()$search_file,
                         full.names = T) |>
      read_parquet(col_select = lat:vals) |> 
      drop_na(vals)
    #Loading time series dataset
    df_ts <- list.files(woa_ts, pattern = lookup_woa()$search_file,
                        full.names = T) |>
      read_parquet(col_select = month:weighted_sd) |>
      mutate(month = factor(month, levels = month.name, ordered = T))
    
    #Loading region boundaries
    shp_map <- fish_reg |> 
      filter(region == str_replace_all(input$region_WOA, "-", " "))
    
    #Getting depth information
    depths <- unique(df_map$depth)
    
    return(list(df_map = df_map,
                df_ts = df_ts,
                depths = depths,
                shp_map = shp_map))
  })

  observeEvent(woa_data(), {
    updateSelectizeInput(session, "depth_woa",
                         choices = woa_data()$depths, server = T)
  })

  woa_maps_df <- reactive({
    df <- woa_data()$df_map |>
      filter(depth == input$depth_woa)
    
    validate(
      need(df$lon != "",
           "Rendering map"))
    
    # Adjusting map proportions
    range_map <- range_map(df, input$region_WOA)
    
    if(max(range_map$df$lon) > 180){
      shp_map <- woa_data()$shp_map |> 
        st_shift_longitude()
    }else{
      shp_map <- woa_data()$shp_map
    }
    
    title <- paste0("Climatological mean (1981-2010) ",
                    lookup_woa()$long_name) |>
      str_to_sentence()

    return(list(df = range_map$df,
                shp_map = shp_map,
                ylim = range_map$ylims,
                xlim = range_map$xlims,
                title = title))
  })

  # Creating first plot
  output$map_WOA <- renderPlot({
    df <- woa_maps_df()$df
    shp_map <- woa_maps_df()$shp_map

    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
      geom_sf(inherit.aes = F, data = shp_map, colour = "red", fill = NA, 
              linewidth = 0.75)+
      coord_sf(ylim = woa_maps_df()$ylim, xlim = woa_maps_df()$xlim, 
               expand = F) +
      guides(fill = guide_colorbar(title = lookup_woa()$cb_lab,
                                   title.position = "top", title.hjust = 0.5))+
      labs(title = str_wrap(woa_maps_df()$title, 65))
  },
  height = 600, width = 750)

  woa_ts_df <- reactive({
    df <- woa_data()$df_ts |>
        filter(depth == input$depth_woa)
    
  title <- paste0("Area weighted climatological monthly mean (1981-2010) for ",
                    lookup_woa()$long_name) |>
      str_to_sentence()

    return(list(df = df,
                title = title))
  })


  output$ts_WOA <- renderPlot({
    df <- woa_ts_df()$df

    ggplot(df, aes(x = month, y = weighted_mean, group = 1)) +
      geom_line(color = "#004488") +
      geom_ribbon(aes(ymin = weighted_mean-weighted_sd,
                      ymax = weighted_mean+weighted_sd),
                  alpha = 0.2, color = "grey") +
      prettyts_theme +
      labs(title = str_wrap(woa_ts_df()$title, 60),
           y = str_wrap(lookup_woa()$cb_lab, 50))
  }, height = 500, width = 800)
  
  
  output$count_WOA <- renderPlot({
    df <- list.files(woa_maps, pattern = lookup_woa()$count_file,
               full.names = T) |>
      read_parquet(col_select = lat:vals) |>
      filter(depth == input$depth_woa)
    
    validate(
      need(df$lon != "",
           "Rendering map"))
    
    # Adjusting map proportions
    range_map <- range_map(df, input$region_WOA)
    
    title <- paste0("Number of available observations (1981-2010) for ",
                    lookup_woa()$long_name) |>
      str_to_sentence()
    
    shp_map <- woa_maps_df()$shp_map
    
    ggplot(range_map$df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
      geom_sf(inherit.aes = F, data = shp_map, colour = "red", fill = NA, 
              linewidth = 0.75)+
      coord_sf(ylim = range_map$ylims, xlim = range_map$xlims, 
               expand = F) +
      guides(fill = guide_colorbar(title = "Number of observations",
                                   title.position = "top", title.hjust = 0.5)) +
      labs(title = str_wrap(title, 65))
  }, height = 600, width = 750)
  
  # Getting data ready for download
  woa_down_data <- reactive({
    file_path <- list.files(woa_maps, pattern = lookup_woa()$search_file,
                            full.names = T)
    df <- read_parquet(file_path)
    file_out <- basename(file_path) |> 
        str_replace(".parquet", ".csv")
    return(list(df = df,
                file_path = file_out))
  })

  output$download_WOA <- downloadHandler(
    filename = function(){
      woa_down_data()$file_path
      },
    # Creating name of download file based on original file name
    content = function(file){
      df <- woa_down_data()$df
      id <- showNotification("Preparing Download...", type = "message", 
                             duration = NULL, closeButton = F)
      on.exit(removeNotification(id), add = TRUE)
      write_csv_arrow(df, file)
    }
  )

  ## Comparison tab ----------------------------------------------------------
  lookup_comp <- reactive({
    #Get variable metadata
    var_meta <- var_metadata |>
      select(contains("woa")) |>
      distinct() |>
      filter(woa_name_code == input$variable_compare)
    
    ## Create title for colour bar
    var <- var_meta$standard_name.woa
    unit <- var_meta$units.woa
    if(unit == "1"){
      unit <- "unitless"
    }
    cb_lab_diff <- paste0("Difference in ", var, " (", unit, ")")
    
    #Create keywords to search files
    diff_file <- paste0("^diff.*_", input$region_compare, "_", 
                        input$variable_compare)
    
    ts_file <- paste0(input$variable_compare, "_", input$region_compare)
    
    #Return items
    return(list(diff_file = diff_file,
                ts_file = ts_file,
                long_name = var,
                cb_lab_diff = cb_lab_diff))
  })
  
  comp_data <- reactive({
    #Loading maps dataset
    diff_map <- list.files(map_comp_files, pattern = lookup_comp()$diff_file,
                           full.names = T) |>
      read_parquet()
    #Loading time series dataset
    ts <- list.files(ts_comp_files, pattern = lookup_comp()$ts_file,
                     full.names = T) |>
      read_parquet()
    
    #Loading region boundaries
    shp_map <- fish_reg |> 
      filter(region == str_replace_all(input$region_compare, "-", " "))
    
    #Getting depth information
    depths <- unique(diff_map$depth)
    
    return(list(diff_map = diff_map,
                ts = ts,
                depths = depths,
                shp_map = shp_map))
  })
  
  observeEvent(comp_data(), {
    updateSelectizeInput(session, "depth_comp",
                         choices = comp_data()$depths, server = T)
  })
  
  comp_maps_diff <- reactive({
    df <- comp_data()$diff_map |>
      filter(depth == input$depth_comp)
    
    validate(
      need(df$lon != "",
           "Rendering map"))
    
    # Adjusting map proportions
    range_map <- range_map(df, input$region_compare)
    
    if(max(range_map$df$lon) > 180){
      shp_map <- comp_data()$shp_map |> 
        st_shift_longitude()
    }else{
      shp_map <- comp_data()$shp_map
    }
    
    title <- paste0("Difference in climatological mean (1981-2010) ",
                    lookup_comp()$long_name) |>
      str_to_sentence()
    
    return(list(df = range_map$df,
                shp_map = shp_map,
                ylim = range_map$ylims,
                xlim = range_map$xlims,
                title = title))
  })
  
  # Creating first plot
  output$map_compare <- renderPlot({
    df <- comp_maps_diff()$df
    shp_map <- comp_maps_diff()$shp_map
    
    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
      geom_sf(inherit.aes = F, data = shp_map, colour = "red", fill = NA, 
              linewidth = 0.75)+
      coord_sf(ylim = comp_maps_diff()$ylim, xlim = comp_maps_diff()$xlim, 
               expand = F) +
      guides(fill = guide_colorbar(title = lookup_comp()$cb_lab_diff,
                                   title.position = "top", title.hjust = 0.5)) +
      labs(title = str_wrap(comp_maps_diff()$title, 65))
  },
  height = 600, width = 750)
  
  
  comp_ts_df <- reactive({
    df <- comp_data()$ts |>
      filter(depth == input$depth_comp)
    
  title <- paste0("Area weighted climatological monthly mean (1981-2010) for ",
                    lookup_comp()$long_name) |>
      str_to_sentence()
    
    return(list(df = df,
                title = title))
  })
  
  output$ts_compare <- renderPlot({
    df <- comp_ts_df()$df
    
    ggplot(df, aes(x = month, y = vals, group = type, colour = type,
                   linetype = type)) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = c("gfdl" = "#997700", 
                                    "woa" = "#994455"),
                         labels = c("GFDL-MOM6-COBALT2", "WOA2023")) +
      scale_linetype_manual(values = c("gfdl" = "twodash",
                                       "woa" = "solid"),
                            labels = c("GFDL-MOM6-COBALT2", "WOA2023")) +
      prettyts_theme +
      labs(title = str_wrap(comp_ts_df()$title, 60),
           y = str_wrap(lookup_comp()$cb_lab, 50))+
      theme(legend.title = element_blank())
  }, height = 500, width = 800)
  
  
  comp_down_data <- reactive({
    file_path <- list.files(file.path(fishmip_dir, "download_correction"),
                            pattern = lookup_comp()$ts_file, full.names = T)
  })

  output$download_comp <- downloadHandler(
    filename = function(){
      basename(comp_down_data())
    },
    # Creating name of download file based on original file name
    content = function(file){
      id <- showNotification("Preparing Download...", type = "message",
                             duration = NULL, closeButton = F)
      df <- comp_down_data()
      file.copy(df, file)
      on.exit(removeNotification(id), add = TRUE)
    }
  )
  
  
  ## Catch and effort tab ------------------------------------------------------
  # Update `variable_effort` choices based on dataset selection; this is because
  # the catch dataset does not have a "Gear" column
  # Loading relevant data based on selection
  selected_data <- reactive({
    if(input$catch_effort_select == "effort"){
      data <- effort_regional_ts
      groups <- effort_variables
      y_axis_label <- "Nominal fishing effort\n(million kW days)"
    }else{
      data <- catch_regional_ts
      groups <- catch_variables
      y_axis_label <- "Catch (thousand tonnes)"
    }
    return(list(data = data,
                groups = groups,
                y_axis = y_axis_label))
  })
  
  observeEvent(selected_data(), {
    updateSelectInput(session, "variable_effort",
                      choices = selected_data()$groups, 
                      selected = selected_data()$groups[1])
  })
  
  # Filtered data for plotting and downloading
  filtered_data <- reactive({
    req(selected_data()$data)
    df <- selected_data()$data |>
      filter(region == input$region_effort) 
    if(input$variable_effort == "Functional Group"){
      df <- df |> 
        group_by(year, !!sym(input$variable_effort), f_group_index)
    }else{
      df <- df |>
        group_by(year, !!sym(input$variable_effort))
    }
    df |>
      summarise(value = ifelse(input$catch_effort_select == "effort",
                               sum(nom_active_million, na.rm = TRUE),
                               sum(catch_thousands, na.rm = TRUE))) |>
      ungroup() |>
      mutate(information = glue("<br>Year: {year}<br>{input$variable_effort}: 
                                {get(input$variable_effort)}<br>Value: {value}"))
  })
  
  output$ts_effort <- renderPlotly({
    # Plotting data
    df <- filtered_data()
    
    # Create ggplot
    
  if("f_group_index" %in% names(df)){
    p <- ggplot(df, aes(x = year, y = value, 
                        fill = fct_reorder(!!sym(input$variable_effort),
                                           f_group_index),
                        label = information))+
      labs(y = selected_data()$y_axis)+
      fishing_theme+
      guides(fill = guide_legend(title.position = "top", 
                                 title = "Functional Group",
                                 title.hjust = 0.5))
  }else{
    p <- ggplot(df, aes(x = year, y = value, 
                        fill = !!sym(input$variable_effort), 
                        label = information))+
      labs(y = selected_data()$y_axis)+
      fishing_theme
  }
    
    # Convert ggplot to an interactive plot with ggplotly
    ggplotly(p, tooltip = 'label', height = 600, width = 800) |>
      layout(
        legend = list(orientation = "h", xanchor = "center", x = 0.5,
                      yanchor = "top", y = -0.2, font = list(size = 12),
                      traceorder = "normal", title = list(side = "top")),
        margin = list(l = 20, r = 20, t = 10, b = 10)
      )
  })
  
  download_fishing_data <- reactive({
    base_eff_cat <- file.path("/rd/gem/public/fishmip/ISIMIP3a/InputData",
                              "effort_catch_data")
    regname <- str_to_lower(input$region_effort) |> 
      str_replace_all(" ", "-") |> 
      str_remove_all("'")
    
    temp_dir <- tempdir()
    #Ensure temporary folder is empty
    file.remove(list.files(temp_dir, full.names = T))
    
    #Copy SAUP file to temporary folder
    file.copy(file.path(base_eff_cat, "SAUPtoCountry.csv"), temp_dir)
    
    if(input$catch_effort_select == "effort"){
      da <- read_parquet(
        file.path(base_eff_cat, "effort_1841_2017_regional_download.parquet"))
      
      fout <- paste0("effort_histsoc_1841_2017_", regname)
      #Copy dictionary file to temporary folder
      file.copy(file.path(base_eff_cat, "effort_dictionary.parquet"), temp_dir)
    }else{
      da <- read_parquet(
        file.path(base_eff_cat, "catch_1850_2017_regional_download.parquet"))
      #Copy dictionary file to temporary folder
      file.copy(file.path(base_eff_cat, "catch_dictionary.parquet"), temp_dir)
      fout <- paste0("calibration_catch_histsoc_1850_2017_", regname)
    }
    
    #Extract rows for region of interest
    da |> 
      filter(region == input$region_effort) |> 
      write_parquet(file.path(temp_dir, paste0(fout, ".parquet")))
    
    zip_out <- file.path(temp_dir, paste0(fout, ".zip"))
    
    zip(zip_out, list.files(temp_dir, pattern = ".parquet|.csv",
                            full.names = T))
   
    return(zip_out)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      basename(download_fishing_data())
    },
    content = function(file) {
      id <- showNotification("Preparing Download...", type = "message",
                             duration = NULL, closeButton = F)
      df <- download_fishing_data()
      file.copy(df, file)
      on.exit(removeNotification(id), add = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
