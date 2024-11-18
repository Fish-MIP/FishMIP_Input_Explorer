# Loading libraries -------------------------------------------------------
library(arrow)
library(shiny)
library(shinyWidgets)
library(shinycustomloader)
library(readr)
library(bslib)
library(tibble)
library(stringr)
library(dplyr)
library(rnaturalearth)
library(tidyr)
library(sf)
library(ggplot2)
library(data.table)
library(plotly)
library(glue)
options(scipen = 99)

# Setting up  -------------------------------------------------------------
# Get list of all regions available
region_keys <- read_csv("www/FishMIP_regions_keys.csv", col_select = !id, 
                        show_col_types = F) |> 
  deframe()

# Getting names of environmental variables available with equivalent from WOA
var_metadata <- read_csv("www/woa_gfdl_var_keys.csv", show_col_types = F) 

  #GFDL variables - named vector
gfdl_variables <- var_metadata |> 
  select(long_name.gfdl, gfdl_name) |> 
  arrange(long_name.gfdl) |> 
  deframe()

woa_variables <- var_metadata |> 
  distinct(standard_name.woa, woa_name_code) |> 
  drop_na() |> 
  deframe()

# First tab data ----------------------------------------------------------
# Folders containing Earth System Model (ESM) data
fishmip_dir <- file.path("/home/ubuntu/gem/public/fishmip/ISIMIP3a/InputData/climate",
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
woa_maps <- "/home/ubuntu/gem/public/fishmip/WOA_data/regional/climatology"
woa_ts <- "/home/ubuntu/gem/public/fishmip/WOA_data/regional/monthly/ts"


# Loading map of the world
world <- ne_countries(returnclass = "sf", scale = "medium")

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


## Catch and effort data and variable choices

effort_regional_ts <- fread(file.path("/home/ubuntu/gem/private/users/yannickr",
                            "DKRZ_EffortFiles",
                            "effort_histsoc_1841_2017_regional_models.csv")) |>
  filter(region != "") |>
  mutate(NomActive = as.numeric(NomActive)) |>
  rename("Functional Group" = "FGroup")

catch_regional_ts <- fread(
  file.path("/home/ubuntu/gem/private/users/yannickr",
            "DKRZ_EffortFiles",
            "calibration_catch_histsoc_1850_2017_regional_models.csv")) |>
  mutate(catch = Reported + IUU + Discards) |>
  rename("Functional Group" = "FGroup")
# how to make these load faster... It currently takes like 20 seconds..

effort_catch_region_keys <- unique(effort_regional_ts$region)

# Define variables for each dataset type
effort_variables <- c("Functional Group", "Sector", "Gear")
catch_variables <- c("Functional Group", "Sector")  # without "Gear"


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
                   these plots by clicking the 'Download' button below."),
                 # Download option
                 downloadButton(outputId = "download_gfdl", 
                                label = "Download")
               ),
               mainPanel(
                 br(),
                 # verbatimTextOutput("test"),
                 "Figures shown in this tab use the ", 
                 em("Observation-based climate related forcing"), " (obsclim) 
                 outputs from the GFDL-MOM6-COBALT2 model. These outputs were
                 obtained from the ", 
                 tags$a(href = 
                          "https://data.isimip.org/search/tree/ISIMIP3a/InputDat
                        a/climate/ocean/gfdl-mom6-cobalt2/obsclim/", 
                        "ISIMIP Data Repository."),
                 br(), br(),
                 "The climatological map shows the mean conditions for the 
                 environmental variable selected on the left panel. These 
                 climatologies use the entire period covered by the model: 
                 1961-2010.",
                 br(), br(),
                 "Time series show the weighted mean for each time step in the 
                 model data. We used grid cell area as weighting.",  
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
                 ", specifically we used the ", 
                 em("objectively analysed climatologies"), "field to create the
                 climatological maps and time series, and the ",
                 em("number of observations "), "field for the maps available in
                 the sub-tab under the same name.",  
                 br(), br(),
                 "The maps in this tab show WOA23 data as is, data was extracted
                 within the boundaries of FishMIP regional models and no further
                 data processing was done. You can download this data using 
                 the ", em('Download'), " button on the left. Note that if you 
                 would like to compare WOA23 data to GFDL outputs or any other 
                 data product, you will need to regrid the WOA23 data to match 
                 the data you are comparing it to. We have an ",
                 tags$a(href = "https://github.com/Fish-MIP/processing_WOA_data
                        /blob/main/scripts/P_regridding_woa_data.ipynb",
                        "example notebook"), " showing you step by step how to 
                 regrid data.",
                 br(), br(),
                 "For time series plots we weighted the monthly climatology by
                 the area of the grid cells.",
                 br(),
                 br(),
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
                 observations.")
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
                 br(),
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
    tabPanel("Explore regional fishing effort and catch",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose catch or effort data
                 p("1. Select dataset to view:"),
                 selectInput(inputId = "catch_effort_select", label = NULL,
                             choices = c("Fishing Effort", "Catch"), 
                             selected = "Fishing Effort"),
                 
                 # Choose region of interest
                 p("2. Select a FishMIP region:"),
                 selectInput(inputId = "region_effort", label = NULL,
                             choices = effort_catch_region_keys, 
                             selected = "East Bass Strait"),
                 
                 # Choose variable of interest
                 p("3. Select variable to view:"),
                 selectInput(inputId = "variable_effort", 
                             label = NULL,
                             choices = effort_variables,
                             selected = "Functional Group"),
                 
                 # Inline layout for download button
                 fluidRow(
                   column(6, p(em("Optional: "), "Get a copy of the data used 
                   to create 
                   these plots by clicking the 'Download Data' button")),
                   column(6, downloadButton(outputId = "download_data", 
                                            label = "Download Data"))
                 )
               ),
               mainPanel(br(),
                        "Figures shown in this tab use fishing effort data from 
                        ", 
                        tags$a(href = 
                        "https://www.nature.com/articles/s41597-023-02824-6", 
                        "A database of mapped global fishing activity 1950–2017 
                        (Rousseau et al. 2024)"), 
                        "and catch data from ", tags$a(href = 
                        "https://metadata.imas.utas.edu.au/geonetwork/srv/api
                        /records/5c4590d3-a45a-4d37-bf8b-ecd145cb356d?language=eng",
                        "Global Fisheries Landings V4.0 (Watson 2017) "),
                                       br(),
                 tabPanel("",
                          mainPanel(
                            br(), 
                            withLoader(plotlyOutput(outputId = "ts_effort", 
                                       width = "100%", height = "500px"),
                                       type = "html", loader = "myloader")
                          )
                 )
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
                 needs to be 
                 applied to the data prior to its use as forcings of a regional
                 marine ecosystem model."),
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
               img(src = "FishMIP_regional_models.png", height = 590,
                   width = 775, style = "display: block; 
                                          margin-left: auto;
                                          margin-right: auto"),
               br(),
               h3(strong("How should I use this tool?")),
               p("This site has three main tabs:"),
               p("1.", em(strong("GFDL model outputs"))),
               p("2.", em(strong("World Ocean Atlas 2023 data"))),
               p("3.", em(strong("Model outputs against observations"))),
               p("You can download data that has been subsetted for the 
                 regional model of your interest in the first two tabs. Note 
                 that all WOA 2023 data are available for download as ",
                 em("csv"), " files. GFDL outputs that do not have a depth 
                 component (i.e., surface or bottom data) are also available as
                 ", em("csv"), " files."),
               p("However, due to the size of GFDL outputs with a depth 
                 component (e.g. temperature of the water column), these data 
                 are only available for download as ", em("zip"), " (i.e., 
                 compressed) folders containing ", em("Zarr"), " files
                 to speed up download times. A ", em("Zarr"), " file is a cloud
                 optimised gridded data file format similar to ", em("netcdf"),
                 " files. If you use Python, we recommend you use the ", 
                 em("xarray"), " library to open these files. If you use R, we 
                 recommend the ", em("Rarrr"), " library. For instructions on 
                 how to load these files in R, refer to ",
                 tags$a(href = "https://github.com/Fish-MIP/FishMIP_extracting-data/blob/main/scripts/loading_zarr_files.md", 
                        "this example.")),
               br(),
               h3(strong("How should I cite data from this site?")),
               p("You can download the data used to create the plots shown in 
                 this interactive tool using the 'Download' button included 
                 under each tab. As a condition of this tool to access data, 
                 you must cite its use. Please use the following citations:"),
               p("- Fierro-Arcos, D., Blanchard, J. L., Flynn, C., 
                 Ortega Cisneros, K., Reimer, T. (2024). FishMIP input explorer 
                 for regional ecosystem modellers. ", 
                 tags$a(href = "https://rstudio.global-ecosystem-model.cloud.edu.au/shiny/FishMIP_Input_Explorer/")),
               p("When using the data products in a publication, please include 
               the following citation in addition to the data product citation 
               provided above:"),
               p("- Ortega-Cisneros, K., Fierro-Arcos, D. Lindmark, M., et al.
                 (Preprint). An Integrated Global-to-Regional Scale Workflow for
                 Simulating Climate Change Impacts on Marine Ecosystems. ESS 
                 Open Archive. DOI:", 
                 tags$a(href ="http://dx.doi.org/10.22541/essoar.171587234.44707846/v1",
                 "10.22541/essoar.171587234.44707846/v1")),
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
                 Future Fellowship Project FT210100798. We gratefully 
                 acknowledge contributions 
                 from coordinators and contributing modellers of the FishMIP 
                 and ISIMIP communities. We would also like to acknowledge 
                 OceanHackWeek participants for contributing to the development 
                 of this tool. Finally, we would also like to acknowledge the 
                 use of computing facilities provided by Digital Research 
                 Services, IT Services at the University of Tasmania."),
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
                               height = 150, width = 300,
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
    }
    cb_lab <- paste0(var, " (", unit, ")")
    
    
    #Create keywords to search files
    search_file <- paste0("_", input$variable_gfdl, "_.*_", input$region_gfdl)
    
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
    df_ts <- list.files(ts_files, pattern = lookup_file()$search_file, 
                        full.names = T) |> 
      read_parquet(col_select = time:vals)
    
    return(list(df_map = df_map,
                df_ts = df_ts))
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

    title <- paste0("Climatological mean (1961-2010) ",
                        lookup_file()$long_name) |>
      str_to_sentence()

    return(list(df = range_map$df,
                ylim = range_map$ylims,
                xlim = range_map$xlims,
                title = title))
  })
  

  # Creating first plot
  output$map_gfdl <- renderPlot({
    df <- gfdl_maps_df()$df
    
    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
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
      df <- gfdl_down_data()
      id <- showNotification("Preparing Download...", type = "message", 
                             duration = NULL, closeButton = F)
      on.exit(removeNotification(id), add = TRUE)
      Sys.sleep(1)
      
      notify("Getting everything together...", id = id)
      Sys.sleep(1)
      
      notify("Almost there...", id = id)
      Sys.sleep(1)
      if(str_detect(file, "csv$")){
        write_csv(df, file)
      }else{
        file.copy(df, file)
      }
      }
  )
  
  # output$test <- renderPrint({
  #   c(gfdl_down_data(),
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
      # drop_na(vals) |> 
      mutate(month = factor(month, levels = month.name, ordered = T))
    
    #Getting depth information
    depths <- unique(df_map$depth)
    
    return(list(df_map = df_map,
                df_ts = df_ts,
                depths = depths))
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
    
    title <- paste0("Climatological mean (1981-2010) ",
                    lookup_woa()$long_name) |>
      str_to_sentence()

    return(list(df = range_map$df,
                ylim = range_map$ylims,
                xlim = range_map$xlims,
                title = title))
  })

  # Creating first plot
  output$map_WOA <- renderPlot({
    df <- woa_maps_df()$df

    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
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
    
    ggplot(range_map$df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
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
      id <- showNotification("Preparing Download...", type = "message", 
                             duration = NULL, closeButton = F)
      on.exit(removeNotification(id), add = TRUE)
      Sys.sleep(1)
      
      notify("Getting everything together...", id = id)
      Sys.sleep(1)
      
      notify("Almost there...", id = id)
      Sys.sleep(1)
      
      write_csv(woa_down_data()$df, file)
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
    cb_lab_per <- paste0("Percentage difference in ", var)
    
    #Create keywords to search files
    diff_file <- paste0("^diff.*", input$region_compare, "_", 
                        input$variable_compare)
    
    per_file <- paste0("^per.*", input$region_compare, "_", 
                       input$variable_compare)
    
    ts_file <- paste0(input$variable_compare, "_", input$region_compare)
    
    #Return items
    return(list(diff_file = diff_file,
                per_file = per_file,
                ts_file = ts_file,
                long_name = var,
                cb_lab_diff = cb_lab_diff,
                cb_lab_per = cb_lab_per))
  })
  
  comp_data <- reactive({
    #Loading maps dataset
    diff_map <- list.files(map_comp_files, pattern = lookup_comp()$diff_file,
                           full.names = T) |>
      read_parquet()
    #Loading time series dataset
    per_map <- list.files(map_comp_files, pattern = lookup_comp()$per_file,
                          full.names = T) |>
      read_parquet()
    
    ts <- list.files(ts_comp_files, pattern = lookup_comp()$ts_file,
                     full.names = T) |>
      read_parquet()
    
    #Getting depth information
    depths <- unique(diff_map$depth)
    
    return(list(diff_map = diff_map,
                per_map = per_map,
                ts = ts,
                depths = depths))
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
    
    title <- paste0("Difference in climatological mean (1981-2010) ",
                    lookup_comp()$long_name) |>
      str_to_sentence()
    
    return(list(df = range_map$df,
                ylim = range_map$ylims,
                xlim = range_map$xlims,
                title = title))
  })
  
  # Creating first plot
  output$map_compare <- renderPlot({
    df <- comp_maps_diff()$df
    
    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
      coord_sf(ylim = comp_maps_diff()$ylim, xlim = comp_maps_diff()$xlim, 
               expand = F) +
      guides(fill = guide_colorbar(title = lookup_comp()$cb_lab_diff,
                                   title.position = "top", title.hjust = 0.5))+
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
  
  # Catch and effort tab ----------------------------------------------------------
  # Update `variable_effort` choices based on dataset selection
  observeEvent(input$catch_effort_select, {
    new_choices <- if (
      input$catch_effort_select == "Fishing Effort") effort_variables 
    else catch_variables
    updateSelectInput(session, "variable_effort", 
                      choices = new_choices, selected = new_choices[1])
  })

  # Loading relevant data based on selection
  selected_data <- reactive({
    if (input$catch_effort_select == "Fishing Effort") {
      effort_regional_ts
    } else {
      catch_regional_ts
    }
  })

  # Filtered data for plotting and downloading
  filtered_data <- reactive({
    req(selected_data()) # Ensure data is available
    selected_data() |>
      filter(region == input$region_effort) |>
      group_by(Year, region, !!sym(input$variable_effort)) |>
      summarise(value = ifelse(input$catch_effort_select == "Fishing Effort",
                               sum(NomActive, na.rm = TRUE),
                               sum(catch, na.rm = TRUE))) |>
      ungroup() |>
      filter(value > 0, Year >= 1950) |>
      mutate(Information = glue("<br>Year: {Year}<br>{input$variable_effort}: 
                              {get(input$variable_effort)}<br>Value: {value}"))
  })

  output$ts_effort <- renderPlotly({
    # Plotting data
    df <- filtered_data() |>
      group_by(region, !!sym(input$variable_effort)) |>
      complete(Year = full_seq(Year, 1), fill = list(value = 0)) |>
      ungroup()

    # Define y-axis label based on dataset choice
    y_axis_label <- if (
      input$catch_effort_select == "Fishing Effort") "Nominal Fishing Hours" 
    else "Catch (tonnes)"

    # Create ggplot
    p <- ggplot(df, aes(x = Year, y = value, fill = !!sym(input$variable_effort)
                        , label = Information)) +
      geom_area(stat = "identity", alpha = 0.85, na.rm = TRUE) +
      prettyts_theme +
      labs(y = y_axis_label, x = "Year")

    # Convert ggplot to an interactive plot with ggplotly
    ggplotly(p, tooltip = 'label') |>
      layout(
        height = 600,
        width = 800,
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          yanchor = "top",
          y = -0.4,
          font = list(size = 10),
          traceorder = "normal"
        ),
        legendtitle = list(text = ""),
        margin = list(l = 20, r = 20, t = 20, b = 20)
      )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", input$region_effort, ".csv", sep = "")
    },
    content = function(file) {
      data <- filtered_data() |>
        mutate(data_type = input$catch_effort_select) |>
        dplyr::select(-Information)
      validate(need(nrow(data) > 0, "No data available for download."))
      write.csv(data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
