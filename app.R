# Loading libraries -------------------------------------------------------
library(arrow)
library(shiny)
library(shinyWidgets)
library(readr)
library(bslib)
library(stringr)
library(dplyr)
library(rnaturalearth)
library(tidyr)
library(sf)
library(plotly)
library(ggplot2)
# library(lubridate)
options(scipen = 99)

# Setting up  -------------------------------------------------------------
# Get list of all regions available
region_keys <- read_csv("www/FishMIP_regions_keys.csv", show_col_types = F)

# Getting names of environmental variables available with equivalent from WOA
var_metadata <- read_csv("www/woa_gfdl_var_keys.csv", show_col_types = F)
woa_variables <- var_metadata |> 
  distinct(standard_name.woa) |> 
  drop_na()

# First tab data ----------------------------------------------------------
# Folders containing Earth System Model (ESM) data
fishmip_dir <- file.path("/rd/gem/public/fishmip/ISIMIP3a/InputData/climate",
                         "ocean/obsclim/regional/monthly/historical",
                         "GFDL-MOM6-COBALT2")
# fishmip_dir <- "/g/data/vf71/fishmip_inputs/ISIMIP3a/regional_inputs/obsclim/025deg"

#Get a list of GFDL files available 
#For download
download_files <- list.files(file.path(fishmip_dir, "download_data"), 
                             full.names = T)
#For mapping
maps_files <- file.path(fishmip_dir, "maps_data") |> 
  list.files(full.names = T) |> 
  str_subset("comp_clim", negate = T)
#For time series
ts_files <- file.path(fishmip_dir, "ts_data") |> 
  list.files(full.names = T) |> 
  str_subset("comp_clim", negate = T)

# Getting list of all files within folders
woa_maps <- list.files("/g/data/vf71/WOA_data/regional/climatology", 
                        full.names = T)
woa_ts <- list.files("/g/data/vf71/WOA_data/regional/monthly/comp_clim", 
                     full.names = T)

# Loading map of the world
world <- ne_countries(returnclass = "sf", scale = "medium")

prettymap_theme <- list(geom_tile(),
                        theme_classic(),
                        scale_fill_viridis_c(na.value = NA),
                        geom_sf(inherit.aes = F, data = world, lwd = 0.25, 
                                color = "black", show.legend = F),
                        theme(text = element_text(colour = "black", size = 15),
                              legend.position = "bottom", 
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

prettyts_theme <- list(geom_line(aes(color = "area weighted monthly mean")),
                       geom_smooth(aes(color = "linear temporal trend")),
                       scale_color_manual(breaks = 
                                            c("area weighted monthly mean", 
                                              "linear temporal trend"),
                                          values = c("#004488", "#bb5566")),
                       scale_x_date(date_labels = "%b-%Y", 
                                    date_breaks = "24 months",
                                    expand = expansion(0.02)),
                       guides(color = guide_legend(title = element_blank())),
                       theme_bw(),
                       theme(axis.text.y = element_text(size = 14),
                             axis.text.x = element_text(angle = 45, vjust = 1, 
                                                        hjust = 1, size = 14),
                             axis.title.y = element_text(size = 15), 
                             axis.title.x = element_blank(),
                             plot.title = element_text(hjust = 0.5, size = 15),
                             legend.position = "bottom", 
                             legend.text = element_text(size = 18)))


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
    tabPanel("Model outputs",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose region of interest
                 p("1. Select a FishMIP regional model:"),
                 selectInput(inputId = "region_gfdl", label = NULL,
                             choices = region_keys$region, 
                             selected = "Central North Pacific"),
                 
                 # Choose variable of interest
                 p("2. Select an environmental variable:"),
                 selectInput(inputId = "variable_gfdl", 
                             label = "Choose your variable of interest",
                             choices = var_metadata$long_name.gfdl),
                 
                 # Select depth (if available)
                 selectInput(inputId = "depth_gfdl", 
                             label = "Choose depth you want to visualise",
                             choices = NULL),
                 
                 p("3a. Click on the ", strong('Climatological map'), 
                 " tab on the right to see a map of the 
                     climatological mean (1961-2010)."),
                 p("3b. Click on the ", strong('Time series plot'), 
                 " tab to see a time series of the area-weighted monthly
                     mean and the linear temporal trend."),
                 
                 p(em("Optional: "), "Get a copy of the data used to create 
                   these plots by clicking the 'Download' button below."),
                 # Download option
                 downloadButton(outputId = "download_data_gfdl", 
                                label = "Download")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Climatological map",
                            mainPanel(
                              br(), 
                              plotOutput(outputId = "map_gfdl", width = "100%"))
                   ),
                   tabPanel("Time series plot",
                            mainPanel(
                              br(), 
                              plotOutput(outputId = "ts_gfdl", 
                                               width = "100%"))
                   )
                 )
               )
             )
    ),
    
    ## Observations tab --------------------------------------------------------
    tabPanel("World Ocean Atlas data",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose region of interest
                 p("1. Select a FishMIP regional model:"),
                 selectInput(inputId = "region_WOA", label = NULL,
                             choices = region_keys$region, 
                             selected = "Central North Pacific"),
                 
                 # Choose variable of interest
                 p("2. Select an environmental variable:"),
                 selectInput(inputId = "variable_WOA", label = NULL,
                             choices = woa_variables, 
                             selected = "Sea Water Temperature"),
                 p("3a. Click on the ", strong('Climatological map'),
                 " tab on the right to see a map of the climatological 
                     mean (1981-2010) of observations."),
                 p("3b. Click on the ", strong('Time series plot'), 
                 " tab to see a time series of area-weighted 
                     monthly mean of observations."),
                 
                 p(em("Optional: "), "Get a copy of the data used to create 
                   these plots by clicking the 'Download' button below."),
                 # Download option
                 downloadButton(outputId = "download_WOA", label = "Download")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Climatological map", 
                            mainPanel(
                              br(), plotOutput(outputId = "map_WOA", 
                                               width = "100%"))
                   ),
                   tabPanel("Time series plot",
                            mainPanel(
                              br(), plotOutput(outputId = "ts_WOA", 
                                               width = "100%"))
                   )
                 )
               )
             )
    ),
    ## Comparison tab ----------------------------------------------------------
    tabPanel("Compare model with observations",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Instructions:")),
                 
                 # Choose region of interest
                 p("1. Select a FishMIP regional model:"),
                 selectInput(inputId = "region_compare", label = NULL,
                             choices = region_keys$region, 
                             selected = "Central North Pacific"),
                 
                 # Choose variable of interest
                 p("2. Select an environmental variable:"),
                 selectInput(inputId = "variable_compare", label = NULL,
                             choices = woa_variables,
                             selected = "Sea Water Temperature"),
                 p("3a. Click on the ", strong('Climatological map'), 
                 " tab on the right to see a map of the differences 
                 in the climatological mean (1981-2010) from the model output
                 and observations."),
                 p("3b. Click on the ", strong('Time series plot'), 
                 " tab to see the difference in climatological monthly 
                 area-weighted mean (1981-2010) between the model output and 
                 observations."),
                 p(em("Optional: "), "Get a copy of the data used to create 
                   these plots by clicking the 'Download' button below."),
                 # Download option
                 downloadButton(outputId = "download_data_compare", 
                                label = "Download")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Climatological maps",
                            mainPanel(
                              br(), plotlyOutput(outputId = "map_compare", 
                                                 width = "140%"))
                   ),
                   tabPanel("Time series plot",
                            mainPanel(
                              br(), plotOutput(outputId = "ts_compare", 
                                               width = "100%"))
                   ),
                   tags$head(tags$style(type = "text/css", 
                   "#loadmessage {
                     position: fixed; bottom: 0px; right: 0px; width: 20%; 
                     padding: 10px 0px 10px 0px;
                     text-align: center; font-weight: bold; font-size: 100%; 
                     color: #fff; background-color: #008cba; z-index: 105;
                     }")),
                   conditionalPanel(
                     condition = "$('html').hasClass('shiny-busy')", 
                     tags$div("Loading plot...", id = "loadmessage"))
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
                 observations to determine if bias correction needs to be 
                 applied to the data prior to its use as forcings of a regional
                 marine ecosystem model."),
               br(),
               img(src = "FishMIP_regional_model_workflow.png", height = 600,
                   width = 750, style = "display: block; 
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
               p("This site has three main tabs."),
               br(),
               h3(strong("How should I cite data from this site?")),
               p("You can download the data used to create the plots shown in 
                 this interactive tool using the 'Download' button included 
                 under each tab. As a condition of this tool to access data, 
                 you must cite its use. Please use the following citations:"),
               p("- Fierro-Arcos, D., Blanchard, J. L., Flynn, C., 
                 Ortega Cisneros, K., Reimer, T. (2024). FishMIP input explorer 
                 for regional ecosystem modellers."),
               p("When using the data product in a publication, please include 
               the following citation in addition to the data product citations 
               provided above:"),
               p("- Ortega-Cisneros, K., Fierro-Arcos, D. Lindmark, M., et al.
                 (Preprint). An Integrated Global-to-Regional Scale Workflow for
                 Simulating Climate Change Impacts on Marine Ecosystems. ESS 
                 Open Archive. DOI:", 
                 tags$a(href ="http://dx.doi.org/10.22541/essoar.171587234.44707846/v1",
                 "10.22541/essoar.171587234.44707846/v1")),
               br(),
               h3(strong("How can I contact you?")),
               p("If you are a regional ecosystem modeller and would like to be 
                 part of the FishMIP community, you can head to the ",
                 tags$a(href =  "https://fishmip.org/joinus.html", "'Join us'"),
                 " section of our website for more information."),
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
                 column(4, img(src = "FishMIP_logo.jpg", height = 150,
                               width = 350,
                               style = "display: block; margin-left: auto; 
                               margin-right: auto")),
                 column(4, img(src = "UN_OceanDecadeLogo_cropped.png", 
                               height = 150, width = 325,
                               style = "display: block; margin-left: auto; 
                               margin-right: auto"))),
               br()
             )
    )
  )
)

# Define actions ---------------------------------------------------------------
server <- function(input, output, session) {
  # bs_themer()
  
  ## Model tab -----------------------------------------------------------------
  # Selecting correct file based on inputs from region and env variable selected
  select_model_file <- reactive({
    validate(
      need(input$variable_gfdl != "", 
           # display custom message 
           "Please choose a variable to visualise.")
    )
    get_filenames(region_nicename = input$region_gfdl,
                  var_nicename = input$variable_gfdl, model = "gfdl")
  })

  # Loading dataset
  gfdl_data <- reactive({
    map_df <- read_parquet(select_model_file()$map) |> 
      select(lat:vals)
    ts_df <- read_parquet(select_model_file()$ts) |> 
      select(time:vals)

    # Create title for colour bar
    cb_lab <- select_model_file()$cb_lab
    
    if("depth_bin_m" %in% names(map_df)){
      depths <- map_df |> 
        distinct(depth_bin_m) |> 
        pull()
    }else{
      depths <- "Not available"
    }

    return(list(
      map_data = map_df,
      ts_data = ts_df,
      cb_lab = cb_lab,
      depths = depths
    ))
  })
  
  observeEvent(gfdl_data(), {
    updateSelectInput(inputId = "depth_gfdl", choices = gfdl_data()$depths)
    })
  
  # Creating first plot
  output$map_gfdl <- renderPlot({
    df <- gfdl_data()$map_data
    
    map_title <- paste0("Climatological mean (1961-2010) ", 
                        input$variable_gfdl) |>
      str_to_sentence()
    
    depth <- input$depth_gfdl
    # Subsetting data for selected depth
    validate(
      need(depth != "", 
           # display custom message 
           "Please wait while we render the map for your chosen depth.")
    )
    if(depth != "Not available"){
      df <- df |> 
        filter(depth_bin_m == depth)
    }
    
    # Adjusting map proportions
    validate(
      need(df$lon != "", 
           # display custom message 
           "Please wait while we render the map for your chosen area.")
    )
    range_map <- range_map(df, input$region_gfdl)

    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
      coord_sf(ylim = range_map$ylims, xlim = range_map$xlims, expand = T) +
      guides(fill = guide_colorbar(title = gfdl_data()$cb_lab,
                                   title.position = "top", title.hjust = 0.5))+
      labs(title = str_wrap(map_title, 60),  x = "Longitude", y = "Latitude")
  }, 
  height = 500, width = 750)

  output$ts_gfdl <- renderPlot({
    df <- gfdl_data()$ts_data
    
    # Create titles for plots
    ts_title <- paste0("Area weighted mean (1961-2010) for ",
                       input$variable_gfdl) |> 
      str_to_sentence()
    
    depth_bin <- input$depth_gfdl
    # Subsetting data for selected depth
    validate(
      need(depth_bin != "", 
           # display custom message 
           "Please wait while we render the map for your chosen depth.")
    )
    if(depth_bin != "Not available"){
      df <- df |> 
        filter(depth == depth_bin)
    }

    # Calculate spatially weighted average of variables selected
    ggplot(df, aes(x = time, y = vals)) +
      prettyts_theme +
      labs(title = str_wrap(gfdl_data()$ts_title, 60),
           y = str_wrap(gfdl_data()$cb_lab, 50), x = "Date")
  }, height = 500, width = 750)

  # # Loading download dataset
  # gfdl_down_data <- reactive({
  #   filestring <- str_split(select_model_file()$map, "/")
  #   filestring <- filestring[[1]][length(filestring[[1]])]
  # 
  #   # Get full file path to relevant file
  #   down_file <- file.path(download_dir, filestring)
  # 
  #   # Load file
  #   down_df <- read_csv(down_file, show_col_types = FALSE)
  #   return(list(
  #     filestring = filestring,
  #     down_file = down_file
  #   ))
  # })

#   # output$download_data <- downloadHandler(
#   #   filename = function(){gfdl_down_data()$filestring},
#   #   # Creating name of download file based on original file name
#   #   content = function(file){write_csv(gfdl_down_data()$down_file, file)}
#   # )
#   
  ## Observations tab ----------------------------------------------------------

  # Select correct file based on inputs from region and variable selected
  select_WOA_file <- reactive({
    get_filenames(region_nicename = input$region_WOA, 
                  var_nicename = input$variable_WOA, model = "woa")
  })

  # Read WOA data from file, surface only
  map_WOA_data <- reactive({
    df <- read_parquet(select_WOA_file()$map) |>
      select(lat:vals) |>
      filter(depth == 0) |> 
      drop_na(vals)

    map_title <- paste0("Climatological mean (1981-2010) ", 
                        input$variable_WOA) |>
      str_to_sentence()
    
    # Get units
    unit <- select_WOA_file()$units

    if(str_detect(input$variable_WOA, "Salinity")){
      figlabel <- "Salinity (ppm)"
    } else {
      figlabel <- paste0(input$variable_WOA, " (", unit, ")")
    }
    
    return(list(
      df = df,
      title = map_title,
      figlabel = figlabel))
  })

  # ts_WOA_data <- reactive({
  #   df <- read_parquet(select_WOA_file()) |>
  #     filter(!is.na(value)) |>
  #     mutate(date = as.Date(time)) |>
  #     select(-depth, -variable, -time) |>
  #     group_by(date) |>
  #     reframe(value = mean(value))
  # 
  #   if (input$variable_WOA == "Temperature") {
  #     ylab <- expression("Temperature ("*degree*"C)")
  #   } else {
  #     ylab <- "Salinity (ppm)"
  #   }
  # 
  #   title <- paste0("Mean ", input$variable_WOA) |>
  #     str_to_sentence()
  #   ts_title <- paste0(title, " from World Ocean Atlas (1981-2010), ",
  #                      input$region_WOA, " region")
  # 
  #   return(list(
  #     df = df,
  #     title = ts_title,
  #     xlab = "Month",
  #     ylab = ylab
  #   ))
  # })
  # 
  output$map_WOA <- renderPlot({
    df <- map_WOA_data()$df
    
    # Adjusting map proportions
    validate(
      need(df$lon != "", 
           # display custom message 
           "Please wait while we render the map for your chosen area.")
    )

    minx <- min(df$lon)
    maxx <- max(df$lon)
    miny <- min(df$lat)
    maxy <- max(df$lat)

    # Calculate range
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))

    # Check if map crosses international date line
    if(rangex == 0 & str_detect(input$region_WOA, "Southern Ocean",
                                negate = T)){
      df <- df |>
        mutate(lon = lon%%360)
      minx <- min(df$lon)
      maxx <- max(df$lon)
      rangex <- abs(abs(maxx)-abs(minx))
    }

    xlims <- c(minx, maxx)
    ylims <- c(miny, maxy)

    # Plotting map
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
      guides(fill = guide_colorbar(title = map_WOA_data()$figlabel,
                                   title.position = "top", title.hjust = 0.5)) +
      coord_sf(ylim = ylims, xlim = xlims, expand = F) +
      labs(title = map_WOA_data()$title, x = "Longitude", y = "Latitude")
  }, height = 500, width = 750)
  # 
  # output$ts_WOA <- renderPlot({
  #   df <- ts_WOA_data()$df
  # 
  #   ggplot(df, aes(x = date, y = value)) +
  #     geom_line() +
  #     prettymap_theme +
  #     scale_x_date(date_labels = "%b", date_breaks = "1 month",
  #                  expand = expansion(0.02)) +
  #     labs(title = ts_WOA_data()$title,
  #          x = ts_WOA_data()$xlab,
  #          y = ts_WOA_data()$ylab)
  # }, height = 500, width = 750)
}
#   
#   # output$download_WOA <- downloadHandler(
#   #   filename = function(){
#   #     str_c("downloaded_", select_WOA_file()$fname, ".csv") # Needs some trimming, currently gives full path
#   #     },
#   #   # Creating name of download file based on original file name
#   #   content = function(file){write_csv(x = map_WOA_data()$df, file = file)}
#   # )
#   
#   ## Comparison tab ----------------------------------------------------------
#   
#   select_compare_file <- reactive({
#     fname_WOA <- get_WdOA_filename(reg_nicename = input$region_compare, 
#                                   var_nicename = input$variable_compare)
#     # returns list of map and ts
#     fname_gfdl <- get_gfdl_filename(reg_nicename = input$region_compare, 
#                                   var_nicename = input$variable_compare) 
#     
#     df_WOA <- read_parquet(fname_WOA) |> drop_na()
#     map_gfdl <- read_csv(fname_gfdl$map, col_select = c("lat", "lon", "vals"), 
#                         show_col_types = FALSE)
#     ts_gfdl <- read_csv(fname_gfdl$ts, col_select = c("date", "vals"), 
#                        show_col_types = FALSE)
#     
#     # fname <- fname # change to combine them somehow
#     return(list(
#       fname = fname_WOA, 
#       df_WOA = df_WOA,
#       map_gfdl = map_gfdl,
#       ts_gfdl = ts_gfdl
#     ))
#   })
#   
#   select_compare_data <- reactive({
#     map_WOA <- select_compare_file()$df_WOA |> 
#       filter(!is.na(value)) |> 
#       select(-depth, -variable) |> 
#       group_by(lat, lon) |> 
#       reframe(value = mean(value)) |> 
#       mutate(source = "WOA observations")
#     
#     map_gfdl <- select_compare_file()$map_gfdl |> 
#       mutate(value = vals,
#              source = "GFDL-MOM6-COBALT2 model output") |> 
#       select(-vals)
#     
#     map_compare = rbind(map_gfdl, map_WOA)
#     
#     map_compare <- map_compare |> 
#       pivot_wider(names_from = source, 
#                   values_from = value) |> 
#       mutate(percent_diff = round((( `GFDL-MOM6-COBALT2 model output` - `WOA observations`) / `WOA observations`) * 100, 2))
#     
#     ts_gfdl <- select_compare_file()$ts_gfdl |> 
#       mutate(value = vals,
#              month = month(date)) |> 
#       group_by(month) |> 
#       reframe(value = mean(value)) |> 
#       mutate(source = "GFDL-MOM6-COBALT2 model output")
#     
#     ts_WOA <- select_compare_file()$df_WOA |> 
#       filter(!is.na(value)) |> 
#       mutate(date = as.Date(time),
#              month = month(date)) |> 
#       group_by(month) |> 
#       reframe(value = mean(value)) |> 
#       mutate(source = "WOA observations")
#     
#     ts_compare = rbind(ts_gfdl, ts_WOA) |> 
#       pivot_wider(names_from = source, 
#                   values_from = value) |> 
#       mutate(percent_diff = round((( `GFDL-MOM6-COBALT2 model output` - `WOA observations`) / `WOA observations`) * 100, 2))
#     
#     ylab <- paste0("% difference in ",input$variable_compare)
#     title <- paste0("Difference in ", input$variable_compare) |> 
#       str_to_sentence()
#     title <- paste0(title, 
#                     " between model outputs (GFDL-MOM6-COBALT2) \n and observations (WOA), ",
#                     input$region_compare, " region")
# 
#     return(list(
#       map_compare = map_compare, 
#       map_title = title,
#       map_figlabel = ylab,
#       map_xlab = "Longitude",
#       map_ylab = "Latitude",
#       ts_compare = ts_compare,
#       ts_title = title,
#       ts_figlabel = NA,
#       ts_xlab = "Month",
#       ts_ylab = ylab
#     ))
#   })
#   
#   output$map_compare <- renderPlotly({
#     df <- select_compare_data()$map_compare 
#     
#     # Compare processing goes here
#     
#     # Adjusting map proportions
#     minx <- min(df$lon)
#     maxx <- max(df$lon)
#     miny <- min(df$lat)
#     maxy <- max(df$lat)
#     
#     # Calculate range
#     rangex <- abs(abs(maxx)-abs(minx))
#     rangey <- abs(abs(maxy)-abs(miny))
#     
#     # Check if map crosses international date line
#     if(rangex == 0 & str_detect(input$region_gfdl, "Southern Ocean", 
#                                 negate = T)){
#       df <- df |>
#         mutate(lon = lon%%360)
#       minx <- min(df$lon)
#       maxx <- max(df$lon)
#       rangex <- abs(abs(maxx)-abs(minx))
#     }
#     
#     xlims <- c(minx, maxx)
#     ylims <- c(miny, maxy)
#     
#     p <- ggplot(df, aes(x = lon, y = lat, fill = percent_diff)) +
#       geom_tile() +
#       coord_cartesian() +
#       scale_fill_viridis_c() +
#       #facet_grid(cols = vars(source)) +
#       geom_sf(inherit.aes = F, data = world, lwd = 0.25, color = "black", 
#               show.legend = F) +
#       guides(fill = guide_colorbar(title = select_compare_data()$map_figlabel, 
#                                    title.position = "top", title.hjust = 0.5)) +
#       coord_sf(xlims, ylims) +
#       # theme_minimal() +
#       # prettymap_theme +
#       labs(title = select_compare_data()$map_title,
#            x = select_compare_data()$map_xlab,
#            y = select_compare_data()$map_ylab) +
#       theme(legend.key.size = unit(0.5, "cm"),    # Decrease size of legend keys
#             legend.text = element_text(size = 8),
#             panel.border = element_blank(),
#             panel.grid.major = element_blank(),
#             panel.grid.minor = element_blank()) +
#       theme_classic() +
#       theme(text = element_text(colour = "black", size = 8),
#             # legend.position = "bottom", 
#             # legend.key.width = unit(3.5, "cm"),
#             plot.title = element_text(size = 13, hjust = 0.5),
#             axis.text.y = element_text(hjust = 0.5, vjust = 0.5), 
#             axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
#       )
#     ggplotly(p)
#     
#   })
#   
#   output$ts_compare <- renderPlot({
#     df <- select_compare_data()$ts_compare
#     
#     ggplot(df, aes(x = month, y = percent_diff)) +
#       geom_line() +
#       prettymap_theme +
#       scale_x_continuous(breaks = 1:12, labels = month.abb) +
#       ggtitle(label = select_compare_data()$ts_title) +
#       labs(x = select_compare_data()$ts_xlab, y = select_compare_data()$ts_ylab)
#   }, height = 500, width = 750)
#   
#   # output$download_compare <- downloadHandler(
#   #   filename = function(){
#   #     str_c("downloaded_", select_WOA_file()$fname, ".csv") # Needs some trimming, currently gives full path
#   #   },
#   #   # Creating name of download file based on original file name
#   #   content = function(file){write_csv(x = map_WOA_data()$data, file = file)}
#   # )
#   
# }

shinyApp(ui = ui, server = server)