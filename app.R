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
library(plotly)
library(ggplot2)
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

# Getting list of all files within folders
woa_maps <- "/rd/gem/public/fishmip/WOA_data/regional/climatology"
woa_ts <- "/rd/gem/public/fishmip/WOA_data/regional/monthly/comp_clim/"

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
                             plot.title = element_text(hjust = 0.5, size = 15),
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
                             selected = "Sea Surface Temperature"),
                 
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
                 downloadButton(outputId = "download_data_gfdl", 
                                label = "Download")
               ),
               mainPanel(
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
    tabPanel("World Ocean Atlas data",
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
                   these plots by clicking the 'Download' button below."),
                 # Download option
                 downloadButton(outputId = "download_WOA", label = "Download")
               ),
               mainPanel(
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
                                loader = "myloader"),
                              br(),
                              br(),
                              br(),
                              br(),
                              strong("Note: "), "The grey ribbon in the plot 
                              above shows the spatial variance in the variable
                              of interest.")
                   ),
                   tabPanel("Number of in-situ observations",
                            mainPanel(
                              br(),
                              withLoader(
                                plotOutput(outputId = "count_WOA",
                                           width = "100%"),
                                type = "html",
                                loader = "myloader")
                            ))
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

  # Loading download dataset
  # gfdl_down_data <- reactive({
  #   # filestring <- str_split(select_model_file()$map, "/")
  #   # filestring <- filestring[[1]][length(filestring[[1]])]
  # 
  #   # Get full file path to relevant file
  #   down_filepath <- select_model_file()$download#file.path(download_dir, filestring)
  #   if(str_detect(down_filepath, "zarr$")){
  #     down_file <- 
  #   }
  # 
  #   # Load file
  #   down_df <- read_csv(down_file, show_col_types = FALSE)
  #   return(list(
  #     filestring = basename(down_file),#filestring,
  #     down_file = down_file
  #   ))
  # })
  # 
  # output$download_data <- downloadHandler(
  #   filename = function(){basename(select_model_file()$download)},
  #   # Creating name of download file based on original file name
  #   content = function(file){
  #     down_df <- read_parquet(select_model_file()$download)
  #     write_csv(down_df, file)}
  # )

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
      read_parquet(col_select = lat:vals)
    #Loading time series dataset
    df_ts <- list.files(woa_ts, pattern = lookup_woa()$search_file,
                        full.names = T) |>
      read_parquet(col_select = month:weighted_sd) |>
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
    
    ggplot(df, aes(x = lon, y = lat, fill = vals)) +
      prettymap_theme +
      coord_sf(ylim = range_map$ylims, xlim = range_map$xlims, 
               expand = F) +
      guides(fill = guide_colorbar(title = "Number of observations",
                                   title.position = "top", title.hjust = 0.5)) +
      labs(title = str_wrap(title, 65))
  }, height = 600, width = 750)
  
}

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