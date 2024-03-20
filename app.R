# Loading libraries -------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(tidyverse)
# library(sf)
library(terra)
# library(tidyterra)

# Setting up  -------------------------------------------------------------
#Load mask for regional ecosystem models
fishmip_masks <- "/rd/gem/private/users/ldfierro/FishMIP_regions/Outputs/FishMIPMasks"

#Keys to interpret raster mask
keys <- read_csv(list.files(fishmip_masks, pattern = "FishMIP_regions_keys.csv",
                            full.names = T))

#Loading area
area_ras <- rast("/rd/gem/private/users/ldfierro/FishMIP_regions/ESM_Sample_Data/area_025deg.nc") |> 
  as.data.frame(xy = T) |> 
  rename(lon = x, lat = y)

# #Get list of variables with four dimensions (lon, lat, time and depth)
# four_dim_mods <- read_csv("Masks_netcdf_csv/four_dimensional_rasters.csv") |> 
#   pull(vars)
# #Get list of depth bins in four dimensional variables
# depth_bins <- read_csv("Masks_netcdf_csv/depth_layers.csv") |> 
#   pull(depths)

# Base folder containing Earth System Model (ESM) data
base_dir <- "/rd/gem/public/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/regional/monthly/historical/GFDL-MOM6-COBALT2"
# Getting list of all files within folder
data_files <- list.files(base_dir, pattern = "15arcmin") |> 
  str_subset("phydiat_depth", negate = T)
#Getting names of environmental variables available
varNames <- str_extract(data_files, ".*obsclim_(.*)_[0-9]{2}arc.*", group = 1) |> 
  unique()


# Definining user interface -----------------------------------------------
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "yeti"),
    titlePanel(title = span(img(src = "FishMIP_logo.jpg", height = 100,
                                width = 300, style = "display: block;
                                                      margin-left: auto;
                                                      margin-right:auto"),
               h1("Regional Climate Forcing Data Explorer",
                  style = "background-color:#f3f3f3;
                            border:1.5px solid #c9d5ea;
                            padding-left: 15px;
                            padding-bottom: 10px;
                            padding-top: 10px;
                            text-align: center;
                            font-weight: bold")),
               windowTitle = "FishMIP Regional Climate Forcing Data Explorer"),
    sidebarLayout(
    sidebarPanel(
      h4("Instructions:"),
      p("1. Select a FishMIP regional model"),
      #Select region - Default to regions present in 1deg mask
      #Option to vary based on resolution
      pickerInput(inputId = "region", 
                  label = NULL,
                  choices = keys$region,
                  selected = "Prydz Bay"),
      p("2. Choose an environmental variable"),
      #to select the variable
      selectInput(inputId = "variable", 
                  label = NULL, 
                  choices = varNames, 
                  selected = "tos"),
      br(),
     p("3. Inspect the climatological mean map and area-weighted\
       timeseries plot on the right."),
     br(),
     p("Optional: Get the data used to create these plots by clicking the\
       'Download' button below.", br(), 
       "This may take a few minutes while we get the data ready for you."),
     #Download button
     downloadButton(outputId = "download_data", label = "Download"),
    ),
    mainPanel(
      em("Climatological mean (1961-2010)"),
      plotOutput(outputId = "plot1", width = "100%"),
      em("Spatially averaged timeseries (1961-2010)"),
      plotOutput(outputId = "plot2", width = "100%"),
      br(),
      br()
    )
  )
)


# Define actions ----------------------------------------------------------
server <- function(input, output, session) {
  #Selecting correct file based on inputs from region and env variable selected
  region_fishmip <- reactive({
    #Get region selected
    name_reg <- input$region |> 
      #Change to all lowercase
      str_to_lower() |> 
      #Replaces spaces " " with dashes "-" to identify correct files
      str_replace_all(" ", "-")
    #Get env variable selected
    name_var <- input$variable
    #Merge region and variable prior to identifying correct file
    sub <- str_c(name_var, "_15arcmin_", name_reg)
    #Identifying correct file
    reg_files <- str_subset(data_files, sub)
  })
  
  #Loading dataset
  var_fishmip <- reactive({
    #Get full file path to relevant file
    env_file <- file.path(base_dir, region_fishmip())
    #Load file
    df <- read_csv(env_file)
    #Getting units from dataset
    unit <- df |> 
      select(units) |> 
      drop_na() |> 
      pull()
    #Getting standard name from dataset
    std_name <- df |> 
      select(standard_name) |> 
      drop_na() |>
      pull() |> 
      str_replace_all("_", " ") |> 
      str_to_sentence()
    #Creating label for figures
    cb_lab <- paste0(input$variable, " (", unit, ")")
    #Selecting relevant data frame columns
    df <- df |> 
      #Metadata not included in data frame used for plots
      select(c(lat, lon, matches("[0-9]{4}"))) |> 
      #Reorganise data
      pivot_longer(!c(lat, lon), names_to = "time", values_to = "vals") |> 
      #Remove rows with NA values
      drop_na() |> 
      #Change time column to date format
      mutate(time = my(time)) |> 
      #Arrange dataset by time column
      arrange(time)
    df$standard_name <- std_name
    df$units <- unit
    #Return data frame
    return(list(data = df,
                fig_label = cb_lab))
  })
  
  # Creating first plot
  output$plot1 <- renderPlot({
    clim <- var_fishmip()$data |> 
      group_by(lon, lat) |>
      summarise(vals = mean(vals))
    
    #Creating plot
    clim |> 
      drop_na(vals) |> 
      ggplot(aes(x = lon, y = lat, fill = vals)) +
      geom_raster()+
      scale_fill_viridis_c(guide = guide_colorbar(ticks.linewidth = 0.75,
                                                  frame.colour = "blue", 
                                                  title.vjust = 0.75),
                           na.value = NA) +
      guides(fill = guide_colorbar(title = var_fishmip()$fig_label, title.position = "top", 
                                   title.hjust = 0.5))+
      labs(title = var_fishmip()$data$standard_name, 
           x = "Longitude", y = "Latitude")+
      theme_classic()+
      theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
            plot.title = element_text(hjust = 0.5))
  
  })

   output$plot2 <- renderPlot({
     # calculate spatially weighted average of variables selected
     ts <- var_fishmip()$data |> 
       left_join(area_ras, by = join_by(lon, lat)) |> 
       group_by(time) |> 
       summarise(vals = weighted.mean(vals, area_m, na.rm = T))
     
     #Plot weighted means
     ggplot(ts, aes(x = time, y = vals)) +
       geom_line() +
       geom_smooth(colour = "steelblue")+
       theme_classic() +
       scale_x_date(date_labels = "%b-%Y", date_breaks = "18 months")+
       labs(title = var_fishmip()$data$standard_name, 
            y = var_fishmip()$fig_label)+
       theme(axis.text = element_text(size = 12),
             axis.text.x = element_text(angle = 90, vjust = 0.5),
             axis.title.y = element_text(size = 12),
             axis.title.x = element_blank(),
             plot.title = element_text(hjust = 0.5))
   })

   output$download_data <- downloadHandler(
     filename = function(){
       region_fishmip()
     },
     #Creating name of download file based on original file name
     content = function(file){
       write_csv(var_fishmip()$data, file)
     }
   )
  
}

shinyApp(ui = ui, server = server)