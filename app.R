# Loading libraries -------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
# library(sf)
library(terra)
# library(tidyterra)

# Setting up  -------------------------------------------------------------
#Load mask for regional ecosystem models
fishmip_masks <- "/rd/gem/private/users/ldfierro/FishMIP_regions/Outputs/FishMIPMasks"
#Loading masks
mask_df <- read_csv(list.files(fishmip_masks,
                               pattern = "FishMIP_regional_mask_025deg.csv",
                               full.names = T))
# mask_ras <- rast(list.files(fishmip_masks, 
#                             pattern = "FishMIP_regional_mask_025deg.nc", 
#                             full.names = T))
#Keys to interpret raster mask
keys <- read_csv(list.files(fishmip_masks, pattern = "FishMIP_regions_keys.csv",
                            full.names = T))

#Loading area
area_ras <- rast("/rd/gem/private/users/ldfierro/FishMIP_regions/ESM_Sample_Data/area_025deg.nc") |> 
  as.data.frame(xy = T) |> 
  rename(lon = x, lat = y)

#Get list of variables with four dimensions (lon, lat, time and depth)
four_dim_mods <- read_csv("Masks_netcdf_csv/four_dimensional_rasters.csv") |> 
  pull(vars)
#Get list of depth bins in four dimensional variables
depth_bins <- read_csv("Masks_netcdf_csv/depth_layers.csv") |> 
  pull(depths)

# Base folder containing Earth System Model (ESM) data
base_dir <- "/rd/gem/public/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/regional/monthly/historical/GFDL-MOM6-COBALT2"
# Getting list of all files within folder
data_files <- list.files(base_dir, pattern = "15arcmin")
#ESMname ="gfdl-mom6-cobalt2"
modelscen <- c("obsclim")
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
                  choices = unique(mask_df$region),
                  selected = "Prydz Bay"),
      p("2. Choose an environmental variable"),
      #to select the variable
      selectInput(inputId = "variable", 
                  label = NULL, 
                  choices = varNames, 
                  selected = "tos"),
      br(),
     p("3. Inspect the climatological mean map (top right) and area-weighted\
       timeseries plot (bottom right)."),
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
  #Region of choice
  region_fishmip <- reactive({
    name_reg <- input$region |> 
      str_to_lower() |> 
      str_replace_all(" ", "-")
    reg_files <- str_subset(data_files, name_reg)
  })
  
  #Environmental variable of choice
  var_fishmip <- reactive({
    env_file <- str_subset(region_fishmip(), input$variable)
    df <- read_csv(file.path(base_dir, env_file))
    return(list(file_name = env_file,
                data = df))
  })
  
  # Creating first plot
  output$plot1 <- renderPlot({
    clim <- var_fishmip()$data |> 
      select(!metadata) |> 
      pivot_longer(!c(lat, lon), names_to = "time", values_to = "vals") |> 
      group_by(lon, lat) |>
      summarise(vals = mean(vals))
    unit <- var_fishmip()$data$metadata[1] |> 
      str_split(", ") |> 
      map_chr(\(x) str_subset(x, "unit")) |> 
      str_split_i(": ", 2) |> 
      str_remove_all("'")
    std_name <- var_fishmip()$data$metadata[1] |> 
      str_split(", ") |> 
      map_chr(\(x) str_subset(x, "standard_name")) |> 
      str_split_i(": ", 2) |> 
      str_remove_all("'") |> 
      str_replace_all("_", " ") |> 
      str_to_sentence()
    cb_lab <- paste0(input$variable, " (", unit, ")")
    
    clim |> 
      drop_na(vals) |> 
      ggplot(aes(x = lon, y = lat, fill = vals)) +
      geom_raster()+
      scale_fill_viridis_c(guide = guide_colorbar(ticks.linewidth = 0.75,
                                                  frame.colour = "blue", 
                                                  title.vjust = 0.75),
                           na.value = NA) +
      guides(fill = guide_colorbar(title = cb_lab, title.position = "top", 
                                   title.hjust = 0.5))+
      labs(title = std_name, x = "Longitude", y = "Latitude")+
      theme_classic()+
      theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
            plot.title = element_text(hjust = 0.5))
  
  })

   output$plot2 <- renderPlot({
     # calculate spatially weighted average of variables selected
     ts <- var_fishmip()$data |> 
       select(!metadata) |> 
       pivot_longer(matches("[0-9]{4}"), names_to = "time", 
                    values_to = "vals") |> 
       mutate(time = my(time)) |> 
       arrange(time) |> 
       left_join(area_ras, by = join_by(lon, lat)) |> 
       group_by(time) |> 
       summarise(vals = weighted.mean(vals, area_m, na.rm = T))
  
     #Plot weighted means
     ggplot(ts, aes(x = time, y = vals)) +
       geom_line() +
       geom_smooth(colour = "steelblue")+
       theme_classic() +
       scale_x_date(date_labels = "%b-%Y", date_breaks = "18 months")+
       theme(axis.text = element_text(size = 12),
             axis.text.x = element_text(angle = 90, vjust = 0.5),
             axis.title.y = element_text(size = 12),
             axis.title.x = element_blank())#+
       # ylab(cb_lab)
   })

   output$download_data <- downloadHandler(
     filename = function(){
       var_fishmip()$file_name
     },
     #Creating name of download file based on original file name
     content = function(file){
       write_csv(var_fishmip()$data, file)
     }
   )
  
}

shinyApp(ui = ui, server = server)