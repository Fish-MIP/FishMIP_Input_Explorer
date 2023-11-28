# This is a Shiny web application: Fish-MIP regional input explorer
# You can run the application by clicking the 'Run App' button above.

# Loading libraries -------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)


# Setting up  -------------------------------------------------------------
#Load mask for regional ecosystem models
fishmip_masks <- "/rd/gem/private/users/ldfierro/FishMIP_regions/Outputs/FishMIPMasks"
#Loading masks
mask_df <- read_csv(list.files(fishmip_masks, pattern = "025deg.csv", full.names = T))
mask_ras <- rast(list.files(fishmip_masks, pattern = "025deg.nc", full.names = T))

#Loading area
area_ras <- rast("/rd/gem/private/users/ldfierro/FishMIP_regions/ESM_Sample_Data/area_025deg.nc")

#Get list of variables with four dimensions (lon, lat, time and depth)
four_dim_mods <- read_csv("Masks_netcdf_csv/four_dimensional_rasters.csv") |> 
  pull(vars)
#Get list of depth bins in four dimensional variables
depth_bins <- read_csv("Masks_netcdf_csv/depth_layers.csv") |> 
  pull(depths)

# Base folder containing Earth System Model (ESM) data
base_dir <- "/rd/gem/public/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/global/monthly/historical/GFDL-MOM6-COBALT2/"
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
    titlePanel(title = span(img(src = "fishmiplogo.jpg", height = 50), "")),
    sidebarLayout(
    sidebarPanel(
      h2("Regional Climate Forcing Explorer"),
      
      #Select region - Default to regions present in 1deg mask - Option to vary based on resolution
      pickerInput(inputId = "region", 
                  label = "Regional Model Name:",
                  choices = unique(mask_df$region),
                  selected = "Prydz Bay"),
      
      #to select the variable
      selectInput(inputId = "variable", 
                  label = "Variable Name:", 
                  choices = varNames, 
                  selected ="tos"),
      
      # select depth - Option to vary based on variable selected
      pickerInput(inputId = "depth", 
                  label = "Earth System Model Depth Bin:", 
                  choices = "None"),
      
      #Download button
      downloadButton(outputId = "download_data", label = "Download"),
      br(),
     
     # Sidepanel instructions
     h4("Instructions:"),
     p("1. Select a regional model from the dropdown list."),
     p("2. Choose an environmental variable of interest."),
     p("3. View climatological mean as a map and as a time-series plot to the right."),
     p("4. Optional: Click the 'Download' button to download data."),
     br(),
    ),
    mainPanel(
      em("Climatological mean (1961-2010)"),
      plotlyOutput(outputId = "plot1", width = "100%"),
      em("Spatially averaged time-series (1961-2010)"),
      plotOutput(outputId = "plot2", width = "100%"),
      br(),
      br()
    )
  )
)


# Define actions ----------------------------------------------------------
server <- function(input, output, session) {
  #Activate depth bin for 4 dimensional variables
  observeEvent(ignoreInit = T, input$variable, {
    if(input$variable %in% four_dim_mods) {
    updatePickerInput(
      session,
      inputId = "depth",
      choices = depth_bins)
    }else{
      updatePickerInput(
        session,
        inputId = "depth",
        choices = "None")
      }
    })
  
  #Selecting region within masks
  filtered_mask <- reactive({
    #Selecting correct region in data frame
    mask_reg_df <- mask_df |> 
      filter(region == input$region)
    #Identifying region ID 
    id_reg <- mask_reg_df |> 
      distinct(id) |> 
      pull()
    #Selecting correct region in raster
    mask_reg_ras <- mask_ras[[id_reg]] |> 
      drop_na()
    #Turning to binary raster
    mask_reg_ras[mask_reg_ras == id_reg] <- 1
    
    #Apply mask to area raster
    area_reg_df <- crop(area_ras, mask_reg_ras, mask = T) |> 
      #Turning to data frame
      as.data.frame(xy = T) |> 
      #Renaming coordinates before joining
      rename(lon = x, lat = y) |> 
      #Joining with data frame mask
      right_join(mask_reg_df, by = c("lon", "lat"))
    
    #Return both mask in list
    mask_reg <- list(ras = mask_reg_ras,
                     area_df = area_reg_df)
    return(mask_reg)
  })
  
  data_path <- reactive({
    # Getting file path for raster data
    var_res <- paste(modelscen, input$variable, sep = "_")
    file_to_get <- file.path(base_dir, data_files[str_detect(data_files, var_res)])
    return(file_to_get)})

  filtered_data <- reactive({
    # Getting region from mask
    mask_to_use <- filtered_mask()
    # Loading raster
    gridded_ts <- rast(data_path())
    # Getting raster units for label
    var_unit <- unique(units(gridded_ts))
    # If CRS is not the same in raster and mask, then transform mask
    if(crs(gridded_ts) != crs(mask_to_use$ras)){
      mask_to_use$ras <- mask_to_use$ras |>
        st_transform(crs = crs(gridded_ts))
    }
    
    # Crop raster using mask
    gridded_ts_reg <- crop(gridded_ts, mask_to_use$ras, mask = T)
    
    # If depth is available select depth_bin
    if(input$depth != "None"){
      lyrs <- names(gridded_ts_reg)
      lyrs <- lyrs[str_detect(lyrs, paste0("=", input$depth, "_"))]
      gridded_ts_reg <- gridded_ts_reg[[lyrs]]
    }
    
    names(gridded_ts_reg) <- seq(as.Date("1961-01-01"), as.Date("2010-12-01"), 
                                 by = "month")
    
    #Return list
    gridded_data <- list(ras = gridded_ts_reg, 
                         unit = var_unit)
    return(gridded_data)
  })
  
  # Creating first plot 
  output$plot1 <- renderPlotly({
    ras_data <- filtered_data()
    clim <- mean(ras_data$ras)
    cb_lab <- paste0(input$variable, " (", ras_data$unit, ")")
    ggplotly({
      ggplot() +
        geom_spatraster(data = clim)+
        scale_fill_viridis_c(guide = guide_colorbar(title = cb_lab, ticks.linewidth = 0.75,
                                                    frame.colour = "blue", title.vjust = 0.75),
                             na.value = NA) +
        theme_classic() +
        theme(legend.position = "bottom", legend.key.width = unit(2, "cm")) })
  })

   output$plot2 <- renderPlot({
     # calculate spatially weighted average of variables selected
     ts <- filtered_data()$ras |> 
       as.data.frame(xy = T) |> 
       #Renaming coordinates before joining
       rename(lon = x, lat = y) |> 
       #Joining with data frame mask (dropping ID and region because they are not needed)
       right_join(filtered_mask()$area_df|> select(!c(id, region)), 
                  by = c("lon", "lat")) |> 
       #Calculating weighted means
       pivot_longer(cols = -c(lon, lat, area_m), names_to = "date", 
                    values_to = "value") |>
       group_by(date) %>% 
       summarise(weighted_mean = weighted.mean(value, area_m, na.rm = TRUE)) |> 
       #Formatting dates correctly
       mutate(date = ymd(date))
     #Create label for y axis
     cb_lab <- paste0("Weighted mean ", input$variable, " (", filtered_data()$unit, ")")
     #Plot weighted means
     ggplot(ts, aes(x = date, y = weighted_mean)) +
       geom_line() +
       geom_smooth(colour = "steelblue")+
       theme_classic() +
       scale_x_date(date_labels = "%b-%Y", date_breaks = "18 months")+
       theme(axis.text = element_text(size = 12),
             axis.text.x = element_text(angle = 90, vjust = 0.5),
             axis.title.y = element_text(size = 12),
             axis.title.x = element_blank())+
       ylab(cb_lab)
   })
   
   output$download_data <- downloadHandler(
     #Creating name of download file based on original file name
     filename <- str_split_i(data_path(), "//", i = 2) |> 
       #Replace "nc" by "region_name".csv
       str_replace(".nc", paste0("_", input$region, ".csv")),
     content = function(file){
       data <- filtered_data()$ras |> 
         #Turning to data frame
         as.data.frame(xy = TRUE) |> 
         #Renaming coordinates before download
         rename(lon = x, lat = y)
       write_csv(data, file)
     }
   )
  
}

shinyApp(ui = ui, server = server)