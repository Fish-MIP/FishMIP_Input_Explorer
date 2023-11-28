#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


# Loading libraries -------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(plotly)
library(stringr)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(tidyterra)


# Setting up  -------------------------------------------------------------
#Load mask for regional ecosystem models
fishmip_masks <- "/rd/gem/private/users/ldfierro/FishMIP_regions/Outputs/FishMIPMasks"
area_folder <- "/rd/gem/private/users/ldfierro/FishMIP_regions/ESM_Sample_Data"
#Loading csv masks
mask_1deg <- read_csv(list.files(fishmip_masks, pattern = "1deg.csv", full.names = T))
mask_025deg <- read_csv(list.files(fishmip_masks, pattern = "025deg.csv", full.names = T))

#Get list of variables with four dimensions (lon, lat, time and depth)
four_dim_mods <- read_csv("Masks_netcdf_csv/four_dimensional_rasters.csv") |> 
  pull(vars)
#Get list of depth bins in four dimensional variables
depth_bins <- read_csv("Masks_netcdf_csv/depth_layers.csv") |> 
  pull(depths)

# Base folder containing Earth System Model (ESM) data
base_dir <- "/rd/gem/public/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/global/monthly/historical/GFDL-MOM6-COBALT2/"
# Getting list of all files within folder
data_files <- list.files(base_dir)
#ESMname ="gfdl-mom6-cobalt2"
modelscen <- c("obsclim")
#Getting names of environmental variables available
varNames <- str_extract(data_files, ".*obsclim_(.*)_[0-9]{2}arc.*", group = 1) |> 
  unique()
#ESM resolutions available
resOptions <- str_extract(data_files, "[0-9]{2}arcmin") |>
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
                  choices = unique(mask_1deg$region),
                  selected = "Prydz Bay"),
      
      #to select the variable
      selectInput(inputId = "variable", 
                  label = "Variable Name:", 
                  choices = varNames, 
                  selected ="tos"),
      
      # select the resolution
      selectInput(inputId = "res", 
                  label = "Earth System Model Resolution:",
                  choices = resOptions,
                  selected = "60arcmin"),
      
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
     p("3. Select the resolution of the Earth System Model."),
     p("Note: Selecting a higher resolution may take longer to load. Due to the grid size, some regions may not be available at lower resolutions."),
     p("4. View climatological mean as a map and as a time-series plot to the right."),
     p("5. Optional: Click the 'Download' button to download data."),
     br(),
    ),
    mainPanel(
      em("Climatological mean (1961-2010)"),
      plotlyOutput(outputId = "plot1", width = "100%"),
      em("Spatially averaged time-series (1961-2010)"),
      # plotOutput(outputId = "plot2", width = "100%"),
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
  
  # Choose correct mask based on resolution selected
  observeEvent(ignoreInit = T, input$res, {
    if(input$res == "60arcmin"){
      updatePickerInput(session = session, 
                        inputId = "region",
                        choices = unique(mask_1deg$region))
      }else{
        updatePickerInput(
          session,
          inputId = "region",
          choices = unique(mask_025deg$region))
        }})
  
  #Choose correct raster mask for resolution
  filtered_mask <- reactive({
    if(input$res == "15arcmin"){
      mask_ras <- rast(list.files(fishmip_masks, pattern = "025deg.nc", full.names = T))
      mask_df <- mask_025deg
    }else if(input$res == "60arcmin"){
      mask_ras <- rast(list.files(fishmip_masks, pattern = "1deg.nc", full.names = T))
      mask_df <- mask_1deg
    }
    #Selecting correct region in data frame
    mask_reg_df <- mask_df |> 
      filter(region == input$region)
    #Identifying region ID 
    id_reg <- mask_reg_df |> 
      distinct(id) |> 
      pull()
    #Selecting correct region in raster
    mask_reg_ras <- mask_ras[[id_reg]]
    #Turning to binary raster
    mask_reg_ras[mask_reg_ras == id_reg] <- 1
    
    #Return both mask in list
    mask_reg <- list(df = mask_reg_df,
                     ras = mask_reg_ras)
    return(mask_reg)
  })

  filtered_data <- reactive({
    # Getting region from mask
    mask_to_use <- filtered_mask()
    # Getting file path for raster data
    var_res <- paste(modelscen, input$variable, input$res, sep = "_")
    file_to_get <- file.path(base_dir, data_files[str_detect(data_files, var_res)])
    # Loading raster
    gridded_ts <- rast(file_to_get)
    # Getting raster units for label
    var_unit <- unique(units(gridded_ts))
    # If CRS is not the same in raster and mask, then transform mask
    if(crs(gridded_ts) != crs(mask_to_use$ras)){
      mask_to_use$ras <- mask_to_use$ras |>
        st_transform(crs = crs(gridded_ts))
    }
    # If depth is available select depth_bin
    # if(input$depth != "None"){
    #   lyrs <- names(gridded_ts)
    #   lyrs <- lyrs[str_detect(lyrs, paste0("=", input$depth, "_"))]
    #   gridded_ts <- gridded_ts[[lyrs]]
    # }
  #   
    # Crop raster using mask
    gridded_ts_reg <- gridded_ts*mask_to_use$ras
    gridded_ts_reg  <- gridded_ts_reg |> 
      drop_na(names(gridded_ts_reg)[1])
    
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
        theme(legend.position = "bottom", legend.key.width = unit(2, "cm")) }) #|> 
      # layout(legend = list(
      #   orientation = "h",
      #   xanchor = "center",S
      #   yanchor = "bottom",
      #   x = 0.5))
  })

   # output$plot2 <- renderPlot({
     # calculate spatially weighthed average of variables selected
   #   ts <- global(filtered_data(), 'mean')
   #   #ts<-cellStats(gridded_ts, 'mean')
   #   timevals<-seq(as.Date("1961-01-01"),as.Date("2010-12-01"), by="month")
   #   df<-data.frame(time=as.Date(timevals),val=ts)
   #   ggplot(df,aes(x=time,y=val)) +
   #     geom_line() +
   #     geom_smooth(colour="steelblue")+
   #     theme_classic() +
   #     theme(axis.text = element_text(size=12),
   #           axis.title = element_text(size=14))+
   #     xlab("") +
   #     ylab(paste(input$variable))
   # 
   #   #plot(timevals,ts,typ="l",ylab=input$variable)
   # 
   # })

   # output$download_data <- downloadHandler(
   #   filename = "download_data.csv",
   #   content = function(file) {
   #     data<-as.data.frame(filtered_data(), xy = TRUE)
   #     timevals<-seq(as.Date("1961-01-01"),as.Date("2010-12-01"), by="month")
   #     colnames(data)<-c("lon","lat",as.character(timevals))
   #     write.csv(data, file, row.names = FALSE)
   #   }
   # )
  
}

shinyApp(ui = ui, server = server)