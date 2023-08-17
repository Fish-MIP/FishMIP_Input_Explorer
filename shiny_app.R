#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
## Loading R libraries

library(shiny)
library(plotly)
library(DT)
library(tidyverse)


library(reticulate)
library(metR)
library(lubridate)
library(raster)
library(sf)
library(terra)

# get the mask for regional ecosystem models
mask <- read.csv(file="Masks_netcdf_csv/fishMIP_regional_1deg_ISIMIP3a.csv")
regions<-unique(mask$region)

#THREDDS folder with the different climate forcings
thredds_dir<-"http://portal.sf.utas.edu.au/thredds/dodsC/gem/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/global/monthly/historical/GFDL-MOM6-COBALT2/gfdl-mom6-cobalt2"
#ESMname ="gfdl-mom6-cobalt2"
modelscen = c("obsclim")
varNames=c("chl","expc-bot","intpoc","intpp","intppdiat","intppdiaz","intpppico","mlotst-125","o2","o2-bot","o2-surf","ph","ph-bot","ph-surf","phyc","phyc-vint","phydiat","phydiat-vint","phydiaz","phydiaz-vint","phypico","phypico-vint","rsntds","siconc","so","so-bot","so-surf","thetao","thkcello","tob","tos","uo","vo","zmeso","zmeso-vint","zooc","zooc-vint")
resOptions = c("60arcmin","15arcmin")
filesuffix= "global_monthly_1961_2010.nc"


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("FishMIP Regional Model Climate Forcing Explorer"),
      selectInput(inputId = "region", label = "Regional Model:",
                  choices = regions,
                  selected = "Prydz.Bay"),
      
      #to select the variable
      selectInput("variable", "Variable:", choices=varNames,selected ="tos"),
      
      # select the resolution
      selectInput(inputId = "res", "Resolution",
                 choices = resOptions,
                   selected = "60arcmin"),
      
     downloadButton(outputId = "download_data", label = "Download"),
    ),
    mainPanel(
      plotOutput(outputId = "plot1")
    )
  )
)


server <- function(input, output) {
  
  filtered_mask <- reactive({
    subset(mask,
           region %in% input$region)})

  output$plot1 <- renderPlot({
    
    mask_to_use <- st_as_sf(filtered_mask(), coords = c("Lon", "Lat"))
    
    file_to_get<-paste(thredds_dir,
                       modelscen,
                       variable=input$variable,
                       res=input$res, filesuffix,sep="_")
    
    #terra::rast("http://portal.sf.utas.edu.au/thredds/fileServer/gem/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/global/monthly/historical/GFDL-MOM6-COBALT2/gfdl-mom6-cobalt2_obsclim_intppdiat_60arcmin_global_monthly_1961_2010.nc")
    #nc<-terra::rast("Masks_netcdf_csv/gfdl-mom6-cobalt2_obsclim_intppdiat_60arcmin_global_monthly_1961_2010.nc")
    # nc<-ncdf4::nc_open(file="Masks_netcdf_csv/gfdl-mom6-cobalt2_obsclim_intppdiat_60arcmin_global_monthly_1961_2010.nc",return_on_error=TRUE)
    # file_to_get<-"Masks_netcdf_csv/gfdl-mom6-cobalt2_obsclim_intppdiat_60arcmin_global_monthly_1961_2010.nc"
    #file_to_get<-"http://portal.sf.utas.edu.au/thredds/dodsC/gem/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/global/monthly/historical/GFDL-MOM6-COBALT2/gfdl-mom6-cobalt2_obsclim_zmicro-vint_15arcmin_global_monthly_1961_2010.nc"
    gridded_ts <- brick(file.path(file_to_get))
    # # cut selected region out
    crs(gridded_ts) = crs(mask_to_use)
    temp <- crop(gridded_ts, extent(mask_to_use))
    gridded_ts <- mask(temp, mask_to_use)
    
    # gridded_ts_df <- as.data.frame(gridded_ts, xy = TRUE)
    # names(gridded_ts_df)<-c("lon","lat","var")
    #  
    # mean <- gridded_ts_df %>%
    # group_by(lat, lon) %>% 
    # summarise(mean = mean(var, na.rm = F))
   
    
    
    twoplots <- function(dat) {
      
      timevals<-720:1319
      par(mfrow = c(2,1), mai=c(rep(0.6,4)))               # This sets the new parameters: 2 columns (plots)
      
      # Plotting the first
      with(dat,
           plot(mean(dat))
      )
      
      # Plotting the second
      with(dat,
           plot(timevals,cellStats(dat*(area(dat)), 'mean'),typ="l",ylab=input$variable)
      )
      
      
    }  
    
   
  twoplots(dat=gridded_ts)
   # calculate spatially weighthed average of variables selected
   # ts<-cellStats(gridded_ts*(area(gridded_ts)), 'mean') 
   # timevals<-720:1319
   # plot(timevals,ts,typ="l")
    
     # ggplotly({
     #  plot<-ggplot() +
     #     geom_raster(data = mean , aes(x = lon, y = lat, fill = var)) +
     #     scale_fill_viridis_c() +
     #     coord_quickmap()
     #  plot
     #  })
    
    
  })
  
  
  # output$plot2 <- renderPlot({
  # 
  # 
  #   
  # })
  
  # output$table <- DT::renderDataTable({
  #   gridded_ts_df <- as.data.frame(gridded_ts, xy = TRUE)
  #   gridded_ts_df
  # })
  # output$download_data <- downloadHandler(
  #   filename = "download_data.csv",
  #   content = function(file) {
  #     data <- filtered_data()
  #     write.csv(data, file, row.names = FALSE)
  #   }
  # )
  
}

shinyApp(ui = ui, server = server)