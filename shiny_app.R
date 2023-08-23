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
library(ncdf4)


# library(reticulate)
# library(metR)
# library(lubridate)
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
    theme = bslib::bs_theme(bootswatch = "yeti"),
    titlePanel(title = span(img(src = "fishmiplogo.jpg", height = 35), "")),
    sidebarLayout(
    sidebarPanel(
      h2("Regional Climate Forcing Explorer"),
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
     br(),
     
     # Sidepanel instructions
     
     h4("Instructions:"),
     p("1. Select a regional model from the dropdown."),
     p("2. Choose a variable of interest."),
     p("3. Select the resolution."),
     p("4. Click the 'Download' button to download data."),
     p("5. View climatology and time-series plots to the right."),
     br(),
     p("Higher resolution selections make take longer to load.")
    ),
    mainPanel(
      em("Climatology (mean 1961-2010)"),
      plotlyOutput(outputId = "plot1",width="100%"),
      em("Spatially averaged time-series"),
      plotOutput(outputId = "plot2",width="100%"),
      br(),
      br()
    )
  )
)


server <- function(input, output) {
 


  
  filtered_mask <- reactive({

    if (input$res =="15arcmin"){
      mask <- read.csv(file="Masks_netcdf_csv/fishMIP_regional_025deg_ISIMIP3a.csv")
    }
    
  subset(mask,
           region %in% input$region)})


  filtered_data <- reactive({
    
    mask_to_use <- st_as_sf(filtered_mask(), coords = c("Lon", "Lat"))
    #mask<-subset(mask,"Prydz.Bay" %in% mask$region)
    #mask_to_use <- st_as_sf(mask, coords = c("Lon", "Lat"))
    
    file_to_get<-paste(thredds_dir,
                       modelscen,
                       variable=input$variable,
                       res=input$res, filesuffix,sep="_")
    
    #terra::rast("http://portal.sf.utas.edu.au/thredds/fileServer/gem/fishmip/ISIMIP3a/InputData/climate/ocean/obsclim/global/monthly/historical/GFDL-MOM6-COBALT2/gfdl-mom6-cobalt2_obsclim_intppdiat_60arcmin_global_monthly_1961_2010.nc")
    #nc<-terra::rast("Masks_netcdf_csv/gfdl-mom6-cobalt2_obsclim_intppdiat_60arcmin_global_monthly_1961_2010.nc")
    # nc<-ncdf4::nc_open(file="Masks_netcdf_csv/gfdl-mom6-cobalt2_obsclim_intppdiat_60arcmin_global_monthly_1961_2010.nc",return_on_error=TRUE)
    # file_to_get<-"Masks_netcdf_csv/gfdl-mom6-cobalt2_obsclim_intppdiat_60arcmin_global_monthly_1961_2010.nc"
    gridded_ts <- brick(file.path(file_to_get))
    # # cut selected region out
    crs(gridded_ts) = crs(mask_to_use)
    temp <- crop(gridded_ts, extent(mask_to_use))
    gridded_ts <- mask(temp, mask_to_use)
    return(gridded_ts)
  })
    
    # gridded_ts_df <- as.data.frame(gridded_ts, xy = TRUE)
    # names(gridded_ts_df)<-c("lon","lat","var")
    #  
    # mean <- gridded_ts_df %>%
    # group_by(lat, lon) %>% 
    # summarise(mean = mean(var, na.rm = F))
   
    output$plot1 <- renderPlotly({
      #plot(mean(filtered_data()))
       clim<-mean(filtered_data())
      ##clim<-mean(gridded_ts)
       clim_df<-as.data.frame(clim, xy = TRUE)
       names(clim_df)<-c("lon","lat","var")
        ggplotly({
          p<- ggplot() +
          geom_raster(data = clim_df , aes(x = lon, y = lat, fill = var)) +
            scale_fill_viridis_c(guide=guide_colorbar(title = paste(input$variable))) +
            coord_quickmap() +
            theme_classic()
          p
         })
   
  })
  
   output$plot2 <- renderPlot({
     ## calculate spatially weighthed average of variables selected
     ts<-cellStats(filtered_data(), 'mean')
     #ts<-cellStats(gridded_ts, 'mean')
     timevals<-seq(as.Date("1961-01-01"),as.Date("2010-12-01"), by="month")
     df<-data.frame(time=as.Date(timevals),val=ts)
     ggplot(df,aes(x=time,y=val)) +
       geom_line() + 
       geom_smooth(colour="steelblue")+
       theme_classic() +
       theme(axis.text = element_text(size=12),
             axis.title = element_text(size=14))+
       xlab("") +
       ylab(paste(input$variable))
     
     #plot(timevals,ts,typ="l",ylab=input$variable)

   })
  
   output$download_data <- downloadHandler(
     filename = "download_data.csv",
     content = function(file) {
       data<-as.data.frame(filtered_data(), xy = TRUE)
       timevals<-seq(as.Date("1961-01-01"),as.Date("2010-12-01"), by="month")
       colnames(data)<-c("lon","lat",as.character(timevals))
       write.csv(data, file, row.names = FALSE)
     }
   )
  
}

shinyApp(ui = ui, server = server)