# Loading libraries -------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bslib)
library(rnaturalearth)
library(sf)

# Setting up  -------------------------------------------------------------
#Load mask for regional ecosystem models
fishmip_masks <- "/rd/gem/private/shared_resources/FishMIPMasks"

#Keys to interpret raster mask
keys <- read_csv(list.files(fishmip_masks, pattern = "FishMIP_regions_keys.csv",
                            full.names = T))

 #Get list of variables with four dimensions (lon, lat, time and depth)
# four_dim_mods <- read_csv("Masks_netcdf_csv/four_dimensional_rasters.csv") |>
#   pull(vars)

# Folders containing Earth System Model (ESM) data
base_dir <- file.path("/rd/gem/public/fishmip/ISIMIP3a/InputData/climate/ocean",
                      "obsclim/regional/monthly/historical/GFDL-MOM6-COBALT2")
download_dir <- file.path(base_dir, "download_data")
maps_dir <- file.path(base_dir, "maps_data")
ts_dir <- file.path(base_dir, "ts_data")

# Getting list of all files within folder
var_files <- list.files(maps_dir) #|> 
  # str_subset("chl", negate = T)
#Getting names of environmental variables available
varNames <- str_extract(var_files, ".*obsclim_(.*)_[0-9]{2}arc.*", 
                        group = 1) |> 
  unique()

world <- ne_countries(returnclass = "sf", scale = "medium")

#Function to improve map ratios for plotting
scaler <- function(x, type, ratio = F){
  if((x > 0 & type == "min") | (x < 0 & type == "min")){
    x <- ifelse(ratio == T, x-3, x-6)
  }else if((x < 0 & type == "max") | (x > 0 & type == "max")){
    x <- ifelse(ratio == T, x+2, x+5)
  }else if(x == 0 & type == "min"){
    x <- ifelse(ratio == T, x-1, x-2)
  }else{
    x <- ifelse(ratio == T, x+1, x+2)
  }
  return(x)
}

# Defining user interface -----------------------------------------------
ui <- fluidPage(
    theme = bs_theme(bootswatch = "yeti"),
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
    tabsetPanel(
      tabPanel("Visualise GFDL-MOM6-COBALT2 outputs",
             sidebarLayout(
               sidebarPanel(h4(strong("Instructions:")),
                            
                            #Choose region of interest
                            p("1. Select the FishMIP regional model you would 
                              like to visualise"),
                            selectInput(inputId = "region_gfdl", 
                            label = NULL,
                            choices = keys$region, selected = "Prydz Bay"),
                            
                            #Choose variable of interest
                            p("2. Choose an environmental variable to 
                              visualise"),
                            selectInput(inputId = "variable_gfdl",
                            label = NULL,
                            choices = varNames, selected = "tos"),
                            
                            p("3a. Click on the ", 
                              strong('Climatological maps'), " tab on the 
                              right to see a map of the climatological mean 
                              (1961-2010) for the variable of your choice 
                              within your region of interest"),
                            p("3b. Click on the ", strong('Time 
                            series plot'), " tab to see a time series of area 
                            weighted yearly mean for the variable and region of
                            your choice."),
                            
                            p(em("Optional: "), "Get a copy of the data 
                            used to create these plots by clicking the 
                            'Download' button below."),
                            
                            #Download option
                            downloadButton(outputId = "download_data_gfdl", 
                                           label = "Download")
                            ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Climatological maps",
                            mainPanel(
                              plotOutput(outputId = "map_gfdl", width = "100%")
                              )
                            ),
                   tabPanel("Time series plot",
                            mainPanel(
                              plotOutput(outputId = "ts_gfdl", width = "100%")
                            )
                            )
                   )
                 )
               )
    ),
    tabPanel("Visualise observational data"),
    tabPanel("Compare GFDL-MOM6-COBALT2 against observations"),
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
               h3(strong("How should I use this tool?")),
               p("This site has xxx tabs."),
               br(),
               h3(strong("How should I cite data from this site?")),
               p("You can download the data used to create the plots shown in 
                 this interactive tool using the 'Download' button included 
                 under each tab. As a condition of this tool to access data, 
                 you must cite its use. Please use the following citations:"),
               p("- Citation 1"),
               p("When using the data product in a publication, please include 
               the following citation in addition to the data product citations 
               provided above:"),
               p("- Citation 2"),
               br(),
               h3(strong("How can I contact you?")),
               p("If you would   "),
               br(),
               h4(strong("Acknowledgments")),
               p("The development of this tool was funded by the Australian 
                 Government through the Australian Research Council (ARC) 
                 XXXXX Project XXXX. We gratefully acknowledge contributions 
                 from coordinators and contributing modellers of the FishMIP 
                 and ISIMIP communities. We would also like to acknowledge the 
                 use of computing facilities provided by Digital Research 
                 Services, IT Services at the University of Tasmania."),
               br(),
               card(img(src = "IMAS_logo.png", height = 150, width = 300, 
               style = "display: block; margin-left: auto; margin-right:auto")),
               br(),
               br()
             )
           )
    )
)


# Define actions ----------------------------------------------------------
server <- function(input, output, session) {
  #Selecting correct file based on inputs from region and env variable selected
  region_fishmip <- reactive({
    #Get region selected
    name_reg <- input$region_gfdl |> 
      #Change to all lowercase
      str_to_lower() |> 
      #Replaces spaces " " with dashes "-" to identify correct files
      str_replace_all(" ", "-") |> 
      str_replace("'", "")
    #Get env variable selected
    name_var <- input$variable_gfdl
    #Merge region and variable prior to identifying correct file
    sub <- str_c(name_var, "_15arcmin_", name_reg)
    #Identifying correct file
    file_path <- str_subset(var_files, sub)
    return(file_path)
  })
  
  #Loading dataset
  var_fishmip <- reactive({
    #Get full file path to relevant file
    map_file <- file.path(maps_dir, region_fishmip())
    #Load file
    df <- read_csv(map_file) |> 
      drop_na(vals)
    #Getting units from dataset
    unit <- df |> 
      distinct(units) |> 
      drop_na() |> 
      pull()
    #Getting standard name from dataset
    if("standard_name" %in% names(df)){
      std_name <- df |> 
        distinct(standard_name) 
    }else{
      std_name <- df |> 
        distinct(long_name) 
    }
    std_name <- std_name |> 
        drop_na() |>
        pull() |> 
        str_replace_all("_", " ") 
    
    std_name <- paste("Mean", std_name, sep = " ") |>
        str_to_sentence()
    
    #Creating label for figures
    cb_lab <- paste0(input$variable_gfdl, " (", unit, ")")
    
    #Get full file path to relevant file
    ts_file <- file.path(ts_dir, region_fishmip())
    #Load file
    df2 <- read_csv(ts_file)
    
    #Return data frame
    return(list(map_data = df,
                ts_data = df2,
                std_name = std_name,
                fig_label = cb_lab))
  })
  
  # Creating first plot
  output$map_gfdl <- renderPlot({
    df <- var_fishmip()$map_data
    
    #Adjusting map proportions
    minx <- min(df$lon)
    maxx <- max(df$lon)
    miny <- min(df$lat)
    maxy <- max(df$lat)
    #Calculate range
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))
    #Check if map crosses international date line
    if(rangex == 0 & str_detect(input$region_gfdl, "Southern Ocean", 
                                negate = T)){
      df <- df |>
        mutate(lon = lon%%360)
      minx <- min(df$lon)
      maxx <- max(df$lon)
      rangex <- abs(abs(maxx)-abs(minx))
    }
    #Apply scaler function
    if(rangex >= 1.15*rangey){
      ylims <- c(scaler(miny, "min"),
                 scaler(maxy, "max"))
      xlims <- c(scaler(minx, "min", ratio = T),
                 scaler(maxx, "max", ratio = T))
    }else if(rangey >= 1.15*rangex){
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
    
    #Plotting map
    df |> 
      ggplot(aes(x = lon, y = lat, fill = vals)) +
      geom_tile()+
      coord_cartesian()+
      scale_fill_viridis_c(guide = guide_colorbar(ticks.linewidth = 0.75,
                                                  frame.colour = "blue", 
                                                  title.vjust = 0.75),
                           na.value = NA) +
      geom_sf(inherit.aes = F, data = world, lwd = 0.25,
              color = "black", show.legend = F)+
      guides(fill = guide_colorbar(title = var_fishmip()$fig_label, 
                                   title.position = "top", 
                                   title.hjust = 0.5))+
      labs(title = var_fishmip()$std_name)+
      theme_classic()+
      theme(legend.position = "bottom", legend.key.width = unit(4, "cm"),
            plot.title = element_text(size = 15, hjust = 0.5), 
            axis.text.y = element_text(size = 15, angle = 45, hjust = 0.5, 
                                       vjust = 0.5),
            axis.text.x = element_text(size = 15, angle = 45, hjust = 0.5, 
                                       vjust = 0.5),
            axis.title = element_blank(), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))+
      coord_sf(xlims, ylims)
  },
  height = 500, width = 1000)

   output$ts_gfdl <- renderPlot({
     #calculate spatially weighted average of variables selected
     var_fishmip()$ts_data |>
       #Plot weighted means
       ggplot(aes(x = date, y = vals)) +
       geom_line(aes(color = "area weighted mothly mean")) +
       geom_smooth(aes(color = "linear temporal trend"))+
       scale_color_manual(breaks = c("area weighted mothly mean", 
                                     "linear temporal trend"), 
                          values = c("#004488", "#bb5566"))+
       theme_bw() +
       scale_x_date(date_labels = "%b-%Y", date_breaks = "24 months", 
                    expand = expansion(0.02))+
       guides(color = guide_legend(title = element_blank()))+
       labs(title = var_fishmip()$std_name,
            y = var_fishmip()$fig_label)+
       theme(axis.text.y = element_text(size = 14),
             axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, 
                                        size = 14),
             axis.title.y = element_text(size = 15), 
             axis.title.x = element_blank(),
             plot.title = element_text(hjust = 0.5, size = 15),
             legend.position = "bottom", legend.text = element_text(size = 15))
     },
     height = 500, width = 1000)

   #Loading download dataset
   down_fishmip <- reactive({
     #Get full file path to relevant file
     down_file <- file.path(download_dir, region_fishmip())
     #Load file
     down_df <- read_csv(down_file)
   })
   
   output$download_data <- downloadHandler(
     filename = function(){
       region_fishmip()
     },
     #Creating name of download file based on original file name
     content = function(file){
       write_csv(down_fishmip(), file)
     }
   )
}

shinyApp(ui = ui, server = server)