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

#file="http://portal.sf.utas.edu.au/thredds/catalog/gem/fishmip/ISIMIP3a/InputData/fishing/histsoc/catalog.html?dataset=fishmip/ISIMIP3a/InputData/fishing/histsoc/calibration_catch_histsoc_1850_2004.csv"
#fishmip <- read_csv(file="http://portal.sf.utas.edu.au/thredds/fileServer/gem/fishmip/ISIMIP3a/InputData/fishing/histsoc/calibration_catch_histsoc_1850_2004.csv")

fishmip <- read.csv(file="EEZ_tcb.csv")
fishmip <- fishmip[fishmip$dataType=="per",]
#fishmip <- fishmip[fishmip$model=="BOATS",]
fishmip <- fishmip[fishmip$value !="Inf",]


#multimodel mean & sd
fishmip<-fishmip %>% group_by(year,EEZ,ssp) %>%
  summarise(mean = mean(value), sd = sd(value),median=median(value),min=min(value),max=max(value))

  
fishmip$EEZ<-as.factor(fishmip$EEZ)
# fishmip$model<-as.factor(fishmip$model)
# fishmip$esm<-as.factor(fishmip$esm)

fishmip$ssp<-as.factor(fishmip$ssp)
fishmip$mean<-fishmip$mean*100
fishmip$sd<-fishmip$sd*100

# test the plot
# filtered_data<-subset(fishmip,
#        EEZ %in% "Australia")
# 
# p <- ggplot(filtered_data, aes_string(x="year", y="mean",colour="ssp")) +
#   geom_line(alpha=0.5) +
#   geom_ribbon(aes(ymin=mean - sd, ymax=mean + sd,fill=ssp), alpha=0.2,color=NA)+
#   # #theme(legend.position = "none") +
#   theme_minimal() +
#   ylab("Multimodel mean % change exploitable biomass")
# 



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("FishMIP Ensemble EEZ Projections"),
      selectInput(inputId = "EEZ", label = "EEZ",
                  choices = levels(fishmip$EEZ),
                  selected = "Australia"),
           # selectInput(inputId = "model", "Model",
           #         choices = levels(fishmip$model),
           #         selected = "BOATS"),
           #  selectInput(inputId = "esm", "CMIP6 Model",
           #                    choices = levels(fishmip$esm),
           #                     # multiple = TRUE,
           #                     selected = c("gfdl-esm4")),
      downloadButton(outputId = "download_data", label = "Download"),
    ),
    mainPanel(
      plotlyOutput(outputId = "plot"), br(),
      em("Postive and negative percentages indicate an increase and decrease from the historical reference period (mean 1990-1999)"),
      br(), br(), br(),
      DT::dataTableOutput(outputId = "table")
    )
  )
)


server <- function(input, output) {
  filtered_data <- reactive({
    subset(fishmip,
           EEZ %in% input$EEZ)})
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="year", y="median",colour="ssp")) +
        geom_line(alpha=0.5) +
        geom_ribbon(aes(ymin=min, ymax=max,fill=ssp), alpha=0.2,color=NA)+
        # #theme(legend.position = "none") +
        theme_minimal() +
        ylab("Multimodel mean % change exploitable biomass")

      p
    })
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)