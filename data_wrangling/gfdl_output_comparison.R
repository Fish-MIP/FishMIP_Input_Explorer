
# Loading libraries -------------------------------------------------------
library(tidyverse)
library(sf)
library(rnaturalearth)
library(cmocean)
library(cowplot)
library(terra)


# Setting up --------------------------------------------------------------
#Location of GFDL outputs
base_dir <- paste0("/rd/gem/public/fishmip/ISIMIP3a/InputData/climate/ocean/",
                   "obsclim")

#Loading map of the world
land <- ne_countries(returnclass = "sf")

#Loading FishMIP regions shapefile
benguela <- paste0("/rd/gem/private/shared_resources/FishMIP_regional_models/",
                   "FishMIP_regional_models.shp") |> 
  read_sf() |> 
  #Keep Benguela only
  filter(str_detect(region, "Benguela"))

#Loading GFDL 0.25 deg outputs for Benguela 
beng_tos_25 <- list.files(base_dir, "tos.*benguela", full.names = T,
                          recursive = T) |> 
  str_subset("maps_data") |> 
  read_csv()

#Loading GFDL 1 deg outputs for the world
world_tos_1 <- list.files(base_dir, "tos.*60arcmin", full.names = T,
                          recursive = T) |> 
  rast()

#Defining outputs folder where figure will be shared
out_folder <- "outputs"
if(!dir.exists(out_folder)){
  dir.create(out_folder)
}


# Extracting data for Benguela --------------------------------------------
#Cropping to extent of Benguela shapefile
beng_tos_1 <- crop(world_tos_1, st_bbox(benguela)) |> 
  #Calculating mean across time 
  mean() |> 
  #Extracting data using Benguela shapefile
  extract(benguela, method = "simple", xy = T, ID = F)


# Creating maps -----------------------------------------------------------
#Getting bounding box for Benguela
beng_box <- st_bbox(benguela)

#Map of Africa to show location of Benguela
inset <- land |> 
  ggplot()+
  geom_sf()+
  geom_sf(inherit.aes = F, data = st_as_sfc(beng_box), color = "#332288", 
          fill = NA, linewidth = 1)+
  lims(x = c(-20, 60), y = c(-40, 40))+
  theme_light()+
  theme(axis.text = element_text(size = 5),
        plot.margin = unit(c(0, 0, 0.1, 0.1), "cm"))

#Define plotting instructions for maps
gg_maps <- list(geom_tile(show.legend = F),  
                #Setting shared colourbar and limits 
                scale_fill_cmocean(name = "thermal", limits = c(15, 23)),
                #Adding land
                geom_sf(inherit.aes = F, data = land),
                #Setting extent of map
                coord_sf(xlim = c(beng_box$xmin, beng_box$xmax),
                         ylim = c(beng_box$ymin, beng_box$ymax)),
                #Changing plot theme
                theme_light(),
                #Change axis label size and removing axis titles
                theme(axis.title = element_blank(), 
                      axis.text = element_text(size = 11)))

#Map 1 deg outputs
beng_map_1 <- beng_tos_1 |> 
  ggplot(aes(x, y, fill = mean))+
  gg_maps
  
#Map 1 deg outputs
beng_map_25 <- beng_tos_25 |> 
  ggplot(aes(lon, lat, fill = vals))+
  gg_maps
  
#Define legend title
leg_lab <- "Temperature (°C)"

#Getting legend for maps
leg <- beng_map_1+
  geom_tile(show.legend = T)+
  #Putting legend at the bottom 
  guides(fill = guide_colorbar(title = leg_lab, title.position = "top", 
                               barwidth = unit(10, units = "cm"), 
                               title.hjust = 0.5, frame.colour = "black", 
                               frame.linewidth = 0.4, ticks.linewidth = 0.5))+
  theme(legend.position = "bottom", legend.text = element_text(size = 11))

#Extract legend
leg <- get_legend(leg)

#Putting maps and legend together
beng_map <- plot_grid(plot_grid(beng_map_1, beng_map_25, ncol = 2, 
                                labels = c("a", "b")),
                      leg, nrow = 2, rel_heights = c(1, 0.2))

#Adding inset
final_map <- ggdraw(beng_map)+
  draw_plot(inset, x = 0.77, y = 0.68, width = 0.3, height = 0.3)
  
#Saving final map
ggsave(file.path(out_folder, "gfdl_comp_res_clim_tos_1961-2010.pdf"), 
       final_map, device = "pdf", width = 10, height = 4.5)


