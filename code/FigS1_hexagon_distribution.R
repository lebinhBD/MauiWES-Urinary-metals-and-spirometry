library(sf)
library(dplyr)
library(ggplot2)
library(sp)


df_geocode <- read.csv("baseline_geocode.csv")
df_geocode <- df_geocode %>% 
  select(participant_id,
         geo_address,
         x,
         y) %>% 
  rename(
      "geo_lon"=  "x",
      "geo_lat"=  "y")

View(df_geocode)



#### getting the fire perimeter shapefile ####

nc_sp <- sf::st_read("final_perimeter/fire_perimeter.shp")

nc_sp <- as(nc_sp, "Spatial")


spdf <- SpatialPointsDataFrame(coords = df_geocode[ , c("geo_lon", "geo_lat")], 
                               data = df_geocode,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

attributes(nc_sp)$proj4string <- attributes(spdf)$proj4string

nc_sp_pl <- as(nc_sp, "SpatialPolygons")

## define the IDs outside the fire perimeter ## 
whatever <- spdf[is.na(  over(spdf, nc_sp_pl)  ), ] 
whatever <- as.data.frame(whatever)
View(whatever)

## define the IDs inside the fire perimeter ## 
whatever_inside <- spdf[!is.na(  over(spdf, nc_sp_pl)  ), ]
whatever_inside <- as.data.frame(whatever_inside)

View(whatever_inside)

#### getting the shapefile for HI ####

usa_sf <- sf::st_read("gadm41_USA_shp/gadm41_USA_2.shp")

hw_sf <- usa_sf |> subset(NAME_1 == "Hawaii")

hw_sp <- as(hw_sf, "Spatial")

#### big geographical map for HI ####

p1 <- ggplot2::ggplot() +
  
  geom_polygon(data = hw_sp,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "#e3ffdb",
               color = "black") +

geom_hex(data = df_geocode,
         mapping = aes(x = geo_lon,
                       y = geo_lat),
         # bins = 1000,
         binwidth = 0.009,
         color = "black",
         alpha = 0.8) + 
  
  scale_fill_viridis_c(option = "plasma",
                       name = "Count",
                       limits = c(0,150),
                       breaks = seq(0,150, by = 25),
                       oob = scales::squish,
                       guide = guide_colourbar(
                         
                         title.position="top",
                         frame.colour = "black",
                         frame.linetype = "solid",
                         ticks.linewidth = 0.5,
                         ticks.colour = "black",
                         # ticks.length = 0,
                         theme = theme(
                          legend.ticks = element_blank(),
                          # legend.ticks.length = unit(0,"cm"),
                           legend.key.width  = unit(0.8, "cm"),
                           legend.key.height = unit(15, "cm"),
                           # legend.ticks.length = unit(0.5, "cm"),
                           legend.title = element_text(size = 15),
                           legend.text = element_text(size = 15)
                         ))) +
  
  geom_polygon(data = nc_sp,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "coral",
               alpha = 0.8,
               linewidth = 1,
               color = "red") +
  
  theme_bw() +
  
  coord_map(xlim = c(-156.75, -155.95),
            ylim = c(20.5, 21.05))

# p1
ggsave(filename = "maui_FULL.png",
       plot = p1,
       width = 10,
       height = 10,
       dpi = 300,
       units = "in")

#### subset geographical map inside the fire perimeter in Lahaina ####

p2 <- ggplot2::ggplot() +
  
  geom_polygon(data = hw_sp,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "#e3ffdb",
               color = "black") +
  
  
  geom_polygon(data = nc_sp,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "coral",
               alpha = 0.8,
               linewidth = 1,
               color = "red") +
  
geom_hex(data = df_geocode,
         mapping = aes(x = geo_lon,
                       y = geo_lat),
         # bins = 1000,
         binwidth = 0.003,
         color = "black",
         alpha = 0.8,
         show.legend = F) + 
  
  
  scale_fill_viridis_c(option = "plasma") +
  
  
  theme_bw() +
  
  coord_map(xlim = c(-156.68777, -156.64697),
            ylim = c(20.84428, 20.90857)) 

p2

ggsave(filename = "Lahaina.png",
       plot = p2, 
       width = 10,
       height = 10,
       dpi = 300,
       units = "in")


#### subset geographical map inside the fire perimeter in Kula ####
p3 <- ggplot2::ggplot() +
  geom_polygon(data = hw_sp,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "#e3ffdb",
               color = "black") +

geom_hex(data = df_geocode,
         mapping = aes(x = geo_lon,
                       y = geo_lat),
         # bins = 1000,
         binwidth = 0.003,
         color = "black",
         alpha = 0.8,
         show.legend = F) + 
  scale_fill_viridis_c(option = "plasma") +
  theme_bw() +
  coord_map(xlim = c(-156.35, -156.3),
            ylim = c(20.746, 20.802)) 


p3

#### combining full map and two geographical maps in Lahaina/Kula into a big map ####

library(patchwork)

p1 + 
  annotate("rect",
           ymin = 20.75,
           ymax = 20.8,
           xmin = -156.35,
           xmax = -156.3,
           color = "red",
           linewidth = 1,
           fill = "coral",
           alpha = 0.8) +
  
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15)) +
  
  geom_segment(aes(x = -156.66,
                   y = 20.84,
                   xend = -156.63,
                   yend = 20.75),
               arrow = arrow(),
               color = "red",
               linewidth = 1)  +
  
  
  geom_segment(aes(x = -156.29,
                   y = 20.8,
                   xend = -156.15,
                   yend = 20.9),
               arrow = arrow(),
               color = "red",
               linewidth = 1)  +
  
  xlab("Longitude") +
  
  ylab("Latitude") +
  
  inset_element(p2 + 
                  theme_void() + 
                  
                  theme(plot.background = element_rect(fill = "white",
                                                       color = "black",
                                                       linewidth = 1),
                        panel.border = element_rect(fill = NA, 
                                                    linewidth = 1,
                                                    color = "black")), 
                left = 0, 
                bottom = 0, 
                right = 0.4, 
                top = 0.45) +
  
  inset_element(p3 + 
                  
                  theme_void() + 
                  
                  theme(plot.background = element_rect(fill = "white",
                                                       color = "black",
                                                       linewidth = 1),
                        panel.border = element_rect(fill = NA, 
                                                    linewidth = 1,
                                                    color = "black")), 
                left = 0.2, 
                bottom = 1, 
                right = 1.5, 
                top = 0.7) 


ggsave(filename = "maui_bin_FULL.png",
       width = 10,
       height = 10,
       dpi = 300,
       units = "in")




