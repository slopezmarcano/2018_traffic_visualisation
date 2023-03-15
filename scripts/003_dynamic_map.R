#SLM
#07032023

#--LIBRARIES--#
library(tidyverse)
library(ggtext) #add and modify text to ggpplot
library(showtext) #fonts
font_add_google("Lato")
showtext_auto()
library(ggmap)
library(sp)
library(sf)
library(gganimate)
library(gifski)
library(magick)

#--READ STREET SHAPEFILES--#
logan_road <- st_read("assets/spatial/QSC_Extracted_Data_20230307_081354809000-55796/Queensland_roads_and_tracks.shp") %>%
            filter(ROAD_NAME_ =='Logan Road' & LGA_NAME_L =='Brisbane City')

#--READ BRISBANE SUBURBS--#
brisbane <- st_read("assets/spatial/QSC_Extracted_Data_20230315_145005058000-42104/Locality_Boundaries.shp") %>%
            filter(LGA=='Brisbane City')

#--CREATE A BUFFER AROUND LOGAN ROAD--#
logan_road_buffer <- st_buffer(logan_road, dist = 10)

#--INTERSECT THE BUFFERED LOGAN ROAD WITH BRISBANE SUBURBS--#
brisbane_cut <- st_intersection(logan_road_buffer, brisbane)

#--CROP BRISBANE USING THE INTERSECTION--#
brisbane_cropped <- st_crop(brisbane, brisbane_cut)

#--READ DATASET--#
traffic <- read_csv('assets/df_cleaned.csv')

logan_road_bne <- merge(logan_road, traffic, by.x = "ROAD_NAME_", by.y = "corridor")


# Create a categorical variable based on the am_peak values
logan_road_bne$category <- cut(logan_road_bne$am_peak, 
                               breaks = quantile(logan_road_bne$am_peak, probs = c(0, 0.25, 0.75, 1)),
                               labels = c("low", "medium", "high"),
                               include.lowest = TRUE)

p <- ggplot() +
  geom_sf(data = brisbane_cropped) +
  geom_sf(data = logan_road_bne, aes(color = category)) +
  coord_sf() +
  labs(title = "Month: {frame_time}")

#TODO: #1 develop code to convert the ggplot into a GIF
