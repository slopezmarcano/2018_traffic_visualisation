#SLM
#07032023

#--LIBRARIES--#
library(tidyverse)
#library(ggtext) #add and modify text to ggpplot
#library(showtext) #fonts
#font_add_google("Lato")
#showtext_auto()
#library(ggmap)
library(sp)
library(sf)
library(gganimate)
library(transformr)
#library(gifski)
#library(magick)

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

#--READ TRAFFIC DATASET--#
traffic <- read_csv('assets/df_cleaned.csv')

#--MERGE LOGAN ROAD SHAPEFILE WITH TRAFFIC DATA--#
logan_road_bne <- merge(logan_road, traffic, by.x = "ROAD_NAME_", by.y = "corridor")


#--CREATE CATEGORICAL VARIABLE BASED ON AM_PEAK--#
logan_road_bne$traffic_level <- cut(logan_road_bne$am_peak, 
                               breaks = quantile(logan_road_bne$am_peak, probs = c(0, 0.25, 0.75, 1)),
                               labels = c("Low Volume", "Medium Volume", "High Volume"),
                               include.lowest = TRUE)

#--CONVERT MONTH TO FACTOR AND ASSIGN ORDER--#
logan_road_bne$Month <- factor(logan_road_bne$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#--DEFINE THE TRAFFIC COLOURS TO EACH CATEGORY--#
traffic_colors <- c("Low Volume" = "green", "Medium Volume" = "orange", "High Volume" = "red")

#--GGPLOT AND GGANIMATE--#
p <- ggplot() +
  geom_sf(data = brisbane_cropped, colour="#585656", fill="grey98") +
  geom_sf(data = logan_road_bne, aes(color = traffic_level)) +
  scale_color_manual(values = traffic_colors) +
  geom_sf_text(data=brisbane_cropped, aes(label = LOCALITY), size=2, color ="black")+
  #coord_sf() +
  labs(x= '',
      y='',
      title = 'Month: {closest_state}',
      subtitle='',
      caption = "Visualisation by Sebastian Lopez Marcano 🀰") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  transition_states(logan_road_bne$Month, transition_length = 1)

#--ANIMATION--#
animate(p)

#--ANIMATION SAVE--#
anim_save('outputs/bne_2018_visualisation.gif', animation = last_animation())