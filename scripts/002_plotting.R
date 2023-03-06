#SLM
#07032023

#--LIBRARIES--#
library(tidyverse)
library(ggtext) #add and modify text to ggpplot
library(showtext) #fonts
font_add_google("Lato")
showtext_auto()

#--SETTING THEME AND FONTS--#
theme_set(theme_clean(base_family = "Lato"))

theme_update(
  # Set size 20 and colour for both y and x axes
  axis.title = element_text(color = "#404E4D", size = 20),
  # Axes labels are greyish
  axis.text = element_text(color = "#404E4D"),
  strip.text = element_text(colour = "#404E4D", size=20),
  # Set the size of the axes labels and margins.
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 20, margin = margin(r = 5)),
  # Also, the ticks have a very dark grey color
  axis.ticks = element_line(color = "#333d3d", size = .5),
  # The length of the axis ticks is increased.
  #axis.ticks.length.x = unit(1.3, "lines"),
  #axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 15, 20, 15),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  strip.background = element_rect(fill='grey98', color = "grey98"),
  panel.border = element_rect(color='#e1dfdf', fill= NA, size=1),
  # Customize title appearence
  plot.title = element_text(
    color = "#404E4D", 
    size = 30, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "#656363", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "#656363", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40)), # Large margin on the top of the caption.
  # Remove legend
  legend.position = "none",
  legend.background = element_rect(fill = 'grey98', color = NA), 
  legend.title = element_text(color = "#404E4D", size= 20),
  legend.text =  element_text(color = "#404E4D", size= 18))

#--READ DATASET--#
df_cleaned <- read_csv('assets/df_cleaned.csv')

#--PLOTTING SETTINGS--#
legend_title <- "Period"
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")


#--PLOTTING--#
ggplot(data = filter(df_cleaned, corridor=='Logan Road'), aes(x = factor(Month, levels = month_names), y = am_peak, group = 1)) +
  geom_line(aes(color = "AM Peak"), size = 1.5) +
  geom_line(aes(y = pm_peak , color = "PM Peak"), size=1.5) +
  scale_color_manual(legend_title, values = c("#4FB477", "#A63A50")) +
  labs(title = "AM and PM Peak Variation Across Logan Road",
       x = "Month",
       y = "Peak Volume",
       subtitle = "2018 traffic data for Logan Road. Monthly volume peak volume calculated by QLD Transport",
       caption = "Visualization by S Lopez Marcano • Traffic values from QLD Transport") +
  theme(legend.position=c(0.3,0.2),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave('outputs/logan_road_2018_traffic.pdf', width=12, height=8)
