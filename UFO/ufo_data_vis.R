#### Library ####
library(tidyverse)
library(maps)
library(chron)
library(lubridate)
library(anytime)
library(ggplot2)
library(gganimate)
library(grid)
library(emoGG)
#### data load ####
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")


# I want to make an animated map showing the location and shape of ufo sightings
# in the UK over the years

#### tidy data set ####
gb_ufo <- ufo_sightings %>%
  filter(country == "gb") %>%
  mutate("date" = as.POSIXct(date_time,format="%m/%d/%Y %H:%M"))%>%
  mutate("year_" = lubridate::year(date)) #I am so clever :) 

gb_ufo$year_ <- as.factor(gb_ufo$year_)



 #### Now time to plot ####
emoji_search("Spaceship")
(ufo_map <- ggplot(gb_ufo, aes(x = longitude, y = latitude)) + 
   # Specify the UK and set the boarder size and colour
   borders(database = "world", regions = "UK", colour = "#a0d29e", size = 0.3) +  
   #make the title bigger
   theme( panel.background = element_rect(fill = "#225149", colour = NA),
         plot.background = element_rect(fill = "#225149", colour = NA),
         panel.grid.major = element_line(colour = NA, size = 0.2),
         panel.grid.minor =  element_line(colour = NA, size = 0.5),
         plot.title = element_text(color="white", size=40, face="bold", hjust = 0.5)) +
   #define the scale for the point sizes
   # Change the colour and transparency of the plotted occurrence points 
   geom_emoji(emoji ="1f47d", size = 0.04) +
   #now the animation part +
   transition_manual(year_) +
   labs(title = '{current_frame}') +
  xlab(NULL) +
  ylab(NULL)) +
  annotation_custom(grid.text(label = 'Data from NUFORC @CameronCosgrov3', x=0.7,  y=0.5, gp=gpar(col="#91b09d", fontsize=14, fontface="bold")))

(ufo_gif <- animate(ufo_map, 1000, duration = 20, detail = 7, width = 300, height = 400))
anim_save(animation = ufo_gif, filename = 'UFO.gif')
view(year)
str(gb_ufo)
