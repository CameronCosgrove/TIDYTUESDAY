library(tidyverse)
library(ggrepel)
library(ggthemr)


gin <- read_csv("gin.csv")
View(gin)
# Get the world polygon and extract UK
library(maps)
UK <- map_data("world") %>% filter(region=="UK")

data= world.cities %>% filter(country.etc=="UK")

ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill= "#FFE4E1") +
  geom_point(data = gin, aes(x=long, y=lat),size = 5, colour = "#FFB5C5") +
  geom_text_repel( data= subset(gin, Company == "Leith Gin" |
                                  Company == "Edinburgh Gin"|
                                  Company == "Pickering Gin"|
                                  Company == "The Old Curiosity"), aes(x=long, y=lat, label=Company, colour = "#FFB5C5", fontface = "bold", family = "Helvetica"), size = 5, point.padding = 1, nudge_x = 2, colour = "#FFB5C5", direction  = "y" , segment.color = "#FFB5C5", hjust = 0, force = 1) +
  geom_text_repel( data= subset(gin, Company == "Caorunn"), aes(x=long, y=lat, label= Company, colour = "#FFB5C5", fontface = "bold", family = "Helvetica"), segment.color = "#FFB5C5", point.padding = 1, nudge_x = 2, nudge_y = 0.5, size = 5, colour = "#FFB5C5") +
  geom_text_repel( data= subset(gin, Company == "Shetland Reel"), aes(x=long, y=lat, label= Company, colour = "#FFB5C5", fontface = "bold", family = "Helvetica"), segment.color = "#FFB5C5", point.padding = 1, nudge_x = 4, nudge_y = -0.5, size = 5, colour = "#FFB5C5") +
  geom_text_repel( data= subset(gin, Company == "The Botanist"), aes(x=long, y=lat, label= Company, colour = "#FFB5C5", fontface = "bold", family = "Helvetica"), segment.color = "#FFB5C5", point.padding = 1, nudge_x = -2, nudge_y = -0.3, size = 5, colour = "#FFB5C5") +
  geom_text_repel( data= subset(gin, Company == "Rock Rose Gin"), aes(x=long, y=lat, label= Company, colour = "#FFB5C5", fontface = "bold", family = "Helvetica"), segment.color = "#FFB5C5", point.padding = 1, nudge_x = -2, nudge_y = 1, size = 5, colour = "#FFB5C5") +
  geom_text_repel( data= subset(gin, Company == "Kinrara Gin"), aes(x=long, y=lat, label= Company, colour = "#FFB5C5", fontface = "bold", family = "Helvetica"), segment.color = "#FFB5C5", point.padding = 1, nudge_x = -5, nudge_y = 1.5, size = 5, colour = "#FFB5C5") +
  geom_text_repel( data= subset(gin, Company == "The Biggar Gin"), aes(x=long, y=lat, label= Company, colour = "#FFB5C5", fontface = "bold", family = "Helvetica"), segment.color = "#FFB5C5", point.padding = 1, nudge_x = 4, nudge_y = 1.5, size = 5, colour = "#FFB5C5") +
  
  theme_void() + coord_map() + coord_cartesian(ylim = c(55.3,61)) + 
  theme(legend.position="none",
        panel.background = element_rect("#27408B"))
  
  
c("#FFB5C5", "#FFE4E1", "#27408B")


