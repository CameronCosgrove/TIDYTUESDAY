library(tidyverse)
library(maps)

myExportTableTask <- read_csv("Desktop/myExportTableTask.csv")
View(myExportTableTask)
     
DVC <- myExportTableTask
## read in csv data; first column is assumed to be Easting and second Northing

#### Convert OS to lat long ####
## rename columns
colnames(DVC)[c(10, 11)] <- c('Easting', 'Northing')

## libraries
library(rgdal)
library(stringr)

### shortcuts
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

### Create coordinates variable
coords <- cbind(Easting = as.numeric(as.character(DVC$Easting)),
                Northing = as.numeric(as.character(DVC$Northing)))

### Create the SpatialPointsDataFrame
dat_SP <- SpatialPointsDataFrame(coords,
                                 data = DVC,
                                 proj4string = CRS("+init=epsg:27700"))

### Convert
dat_SP_LL <- spTransform(dat_SP, CRS(latlong))

## replace Lat, Long
dat_SP_LL@data$Long <- coordinates(dat_SP_LL)[, 1]
dat_SP_LL@data$Lat <- coordinates(dat_SP_LL)[, 2]


DVC_latlong <- as_tibble(dat_SP_LL)



view(DVC_latlong)

#### Map DVC densities ####

DVC_latlong$Long <- round(DVC_latlong$Long,digits=4)
#DVC_latlong$Long <- as.factor(DVC_latlong$Long)
DVC_latlong$Lat <- round(DVC_latlong$Lat,digits=4)
#DVC_latlong$Lat <- as.factor(DVC_latlong$Lat)



# Bin size control + color palette
ggplot(DVC_latlong, aes(x=Long, y=Lat)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw()+
  borders(database = "world", regions = "UK", colour = "Black", size = 0.3) 


ggplot(DVC_latlong, aes(x=Long, y=Lat) ) +
  geom_hex(bins = 45) +
  scale_fill_distiller(palette = "Spectral") +
  theme_classic() +
  borders(database = "world", regions = "UK", colour = "Black", size = 0.3) 

#### Theme ipsum ####
theme_ipsum <- function(base_family="Arial Narrow", base_size = 11.5,
                        plot_title_family=base_family, plot_title_size = 18,
                        plot_title_face="bold", plot_title_margin = 10,
                        subtitle_family=base_family, subtitle_size = 12,
                        subtitle_face = "plain", subtitle_margin = 15,
                        strip_text_family = base_family, strip_text_size = 12,
                        strip_text_face = "plain",
                        caption_family = base_family, caption_size = 9,
                        caption_face = "italic", caption_margin = 10,
                        axis_text_size = base_size,
                        axis_title_family = subtitle_family, axis_title_size = 9,
                        axis_title_face = "plain", axis_title_just = "rt",
                        plot_margin = margin(30, 30, 30, 30),
                        grid_col = "#cccccc", grid = TRUE,
                        axis_col = "#cccccc", axis = FALSE, ticks = FALSE) {
  
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  
  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())
  
  if (inherits(grid, "character") | grid == TRUE) {
    
    ret <- ret + theme(panel.grid=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.major=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.minor=element_line(color=grid_col, size=0.15))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }
    
  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }
  
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  
  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)
  
  ret
  
}
#### What deer are being hit? ####

str(DVC)
DVC$ROAD_NO <- as.factor(DVC$ROAD_NO)
DVC$INC_MONTH <- as.factor(DVC$INC_MONTH)
DVC$LOCALAUTHO <- as.factor(DVC$LOCALAUTHO)

DVC_ROAD <- DVC %>%
  add_count(ROAD_NO, name = "road_n") %>%
  filter(ROAD_NO != "NotAlloc" & ROAD_NO != "NotAllocated") %>%
  group_by(ROAD_NO) %>%
  filter(road_n > 400)

# With a bit more style

ggplot(DVC_ROAD, aes(x= reorder(ROAD_NO, +road_n), y= road_n) ) +
  geom_segment( aes(x=reorder(ROAD_NO, +road_n),xend=reorder(ROAD_NO, +road_n), y=0, yend=road_n), color="grey") +
  geom_point(size=4, color="#4F94CD") +
  coord_flip() +
  theme_ipsum() +
  xlab("") +
  ylab("")

DVC_incmonth <- DVC %>%
  add_count(INC_MONTH, name = "month_n")

DVC_incmonth$INC_MONTH <- as.numeric(DVC_incmonth$INC_MONTH)

ggplot(DVC_incmonth, aes(x=INC_MONTH, y = month_n)) +
  geom_line(color="#69b3a2") +
  geom_point(color="#69b3a2", size=4) +
  theme_ipsum()+
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(0,5000)+
  xlim(1,12) +
  scale_x_continuous(expand = c(0, 0))
