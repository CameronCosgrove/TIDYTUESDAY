library(tidyverse)
A9_deer_northing <- read_csv("Desktop/A9_deer_northing.csv")
View(A9_deer_northing)

deer_data <- A9_deer_northing %>%
  select(DEER_SPECI : ROAD_NO) %>%
  filter(DEER_SPECI != 'DeSppNK')
