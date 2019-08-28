#This Script is for Tidy tuesday Plasic Vis 

#Libraries 
library(tidyverse)
library(ggthemr)

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
str(mismanaged_vs_gdp)

mismanaged_vs_gdp$Entity <- as.factor(mismanaged_vs_gdp$Entity)
mismanaged_vs_gdp$Code <- as.factor(mismanaged_vs_gdp$Code)
mismanaged_vs_gdp$Year <- as.factor(mismanaged_vs_gdp$Year)
colnames(mismanaged_vs_gdp)[colnames(mismanaged_vs_gdp) == 'Per capita mismanaged plastic waste (kilograms per person per day)'] <- 'mismanaged_plastic_waste'
colnames(mismanaged_vs_gdp)[colnames(mismanaged_vs_gdp) == 'GDP per capita, PPP (constant 2011 international $) (Rate)'] <- 'gdp'
colnames(mismanaged_vs_gdp)[colnames(mismanaged_vs_gdp) == 'Total population (Gapminder)'] <- 'population'
colnames(mismanaged_vs_gdp)[colnames(mismanaged_vs_gdp) == 'Entity'] <- 'country'



#filter 
mismanaged_plastic_data <- mismanaged_vs_gdp %>%
  select(country,gdp,Code, mismanaged_plastic_waste, population) %>%
  filter(!is.na(mismanaged_plastic_waste)) %>%
  filter(!is.na(population)) %>%
  mutate(mismanaged_plastic_waste_per_year = (mismanaged_plastic_waste*population*365)/1000000)

# filter(country == 'China' | country == 'United Kingdom' | country == 'France' | country== 'United States')


waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp$Entity <- as.factor(waste_vs_gdp$Entity)
waste_vs_gdp$Year <- as.factor(waste_vs_gdp$Year)
colnames(waste_vs_gdp)[colnames(waste_vs_gdp) == 'Per capita plastic waste (kilograms per person per day)'] <- 'total_plastic_waste'
colnames(waste_vs_gdp)[colnames(waste_vs_gdp) == 'GDP per capita, PPP (constant 2011 international $) (Rate)'] <- 'gdp'
colnames(waste_vs_gdp)[colnames(waste_vs_gdp) == 'Total population (Gapminder)'] <- 'population'
colnames(waste_vs_gdp)[colnames(waste_vs_gdp) == 'Entity'] <- 'country'

total_plastic_data <- waste_vs_gdp %>%
  select(country, Code, total_plastic_waste, population) %>%
  filter(!is.na(total_plastic_waste)) %>%
  filter(!is.na(population)) %>%
  mutate(total_plastic_waste_per_year = (total_plastic_waste*population*365)/1000000)

#%>%
#  filter(country == 'China' | country == 'United Kingdom' | country == 'France' | country== 'United States')



plastic_data <- merge(mismanaged_plastic_data, total_plastic_data, by=c("country"))

unfiltered_plastic_data <- plastic_data %>%
  mutate(well_managed_waste = ((total_plastic_waste_per_year - mismanaged_plastic_waste_per_year)/total_plastic_waste_per_year)*100)

filtered_plastic_data <- plastic_data %>%
  mutate(well_managed_waste = ((total_plastic_waste_per_year - mismanaged_plastic_waste_per_year)/total_plastic_waste_per_year)*100)%>%
  filter(population.x > 46294840
)

filtered_plastic_data$country <- factor(filtered_plastic_data$country, levels = filtered_plastic_data$country[order(filtered_plastic_data$total_plastic_waste_per_year)])

ggthemr(palette = "dust", layout = "clear")

ggplot(filtered_plastic_data) +
  geom_segment( aes(x= country , xend= country , y= mismanaged_plastic_waste_per_year, yend= total_plastic_waste_per_year), color="#fde725", size = 3) +
  geom_segment( aes(x= country , xend= country , y= mismanaged_plastic_waste_per_year, yend= 0), color="#472c7a", size = 3) +
  coord_flip()+
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  xlab("") +
  ylab("\n Plastic waste per year (1000's of Tonnes)")
  

library(rworldmap)
library(viridis)

Map <- joinCountryData2Map(unfiltered_plastic_data, joinCode = "NAME",
                              nameJoinColumn = "country")
# This will join your malDF data.frame to the country map data

mapCountryData(Map, nameColumnToPlot="well_managed_waste", catMethod = "categorical",
               missingCountryCol = gray(.8), colourPalette = viridis(143), addLegend = F)

