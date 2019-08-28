library(tidyverse)
library(ggthemr)
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

str(wine_ratings)

red_wine_ratings <- wine_ratings %>%
  select(country, points, price, province, variety, title, description) %>%
  filter(!is.na(points)) %>%
  filter(!is.na(price)) %>%
  filter(!is.na(variety)) %>%
  filter(!is.na(country)) %>%
  filter(variety == "Merlot" |variety == "Malbec" | variety == 
           "Cabernet Sauvignon" |variety == "Pinot Noir" | variety == 
           "Zinfandel" | variety == "Syrah"| variety == "Tempranillo" | variety =="Garnacha") %>%
  group_by(country) %>%
  mutate(average_score = mean(points)) %>%
  ungroup()

red_wine_simple <- red_wine_ratings %>%
  select(country, average_score) %>%
  unique()
red_wine_simple$country <- as.factor(red_wine_simple$country )

levels(red_wine_simple$country)[levels(red_wine_simple$country)=="US"] <- "United States"
levels(red_wine_simple$country)[levels(red_wine_simple$country)=="England"] <- "United Kingdom"
as.tibble(red_wine_simple)
library(rworldmap)
library(viridis)
str(red_wine_simple)
Map2 <- joinCountryData2Map(red_wine_simple, joinCode = "NAME",
                           nameJoinColumn = "country")

# This will join your malDF data.frame to the country map data

mapCountryData(Map2, nameColumnToPlot="average_score",xlim=c(-10, 40), ylim=c(35, 70),
               missingCountryCol = gray(.8), colourPalette = magma(7), addLegend = T)



#ggplot(red_wine_ratings, aes(x= price, y= points, colour = variety)) +
#  geom_density(aes(x= price, y= points))


#### Lets look at the red wines of France.
france_red <- red_wine_ratings %>% 
  filter(country == "France")


ggplot(france_red, aes(x= variety, y= points, fill= variety)) +
  geom_violin(alpha=0.6) +
  geom_jitter(size=0.7, alpha=0.5) 

#### Lets try to build a word cloud of tastine notes 


library(SnowballC)
library(wordcloud2)
library(wordcloud)
library(ggwordcloud)
library(tm)
library(stringi)


gsub("[^[:alnum:]\\-\\.\\s]", "", france_red$description)

wineCorpus <- Corpus(VectorSource(france_red$description))
wineCorpus <- tm_map(wineCorpus, PlainTextDocument)
wineCorpus <- tm_map(wineCorpus, removePunctuation)
wineCorpus <- tm_map(wineCorpus, removeWords, stopwords('english'))
wineCorpus <- tm_map(wineCorpus, stemDocument)
wineCorpus <- tm_map(wineCorpus, stripWhitespace)
wineCorpus <- tm_map(wineCorpus, removeNumbers)
wineCorpus <- Corpus(VectorSource(wineCorpus))
wordcloud(wineCorpus, max.words=50,rot.per=0.35, use.r.layout=T)

matriz_terms <- DocumentTermMatrix(wineCorpus)
str(matriz_terms)
words <- matriz_terms$dimnames$Terms
frequency <- matriz_terms$v

word_frequency <- data.frame(words, frequency)
vis_data<-as_tibble(word_frequency)

filtered_data<-vis_data %>%
  filter(words== "aroma" | words == "acidic" |words == "aftertaste" |words == "astringent" |
           words == "balanced" | words == "body" |words == "bouquet" | words == "bright" |
           words == "sweet" | words == "tannic" | words == "tart" | words == "toasty" |
           words == "baked" |words == "big" |words == "bitter" | words == "chewy" |
           words == "complex" |words == "concentrated" | words == "crisp" |words == "dry"|
           words == "earthy" |words == "elegant" |words == "firm" |words == "fresh" |
           words == "full" |words == "green" |words == "hard" |words == "heavy" |
           words == "herbal" |words == "jammy" | words == "lean" |words == "oaky" |
           words == "powerful" | words == "rich" | words == "round" |words == "smooth" |
           words == "soft" | words == "sour" | words == "spicy" | words == "supple" | 
           words == "citrus"| words == "burnt" | words == "caramel" | words == "cherri" |
           words == "clean")


wordcloud2(filtered_data,size = 1.5, color = "skyblue", backgroundColor="black", shape = 'star')
wordcloud2(filtered_data, figPath = "wine-glass-png-clipart-0.png", size = 10, color = "skyblue", backgroundColor="black")
wordcloud2(word_frequency,size = 2, color = "skyblue", backgroundColor="black", shape = 'star')




