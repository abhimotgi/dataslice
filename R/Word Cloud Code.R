library(wordcloud2)
# devtools::install_github("lchiffon/wordcloud2")

library(tm)
library(readr)
library(dplyr)

medium_data <- read_csv("dataslice/Word Clouds in R/medium_data.csv")
# https://www.kaggle.com/dorianlazar/medium-articles-dataset?select=medium_data.csv

medium.corpus = Corpus(VectorSource(medium_data$title))

removeHTML = function(text){
  text = gsub(pattern = '<.+\\">', '', text)
  text = gsub(pattern = '</.+>', '', text)
  return(text)
}

medium.corpus = medium.corpus %>%
  tm_map(content_transformer(removeHTML)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART"))
# tm_map has other functions for text cleaning


dtm = TermDocumentMatrix(medium.corpus)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Additional text cleaning
df = df %>%
  filter(nchar(as.character(word)) > 2)

# Create wordcloud
wordcloud2(df)





#####

medium_data <- read_csv("medium_data.csv") %>%
  filter(publication == 'UX Collective')
# https://www.kaggle.com/dorianlazar/medium-articles-dataset?select=medium_data.csv

medium.corpus = Corpus(VectorSource(medium_data$title))

removeHTML = function(text){
  text = gsub(pattern = '<.+\\">', '', text)
  text = gsub(pattern = '</.+>', '', text)
  return(text)
}

medium.corpus = medium.corpus %>%
  tm_map(content_transformer(removeHTML)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART"))
# tm_map has other functions for text cleaning


dtm = TermDocumentMatrix(medium.corpus)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Additional text cleaning
df = df %>%
  filter(nchar(as.character(word)) > 2)

# Create wordcloud

uxc.colors = c("#fefefe", "#f4f2a8", "#030303")
uxc.background = "#00ccff"

library(extrafont)
# font_import()
fonts()

wordcloud2(df, 
           color = rep_len(uxc.colors, nrow(df)), 
           backgroundColor = uxc.background,
           fontFamily = "DM Sans",
           # rotateRatio = '0',
           size = 2.5,
           minSize = 5)



###################

medium_data <- read_csv("medium_data.csv") %>%
  filter(publication == 'Towards Data Science')
# https://www.kaggle.com/dorianlazar/medium-articles-dataset?select=medium_data.csv

medium.corpus = Corpus(VectorSource(medium_data$title))

removeHTML = function(text){
  text = gsub(pattern = '<.+\\">', '', text)
  text = gsub(pattern = '</.+>', '', text)
  return(text)
}

medium.corpus = medium.corpus %>%
  tm_map(content_transformer(removeHTML)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART"))
# tm_map has other functions for text cleaning


dtm = TermDocumentMatrix(medium.corpus)
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=(words))

# Additional text cleaning
df = df %>%
  filter(nchar(as.character(word)) > 2)

# Create wordcloud

tds.colors = c("#ffffff")
tds.background = "#365977"

library(extrafont)
# font_import()
fonts()

wordcloud2(df, 
           color = rep_len(tds.colors, nrow(df)), 
           backgroundColor = tds.background,
           fontFamily = "DM Sans",
           rotateRatio = '0',
           size = 2.5,
           minSize = 5)

