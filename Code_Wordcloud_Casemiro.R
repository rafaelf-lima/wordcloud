library(tidyverse)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(rtweet)
library(wordcloud2)

auth_setup_default()

case_tweets <- search_tweets("#GraciasCasemiro", n = 12000, include_rts = FALSE, lang = "es")

names(case_tweets)


case_text <- iconv(case_tweets$text, to = "utf-8")
case_text <- Corpus(VectorSource(case_text))

case_text <- tm_map(case_text, tolower)
case_text <- tm_map(case_text, removePunctuation)
case_text <- tm_map(case_text, removeNumbers)
case_text2 <- tm_map(case_text, removeWords, stopwords("spanish"))
inspect(case_text2[1:10])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
case_text2 <- tm_map(case_text2, content_transformer(removeURL))
case_text2 <- tm_map(case_text2, gsub, pattern = 'stocks', replacement = 'stock')
case_text2 <- tm_map(case_text2, removeWords, c("graciascasemiro", "casemiro"))
case_text2 <- tm_map(case_text2, stripWhitespace)


case_dtm <- TermDocumentMatrix(case_text2)
case_dtm <- as.matrix(case_dtm)

case_dtm[1:10, 1:20]

bp <- rowSums(case_dtm)
bp <- subset(bp, bp > 25)
bp
barplot(bp, las = 2, col = rainbow(50))


bp <- sort(rowSums(case_dtm), decreasing = TRUE)
set.seed(222)

#bp <- data.frame(names(bp), bp)
#colnames(bp) <- c("Word", "Freq")

wordcloud(words = names(bp), freq = bp, random.order = FALSE, max.words = 150, min.freq = 20)

bp <- data.frame(names(bp), bp)
colnames(bp) <- c('Word', 'Freq')
bp <- filter(bp, Freq > 21)

view(bp)
str(bp)

wordcloud2(bp,  size = 1, color = "white", fontWeight = "bold", backgroundColor="black")
