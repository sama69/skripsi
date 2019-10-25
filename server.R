library(shiny)
library(rsconnect)
library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library(arules)
library(arulesViz)
library(ggplot2)

library(readr)
library(RColorBrewer)

library(arules)
library(arulesViz)
library(datasets)

library(e1071)
#library(RTextTools)
library(readxl)
library(tm)
library(dplyr)
library(caret)
# Library for parallel processing

library(tm)
library(readxl)


library("SnowballC")
library("wordcloud")
library("RColorBrewer")





df <- read_excel("datakorupsi.xlsx")
stopwordID <- "stoplist.txt"
cStopwordID<-readLines(stopwordID)

set.seed(1234)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]

df$class <- as.factor(df$label)
corpus <- Corpus(VectorSource(df$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords,cStopwordID)
corpus.clean <- tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus.clean)


# Frequency
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf <- data.frame(word=names(freq), freq=freq)
# Plot Histogram
##################
tdm <-TermDocumentMatrix(corpus.clean)

m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)





############






df.train <- df[1:120,]
df.test <- df[121:150,]

dtm.train <- dtm[1:120,]
dtm.test <- dtm[121:150,]

corpus.clean.train <- corpus.clean[1:120]
corpus.clean.test <- corpus.clean[121:150]

fivefreq <- findFreqTerms(dtm.train, 2)

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))





function(input, output){
  
  
  ##inputan<-reactive(data.frame("Tolak",input$text))
  
  output$dataasli <- renderDataTable( {
    
    
    dfdata <- data.frame(df)
    dfdata
    #inspect(head(sort(income_rules, by='confidence'),10))
    
    
  })
  output$histoplot <- renderPlot(
    
    subset(wf, freq>30)    %>%
      ggplot(aes(word, freq)) +
      geom_bar(stat="identity", fill="darkred", colour="darkgreen") +
      theme(axis.text.x=element_text(angle=45, hjust=1))
  )
  output$wordcloudplot <- renderPlot(
    
    wordcloud(words = d$word, freq = d$freq, min.freq = 2,
              max.words=100, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  )
  
}