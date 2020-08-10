options(repos = "https://cran.r-project.org/")
rm(list = ls())
library(tm)
library(SnowballC)
library(wordcloud)
library(shiny)



df <- read.csv("D:\\R_Study\\debate.csv")
print(df$Text[1])
View(df)

df1 <- df[df$Speaker == 'Trump',]
View(df1)

corp <- Corpus(VectorSource(df1$Text))
inspect(corp[1:5])
writeLines(as.character(corp[1:5]))

ts <- content_transformer(function(x, pattern) gsub (pattern, " ", x))


corp <- tm_map(corp,ts,":")
corp <- tm_map(corp,ts, ",")
corp <- tm_map(corp,ts, "-")
corp <- tm_map(corp,ts, "--")
corp <- tm_map(corp,ts,"\"")

writeLines(as.character(corp[1:5]))

corp <- tm_map(corp,content_transformer(tolower))
corp <- tm_map(corp,removeNumbers)
corp <- tm_map(corp,removeWords,c(stopwords("english"),"can","will","going","now","back","much","say","like","percent","really"))
corp <- tm_map(corp,stripWhitespace)
# corp1 <- tm_map(corp,content_transformer(stemDocument), language="english")
corp <- tm_map(corp, content_transformer(gsub),pattern="countries",replacement="country")
corp <- tm_map(corp, content_transformer(gsub),pattern="companies",replacement="company")
corp <- tm_map(corp,removePunctuation)
corp <- tm_map(corp,stripWhitespace)


writeLines(as.character(corp[1:5]))
dtm <- DocumentTermMatrix(corp)
dtm
inspect(dtm[1:5,1000:1026])
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing = TRUE)
freq[head(ord)]
freq[tail(ord)]

dtmr <- DocumentTermMatrix(corp, control = list(wordlengths = c(4,20),bounds=list(global=c(3,27))))
dtmr
freq <- colSums(as.matrix(dtmr))
ordr <- order(freq, decreasing = TRUE)
freq[head(ordr)]
freq[tail(ordr)]

length(freq)
freq1 <- freq[ordr]

View(freq1)
hi <- data.frame(term=names(freq1),occurances=freq1)
View(hi)
p <- ggplot2::ggplot(data = subset(hi,occurances >40),
  ggplot2::aes(x=term,y=occurances))+
  ggplot2::geom_bar(stat = "identity")+
  ggplot2::theme_minimal()+
  ggplot2::coord_flip()

p


