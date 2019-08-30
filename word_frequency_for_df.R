library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(qdap)
ep= read.csv("file:///D:/sample.csv", sep = ',')

# ep$business=gsub('[[:digit:]]+', '', ep$First_business_2)
# sentences=strsplit(ep$business," ")
# 
# words.freq<-table(unlist(sentences))
# ss=data.frame(cbind(names(words.freq),as.integer(words.freq)))

### Remove digit
ep$Critical.Observation=gsub('[[:digit:]]+', '', ep$Critical.Observation)
### Remove punctuation
ep$Critical.Observation=gsub('[[:punct:] ]+',' ',ep$Critical.Observation)
### Remove Stopwords
ep$Critical.Observation= rm_stopwords(ep$Critical.Observation, tm::stopwords("english"))

### Remove punctuation
ep$Critical.Observation=gsub('[[:punct:] ]+',' ',ep$Critical.Observation)


# Load the data as a corpus
docs <- Corpus(VectorSource(ep$Critical.Observation))

### Word frequency
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
write.csv(d,"D:/HE_Bounce_prediction/HE_approval_note/keywords.csv")


wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=70, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(5, "Dark2"))

