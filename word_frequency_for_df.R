library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
ep= read.csv("file:///D:/HE_Bounce_prediction/HE_approval_note/Applicant_coapplicant_business_information.csv", sep = ',')

### Word frequency
# ep$business=gsub('[[:digit:]]+', '', ep$First_business_2)
# sentences=strsplit(ep$business," ")
# 
# words.freq<-table(unlist(sentences))
# ss=data.frame(cbind(names(words.freq),as.integer(words.freq)))


# Load the data as a corpus
docs <- Corpus(VectorSource(ep$Business_of_applicant_all))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
write.csv(d,"D:/HE_Bounce_prediction/HE_approval_note/keywords.csv")


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=70, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(5, "Dark2"))

