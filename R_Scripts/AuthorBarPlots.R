library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(readxl)
data1 <-read_excel("Authors, Papers, and Affiliations.xlsx")
for (i in 1997:2015){
#Read Excel file for all years


#Use Title column for word cloud and omit NA entries
data = data1[which(data1[,2] == i),3]
data <- gsub(" ","",data)
data <- na.omit(data)

#Create corpus
corpus <- Corpus(VectorSource(data))

data.text <- tm_map(corpus, PlainTextDocument)

#Remove punctuation
data.text <- tm_map(data.text, removePunctuation)


#Create document term matrix
dtm <- DocumentTermMatrix(data.text)

word.freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)

jpeg(sprintf('AuthorsFreq_%d.jpg',i), width = 800)
barplot(word.freq[1:5],main = sprintf('Top 5 Frequent Authors in %d',i), xlab = "Author Names", ylab = "Frequency")
dev.off()
}
