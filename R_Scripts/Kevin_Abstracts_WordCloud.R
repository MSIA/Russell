#Kevin - R script to create word counts by year to pipe into Tableau, based off of Final Data
#By year

library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(readxl)
library(xlsx)

#Read Excel file for all years
wcAbstract <- function(year) {
  sim.data <-read_excel("Final_Data_by_years.xlsx", sheet=year)
  data <- sim.data$Abstract
  data <- na.omit(data)
  
  #Create corpus
  corpus <- Corpus(VectorSource(data))
  
  data.text <- tm_map(corpus, PlainTextDocument)
  
  #Remove punctuation
  data.text <- tm_map(data.text, removePunctuation)
  
  #Convert to all lower case
  data.text <- tm_map(data.text, content_transformer(tolower))
  
  #Convert words to their stems (e.g. simulating --> simulate)
  data.text <- tm_map(data.text, stemDocument)
  
  #Create document term matrix
  dtm <- DocumentTermMatrix(data.text)
  
  word.freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
  df <- data.frame(word = names(word.freq), freq = word.freq)
  
  #Remove common words
  stopwords <- c("the", "and", "for", "with", "can", "use", "simul", "model", "system",'that','are', 'this')
  df <- df[!rownames(df) %in% stopwords,]
  df$year<-year
  
  return(df)
}

a <- wcAbstract('1997')
b <- wcAbstract('1998')
c <- wcAbstract('1999')
d <- wcAbstract('2000')
e <- wcAbstract('2001')
f <- wcAbstract('2002')
g <- wcAbstract('2003')
h <- wcAbstract('2004')
i <- wcAbstract('2005')
j <- wcAbstract('2006')
k <- wcAbstract('2007')
l <- wcAbstract('2008')
m <- wcAbstract('2009')
n <- wcAbstract('2010')
o <- wcAbstract('2011')
p <- wcAbstract('2012')
q <- wcAbstract('2013')
r <- wcAbstract('2014')
s <- wcAbstract('2015')
master <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

write.xlsx(master, file = "AbstractWC_Year.xlsx",
           sheetName = "Wordcounts")


