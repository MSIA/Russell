library(tm)
topics <- read.csv(file='tm_wordsonly.csv',head=TRUE,sep=',',check.names=FALSE)
corpus <- Corpus(VectorSource(topics))
data <- tm_map(corpus, PlainTextDocument)
dtm <- DocumentTermMatrix(data)

word.freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
df <- data.frame(word = names(word.freq), freq = word.freq)
years <- c()

#write loop to populate column with years in which each word appeared
for (word in df$word) {
  y <- ''
  word <- as.String(word)
  for (year in names(topics)) {
    year <- as.String(year)
    if (word %in% topics[,year]) {
      y <- paste (y, year, sep = " ", collapse = NULL)
    }
  }
  years <- append(years,y)
}

df$years <- years

write.xlsx(df, file = "TM_WC.xlsx",
           sheetName = "Wordcounts")