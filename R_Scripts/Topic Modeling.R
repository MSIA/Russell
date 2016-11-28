#install.packages("tm")
library(tm)
library(slam)
#install.packages("lda")
#install.packages("topicmodels")
#library(lda)
library(gsl)
library(topicmodels)
#install.packages("RWeka")
library(RWeka)
library(xlsx)
library(plyr)
install.packages("Rmpfr")
library(rmpfr)

# Function for calculating harmonic mean of the topic distribution

harmonicMean <- function(logLikelihoods, precision=2000L) {
  llMed <- median (logLikelihoods)
  as.double(llMed - log(mean(exp(-logLikelihoods + llMed))))}

# Read in data
data <- read.xlsx(file="1997-2015_FINAL.xlsx", sheetName = "Sheet1", stringsAsFactors = FALSE)
abstracts <- tolower(data[1:nrow(data),7])

# Remove stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
abstracts <- stringr::str_replace_all(abstracts, stopwords_regex, '')

# Create corpus
corpus <- Corpus(VectorSource(abstracts))
corpus <- tm_map(corpus,content_transformer(tolower),lazy=TRUE)
corpus[[1]]$content

# Tokenizer, can be used for bigrams/n-grams
# BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

# Create document term matrix
# "Bounds" parameter specifies minimum and maximum number of times each word/token is allowed to appear in given set of documents
abs_DTM <- DocumentTermMatrix(corpus, control = list(stopwords=TRUE,
                                                     removePunctuation=TRUE,
                                                     removeNumbers=TRUE,
                                                     bounds = list(global = c(2,4200))))

# Sanity checks
dim(abs_DTM)
summary(col_sums(abs_DTM))
abs_DTM$dimnames$Terms

# Calculate tf-idf scores and remove those less than the median
term_tfidf <- tapply(abs_DTM$v/row_sums(abs_DTM)[abs_DTM$i], abs_DTM$j, mean) *
  log2(nDocs(abs_DTM)/col_sums(abs_DTM > 0))

tfidf_sum <- summary(term_tfidf)

abs_DTM2 <- abs_DTM[,term_tfidf >= tfidf_sum[3]]
abs_DTM2 <- abs_DTM2[row_sums(abs_DTM2) > 0,]

# More summary statistics/sanity checks
summary(col_sums(abs_DTM2))
dim(abs_DTM2)
abs_DTM2$dimnames$Terms

# Parameters for Gibbs sampling (not used here; VEM is used instead)
burnin <- 1000
iter <- 1000
keep <- 50

# Range of number of topics to try
seq <- seq(18,70,2)

# For each of the number of topics, do LDA
abs_TM <- lapply(seq, function(k) LDA(abs_DTM2,k=k))

# For each of the calculated topic models, extract the log likelihoods
# Then find the corresponding harmonic means and extract the highest
logliks <- lapply(abs_TM, function(L) L@loglikelihood)
hm <- sapply(logliks, function(z) harmonicMean(z))
k <- seq[which.max(hm)]


# Output topics and terms
topics <- topics(abs_TM,1)
terms <- terms(abs_TM,10)

terms
count(topics)