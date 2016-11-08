library(readxl)
library(topicmodels)
library(tm)
library(slam)
#row_sums
library(SnowballC)
library(Rmpfr)
library(xlsx)

#Divide annual data into three groups
#Group 1: 1997 - 2002
g1 <- 1997:2002
#Group 2: 2003 - 2008
g2 <- 2003:2008
#Group 3: 2009 - 2015
g3 <- 2009:2015
g <- list(g1, g2, g3)

#Read Excel file for all years
sim.data <-read_excel("1997-2015_FINAL.xlsx")

topTenTermsEachTopic <- character(0)

#Loop through each group of years
for (i in 1:3) {

#Use Abstract column for word cloud and omit NA entries
#data <- sim.data$Abstract
data <- sim.data$Abstract[sim.data$Year %in% g[[i]]]
data <- na.omit(data)

#Create corpus
corpus <- Corpus(VectorSource(data))

#need to fix next 2 lines
#stopwords <- c("you","your","iii")
#corpus <- tm_map(corpus, removeWords, stopwords)

#create dtm
dtmTopicModeling <- DocumentTermMatrix(corpus,control = list(tolower = TRUE,
                                                             removePunctuation = TRUE,
                                                             stopwords = TRUE,
                                                             stemming = TRUE,
                                                             wordLengths = c(3, 30)))
#tf-idf
term_tfidf <- tapply(dtmTopicModeling$v/row_sums(dtmTopicModeling)[dtmTopicModeling$i], 
                     dtmTopicModeling$j, mean) * log2(nDocs(dtmTopicModeling)/col_sums(dtmTopicModeling >0 ))

#only include terms with tf-idf greater than median (exclude very frequent terms)
median_tfidf <- summary(term_tfidf)[3]
dtmTopicModeling <- dtmTopicModeling[, term_tfidf >= median_tfidf]

#Remove rows that contain no non-zero entries
toRemove <- which(row_sums(dtmTopicModeling) == 0,)
dtmTopicModeling <- dtmTopicModeling[row_sums(dtmTopicModeling) > 0,]

#Maximum likelihood to calculate k
harmonicMean <- function(logLikelihoods, precision=2000L) { 
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

burnin = 1000
iter = 1000
keep = 50

#possible k values
sequ <- seq(2, 100, 10)

fitted_many <- lapply(sequ, function(k) LDA(dtmTopicModeling, k = k,
                                            method = "Gibbs",
                                            control = list(burnin = burnin, iter = iter, keep = keep) ))
                                                          
# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

#plot k values
plot(sequ, hm_many, type = "l")
#max k
k <- sequ[which.max(hm_many)]
print(k)

#perform LDA
seedNum <- 42
lda <- LDA(dtmTopicModeling, k = k, method = "Gibbs", control = list(
  burnin = burnin, iter = iter, keep = keep, seed=seedNum))

#Top 10 terms from each topic
topTenTermsEachTopic <- cbind(topTenTermsEachTopic,terms(lda,10))
View(topTenTermsEachTopic)

}

#Save output in excel
write.xlsx(x = topTenTermsEachTopic, file = "topics.xlsx", sheetName = "Sheet1")


